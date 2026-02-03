;;; -*- Gerbil -*-
;;; Core S3 client — signing, REST requests, error handling

(import :std/net/s3/sigv4
        :std/net/request
        :std/net/uri
        :std/crypto/digest
        :std/text/hex
        :std/markup/xml
        :std/error
        :std/sugar
        :std/srfi/19
        :gerbil-aws/aws/creds
        :gerbil-aws/s3/xml)
(export s3-client s3-client?
        s3-client-endpoint s3-client-access-key
        s3-client-secret-key s3-client-region
        s3-client-token
        S3Client
        S3ClientError s3-client-error?
        s3-request s3-request/xml s3-request/check)

(defstruct s3-client (endpoint access-key secret-key region token)
  final: #t)

(deferror-class (S3ClientError Error) () s3-client-error?)

(defraise/context (raise-s3-error where message irritants ...)
  (S3ClientError message irritants: [irritants ...]))

;; Precomputed empty SHA256
(def emptySHA256 (sha256 #u8()))

;; Create an S3 client with credential resolution
(def (S3Client
       endpoint: (endpoint #f)
       profile: (profile #f)
       access-key: (access-key #f)
       secret-key: (secret-key #f)
       region: (region #f)
       token: (token #f))
  (let-values (((resolved-access-key resolved-secret-key resolved-region resolved-token)
                (aws-resolve-credentials profile)))
    (let ((access-key (or access-key resolved-access-key))
          (secret-key (or secret-key resolved-secret-key))
          (region (or region resolved-region))
          (token (or token resolved-token))
          (endpoint (or endpoint "s3.amazonaws.com")))
      (unless access-key
        (raise-s3-error S3Client "Must provide access key" "access-key"))
      (unless secret-key
        (raise-s3-error S3Client "Must provide secret key" "secret-key"))
      (make-s3-client endpoint access-key secret-key region token))))

;; Make a signed REST request to the S3 API
;; verb: HTTP verb symbol (GET, PUT, DELETE, HEAD)
;; bucket: bucket name (optional, for virtual-hosted style)
;; key: object key (optional, becomes the URI path)
;; query: alist of query params [["key" :: "value"] ...]
;; body: request body (string or u8vector, optional)
;; content-type: content type for body
;; extra-headers: additional headers [["key" :: "value"] ...]
;; Returns the raw request object
(def (s3-request client
       verb: (verb 'GET)
       bucket: (bucket #f)
       key: (key #f)
       query: (query #f)
       body: (body #f)
       content-type: (content-type #f)
       extra-headers: (extra-headers []))
  (using (client :- s3-client)
    (let* ((now (current-date))
           (ts (date->string now "~Y~m~dT~H~M~SZ"))
           (scopets (date->string now "~Y~m~d"))
           (scope (string-append scopets "/" client.region "/s3"))
           (body-bytes (cond
                         ((not body) #f)
                         ((u8vector? body) body)
                         ((string? body) (string->bytes body))
                         (else (error "body must be string or u8vector" body))))
           (body-hash (if body-bytes (sha256 body-bytes) emptySHA256))
           ;; Virtual-hosted-style: bucket.s3.region.amazonaws.com
           (host (if bucket
                   (string-append bucket "." client.endpoint)
                   client.endpoint))
           (path (if key
                   (string-append "/" key)
                   "/"))
           (headers [["Host" :: host]
                     ["x-amz-date" :: ts]
                     ["x-amz-content-sha256" :: (hex-encode body-hash)]])
           (headers (if content-type
                      (append headers [["Content-Type" :: content-type]])
                      headers))
           (headers (if client.token
                      (append headers [["X-Amz-Security-Token" :: client.token]])
                      headers))
           (headers (if (null? extra-headers)
                      headers
                      (append headers extra-headers)))
           (creq (aws4-canonical-request
                   verb: verb
                   uri: path
                   query: query
                   headers: headers
                   hash: body-hash))
           (auth (aws4-auth scope creq ts headers
                            client.secret-key client.access-key))
           (headers [["Authorization" :: auth] :: headers])
           (url (string-append "https://" host path)))
      (case verb
        ((GET)
         (http-get url headers: headers params: query))
        ((PUT)
         (http-put url headers: headers params: query data: (or body-bytes "")))
        ((DELETE)
         (http-delete url headers: headers params: query))
        ((HEAD)
         (http-head url headers: headers params: query))
        (else
         (error "Bad request verb" verb))))))

;; Make an S3 request and parse the XML response
;; Returns a hash table from the parsed XML
(def (s3-request/xml client
       verb: (verb 'GET)
       bucket: (bucket #f)
       key: (key #f)
       query: (query #f)
       body: (body #f)
       content-type: (content-type #f)
       extra-headers: (extra-headers []))
  (let* ((req (s3-request client
                verb: verb bucket: bucket key: key
                query: query body: body
                content-type: content-type
                extra-headers: extra-headers))
         (status (request-status req)))
    (if (and (fx>= status 200) (fx< status 300))
      (let* ((content (request-text req))
             (xml (s3-parse-xml content))
             (result (s3-response->hash xml)))
        (request-close req)
        result)
      ;; Error response — try to parse XML error
      (let ((content (request-text req)))
        (request-close req)
        (let (xml (try (s3-parse-xml content)
                    (catch (_) #f)))
          (if xml
            (let* ((err (s3-response->hash xml))
                   (code-str (if (and (hash-table? err) (hash-get err 'Code))
                               (hash-get err 'Code)
                               "Unknown"))
                   (msg-str (if (and (hash-table? err) (hash-get err 'Message))
                              (hash-get err 'Message)
                              content)))
              (raise-s3-error s3-request/xml msg-str code-str status))
            (raise-s3-error s3-request/xml
                            (string-append "HTTP " (number->string status))
                            content)))))))

;; Make an S3 request and check for success (2xx status)
;; Returns the raw request object on success, raises on error
(def (s3-request/check client
       verb: (verb 'GET)
       bucket: (bucket #f)
       key: (key #f)
       query: (query #f)
       body: (body #f)
       content-type: (content-type #f)
       extra-headers: (extra-headers []))
  (let* ((req (s3-request client
                verb: verb bucket: bucket key: key
                query: query body: body
                content-type: content-type
                extra-headers: extra-headers))
         (status (request-status req)))
    (if (and (fx>= status 200) (fx< status 300))
      req
      ;; Error response
      (let ((content (request-text req)))
        (request-close req)
        (let (xml (try (s3-parse-xml content)
                    (catch (_) #f)))
          (if xml
            (let* ((err (s3-response->hash xml))
                   (code-str (if (and (hash-table? err) (hash-get err 'Code))
                               (hash-get err 'Code)
                               "Unknown"))
                   (msg-str (if (and (hash-table? err) (hash-get err 'Message))
                              (hash-get err 'Message)
                              content)))
              (raise-s3-error s3-request/check msg-str code-str status))
            (raise-s3-error s3-request/check
                            (string-append "HTTP " (number->string status))
                            content)))))))
