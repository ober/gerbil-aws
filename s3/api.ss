;;; -*- Gerbil -*-
;;; Core S3 client — signing, REST requests, error handling
;;; Supports persistent HTTPS connections with HTTP/1.1 keep-alive

(import :std/net/s3/sigv4
        :std/net/request
        :std/net/uri
        :std/crypto/digest
        :std/text/hex
        :std/text/utf8
        :std/text/zlib
        :std/markup/xml
        :std/error
        :std/sugar
        :std/format
        :std/srfi/19
        :gerbil-aws/aws/creds
        :gerbil-aws/s3/xml
        :gerbil-aws/s3/connection)
(export s3-client s3-client?
        s3-client-endpoint s3-client-access-key
        s3-client-secret-key s3-client-region
        s3-client-token s3-client-conn
        S3Client S3Client-close!
        S3ClientError s3-client-error?
        s3-request s3-request/xml s3-request/check
        resp-status resp-status-text resp-headers
        resp-content resp-text resp-close)

(defstruct s3-client (endpoint access-key secret-key region token conn)
  final: #t)

(deferror-class (S3ClientError Error) () s3-client-error?)

(defraise/context (raise-s3-error where message irritants ...)
  (S3ClientError message irritants: [irritants ...]))

;; Precomputed empty SHA256
(def emptySHA256 (sha256 #u8()))

;; Create an S3 client with credential resolution
;; When keep-alive?: #t (default), uses persistent HTTPS connections
(def (S3Client
       endpoint: (endpoint #f)
       profile: (profile #f)
       access-key: (access-key #f)
       secret-key: (secret-key #f)
       region: (region #f)
       token: (token #f)
       keep-alive?: (keep-alive? #t))
  (let-values (((resolved-access-key resolved-secret-key resolved-region resolved-token)
                (aws-resolve-credentials profile)))
    (let ((access-key (or access-key resolved-access-key))
          (secret-key (or secret-key resolved-secret-key))
          (region (or region resolved-region))
          (token (or token resolved-token))
          (endpoint (or endpoint
                        (string-append "s3." (or region resolved-region "us-east-1")
                                       ".amazonaws.com"))))
      (unless access-key
        (raise-s3-error S3Client "Must provide access key" "access-key"))
      (unless secret-key
        (raise-s3-error S3Client "Must provide secret key" "secret-key"))
      (make-s3-client endpoint access-key secret-key region token
                      (if keep-alive? 'lazy #f)))))

;; Close the client's persistent connection (if any)
(def (S3Client-close! client)
  (using (client :- s3-client)
    (when (s3-conn? client.conn)
      (s3-conn-close! client.conn)
      (set! client.conn #f))))

;; Ensure the persistent connection is open for the given host.
;; Returns the connection, or #f if keep-alive is disabled.
(def (s3-client-ensure-conn! client host)
  (using (client :- s3-client)
    (cond
      ;; Keep-alive disabled
      ((not client.conn) #f)
      ;; Lazy init or reconnect needed
      ((or (eq? client.conn 'lazy)
           (not (s3-conn-alive? client.conn))
           (not (equal? (s3-conn-host client.conn) host)))
       ;; Close old connection if any
       (when (s3-conn? client.conn)
         (s3-conn-close! client.conn))
       (let ((conn (with-catch
                     (lambda (e) #f)  ; fall back to non-persistent on connect failure
                     (cut s3-conn-open host))))
         (set! client.conn (or conn #f))
         conn))
      ;; Already connected to the right host
      (else client.conn))))

;;; --- Response adapter functions ---
;;; Work with both :std/net/request `request` and our `s3-response`

(def (resp-status r)
  (if (s3-response? r) (s3-response-status r) (request-status r)))

(def (resp-status-text r)
  (if (s3-response? r) (s3-response-status-text r) (request-status-text r)))

(def (resp-headers r)
  (if (s3-response? r) (s3-response-headers r) (request-headers r)))

(def (resp-content r)
  (if (s3-response? r)
    ;; Persistent connection: decompress gzip/deflate like :std/net/request does
    (let* ((body (s3-response-body r))
           (hdrs (s3-response-headers r))
           (enc-pair (assoc "Content-Encoding" hdrs))
           (enc (and enc-pair (cdr enc-pair))))
      (if (and body enc (member enc '("gzip" "deflate")))
        (uncompress body)
        body))
    (request-content r)))

(def (resp-text r)
  (if (s3-response? r)
    (let ((body (s3-response-body r)))
      (if body (utf8->string body) ""))
    (request-text r)))

(def (resp-close r)
  (if (s3-response? r) (void) (request-close r)))

;;; --- Core request functions ---

;; Build signed headers for an S3 request.
;; Returns (values host path signed-headers body-bytes)
(def (s3-build-signed-request client verb bucket key query body content-type extra-headers)
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
           (headers [["Authorization" :: auth] :: headers]))
      (values host path headers body-bytes))))

;; Convert [key :: value] pairs to (key . value) alist for persistent connection
(def (headers->alist headers)
  (map (lambda (h) (cons (car h) (cdr h))) headers))

;; Build the target path with query string for persistent connection
(def (build-target path query)
  (if (and query (pair? query))
    (string-append path "?" (form-url-encode query #f))
    path))

;; Make a signed REST request to the S3 API
;; Returns a request object (non-persistent) or s3-response (persistent)
(def (s3-request client
       verb: (verb 'GET)
       bucket: (bucket #f)
       key: (key #f)
       query: (query #f)
       body: (body #f)
       content-type: (content-type #f)
       extra-headers: (extra-headers []))
  (let-values (((host path headers body-bytes)
                (s3-build-signed-request client verb bucket key query
                                        body content-type extra-headers)))
    (let ((conn (s3-client-ensure-conn! client host)))
      (if conn
        ;; Persistent connection path
        (let ((target (build-target path query))
              (hdr-alist (headers->alist headers))
              (content-length (if body-bytes
                                (number->string (u8vector-length body-bytes))
                                "0")))
          ;; Add Content-Length for methods with body
          (let ((hdr-alist (if (memq verb '(PUT POST))
                             (cons (cons "Content-Length" content-length) hdr-alist)
                             hdr-alist)))
            (try
              (s3-conn-request! conn verb target hdr-alist body-bytes)
              (catch (e)
                ;; Connection died — close, disable keep-alive for this request,
                ;; and fall back to single-use connection
                (using (client :- s3-client)
                  (when (s3-conn? client.conn)
                    (s3-conn-close! client.conn))
                  (set! client.conn 'lazy))
                (s3-request-fallback verb host path headers body-bytes query)))))
        ;; Non-persistent fallback
        (s3-request-fallback verb host path headers body-bytes query)))))

;; Fallback: use :std/net/request (one connection per request)
(def (s3-request-fallback verb host path headers body-bytes query)
  (let ((url (string-append "https://" host path)))
    (case verb
      ((GET)
       (http-get url headers: headers params: query))
      ((PUT)
       (http-put url headers: headers params: query data: (or body-bytes "")))
      ((POST)
       (http-any 'POST url headers: headers params: query data: (or body-bytes "")))
      ((DELETE)
       (http-delete url headers: headers params: query))
      ((HEAD)
       (http-head url headers: headers params: query))
      (else
       (error "Bad request verb" verb)))))

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
         (status (resp-status req)))
    (if (and (fx>= status 200) (fx< status 300))
      (let* ((content (resp-text req))
             (xml (s3-parse-xml content))
             (result (s3-response->hash xml)))
        (resp-close req)
        result)
      ;; Error response — try to parse XML error
      (let ((content (resp-text req)))
        (resp-close req)
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
;; Returns the response on success, raises on error
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
         (status (resp-status req)))
    (if (and (fx>= status 200) (fx< status 300))
      req
      ;; Error response
      (let ((content (resp-text req)))
        (resp-close req)
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
