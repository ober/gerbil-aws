;;; -*- Gerbil -*-
;;; Lambda client — REST JSON API
;;; Lambda uses a REST API with path-based routing, not X-Amz-Target

(import :std/net/s3/sigv4
        :std/net/request
        :std/crypto/digest
        :std/text/hex
        :std/text/json
        :std/misc/bytes
        :std/error
        :std/sugar
        :std/srfi/19
        :gerbil-aws/aws/creds)
(export lambda-client lambda-client?
        LambdaClient
        LambdaClientError lambda-client-error?
        lambda-rest-request)

(defstruct lambda-client (endpoint access-key secret-key region token)
  final: #t)

(deferror-class (LambdaClientError Error) () lambda-client-error?)

(defraise/context (raise-lambda-error where message irritants ...)
  (LambdaClientError message irritants: [irritants ...]))

;; Precomputed empty SHA256
(def emptySHA256 (sha256 #u8()))

(def (LambdaClient
       profile: (profile #f)
       region: (region #f)
       access-key: (access-key #f)
       secret-key: (secret-key #f)
       token: (token #f))
  (let-values (((resolved-access-key resolved-secret-key resolved-region resolved-token)
                (aws-resolve-credentials profile)))
    (let ((access-key (or access-key resolved-access-key))
          (secret-key (or secret-key resolved-secret-key))
          (region (or region resolved-region))
          (token (or token resolved-token)))
      (unless access-key
        (raise-lambda-error LambdaClient "Must provide access key" "access-key"))
      (unless secret-key
        (raise-lambda-error LambdaClient "Must provide secret key" "secret-key"))
      (make-lambda-client
        (string-append "lambda." region ".amazonaws.com")
        access-key secret-key region token))))

;; Make a signed REST request to the Lambda API
;; verb: HTTP verb (GET, POST, PUT, DELETE)
;; path: URI path (e.g., "/2015-03-31/functions")
;; query: optional query params alist
;; payload: optional hash table or u8vector for body
;; Returns parsed JSON hash table
(def (lambda-rest-request client
       verb: (verb 'GET)
       path: path
       query: (query #f)
       payload: (payload #f))
  (using (client :- lambda-client)
    (let* ((body-str (cond
                       ((not payload) #f)
                       ((u8vector? payload) #f) ;; raw bytes
                       ((hash-table? payload)
                        (call-with-output-string
                          (lambda (p) (write-json payload p))))
                       ((string? payload) payload)
                       (else #f)))
           (body-bytes (cond
                         ((not payload) #f)
                         ((u8vector? payload) payload)
                         (body-str (string->bytes body-str))
                         (else #f)))
           (body-hash (if body-bytes (sha256 body-bytes) emptySHA256))
           (now (current-date))
           (ts (date->string now "~Y~m~dT~H~M~SZ"))
           (scopets (date->string now "~Y~m~d"))
           (scope (string-append scopets "/" client.region "/lambda"))
           (host client.endpoint)
           (headers [["Host" :: host]
                     ["x-amz-date" :: ts]
                     ["x-amz-content-sha256" :: (hex-encode body-hash)]])
           (headers (if (and body-bytes (not (u8vector? payload)))
                      (append headers [["Content-Type" :: "application/json"]])
                      (if (u8vector? payload)
                        (append headers [["Content-Type" :: "application/zip"]])
                        headers)))
           (headers (if client.token
                      (append headers [["X-Amz-Security-Token" :: client.token]])
                      headers))
           (creq (aws4-canonical-request
                   verb: verb
                   uri: path
                   query: query
                   headers: headers
                   hash: body-hash))
           (auth (aws4-auth scope creq ts headers
                            client.secret-key client.access-key))
           (headers [["Authorization" :: auth] :: headers])
           (url (string-append "https://" host path))
           (req (case verb
                  ((GET) (http-get url headers: headers params: query))
                  ((POST) (http-post url headers: headers params: query
                                     data: (or body-bytes body-str "")))
                  ((PUT) (http-put url headers: headers params: query
                                   data: (or body-bytes body-str "")))
                  ((DELETE) (http-delete url headers: headers params: query))
                  (else (error "Bad HTTP verb" verb))))
           (status (request-status req)))
      (if (and (fx>= status 200) (fx< status 300))
        (let ((content (request-text req)))
          (request-close req)
          (if (and content (not (equal? content "")))
            (call-with-input-string content read-json)
            (make-hash-table)))
        ;; Error response
        (let ((content (request-text req)))
          (request-close req)
          (let (json (try (call-with-input-string content read-json)
                       (catch (_) #f)))
            (if json
              (let ((msg (or (hash-get json 'Message)
                             (hash-get json 'message)
                             (hash-get json "Message")
                             content))
                    (type (or (hash-get json 'Type)
                              (hash-get json '__type)
                              (hash-get json "Type")
                              "Unknown")))
                (raise-lambda-error lambda-rest-request msg type status))
              (raise-lambda-error lambda-rest-request
                                  (string-append "HTTP " (number->string status))
                                  content))))))))
