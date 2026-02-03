;;; -*- Gerbil -*-
;;; Generic AWS JSON API client
;;; For JSON-protocol services: Lambda, DynamoDB, CloudWatch Logs, SQS
;;; SigV4-signed POST with JSON body, JSON response parsing

(import :std/net/s3/sigv4
        :std/net/request
        :std/crypto/digest
        :std/text/hex
        :std/text/json
        :std/format
        :std/misc/bytes
        :std/error
        :std/sugar
        :std/srfi/19
        :gerbil-aws/aws/creds)
(export aws-json-client aws-json-client?
        aws-json-client-endpoint aws-json-client-access-key
        aws-json-client-secret-key aws-json-client-region
        aws-json-client-token aws-json-client-service
        AWSJsonClient
        AWSJsonClientError aws-json-client-error?
        aws-json-request aws-json-action)

(defstruct aws-json-client (endpoint access-key secret-key region token service target-prefix content-type)
  final: #t)

(deferror-class (AWSJsonClientError Error) () aws-json-client-error?)

(defraise/context (raise-aws-json-error where message irritants ...)
  (AWSJsonClientError message irritants: [irritants ...]))

;; Precomputed empty SHA256
(def emptySHA256 (sha256 #u8()))

;; Create an AWS JSON API client with credential resolution
;; service: the AWS service name for signing (e.g., "dynamodb", "logs", "lambda")
;; endpoint: the service endpoint
;; target-prefix: the X-Amz-Target prefix (e.g., "DynamoDB_20120810")
;; content-type: the content type (default: application/x-amz-json-1.0)
(def (AWSJsonClient
       service: service
       endpoint: (endpoint #f)
       target-prefix: (target-prefix #f)
       content-type: (content-type "application/x-amz-json-1.0")
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
          (token (or token resolved-token)))
      (unless access-key
        (raise-aws-json-error AWSJsonClient "Must provide access key" "access-key"))
      (unless secret-key
        (raise-aws-json-error AWSJsonClient "Must provide secret key" "secret-key"))
      (make-aws-json-client
        (or endpoint (string-append service "." region ".amazonaws.com"))
        access-key secret-key region token
        service (or target-prefix "") content-type))))

;; Make a signed POST request to an AWS JSON API
;; target: the full X-Amz-Target value (e.g., "DynamoDB_20120810.ListTables")
;; payload: a hash table or #f for the JSON body
;; Returns a hash table from the parsed JSON response
(def (aws-json-request client target payload)
  (using (client :- aws-json-client)
    (let* ((body-str (if payload
                       (call-with-output-string
                         (lambda (p) (write-json payload p)))
                       "{}"))
           (body-bytes (string->bytes body-str))
           (body-hash (sha256 body-bytes))
           (now (current-date))
           (ts (date->string now "~Y~m~dT~H~M~SZ"))
           (scopets (date->string now "~Y~m~d"))
           (scope (string-append scopets "/" client.region "/" client.service))
           (host client.endpoint)
           (headers [["Host" :: host]
                     ["x-amz-date" :: ts]
                     ["Content-Type" :: client.content-type]
                     ["X-Amz-Target" :: target]])
           (headers (if client.token
                      (append headers [["X-Amz-Security-Token" :: client.token]])
                      headers))
           (creq (aws4-canonical-request
                   verb: 'POST
                   uri: "/"
                   query: #f
                   headers: headers
                   hash: body-hash))
           (auth (aws4-auth scope creq ts headers
                            client.secret-key client.access-key))
           (headers [["Authorization" :: auth] :: headers])
           (url (string-append "https://" host "/"))
           (req (http-post url headers: headers data: body-str))
           (status (request-status req)))
      (if (and (fx>= status 200) (fx< status 300))
        (let* ((content (request-text req)))
          (request-close req)
          (if (and content (not (equal? content "")))
            (call-with-input-string content read-json)
            (make-hash-table)))
        ;; Error response — try to parse JSON error
        (let ((content (request-text req)))
          (request-close req)
          (let (json (try (call-with-input-string content read-json)
                       (catch (_) #f)))
            (if json
              (let* ((type-raw (or (hash-get json '__type)
                                   (hash-get json 'code)
                                   (hash-get json "code")
                                   (hash-get json "__type")
                                   "Unknown"))
                     (type-str (if (string? type-raw)
                                 ;; Strip prefix like "com.amazonaws.dynamodb.v20120810#"
                                 (let (pos (string-index type-raw #\#))
                                   (if pos
                                     (substring type-raw (+ pos 1) (string-length type-raw))
                                     type-raw))
                                 (format "~a" type-raw)))
                     (msg-str (or (hash-get json 'message)
                                  (hash-get json 'Message)
                                  (hash-get json "message")
                                  (hash-get json "Message")
                                  content)))
                (raise-aws-json-error aws-json-request msg-str type-str status))
              (raise-aws-json-error aws-json-request
                                    (string-append "HTTP " (number->string status))
                                    content))))))))

;; Execute an AWS JSON action
;; action: the action name (e.g., "ListTables", "GetLogEvents")
;; payload: a hash table for the JSON body (or #f for empty)
;; Returns parsed JSON as hash table
(def (aws-json-action client action (payload #f))
  (let (target (string-append (aws-json-client-target-prefix client) "." action))
    (aws-json-request client target payload)))
