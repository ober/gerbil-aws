;;; -*- Gerbil -*-
;;; Generic AWS Query API client
;;; For EC2-style services: STS, IAM, CloudFormation, SNS
;;; SigV4-signed POST with form-encoded body, XML response parsing

(import :std/net/s3/sigv4
        :std/net/request
        :std/net/uri
        :std/crypto/digest
        :std/text/hex
        :std/misc/bytes
        :std/markup/xml
        :std/error
        :std/sugar
        :std/srfi/19
        :gerbil-aws/aws/creds
        :gerbil-aws/aws/xml)
(export aws-client aws-client?
        aws-client-endpoint aws-client-access-key
        aws-client-secret-key aws-client-region
        aws-client-token aws-client-service
        AWSClient
        AWSClientError aws-client-error?
        aws-query-request aws-query-action aws-query-action/items aws-query-action/hash)

(defstruct aws-client (endpoint access-key secret-key region token service api-version namespaces)
  final: #t)

(deferror-class (AWSClientError Error) () aws-client-error?)

(defraise/context (raise-aws-error where message irritants ...)
  (AWSClientError message irritants: [irritants ...]))

;; Create an AWS Query API client with credential resolution
;; service: the AWS service name for signing (e.g., "sts", "iam", "cloudformation", "sns")
;; endpoint: the service endpoint (e.g., "sts.amazonaws.com")
;; api-version: the API version string (e.g., "2011-06-15")
;; namespaces: XML namespace mappings for response parsing
(def (AWSClient
       service: service
       endpoint: (endpoint #f)
       api-version: (api-version #f)
       namespaces: (namespaces [])
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
        (raise-aws-error AWSClient "Must provide access key" "access-key"))
      (unless secret-key
        (raise-aws-error AWSClient "Must provide secret key" "secret-key"))
      (make-aws-client (or endpoint (string-append service "." region ".amazonaws.com"))
                       access-key secret-key region token
                       service (or api-version "") namespaces))))

;; Make a signed POST request to an AWS Query API
;; params is an alist of (key . value) pairs
;; Returns the parsed SXML response
(def (aws-query-request client params)
  (using (client :- aws-client)
    (let* ((body-str (form-url-encode params))
           (body-bytes (string->bytes body-str))
           (body-hash (sha256 body-bytes))
           (now (current-date))
           (ts (date->string now "~Y~m~dT~H~M~SZ"))
           (scopets (date->string now "~Y~m~d"))
           (scope (string-append scopets "/" client.region "/" client.service))
           (host client.endpoint)
           (headers [["Host" :: host]
                     ["x-amz-date" :: ts]
                     ["Content-Type" :: "application/x-www-form-urlencoded; charset=utf-8"]])
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
        (let* ((content (request-text req))
               (xml (aws-parse-xml content namespaces: client.namespaces)))
          (request-close req)
          xml)
        ;; Error response — try to parse XML error
        (let ((content (request-text req)))
          (request-close req)
          (let (xml (try (aws-parse-xml content namespaces: client.namespaces)
                      (catch (_) #f)))
            (if xml
              (let* ((hash (aws-response->hash xml))
                     (err (and (hash-table? hash) (hash-get hash 'Error)))
                     (code-str (if (and (hash-table? err) (hash-get err 'Code))
                                 (hash-get err 'Code) "Unknown"))
                     (msg-str (if (and (hash-table? err) (hash-get err 'Message))
                                (hash-get err 'Message) content)))
                (raise-aws-error aws-query-request msg-str code-str status))
              (raise-aws-error aws-query-request
                               (string-append "HTTP " (number->string status))
                               content))))))))

;; Execute an AWS action and return raw SXML
(def (aws-query-action client action (extra-params []))
  (aws-query-request client
    (append [["Action" :: action]
             ["Version" :: (aws-client-api-version client)]]
            extra-params)))

;; Execute an AWS action and extract items from a specific response tag
;; Returns a list of hash tables
(def (aws-query-action/items client action response-tag (extra-params [])
       item-tag: (item-tag #f)
       ns-prefix: (ns-prefix #f))
  (let* ((xml (aws-query-action client action extra-params))
         (ns-tag (if ns-prefix
                   (string->symbol (string-append ns-prefix (symbol->string response-tag)))
                   response-tag))
         (set-elem (sxml-find xml (sxml-e? ns-tag))))
    (if set-elem
      (let ((items (if item-tag
                     (sxml-items set-elem item-tag)
                     (sxml-items set-elem))))
        (map sxml->hash items))
      ;; Fallback: try without namespace prefix
      (let (set-elem (sxml-find xml (lambda (node)
                                       (and (pair? node)
                                            (symbol? (car node))
                                            (eq? (strip-ns (car node))
                                                 response-tag)))))
        (if set-elem
          (let ((items (if item-tag
                         (sxml-items set-elem item-tag)
                         (sxml-items set-elem))))
            (map sxml->hash items))
          [])))))

;; Extract the full response as a hash table
(def (aws-query-action/hash client action (extra-params []))
  (let ((xml (aws-query-action client action extra-params)))
    (aws-response->hash xml)))
