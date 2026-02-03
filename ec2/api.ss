;;; -*- Gerbil -*-
;;; Core EC2 client — signing, POST, error handling

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
        :gerbil-aws/ec2/xml)
(export ec2-client ec2-client?
        ec2-client-endpoint ec2-client-access-key
        ec2-client-secret-key ec2-client-region
        ec2-client-token
        EC2Client
        EC2ClientError ec2-client-error?
        ec2-request ec2-action ec2-action/items ec2-action/hash)

(defstruct ec2-client (endpoint access-key secret-key region token)
  final: #t)

(deferror-class (EC2ClientError Error) () ec2-client-error?)

(defraise/context (raise-ec2-error where message irritants ...)
  (EC2ClientError message irritants: [irritants ...]))

;; EC2 API version
(def ec2-api-version "2016-11-15")

;; Precomputed empty SHA256
(def emptySHA256 (sha256 #u8()))

;; Create an EC2 client with credential resolution
(def (EC2Client
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
          (endpoint (or endpoint
                        (string-append "ec2." (or region resolved-region) ".amazonaws.com"))))
      (unless access-key
        (raise-ec2-error EC2Client "Must provide access key" "access-key"))
      (unless secret-key
        (raise-ec2-error EC2Client "Must provide secret key" "secret-key"))
      (make-ec2-client endpoint access-key secret-key region token))))

;; Make a signed POST request to the EC2 API
;; params is an alist of (key . value) pairs
;; Returns the parsed SXML response
(def (ec2-request client params)
  (using (client :- ec2-client)
    (let* ((body-str (form-url-encode params))
           (body-bytes (string->bytes body-str))
           (body-hash (sha256 body-bytes))
           (now (current-date))
           (ts (date->string now "~Y~m~dT~H~M~SZ"))
           (scopets (date->string now "~Y~m~d"))
           (scope (string-append scopets "/" client.region "/ec2"))
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
               (xml (ec2-parse-xml content)))
          (request-close req)
          xml)
        ;; Error response — try to parse XML error
        (let ((content (request-text req)))
          (request-close req)
          (let (xml (try (ec2-parse-xml content)
                      (catch (_) #f)))
            (if xml
              ;; Extract error code and message from XML
              (let* ((err (sxml-find xml (sxml-e? 'ec2:Error)))
                     (code (and err (sxml-find err (sxml-e? 'ec2:Code))))
                     (msg (and err (sxml-find err (sxml-e? 'ec2:Message))))
                     (code-str (if (and code (pair? (cdr code))) (cadr code) "Unknown"))
                     (msg-str (if (and msg (pair? (cdr msg))) (cadr msg) content)))
                (raise-ec2-error ec2-request msg-str code-str status))
              (raise-ec2-error ec2-request
                               (string-append "HTTP " (number->string status))
                               content))))))))

;; Execute an EC2 action and return raw SXML
(def (ec2-action client action (extra-params []))
  (ec2-request client
    (append [["Action" :: action]
             ["Version" :: ec2-api-version]]
            extra-params)))

;; Execute an EC2 action and extract items from a specific response tag
;; Returns a list of hash tables
(def (ec2-action/items client action response-tag (extra-params []))
  (let* ((xml (ec2-action client action extra-params))
         (ns-tag (string->symbol (string-append "ec2:" (symbol->string response-tag))))
         (set-elem (sxml-find xml (sxml-e? ns-tag))))
    (if set-elem
      (let ((items (sxml-items set-elem)))
        (map sxml->hash items))
      [])))

;; Extract the full response as a hash table
(def (ec2-action/hash client action (extra-params []))
  (let ((xml (ec2-action client action extra-params)))
    (ec2-response->hash xml)))
