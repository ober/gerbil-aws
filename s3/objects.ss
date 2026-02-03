;;; -*- Gerbil -*-
;;; S3 Object operations

(import :std/net/request
        :std/crypto/digest
        :std/text/base64
        :gerbil-aws/s3/api)
(export list-objects get-object put-object delete-object
        head-object copy-object
        delete-objects list-object-versions
        get-object-tagging put-object-tagging delete-object-tagging
        list-multipart-uploads abort-multipart-upload)

;; List objects in a bucket using ListObjectsV2
;; Returns a hash table with Contents (list of objects), optional
;; CommonPrefixes, IsTruncated, NextContinuationToken, etc.
(def (list-objects client bucket-name
       prefix: (prefix #f)
       delimiter: (delimiter #f)
       max-keys: (max-keys #f)
       continuation-token: (continuation-token #f)
       start-after: (start-after #f))
  (let* ((query (append
                  [["list-type" :: "2"]]
                  (if prefix [["prefix" :: prefix]] [])
                  (if delimiter [["delimiter" :: delimiter]] [])
                  (if max-keys [["max-keys" :: (if (number? max-keys)
                                                 (number->string max-keys)
                                                 max-keys)]] [])
                  (if continuation-token
                    [["continuation-token" :: continuation-token]]
                    [])
                  (if start-after [["start-after" :: start-after]] []))))
    (s3-request/xml client
      verb: 'GET
      bucket: bucket-name
      query: query)))

;; Get an object's content
;; Returns the raw content as a u8vector
(def (get-object client bucket-name key)
  (let* ((req (s3-request/check client
                verb: 'GET
                bucket: bucket-name
                key: key))
         (data (request-content req)))
    (request-close req)
    data))

;; Upload an object
;; data can be a string or u8vector
(def (put-object client bucket-name key data
       content-type: (content-type "application/octet-stream"))
  (let (req (s3-request/check client
              verb: 'PUT
              bucket: bucket-name
              key: key
              body: data
              content-type: content-type))
    (request-close req)
    (void)))

;; Delete an object
(def (delete-object client bucket-name key)
  (let (req (s3-request/check client
              verb: 'DELETE
              bucket: bucket-name
              key: key))
    (request-close req)
    (void)))

;; Get object metadata (HEAD request)
;; Returns a hash table of response headers
(def (head-object client bucket-name key)
  (let* ((req (s3-request/check client
                verb: 'HEAD
                bucket: bucket-name
                key: key))
         (headers (request-headers req)))
    (request-close req)
    (let (ht (make-hash-table))
      (for-each (lambda (h) (hash-put! ht (car h) (cdr h))) headers)
      ht)))

;; Server-side copy of an object
;; source is "bucket/key" format
(def (copy-object client bucket-name key source)
  (let (req (s3-request/check client
              verb: 'PUT
              bucket: bucket-name
              key: key
              extra-headers: [["x-amz-copy-source" :: source]]))
    (request-close req)
    (void)))

;; Batch delete objects
;; keys is a list of object key strings
(def (delete-objects client bucket-name keys quiet: (quiet #t))
  (let* ((objects-xml (apply string-append
                        (map (lambda (k)
                               (string-append "<Object><Key>" k "</Key></Object>"))
                             keys)))
         (body (string-append
                 "<Delete>"
                 (if quiet "<Quiet>true</Quiet>" "")
                 objects-xml
                 "</Delete>"))
         (body-bytes (string->bytes body))
         (md5-hash (md5 body-bytes))
         (req (s3-request/check client
                verb: 'POST
                bucket: bucket-name
                query: [["delete" :: ""]]
                body: body
                content-type: "application/xml"
                extra-headers: [["Content-MD5" :: (u8vector->base64-string md5-hash)]])))
    (request-close req)
    (void)))

;; List object versions in a bucket
(def (list-object-versions client bucket-name
       prefix: (prefix #f)
       delimiter: (delimiter #f)
       max-keys: (max-keys #f)
       key-marker: (key-marker #f)
       version-id-marker: (version-id-marker #f))
  (let* ((query (append
                  [["versions" :: ""]]
                  (if prefix [["prefix" :: prefix]] [])
                  (if delimiter [["delimiter" :: delimiter]] [])
                  (if max-keys [["max-keys" :: (if (number? max-keys)
                                                 (number->string max-keys)
                                                 max-keys)]] [])
                  (if key-marker [["key-marker" :: key-marker]] [])
                  (if version-id-marker [["version-id-marker" :: version-id-marker]] []))))
    (s3-request/xml client
      verb: 'GET
      bucket: bucket-name
      query: query)))

;; Get object tagging
(def (get-object-tagging client bucket-name key)
  (s3-request/xml client
    verb: 'GET
    bucket: bucket-name
    key: key
    query: [["tagging" :: ""]]))

;; Put object tagging
;; tags is an alist of (key . value) pairs
(def (put-object-tagging client bucket-name key tags)
  (let* ((tag-xml (apply string-append
                    (map (lambda (t)
                           (string-append "<Tag><Key>" (car t) "</Key><Value>" (cdr t) "</Value></Tag>"))
                         tags)))
         (body (string-append
                 "<Tagging><TagSet>" tag-xml "</TagSet></Tagging>"))
         (req (s3-request/check client
                verb: 'PUT
                bucket: bucket-name
                key: key
                query: [["tagging" :: ""]]
                body: body
                content-type: "application/xml")))
    (request-close req)
    (void)))

;; Delete object tagging
(def (delete-object-tagging client bucket-name key)
  (let (req (s3-request/check client
              verb: 'DELETE
              bucket: bucket-name
              key: key
              query: [["tagging" :: ""]]))
    (request-close req)
    (void)))

;; List in-progress multipart uploads
(def (list-multipart-uploads client bucket-name
       prefix: (prefix #f)
       delimiter: (delimiter #f)
       max-uploads: (max-uploads #f)
       key-marker: (key-marker #f)
       upload-id-marker: (upload-id-marker #f))
  (let* ((query (append
                  [["uploads" :: ""]]
                  (if prefix [["prefix" :: prefix]] [])
                  (if delimiter [["delimiter" :: delimiter]] [])
                  (if max-uploads [["max-uploads" :: (if (number? max-uploads)
                                                       (number->string max-uploads)
                                                       max-uploads)]] [])
                  (if key-marker [["key-marker" :: key-marker]] [])
                  (if upload-id-marker [["upload-id-marker" :: upload-id-marker]] []))))
    (s3-request/xml client
      verb: 'GET
      bucket: bucket-name
      query: query)))

;; Abort a multipart upload
(def (abort-multipart-upload client bucket-name key upload-id)
  (let (req (s3-request/check client
              verb: 'DELETE
              bucket: bucket-name
              key: key
              query: [["uploadId" :: upload-id]]))
    (request-close req)
    (void)))
