;;; -*- Gerbil -*-
;;; S3 Object operations

(import :std/net/request
        :gerbil-aws/s3/api)
(export list-objects get-object put-object delete-object
        head-object copy-object)

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
