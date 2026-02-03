;;; -*- Gerbil -*-
;;; S3 Bucket operations

(import :std/net/request
        :gerbil-aws/s3/api)
(export list-buckets create-bucket delete-bucket head-bucket
        get-bucket-location
        get-bucket-versioning put-bucket-versioning
        get-bucket-tagging put-bucket-tagging delete-bucket-tagging
        get-bucket-policy put-bucket-policy delete-bucket-policy)

;; List all buckets accessible to this client
;; Returns a list of hash tables with Name and CreationDate keys
(def (list-buckets client)
  (let (result (s3-request/xml client verb: 'GET))
    (if (and (hash-table? result)
             (hash-get result 'Buckets))
      (let (buckets (hash-get result 'Buckets))
        (cond
          ((hash-table? buckets)
           (let (bucket-list (hash-get buckets 'Bucket))
             (cond
               ((list? bucket-list) bucket-list)
               ((hash-table? bucket-list) [bucket-list])
               (else []))))
          (else [])))
      [])))

;; Create a bucket
;; For regions other than us-east-1, sends a LocationConstraint
(def (create-bucket client bucket-name)
  (using (client :- s3-client)
    (let* ((body (if (equal? client.region "us-east-1")
                   #f
                   (string-append
                     "<CreateBucketConfiguration xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">"
                     "<LocationConstraint>" client.region "</LocationConstraint>"
                     "</CreateBucketConfiguration>")))
           (req (s3-request/check client
                  verb: 'PUT
                  bucket: bucket-name
                  body: body
                  content-type: (and body "application/xml"))))
      (request-close req)
      (void))))

;; Delete a bucket (must be empty)
(def (delete-bucket client bucket-name)
  (let (req (s3-request/check client
              verb: 'DELETE
              bucket: bucket-name))
    (request-close req)
    (void)))

;; Check if a bucket exists
;; Returns #t if the bucket exists and is accessible, #f otherwise
(def (head-bucket client bucket-name)
  (let* ((req (s3-request client
                verb: 'HEAD
                bucket: bucket-name))
         (status (request-status req)))
    (request-close req)
    (and (fx>= status 200) (fx< status 300))))

;; Get bucket location
(def (get-bucket-location client bucket-name)
  (s3-request/xml client
    verb: 'GET
    bucket: bucket-name
    query: [["location" :: ""]]))

;; Get bucket versioning status
(def (get-bucket-versioning client bucket-name)
  (s3-request/xml client
    verb: 'GET
    bucket: bucket-name
    query: [["versioning" :: ""]]))

;; Enable or suspend bucket versioning
(def (put-bucket-versioning client bucket-name status)
  (let* ((body (string-append
                 "<VersioningConfiguration xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">"
                 "<Status>" status "</Status>"
                 "</VersioningConfiguration>"))
         (req (s3-request/check client
                verb: 'PUT
                bucket: bucket-name
                query: [["versioning" :: ""]]
                body: body
                content-type: "application/xml")))
    (request-close req)
    (void)))

;; Get bucket tagging
(def (get-bucket-tagging client bucket-name)
  (s3-request/xml client
    verb: 'GET
    bucket: bucket-name
    query: [["tagging" :: ""]]))

;; Put bucket tagging
;; tags is an alist of (key . value) pairs
(def (put-bucket-tagging client bucket-name tags)
  (let* ((tag-xml (apply string-append
                    (map (lambda (t)
                           (string-append "<Tag><Key>" (car t) "</Key><Value>" (cdr t) "</Value></Tag>"))
                         tags)))
         (body (string-append
                 "<Tagging><TagSet>" tag-xml "</TagSet></Tagging>"))
         (req (s3-request/check client
                verb: 'PUT
                bucket: bucket-name
                query: [["tagging" :: ""]]
                body: body
                content-type: "application/xml")))
    (request-close req)
    (void)))

;; Delete bucket tagging
(def (delete-bucket-tagging client bucket-name)
  (let (req (s3-request/check client
              verb: 'DELETE
              bucket: bucket-name
              query: [["tagging" :: ""]]))
    (request-close req)
    (void)))

;; Get bucket policy (returns JSON string)
(def (get-bucket-policy client bucket-name)
  (let* ((req (s3-request/check client
                verb: 'GET
                bucket: bucket-name
                query: [["policy" :: ""]]))
         (content (request-text req)))
    (request-close req)
    content))

;; Put bucket policy
;; policy is a JSON string
(def (put-bucket-policy client bucket-name policy)
  (let (req (s3-request/check client
              verb: 'PUT
              bucket: bucket-name
              query: [["policy" :: ""]]
              body: policy
              content-type: "application/json"))
    (request-close req)
    (void)))

;; Delete bucket policy
(def (delete-bucket-policy client bucket-name)
  (let (req (s3-request/check client
              verb: 'DELETE
              bucket: bucket-name
              query: [["policy" :: ""]]))
    (request-close req)
    (void)))
