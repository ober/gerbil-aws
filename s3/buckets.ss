;;; -*- Gerbil -*-
;;; S3 Bucket operations

(import :std/net/request
        :gerbil-aws/s3/api)
(export list-buckets create-bucket delete-bucket head-bucket)

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
