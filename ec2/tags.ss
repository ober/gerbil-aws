;;; -*- Gerbil -*-
;;; EC2 Tag operations

(import :gerbil-aws/ec2/api
        :gerbil-aws/ec2/params)
(export describe-tags create-tags delete-tags)

(def (describe-tags client
       filters: (filters [])
       max-results: (max-results #f)
       next-token: (next-token #f))
  (ec2-action/items client "DescribeTags" 'tagSet
    (params-merge
      (ec2-param-filters filters)
      (if max-results [["MaxResults" :: max-results]] [])
      (if next-token [["NextToken" :: next-token]] []))))

;; Create tags on one or more resources
;; resources: list of resource IDs
;; tags: alist of (key . value) pairs
(def (create-tags client resources tags)
  (let ((tag-params
         (let loop ((ts tags) (i 1) (acc []))
           (if (null? ts)
             (reverse acc)
             (let ((tag (car ts)))
               (loop (cdr ts) (+ i 1)
                     (cons (cons (string-append "Tag." (number->string i) ".Value") (cdr tag))
                           (cons (cons (string-append "Tag." (number->string i) ".Key") (car tag))
                                 acc))))))))
    (ec2-action client "CreateTags"
      (params-merge
        (ec2-param-list "ResourceId" resources)
        tag-params)))
  (void))

;; Delete tags from one or more resources
;; resources: list of resource IDs
;; tags: alist of (key . value) pairs (value can be "" to match any)
(def (delete-tags client resources tags)
  (let ((tag-params
         (let loop ((ts tags) (i 1) (acc []))
           (if (null? ts)
             (reverse acc)
             (let ((tag (car ts)))
               (loop (cdr ts) (+ i 1)
                     (cons (cons (string-append "Tag." (number->string i) ".Value") (cdr tag))
                           (cons (cons (string-append "Tag." (number->string i) ".Key") (car tag))
                                 acc))))))))
    (ec2-action client "DeleteTags"
      (params-merge
        (ec2-param-list "ResourceId" resources)
        tag-params)))
  (void))
