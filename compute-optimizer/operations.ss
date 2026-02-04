;;; -*- Gerbil -*-
;;; Compute Optimizer operations

(import :gerbil-aws/compute-optimizer/api)
(export get-ec2-instance-recommendations)

;; GetEC2InstanceRecommendations — get rightsizing recommendations for EC2 instances
;; Returns: hash with "instanceRecommendations" key (string keys, JSON protocol)
(def (get-ec2-instance-recommendations client
       instance-arns: (instance-arns #f)
       max-results: (max-results #f)
       next-token: (next-token #f)
       filters: (filters #f))
  (let ((payload (make-hash-table)))
    (when instance-arns
      (hash-put! payload "instanceArns" (list->vector instance-arns)))
    (when max-results
      (hash-put! payload "maxResults" max-results))
    (when next-token
      (hash-put! payload "nextToken" next-token))
    (when filters
      (hash-put! payload "filters" (list->vector filters)))
    (compute-optimizer-action client "GetEC2InstanceRecommendations" payload)))
