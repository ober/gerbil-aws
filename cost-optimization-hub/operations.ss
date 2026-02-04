;;; -*- Gerbil -*-
;;; Cost Optimization Hub operations

(import :gerbil-aws/cost-optimization-hub/api)
(export list-recommendations)

;; ListRecommendations — list cost optimization recommendations
;; Returns: hash with "items" key (string keys, JSON protocol)
(def (list-recommendations client
       max-results: (max-results #f)
       next-token: (next-token #f)
       filter: (filter #f))
  (let ((payload (make-hash-table)))
    (when max-results
      (hash-put! payload "maxResults" max-results))
    (when next-token
      (hash-put! payload "nextToken" next-token))
    (when filter
      (hash-put! payload "filter" filter))
    (cost-optimization-hub-action client "ListRecommendations" payload)))
