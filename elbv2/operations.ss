;;; -*- Gerbil -*-
;;; ELBv2 (Elastic Load Balancing v2) operations

(import :gerbil-aws/elbv2/api)
(export describe-load-balancers describe-target-groups describe-target-health)

;; Encode a list of strings as member-list parameters
(def (encode-member-list prefix values)
  (let loop ((vs values) (i 1) (acc []))
    (if (null? vs)
      (reverse acc)
      (loop (cdr vs) (+ i 1)
            (cons (cons (string-append prefix ".member." (number->string i))
                        (car vs))
                  acc)))))

;; DescribeLoadBalancers — list load balancers
(def (describe-load-balancers client
       load-balancer-arns: (load-balancer-arns #f)
       names: (names #f)
       marker: (marker #f)
       page-size: (page-size #f))
  (elbv2-action/items client "DescribeLoadBalancers" 'LoadBalancers
    (append
      (if load-balancer-arns
        (encode-member-list "LoadBalancerArns" load-balancer-arns)
        [])
      (if names (encode-member-list "Names" names) [])
      (if marker [["Marker" :: marker]] [])
      (if page-size [["PageSize" :: (if (number? page-size)
                                       (number->string page-size)
                                       page-size)]] []))
    item-tag: 'member))

;; DescribeTargetGroups — list target groups
(def (describe-target-groups client
       load-balancer-arn: (load-balancer-arn #f)
       target-group-arns: (target-group-arns #f)
       names: (names #f)
       marker: (marker #f)
       page-size: (page-size #f))
  (elbv2-action/items client "DescribeTargetGroups" 'TargetGroups
    (append
      (if load-balancer-arn
        [["LoadBalancerArn" :: load-balancer-arn]]
        [])
      (if target-group-arns
        (encode-member-list "TargetGroupArns" target-group-arns)
        [])
      (if names (encode-member-list "Names" names) [])
      (if marker [["Marker" :: marker]] [])
      (if page-size [["PageSize" :: (if (number? page-size)
                                       (number->string page-size)
                                       page-size)]] []))
    item-tag: 'member))

;; DescribeTargetHealth — check health of targets in a target group
(def (describe-target-health client target-group-arn)
  (elbv2-action/items client "DescribeTargetHealth" 'TargetHealthDescriptions
    [["TargetGroupArn" :: target-group-arn]]
    item-tag: 'member))
