;;; -*- Gerbil -*-
;;; Cost Optimization Hub client — wraps the generic AWS JSON API client
;;; This service is only available in us-east-1.

(import :gerbil-aws/aws/json-api)
(export CostOptimizationHubClient cost-optimization-hub-action)

(def (CostOptimizationHubClient
       profile: (profile #f)
       access-key: (access-key #f)
       secret-key: (secret-key #f)
       token: (token #f))
  (AWSJsonClient
    service: "cost-optimization-hub"
    target-prefix: "CostOptimizationHubService"
    content-type: "application/x-amz-json-1.0"
    profile: profile
    access-key: access-key
    secret-key: secret-key
    region: "us-east-1"
    token: token))

(def (cost-optimization-hub-action client action (payload #f))
  (aws-json-action client action payload))
