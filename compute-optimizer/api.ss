;;; -*- Gerbil -*-
;;; Compute Optimizer client — wraps the generic AWS JSON API client

(import :gerbil-aws/aws/json-api)
(export ComputeOptimizerClient compute-optimizer-action)

(def (ComputeOptimizerClient
       profile: (profile #f)
       region: (region #f)
       access-key: (access-key #f)
       secret-key: (secret-key #f)
       token: (token #f))
  (AWSJsonClient
    service: "compute-optimizer"
    target-prefix: "ComputeOptimizerService"
    content-type: "application/x-amz-json-1.0"
    profile: profile
    access-key: access-key
    secret-key: secret-key
    region: region
    token: token))

(def (compute-optimizer-action client action (payload #f))
  (aws-json-action client action payload))
