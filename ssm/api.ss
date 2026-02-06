;;; -*- Gerbil -*-
;;; AWS Systems Manager (SSM) client — wraps the generic AWS JSON API client

(import :gerbil-aws/aws/json-api)
(export SSMClient ssm-action)

(def (SSMClient
       profile: (profile #f)
       region: (region #f)
       access-key: (access-key #f)
       secret-key: (secret-key #f)
       token: (token #f))
  (AWSJsonClient
    service: "ssm"
    target-prefix: "AmazonSSM"
    content-type: "application/x-amz-json-1.1"
    profile: profile
    access-key: access-key
    secret-key: secret-key
    region: region
    token: token))

(def (ssm-action client action (payload #f))
  (aws-json-action client action payload))
