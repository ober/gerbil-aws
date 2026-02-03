;;; -*- Gerbil -*-
;;; SQS client — wraps the generic AWS JSON API client
;;; SQS uses JSON protocol since 2024

(import :gerbil-aws/aws/json-api)
(export SQSClient sqs-action)

(def (SQSClient
       profile: (profile #f)
       region: (region #f)
       access-key: (access-key #f)
       secret-key: (secret-key #f)
       token: (token #f))
  (AWSJsonClient
    service: "sqs"
    target-prefix: "AmazonSQS"
    content-type: "application/x-amz-json-1.0"
    profile: profile
    access-key: access-key
    secret-key: secret-key
    region: region
    token: token))

(def (sqs-action client action (payload #f))
  (aws-json-action client action payload))
