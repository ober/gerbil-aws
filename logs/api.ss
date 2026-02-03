;;; -*- Gerbil -*-
;;; CloudWatch Logs client — wraps the generic AWS JSON API client

(import :gerbil-aws/aws/json-api)
(export LogsClient logs-action)

(def (LogsClient
       profile: (profile #f)
       region: (region #f)
       access-key: (access-key #f)
       secret-key: (secret-key #f)
       token: (token #f))
  (AWSJsonClient
    service: "logs"
    target-prefix: "Logs_20140328"
    content-type: "application/x-amz-json-1.1"
    profile: profile
    access-key: access-key
    secret-key: secret-key
    region: region
    token: token))

(def (logs-action client action (payload #f))
  (aws-json-action client action payload))
