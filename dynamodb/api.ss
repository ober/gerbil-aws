;;; -*- Gerbil -*-
;;; DynamoDB client — wraps the generic AWS JSON API client

(import :gerbil-aws/aws/json-api)
(export DynamoDBClient dynamodb-action)

(def (DynamoDBClient
       profile: (profile #f)
       region: (region #f)
       access-key: (access-key #f)
       secret-key: (secret-key #f)
       token: (token #f))
  (AWSJsonClient
    service: "dynamodb"
    target-prefix: "DynamoDB_20120810"
    content-type: "application/x-amz-json-1.0"
    profile: profile
    access-key: access-key
    secret-key: secret-key
    region: region
    token: token))

(def (dynamodb-action client action (payload #f))
  (aws-json-action client action payload))
