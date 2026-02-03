;;; -*- Gerbil -*-
;;; SNS client — wraps the generic AWS Query API client

(import :gerbil-aws/aws/api)
(export SNSClient sns-action sns-action/hash sns-action/items)

;; SNS API version
(def sns-api-version "2010-03-31")

(def (SNSClient
       profile: (profile #f)
       region: (region #f)
       access-key: (access-key #f)
       secret-key: (secret-key #f)
       token: (token #f))
  (AWSClient
    service: "sns"
    api-version: sns-api-version
    profile: profile
    access-key: access-key
    secret-key: secret-key
    region: region
    token: token))

(def (sns-action client action (extra-params []))
  (aws-query-action client action extra-params))

(def (sns-action/hash client action (extra-params []))
  (aws-query-action/hash client action extra-params))

(def (sns-action/items client action response-tag (extra-params [])
       item-tag: (item-tag #f))
  (aws-query-action/items client action response-tag extra-params
    item-tag: item-tag))
