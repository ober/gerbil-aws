;;; -*- Gerbil -*-
;;; CloudWatch Monitoring client — wraps the generic AWS Query API client

(import :gerbil-aws/aws/api)
(export CloudWatchClient cw-action cw-action/hash cw-action/items)

;; CloudWatch Monitoring API version
(def cw-api-version "2010-08-01")

(def (CloudWatchClient
       profile: (profile #f)
       region: (region #f)
       access-key: (access-key #f)
       secret-key: (secret-key #f)
       token: (token #f))
  (AWSClient
    service: "monitoring"
    api-version: cw-api-version
    profile: profile
    access-key: access-key
    secret-key: secret-key
    region: region
    token: token))

(def (cw-action client action (extra-params []))
  (aws-query-action client action extra-params))

(def (cw-action/hash client action (extra-params []))
  (aws-query-action/hash client action extra-params))

(def (cw-action/items client action response-tag (extra-params [])
       item-tag: (item-tag #f))
  (aws-query-action/items client action response-tag extra-params
    item-tag: item-tag))
