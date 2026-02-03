;;; -*- Gerbil -*-
;;; IAM client — wraps the generic AWS Query API client

(import :gerbil-aws/aws/api)
(export IAMClient iam-action iam-action/hash iam-action/items)

;; IAM API version
(def iam-api-version "2010-05-08")

;; IAM uses a global endpoint
(def (IAMClient
       profile: (profile #f)
       region: (region #f)
       access-key: (access-key #f)
       secret-key: (secret-key #f)
       token: (token #f))
  (AWSClient
    service: "iam"
    endpoint: "iam.amazonaws.com"
    api-version: iam-api-version
    profile: profile
    access-key: access-key
    secret-key: secret-key
    region: region
    token: token))

(def (iam-action client action (extra-params []))
  (aws-query-action client action extra-params))

(def (iam-action/hash client action (extra-params []))
  (aws-query-action/hash client action extra-params))

(def (iam-action/items client action response-tag (extra-params [])
       item-tag: (item-tag #f))
  (aws-query-action/items client action response-tag extra-params
    item-tag: item-tag))
