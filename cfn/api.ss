;;; -*- Gerbil -*-
;;; CloudFormation client — wraps the generic AWS Query API client

(import :gerbil-aws/aws/api)
(export CFNClient cfn-action cfn-action/hash cfn-action/items)

;; CloudFormation API version
(def cfn-api-version "2010-05-15")

(def (CFNClient
       profile: (profile #f)
       region: (region #f)
       access-key: (access-key #f)
       secret-key: (secret-key #f)
       token: (token #f))
  (AWSClient
    service: "cloudformation"
    api-version: cfn-api-version
    profile: profile
    access-key: access-key
    secret-key: secret-key
    region: region
    token: token))

(def (cfn-action client action (extra-params []))
  (aws-query-action client action extra-params))

(def (cfn-action/hash client action (extra-params []))
  (aws-query-action/hash client action extra-params))

(def (cfn-action/items client action response-tag (extra-params [])
       item-tag: (item-tag #f))
  (aws-query-action/items client action response-tag extra-params
    item-tag: item-tag))
