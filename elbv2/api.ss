;;; -*- Gerbil -*-
;;; ELBv2 (Elastic Load Balancing v2) client — wraps the generic AWS Query API client

(import :gerbil-aws/aws/api)
(export ELBv2Client elbv2-action elbv2-action/hash elbv2-action/items)

;; ELBv2 API version
(def elbv2-api-version "2015-12-01")

(def (ELBv2Client
       profile: (profile #f)
       region: (region #f)
       access-key: (access-key #f)
       secret-key: (secret-key #f)
       token: (token #f))
  (AWSClient
    service: "elasticloadbalancing"
    api-version: elbv2-api-version
    profile: profile
    access-key: access-key
    secret-key: secret-key
    region: region
    token: token))

(def (elbv2-action client action (extra-params []))
  (aws-query-action client action extra-params))

(def (elbv2-action/hash client action (extra-params []))
  (aws-query-action/hash client action extra-params))

(def (elbv2-action/items client action response-tag (extra-params [])
       item-tag: (item-tag #f))
  (aws-query-action/items client action response-tag extra-params
    item-tag: item-tag))
