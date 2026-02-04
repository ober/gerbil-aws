;;; -*- Gerbil -*-
;;; RDS client — wraps the generic AWS Query API client

(import :gerbil-aws/aws/api)
(export RDSClient rds-action rds-action/hash rds-action/items)

;; RDS API version
(def rds-api-version "2014-10-31")

(def (RDSClient
       profile: (profile #f)
       region: (region #f)
       access-key: (access-key #f)
       secret-key: (secret-key #f)
       token: (token #f))
  (AWSClient
    service: "rds"
    api-version: rds-api-version
    profile: profile
    access-key: access-key
    secret-key: secret-key
    region: region
    token: token))

(def (rds-action client action (extra-params []))
  (aws-query-action client action extra-params))

(def (rds-action/hash client action (extra-params []))
  (aws-query-action/hash client action extra-params))

(def (rds-action/items client action response-tag (extra-params [])
       item-tag: (item-tag #f))
  (aws-query-action/items client action response-tag extra-params
    item-tag: item-tag))
