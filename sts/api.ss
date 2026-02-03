;;; -*- Gerbil -*-
;;; STS client — wraps the generic AWS Query API client

(import :gerbil-aws/aws/api)
(export STSClient sts-action sts-action/hash)

;; STS API version
(def sts-api-version "2011-06-15")

;; STS uses a global endpoint by default
(def (STSClient
       profile: (profile #f)
       region: (region #f)
       access-key: (access-key #f)
       secret-key: (secret-key #f)
       token: (token #f))
  (AWSClient
    service: "sts"
    endpoint: "sts.amazonaws.com"
    api-version: sts-api-version
    profile: profile
    access-key: access-key
    secret-key: secret-key
    region: region
    token: token))

(def (sts-action client action (extra-params []))
  (aws-query-action client action extra-params))

(def (sts-action/hash client action (extra-params []))
  (aws-query-action/hash client action extra-params))
