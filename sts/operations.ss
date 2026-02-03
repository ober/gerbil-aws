;;; -*- Gerbil -*-
;;; STS operations

(import :gerbil-aws/sts/api)
(export get-caller-identity assume-role get-session-token get-access-key-info)

(def (get-caller-identity client)
  (sts-action/hash client "GetCallerIdentity"))

(def (assume-role client
       role-arn: role-arn
       role-session-name: role-session-name
       duration-seconds: (duration-seconds #f)
       external-id: (external-id #f)
       policy: (policy #f))
  (sts-action/hash client "AssumeRole"
    (append
      [["RoleArn" :: role-arn]
       ["RoleSessionName" :: role-session-name]]
      (if duration-seconds [["DurationSeconds" :: (if (number? duration-seconds)
                                                    (number->string duration-seconds)
                                                    duration-seconds)]] [])
      (if external-id [["ExternalId" :: external-id]] [])
      (if policy [["Policy" :: policy]] []))))

(def (get-session-token client
       duration-seconds: (duration-seconds #f)
       serial-number: (serial-number #f)
       token-code: (token-code #f))
  (sts-action/hash client "GetSessionToken"
    (append
      (if duration-seconds [["DurationSeconds" :: (if (number? duration-seconds)
                                                    (number->string duration-seconds)
                                                    duration-seconds)]] [])
      (if serial-number [["SerialNumber" :: serial-number]] [])
      (if token-code [["TokenCode" :: token-code]] []))))

(def (get-access-key-info client access-key-id)
  (sts-action/hash client "GetAccessKeyInfo"
    [["AccessKeyId" :: access-key-id]]))
