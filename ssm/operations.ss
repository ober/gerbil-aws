;;; -*- Gerbil -*-
;;; AWS Systems Manager (SSM) operations

(import :gerbil-aws/ssm/api)
(export start-session
        terminate-session
        describe-instance-information)

;; Start an SSM session to a managed instance.
;; Returns hash with SessionId, StreamUrl, TokenValue.
(def (start-session client target
       document-name: (document-name #f)
       reason: (reason #f))
  (let ((payload (make-hash-table)))
    (hash-put! payload "Target" target)
    (when document-name
      (hash-put! payload "DocumentName" document-name))
    (when reason
      (hash-put! payload "Reason" reason))
    (ssm-action client "StartSession" payload)))

;; Terminate an active SSM session.
(def (terminate-session client session-id)
  (let ((payload (make-hash-table)))
    (hash-put! payload "SessionId" session-id)
    (ssm-action client "TerminateSession" payload)))

;; List SSM-managed instances.
(def (describe-instance-information client
       filters: (filters #f)
       max-results: (max-results #f)
       next-token: (next-token #f))
  (let ((payload (make-hash-table)))
    (when filters
      (hash-put! payload "Filters" (list->vector filters)))
    (when max-results
      (hash-put! payload "MaxResults" max-results))
    (when next-token
      (hash-put! payload "NextToken" next-token))
    (ssm-action client "DescribeInstanceInformation" payload)))
