;;; -*- Gerbil -*-
;;; CloudWatch Logs operations

(import :gerbil-aws/logs/api)
(export describe-log-groups describe-log-streams
        create-log-group delete-log-group
        get-log-events filter-log-events put-log-events
        put-retention-policy delete-retention-policy)

(def (describe-log-groups client
       log-group-name-prefix: (log-group-name-prefix #f)
       limit: (limit #f)
       next-token: (next-token #f))
  (let ((payload (make-hash-table)))
    (when log-group-name-prefix
      (hash-put! payload "logGroupNamePrefix" log-group-name-prefix))
    (when limit (hash-put! payload "limit" limit))
    (when next-token (hash-put! payload "nextToken" next-token))
    (logs-action client "DescribeLogGroups" payload)))

(def (describe-log-streams client log-group-name
       log-stream-name-prefix: (log-stream-name-prefix #f)
       order-by: (order-by #f)
       descending: (descending #f)
       limit: (limit #f)
       next-token: (next-token #f))
  (let ((payload (make-hash-table)))
    (hash-put! payload "logGroupName" log-group-name)
    (when log-stream-name-prefix
      (hash-put! payload "logStreamNamePrefix" log-stream-name-prefix))
    (when order-by (hash-put! payload "orderBy" order-by))
    (when descending (hash-put! payload "descending" descending))
    (when limit (hash-put! payload "limit" limit))
    (when next-token (hash-put! payload "nextToken" next-token))
    (logs-action client "DescribeLogStreams" payload)))

(def (create-log-group client log-group-name
       tags: (tags #f))
  (let ((payload (make-hash-table)))
    (hash-put! payload "logGroupName" log-group-name)
    (when tags (hash-put! payload "tags" tags))
    (logs-action client "CreateLogGroup" payload))
  (void))

(def (delete-log-group client log-group-name)
  (let ((payload (make-hash-table)))
    (hash-put! payload "logGroupName" log-group-name)
    (logs-action client "DeleteLogGroup" payload))
  (void))

(def (get-log-events client log-group-name log-stream-name
       start-time: (start-time #f)
       end-time: (end-time #f)
       limit: (limit #f)
       next-token: (next-token #f)
       start-from-head: (start-from-head #f))
  (let ((payload (make-hash-table)))
    (hash-put! payload "logGroupName" log-group-name)
    (hash-put! payload "logStreamName" log-stream-name)
    (when start-time (hash-put! payload "startTime" start-time))
    (when end-time (hash-put! payload "endTime" end-time))
    (when limit (hash-put! payload "limit" limit))
    (when next-token (hash-put! payload "nextToken" next-token))
    (when start-from-head (hash-put! payload "startFromHead" start-from-head))
    (logs-action client "GetLogEvents" payload)))

(def (filter-log-events client log-group-name
       log-stream-names: (log-stream-names #f)
       filter-pattern: (filter-pattern #f)
       start-time: (start-time #f)
       end-time: (end-time #f)
       limit: (limit #f)
       next-token: (next-token #f))
  (let ((payload (make-hash-table)))
    (hash-put! payload "logGroupName" log-group-name)
    (when log-stream-names
      (hash-put! payload "logStreamNames" (list->vector log-stream-names)))
    (when filter-pattern (hash-put! payload "filterPattern" filter-pattern))
    (when start-time (hash-put! payload "startTime" start-time))
    (when end-time (hash-put! payload "endTime" end-time))
    (when limit (hash-put! payload "limit" limit))
    (when next-token (hash-put! payload "nextToken" next-token))
    (logs-action client "FilterLogEvents" payload)))

;; Put log events to a log stream
;; events is a list of hash tables with 'timestamp and 'message keys
(def (put-log-events client log-group-name log-stream-name events
       sequence-token: (sequence-token #f))
  (let ((payload (make-hash-table)))
    (hash-put! payload "logGroupName" log-group-name)
    (hash-put! payload "logStreamName" log-stream-name)
    (hash-put! payload "logEvents" (list->vector events))
    (when sequence-token (hash-put! payload "sequenceToken" sequence-token))
    (logs-action client "PutLogEvents" payload)))

(def (put-retention-policy client log-group-name retention-in-days)
  (let ((payload (make-hash-table)))
    (hash-put! payload "logGroupName" log-group-name)
    (hash-put! payload "retentionInDays" retention-in-days)
    (logs-action client "PutRetentionPolicy" payload))
  (void))

(def (delete-retention-policy client log-group-name)
  (let ((payload (make-hash-table)))
    (hash-put! payload "logGroupName" log-group-name)
    (logs-action client "DeleteRetentionPolicy" payload))
  (void))
