;;; -*- Gerbil -*-
;;; SQS operations

(import :gerbil-aws/sqs/api)
(export list-queues create-queue delete-queue
        get-queue-url get-queue-attributes set-queue-attributes
        send-message receive-message delete-message purge-queue)

(def (list-queues client
       queue-name-prefix: (queue-name-prefix #f)
       max-results: (max-results #f)
       next-token: (next-token #f))
  (let ((payload (make-hash-table)))
    (when queue-name-prefix (hash-put! payload "QueueNamePrefix" queue-name-prefix))
    (when max-results (hash-put! payload "MaxResults" max-results))
    (when next-token (hash-put! payload "NextToken" next-token))
    (sqs-action client "ListQueues" payload)))

(def (create-queue client queue-name
       attributes: (attributes #f)
       tags: (tags #f))
  (let ((payload (make-hash-table)))
    (hash-put! payload "QueueName" queue-name)
    (when attributes (hash-put! payload "Attributes" attributes))
    (when tags (hash-put! payload "tags" tags))
    (sqs-action client "CreateQueue" payload)))

(def (delete-queue client queue-url)
  (let ((payload (make-hash-table)))
    (hash-put! payload "QueueUrl" queue-url)
    (sqs-action client "DeleteQueue" payload))
  (void))

(def (get-queue-url client queue-name
       queue-owner-aws-account-id: (queue-owner-aws-account-id #f))
  (let ((payload (make-hash-table)))
    (hash-put! payload "QueueName" queue-name)
    (when queue-owner-aws-account-id
      (hash-put! payload "QueueOwnerAWSAccountId" queue-owner-aws-account-id))
    (sqs-action client "GetQueueUrl" payload)))

(def (get-queue-attributes client queue-url
       attribute-names: (attribute-names #f))
  (let ((payload (make-hash-table)))
    (hash-put! payload "QueueUrl" queue-url)
    (when attribute-names
      (hash-put! payload "AttributeNames" (list->vector attribute-names)))
    (sqs-action client "GetQueueAttributes" payload)))

(def (set-queue-attributes client queue-url attributes)
  (let ((payload (make-hash-table)))
    (hash-put! payload "QueueUrl" queue-url)
    (hash-put! payload "Attributes" attributes)
    (sqs-action client "SetQueueAttributes" payload))
  (void))

(def (send-message client queue-url message-body
       delay-seconds: (delay-seconds #f)
       message-attributes: (message-attributes #f)
       message-group-id: (message-group-id #f)
       message-deduplication-id: (message-deduplication-id #f))
  (let ((payload (make-hash-table)))
    (hash-put! payload "QueueUrl" queue-url)
    (hash-put! payload "MessageBody" message-body)
    (when delay-seconds (hash-put! payload "DelaySeconds" delay-seconds))
    (when message-attributes (hash-put! payload "MessageAttributes" message-attributes))
    (when message-group-id (hash-put! payload "MessageGroupId" message-group-id))
    (when message-deduplication-id
      (hash-put! payload "MessageDeduplicationId" message-deduplication-id))
    (sqs-action client "SendMessage" payload)))

(def (receive-message client queue-url
       max-number-of-messages: (max-number-of-messages #f)
       wait-time-seconds: (wait-time-seconds #f)
       visibility-timeout: (visibility-timeout #f)
       attribute-names: (attribute-names #f)
       message-attribute-names: (message-attribute-names #f))
  (let ((payload (make-hash-table)))
    (hash-put! payload "QueueUrl" queue-url)
    (when max-number-of-messages
      (hash-put! payload "MaxNumberOfMessages" max-number-of-messages))
    (when wait-time-seconds (hash-put! payload "WaitTimeSeconds" wait-time-seconds))
    (when visibility-timeout (hash-put! payload "VisibilityTimeout" visibility-timeout))
    (when attribute-names
      (hash-put! payload "AttributeNames" (list->vector attribute-names)))
    (when message-attribute-names
      (hash-put! payload "MessageAttributeNames" (list->vector message-attribute-names)))
    (sqs-action client "ReceiveMessage" payload)))

(def (delete-message client queue-url receipt-handle)
  (let ((payload (make-hash-table)))
    (hash-put! payload "QueueUrl" queue-url)
    (hash-put! payload "ReceiptHandle" receipt-handle)
    (sqs-action client "DeleteMessage" payload))
  (void))

(def (purge-queue client queue-url)
  (let ((payload (make-hash-table)))
    (hash-put! payload "QueueUrl" queue-url)
    (sqs-action client "PurgeQueue" payload))
  (void))
