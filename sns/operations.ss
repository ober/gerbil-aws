;;; -*- Gerbil -*-
;;; SNS operations

(import :gerbil-aws/sns/api)
(export list-topics create-topic delete-topic
        subscribe unsubscribe publish
        list-subscriptions list-subscriptions-by-topic
        set-topic-attributes get-topic-attributes)

(def (list-topics client
       next-token: (next-token #f))
  (sns-action/items client "ListTopics" 'Topics
    (if next-token [["NextToken" :: next-token]] [])
    item-tag: 'member))

(def (create-topic client name
       attributes: (attributes [])
       tags: (tags []))
  (sns-action/hash client "CreateTopic"
    (append
      [["Name" :: name]]
      (let loop ((as attributes) (i 1) (acc []))
        (if (null? as)
          (reverse acc)
          (loop (cdr as) (+ i 1)
                (cons (cons (string-append "Attributes.entry." (number->string i) ".value")
                            (cdar as))
                      (cons (cons (string-append "Attributes.entry." (number->string i) ".key")
                                  (caar as))
                            acc)))))
      (let loop ((ts tags) (i 1) (acc []))
        (if (null? ts)
          (reverse acc)
          (loop (cdr ts) (+ i 1)
                (cons (cons (string-append "Tags.member." (number->string i) ".Value") (cdar ts))
                      (cons (cons (string-append "Tags.member." (number->string i) ".Key") (caar ts))
                            acc))))))))

(def (delete-topic client topic-arn)
  (sns-action client "DeleteTopic"
    [["TopicArn" :: topic-arn]])
  (void))

(def (subscribe client
       topic-arn: topic-arn
       protocol: protocol
       endpoint: endpoint
       return-subscription-arn: (return-subscription-arn #f))
  (sns-action/hash client "Subscribe"
    (append
      [["TopicArn" :: topic-arn]
       ["Protocol" :: protocol]
       ["Endpoint" :: endpoint]]
      (if return-subscription-arn [["ReturnSubscriptionArn" :: "true"]] []))))

(def (unsubscribe client subscription-arn)
  (sns-action client "Unsubscribe"
    [["SubscriptionArn" :: subscription-arn]])
  (void))

(def (publish client
       topic-arn: (topic-arn #f)
       target-arn: (target-arn #f)
       message: message
       subject: (subject #f)
       message-structure: (message-structure #f))
  (sns-action/hash client "Publish"
    (append
      [["Message" :: message]]
      (if topic-arn [["TopicArn" :: topic-arn]] [])
      (if target-arn [["TargetArn" :: target-arn]] [])
      (if subject [["Subject" :: subject]] [])
      (if message-structure [["MessageStructure" :: message-structure]] []))))

(def (list-subscriptions client
       next-token: (next-token #f))
  (sns-action/items client "ListSubscriptions" 'Subscriptions
    (if next-token [["NextToken" :: next-token]] [])
    item-tag: 'member))

(def (list-subscriptions-by-topic client topic-arn
       next-token: (next-token #f))
  (sns-action/items client "ListSubscriptionsByTopic" 'Subscriptions
    (append
      [["TopicArn" :: topic-arn]]
      (if next-token [["NextToken" :: next-token]] []))
    item-tag: 'member))

(def (set-topic-attributes client topic-arn attribute-name attribute-value)
  (sns-action client "SetTopicAttributes"
    [["TopicArn" :: topic-arn]
     ["AttributeName" :: attribute-name]
     ["AttributeValue" :: attribute-value]])
  (void))

(def (get-topic-attributes client topic-arn)
  (sns-action/hash client "GetTopicAttributes"
    [["TopicArn" :: topic-arn]]))
