;;; -*- Gerbil -*-
;;; CloudFormation Stack operations

(import :gerbil-aws/cfn/api)
(export list-stacks describe-stacks create-stack update-stack delete-stack
        describe-stack-events describe-stack-resources
        get-template validate-template list-stack-resources)

(def (list-stacks client
       stack-status-filter: (stack-status-filter [])
       next-token: (next-token #f))
  (cfn-action/items client "ListStacks" 'StackSummaries
    (append
      (let loop ((ss stack-status-filter) (i 1) (acc []))
        (if (null? ss)
          (reverse acc)
          (loop (cdr ss) (+ i 1)
                (cons (cons (string-append "StackStatusFilter.member." (number->string i))
                            (car ss))
                      acc))))
      (if next-token [["NextToken" :: next-token]] []))
    item-tag: 'member))

(def (describe-stacks client
       stack-name: (stack-name #f)
       next-token: (next-token #f))
  (cfn-action/items client "DescribeStacks" 'Stacks
    (append
      (if stack-name [["StackName" :: stack-name]] [])
      (if next-token [["NextToken" :: next-token]] []))
    item-tag: 'member))

;; Create a stack
;; parameters is an alist of (key . value) pairs
;; capabilities is a list of strings (e.g., ["CAPABILITY_IAM"])
(def (create-stack client stack-name
       template-body: (template-body #f)
       template-url: (template-url #f)
       parameters: (parameters [])
       capabilities: (capabilities [])
       on-failure: (on-failure #f)
       tags: (tags []))
  (cfn-action/hash client "CreateStack"
    (append
      [["StackName" :: stack-name]]
      (if template-body [["TemplateBody" :: template-body]] [])
      (if template-url [["TemplateURL" :: template-url]] [])
      (let loop ((ps parameters) (i 1) (acc []))
        (if (null? ps)
          (reverse acc)
          (loop (cdr ps) (+ i 1)
                (cons (cons (string-append "Parameters.member." (number->string i) ".ParameterValue")
                            (cdar ps))
                      (cons (cons (string-append "Parameters.member." (number->string i) ".ParameterKey")
                                  (caar ps))
                            acc)))))
      (let loop ((cs capabilities) (i 1) (acc []))
        (if (null? cs)
          (reverse acc)
          (loop (cdr cs) (+ i 1)
                (cons (cons (string-append "Capabilities.member." (number->string i))
                            (car cs))
                      acc))))
      (if on-failure [["OnFailure" :: on-failure]] [])
      (let loop ((ts tags) (i 1) (acc []))
        (if (null? ts)
          (reverse acc)
          (loop (cdr ts) (+ i 1)
                (cons (cons (string-append "Tags.member." (number->string i) ".Value") (cdar ts))
                      (cons (cons (string-append "Tags.member." (number->string i) ".Key") (caar ts))
                            acc))))))))

(def (update-stack client stack-name
       template-body: (template-body #f)
       template-url: (template-url #f)
       parameters: (parameters [])
       capabilities: (capabilities []))
  (cfn-action/hash client "UpdateStack"
    (append
      [["StackName" :: stack-name]]
      (if template-body [["TemplateBody" :: template-body]] [])
      (if template-url [["TemplateURL" :: template-url]] [])
      (let loop ((ps parameters) (i 1) (acc []))
        (if (null? ps)
          (reverse acc)
          (loop (cdr ps) (+ i 1)
                (cons (cons (string-append "Parameters.member." (number->string i) ".ParameterValue")
                            (cdar ps))
                      (cons (cons (string-append "Parameters.member." (number->string i) ".ParameterKey")
                                  (caar ps))
                            acc)))))
      (let loop ((cs capabilities) (i 1) (acc []))
        (if (null? cs)
          (reverse acc)
          (loop (cdr cs) (+ i 1)
                (cons (cons (string-append "Capabilities.member." (number->string i))
                            (car cs))
                      acc)))))))

(def (delete-stack client stack-name)
  (cfn-action client "DeleteStack"
    [["StackName" :: stack-name]])
  (void))

(def (describe-stack-events client stack-name
       next-token: (next-token #f))
  (cfn-action/items client "DescribeStackEvents" 'StackEvents
    (append
      [["StackName" :: stack-name]]
      (if next-token [["NextToken" :: next-token]] []))
    item-tag: 'member))

(def (describe-stack-resources client stack-name
       logical-resource-id: (logical-resource-id #f))
  (cfn-action/items client "DescribeStackResources" 'StackResources
    (append
      [["StackName" :: stack-name]]
      (if logical-resource-id [["LogicalResourceId" :: logical-resource-id]] []))
    item-tag: 'member))

(def (get-template client stack-name
       template-stage: (template-stage #f))
  (cfn-action/hash client "GetTemplate"
    (append
      [["StackName" :: stack-name]]
      (if template-stage [["TemplateStage" :: template-stage]] []))))

(def (validate-template client
       template-body: (template-body #f)
       template-url: (template-url #f))
  (cfn-action/hash client "ValidateTemplate"
    (append
      (if template-body [["TemplateBody" :: template-body]] [])
      (if template-url [["TemplateURL" :: template-url]] []))))

(def (list-stack-resources client stack-name
       next-token: (next-token #f))
  (cfn-action/items client "ListStackResources" 'StackResourceSummaries
    (append
      [["StackName" :: stack-name]]
      (if next-token [["NextToken" :: next-token]] []))
    item-tag: 'member))
