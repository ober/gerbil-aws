;;; -*- Gerbil -*-
;;; DynamoDB operations

(import :gerbil-aws/dynamodb/api)
(export list-tables describe-table create-table delete-table
        put-item get-item delete-item update-item
        dynamodb-query dynamodb-scan)

(def (list-tables client
       limit: (limit #f)
       exclusive-start-table-name: (exclusive-start-table-name #f))
  (let ((payload (make-hash-table)))
    (when limit (hash-put! payload "Limit" limit))
    (when exclusive-start-table-name
      (hash-put! payload "ExclusiveStartTableName" exclusive-start-table-name))
    (dynamodb-action client "ListTables" payload)))

(def (describe-table client table-name)
  (let ((payload (make-hash-table)))
    (hash-put! payload "TableName" table-name)
    (dynamodb-action client "DescribeTable" payload)))

;; Create a DynamoDB table
;; attribute-definitions: vector of hash tables with AttributeName and AttributeType
;; key-schema: vector of hash tables with AttributeName and KeyType
;; billing-mode: "PROVISIONED" or "PAY_PER_REQUEST"
(def (create-table client table-name
       attribute-definitions: attribute-definitions
       key-schema: key-schema
       billing-mode: (billing-mode "PAY_PER_REQUEST")
       provisioned-throughput: (provisioned-throughput #f))
  (let ((payload (make-hash-table)))
    (hash-put! payload "TableName" table-name)
    (hash-put! payload "AttributeDefinitions" attribute-definitions)
    (hash-put! payload "KeySchema" key-schema)
    (hash-put! payload "BillingMode" billing-mode)
    (when provisioned-throughput
      (hash-put! payload "ProvisionedThroughput" provisioned-throughput))
    (dynamodb-action client "CreateTable" payload)))

(def (delete-table client table-name)
  (let ((payload (make-hash-table)))
    (hash-put! payload "TableName" table-name)
    (dynamodb-action client "DeleteTable" payload)))

;; Put an item into a table
;; item: hash table of attribute name -> DynamoDB attribute value
;;   e.g., (hash ("id" (hash ("S" "123"))) ("name" (hash ("S" "Alice"))))
(def (put-item client table-name item
       condition-expression: (condition-expression #f)
       expression-attribute-names: (expression-attribute-names #f)
       expression-attribute-values: (expression-attribute-values #f)
       return-values: (return-values #f))
  (let ((payload (make-hash-table)))
    (hash-put! payload "TableName" table-name)
    (hash-put! payload "Item" item)
    (when condition-expression
      (hash-put! payload "ConditionExpression" condition-expression))
    (when expression-attribute-names
      (hash-put! payload "ExpressionAttributeNames" expression-attribute-names))
    (when expression-attribute-values
      (hash-put! payload "ExpressionAttributeValues" expression-attribute-values))
    (when return-values (hash-put! payload "ReturnValues" return-values))
    (dynamodb-action client "PutItem" payload)))

;; Get an item from a table
;; key: hash table of key attribute name -> DynamoDB attribute value
(def (get-item client table-name key
       consistent-read: (consistent-read #f)
       projection-expression: (projection-expression #f)
       expression-attribute-names: (expression-attribute-names #f))
  (let ((payload (make-hash-table)))
    (hash-put! payload "TableName" table-name)
    (hash-put! payload "Key" key)
    (when consistent-read (hash-put! payload "ConsistentRead" consistent-read))
    (when projection-expression
      (hash-put! payload "ProjectionExpression" projection-expression))
    (when expression-attribute-names
      (hash-put! payload "ExpressionAttributeNames" expression-attribute-names))
    (dynamodb-action client "GetItem" payload)))

(def (delete-item client table-name key
       condition-expression: (condition-expression #f)
       expression-attribute-names: (expression-attribute-names #f)
       expression-attribute-values: (expression-attribute-values #f)
       return-values: (return-values #f))
  (let ((payload (make-hash-table)))
    (hash-put! payload "TableName" table-name)
    (hash-put! payload "Key" key)
    (when condition-expression
      (hash-put! payload "ConditionExpression" condition-expression))
    (when expression-attribute-names
      (hash-put! payload "ExpressionAttributeNames" expression-attribute-names))
    (when expression-attribute-values
      (hash-put! payload "ExpressionAttributeValues" expression-attribute-values))
    (when return-values (hash-put! payload "ReturnValues" return-values))
    (dynamodb-action client "DeleteItem" payload)))

(def (update-item client table-name key
       update-expression: update-expression
       condition-expression: (condition-expression #f)
       expression-attribute-names: (expression-attribute-names #f)
       expression-attribute-values: (expression-attribute-values #f)
       return-values: (return-values #f))
  (let ((payload (make-hash-table)))
    (hash-put! payload "TableName" table-name)
    (hash-put! payload "Key" key)
    (hash-put! payload "UpdateExpression" update-expression)
    (when condition-expression
      (hash-put! payload "ConditionExpression" condition-expression))
    (when expression-attribute-names
      (hash-put! payload "ExpressionAttributeNames" expression-attribute-names))
    (when expression-attribute-values
      (hash-put! payload "ExpressionAttributeValues" expression-attribute-values))
    (when return-values (hash-put! payload "ReturnValues" return-values))
    (dynamodb-action client "UpdateItem" payload)))

;; Query a table (named dynamodb-query to avoid conflict with standard query)
(def (dynamodb-query client table-name
       key-condition-expression: key-condition-expression
       filter-expression: (filter-expression #f)
       projection-expression: (projection-expression #f)
       expression-attribute-names: (expression-attribute-names #f)
       expression-attribute-values: (expression-attribute-values #f)
       index-name: (index-name #f)
       scan-index-forward: (scan-index-forward #f)
       limit: (limit #f)
       consistent-read: (consistent-read #f)
       exclusive-start-key: (exclusive-start-key #f))
  (let ((payload (make-hash-table)))
    (hash-put! payload "TableName" table-name)
    (hash-put! payload "KeyConditionExpression" key-condition-expression)
    (when filter-expression
      (hash-put! payload "FilterExpression" filter-expression))
    (when projection-expression
      (hash-put! payload "ProjectionExpression" projection-expression))
    (when expression-attribute-names
      (hash-put! payload "ExpressionAttributeNames" expression-attribute-names))
    (when expression-attribute-values
      (hash-put! payload "ExpressionAttributeValues" expression-attribute-values))
    (when index-name (hash-put! payload "IndexName" index-name))
    (when (not (eq? scan-index-forward #f))
      (hash-put! payload "ScanIndexForward" scan-index-forward))
    (when limit (hash-put! payload "Limit" limit))
    (when consistent-read (hash-put! payload "ConsistentRead" consistent-read))
    (when exclusive-start-key
      (hash-put! payload "ExclusiveStartKey" exclusive-start-key))
    (dynamodb-action client "Query" payload)))

;; Scan a table
(def (dynamodb-scan client table-name
       filter-expression: (filter-expression #f)
       projection-expression: (projection-expression #f)
       expression-attribute-names: (expression-attribute-names #f)
       expression-attribute-values: (expression-attribute-values #f)
       index-name: (index-name #f)
       limit: (limit #f)
       consistent-read: (consistent-read #f)
       exclusive-start-key: (exclusive-start-key #f))
  (let ((payload (make-hash-table)))
    (hash-put! payload "TableName" table-name)
    (when filter-expression
      (hash-put! payload "FilterExpression" filter-expression))
    (when projection-expression
      (hash-put! payload "ProjectionExpression" projection-expression))
    (when expression-attribute-names
      (hash-put! payload "ExpressionAttributeNames" expression-attribute-names))
    (when expression-attribute-values
      (hash-put! payload "ExpressionAttributeValues" expression-attribute-values))
    (when index-name (hash-put! payload "IndexName" index-name))
    (when limit (hash-put! payload "Limit" limit))
    (when consistent-read (hash-put! payload "ConsistentRead" consistent-read))
    (when exclusive-start-key
      (hash-put! payload "ExclusiveStartKey" exclusive-start-key))
    (dynamodb-action client "Scan" payload)))
