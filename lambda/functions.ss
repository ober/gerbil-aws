;;; -*- Gerbil -*-
;;; Lambda Function operations

(import :std/text/json
        :gerbil-aws/lambda/api)
(export list-functions get-function get-function-configuration
        create-function delete-function
        update-function-code update-function-configuration
        invoke-function
        list-event-source-mappings create-event-source-mapping delete-event-source-mapping)

(def (list-functions client
       max-items: (max-items #f)
       marker: (marker #f))
  (lambda-rest-request client
    verb: 'GET
    path: "/2015-03-31/functions"
    query: (append
             (if max-items [["MaxItems" :: max-items]] [])
             (if marker [["Marker" :: marker]] []))))

(def (get-function client function-name)
  (lambda-rest-request client
    verb: 'GET
    path: (string-append "/2015-03-31/functions/" function-name)))

(def (get-function-configuration client function-name)
  (lambda-rest-request client
    verb: 'GET
    path: (string-append "/2015-03-31/functions/" function-name "/configuration")))

(def (create-function client
       function-name: function-name
       runtime: runtime
       role: role
       handler: handler
       zip-file: (zip-file #f)
       s3-bucket: (s3-bucket #f)
       s3-key: (s3-key #f)
       description: (description #f)
       timeout: (timeout #f)
       memory-size: (memory-size #f)
       environment: (environment #f))
  (let ((payload (make-hash-table)))
    (hash-put! payload "FunctionName" function-name)
    (hash-put! payload "Runtime" runtime)
    (hash-put! payload "Role" role)
    (hash-put! payload "Handler" handler)
    (let ((code (make-hash-table)))
      (when s3-bucket (hash-put! code "S3Bucket" s3-bucket))
      (when s3-key (hash-put! code "S3Key" s3-key))
      (when zip-file (hash-put! code "ZipFile" zip-file))
      (hash-put! payload "Code" code))
    (when description (hash-put! payload "Description" description))
    (when timeout (hash-put! payload "Timeout" timeout))
    (when memory-size (hash-put! payload "MemorySize" memory-size))
    (when environment
      (let ((env (make-hash-table)))
        (hash-put! env "Variables" environment)
        (hash-put! payload "Environment" env)))
    (lambda-rest-request client
      verb: 'POST
      path: "/2015-03-31/functions"
      payload: payload)))

(def (delete-function client function-name)
  (lambda-rest-request client
    verb: 'DELETE
    path: (string-append "/2015-03-31/functions/" function-name))
  (void))

(def (update-function-code client function-name
       s3-bucket: (s3-bucket #f)
       s3-key: (s3-key #f)
       zip-file: (zip-file #f))
  (let ((payload (make-hash-table)))
    (when s3-bucket (hash-put! payload "S3Bucket" s3-bucket))
    (when s3-key (hash-put! payload "S3Key" s3-key))
    (when zip-file (hash-put! payload "ZipFile" zip-file))
    (lambda-rest-request client
      verb: 'PUT
      path: (string-append "/2015-03-31/functions/" function-name "/code")
      payload: payload)))

(def (update-function-configuration client function-name
       runtime: (runtime #f)
       handler: (handler #f)
       description: (description #f)
       timeout: (timeout #f)
       memory-size: (memory-size #f)
       role: (role #f)
       environment: (environment #f))
  (let ((payload (make-hash-table)))
    (when runtime (hash-put! payload "Runtime" runtime))
    (when handler (hash-put! payload "Handler" handler))
    (when description (hash-put! payload "Description" description))
    (when timeout (hash-put! payload "Timeout" timeout))
    (when memory-size (hash-put! payload "MemorySize" memory-size))
    (when role (hash-put! payload "Role" role))
    (when environment
      (let ((env (make-hash-table)))
        (hash-put! env "Variables" environment)
        (hash-put! payload "Environment" env)))
    (lambda-rest-request client
      verb: 'PUT
      path: (string-append "/2015-03-31/functions/" function-name "/configuration")
      payload: payload)))

;; Invoke a Lambda function
;; payload is a hash table (the function's input event)
;; Returns the function's response
(def (invoke-function client function-name
       payload: (payload #f)
       invocation-type: (invocation-type #f))
  (lambda-rest-request client
    verb: 'POST
    path: (string-append "/2015-03-31/functions/" function-name "/invocations")
    query: (if invocation-type
              [["InvocationType" :: invocation-type]]
              [])
    payload: payload))

(def (list-event-source-mappings client
       function-name: (function-name #f)
       event-source-arn: (event-source-arn #f)
       max-items: (max-items #f)
       marker: (marker #f))
  (lambda-rest-request client
    verb: 'GET
    path: "/2015-03-31/event-source-mappings"
    query: (append
             (if function-name [["FunctionName" :: function-name]] [])
             (if event-source-arn [["EventSourceArn" :: event-source-arn]] [])
             (if max-items [["MaxItems" :: max-items]] [])
             (if marker [["Marker" :: marker]] []))))

(def (create-event-source-mapping client
       function-name: function-name
       event-source-arn: event-source-arn
       starting-position: (starting-position #f)
       batch-size: (batch-size #f)
       enabled: (enabled #t))
  (let ((payload (make-hash-table)))
    (hash-put! payload "FunctionName" function-name)
    (hash-put! payload "EventSourceArn" event-source-arn)
    (hash-put! payload "Enabled" enabled)
    (when starting-position (hash-put! payload "StartingPosition" starting-position))
    (when batch-size (hash-put! payload "BatchSize" batch-size))
    (lambda-rest-request client
      verb: 'POST
      path: "/2015-03-31/event-source-mappings"
      payload: payload)))

(def (delete-event-source-mapping client uuid)
  (lambda-rest-request client
    verb: 'DELETE
    path: (string-append "/2015-03-31/event-source-mappings/" uuid))
  (void))
