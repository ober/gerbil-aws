;;; -*- Gerbil -*-
;;; CloudWatch Monitoring operations

(import :gerbil-aws/cloudwatch/api
        :gerbil-aws/aws/xml)
(export get-metric-statistics get-metric-data list-metrics
        describe-alarms list-dashboards)

;; Encode dimensions as member-list parameters
;; dimensions: list of (name . value) pairs
;; prefix: e.g. "Dimensions"
(def (encode-dimensions prefix dimensions)
  (let loop ((ds dimensions) (i 1) (acc []))
    (if (null? ds)
      (reverse acc)
      (let ((d (car ds)))
        (loop (cdr ds) (+ i 1)
              (cons (cons (string-append prefix ".member." (number->string i) ".Value")
                          (cdr d))
                    (cons (cons (string-append prefix ".member." (number->string i) ".Name")
                                (car d))
                          acc)))))))

;; Encode a list of strings as member-list parameters
;; (encode-member-list "Statistics" '("Average" "Maximum"))
;; => (("Statistics.member.1" . "Average") ("Statistics.member.2" . "Maximum"))
(def (encode-member-list prefix values)
  (let loop ((vs values) (i 1) (acc []))
    (if (null? vs)
      (reverse acc)
      (loop (cdr vs) (+ i 1)
            (cons (cons (string-append prefix ".member." (number->string i))
                        (car vs))
                  acc)))))

;; GetMetricStatistics — retrieve datapoints for a metric
;; dimensions: list of (name . value) pairs, e.g. '(("InstanceId" . "i-abc"))
;; statistics: list of statistic names, e.g. '("Average" "Maximum")
;; Returns: list of datapoint hashes with symbol keys
(def (get-metric-statistics client
       namespace: namespace
       metric-name: metric-name
       start-time: start-time
       end-time: end-time
       period: period
       statistics: statistics
       dimensions: (dimensions [])
       unit: (unit #f))
  (cw-action/items client "GetMetricStatistics" 'Datapoints
    (append
      [["Namespace" :: namespace]
       ["MetricName" :: metric-name]
       ["StartTime" :: start-time]
       ["EndTime" :: end-time]
       ["Period" :: (if (number? period) (number->string period) period)]]
      (encode-member-list "Statistics" statistics)
      (encode-dimensions "Dimensions" dimensions)
      (if unit [["Unit" :: unit]] []))
    item-tag: 'member))

;; Extract a list from an XML member-list structure
;; Handles: #f -> [], string -> [string], list -> list, {member: list} -> list, {member: string} -> [string]
(def (extract-member-list raw)
  (cond
    ((not raw) [])
    ((list? raw) raw)
    ((hash-table? raw)
     (let ((m (hash-get raw 'member)))
       (cond
         ((list? m) m)
         ((string? m) [m])
         ((not m) [])
         (else [m]))))
    ((string? raw) [raw])
    (else [])))

;; GetMetricData — retrieve data for multiple metrics at once
;; metric-data-queries: list of query specs, each is a hash with keys:
;;   Id: string, MetricStat: hash with Metric/Period/Stat, or Expression: string
;; Returns: list of result hashes with 'Id, 'Values (list of strings), 'Timestamps (list), 'StatusCode
(def (get-metric-data client
       metric-data-queries: queries
       start-time: start-time
       end-time: end-time)
  (let ((raw-results
         (cw-action/items client "GetMetricData" 'MetricDataResults
           (append
             [["StartTime" :: start-time]
              ["EndTime" :: end-time]]
             (encode-metric-data-queries queries))
           item-tag: 'member)))
    ;; Clean up Values and Timestamps from {member: ...} to plain lists
    (map (lambda (r)
           (let ((ht (make-hash-table)))
             (hash-put! ht 'Id (hash-ref r 'Id ""))
             (hash-put! ht 'Values (extract-member-list (hash-get r 'Values)))
             (hash-put! ht 'Timestamps (extract-member-list (hash-get r 'Timestamps)))
             (hash-put! ht 'StatusCode (hash-ref r 'StatusCode ""))
             ht))
         raw-results)))

;; Encode metric data queries for GetMetricData
(def (encode-metric-data-queries queries)
  (let loop ((qs queries) (i 1) (acc []))
    (if (null? qs)
      (reverse acc)
      (let* ((q (car qs))
             (prefix (string-append "MetricDataQueries.member." (number->string i)))
             (params
              (append
               [(cons (string-append prefix ".Id") (hash-ref q "Id"))]
               (if (hash-key? q "MetricStat")
                 (encode-metric-stat (string-append prefix ".MetricStat")
                                     (hash-ref q "MetricStat"))
                 [])
               (if (hash-key? q "Expression")
                 [(cons (string-append prefix ".Expression") (hash-ref q "Expression"))]
                 [])
               (if (hash-key? q "Label")
                 [(cons (string-append prefix ".Label") (hash-ref q "Label"))]
                 [])
               (if (hash-key? q "ReturnData")
                 [(cons (string-append prefix ".ReturnData")
                        (if (hash-ref q "ReturnData") "true" "false"))]
                 []))))
        (loop (cdr qs) (+ i 1) (append (reverse params) acc))))))

;; Encode a MetricStat structure
(def (encode-metric-stat prefix ms)
  (let ((metric (hash-ref ms "Metric")))
    (append
     [(cons (string-append prefix ".Metric.Namespace")
            (hash-ref metric "Namespace"))
      (cons (string-append prefix ".Metric.MetricName")
            (hash-ref metric "MetricName"))]
     (if (hash-key? metric "Dimensions")
       (encode-dimensions
        (string-append prefix ".Metric.Dimensions")
        ;; Convert from hash list format to alist format
        (map (lambda (d)
               (cons (hash-ref d "Name") (hash-ref d "Value")))
             (if (vector? (hash-ref metric "Dimensions"))
               (vector->list (hash-ref metric "Dimensions"))
               (hash-ref metric "Dimensions"))))
       [])
     [(cons (string-append prefix ".Period")
            (let ((p (hash-ref ms "Period")))
              (if (number? p) (number->string p) p)))
      (cons (string-append prefix ".Stat")
            (hash-ref ms "Stat"))])))

;; ListMetrics — list available metrics
;; dimensions: optional list of (name . value) pairs to filter by
(def (list-metrics client
       namespace: (namespace #f)
       metric-name: (metric-name #f)
       dimensions: (dimensions #f)
       next-token: (next-token #f))
  (cw-action/items client "ListMetrics" 'Metrics
    (append
      (if namespace [["Namespace" :: namespace]] [])
      (if metric-name [["MetricName" :: metric-name]] [])
      (if dimensions (encode-dimensions "Dimensions" dimensions) [])
      (if next-token [["NextToken" :: next-token]] []))
    item-tag: 'member))

;; DescribeAlarms — list configured alarms
;; Returns: hash with 'MetricAlarms (list) and 'CompositeAlarms (list)
(def (describe-alarms client
       alarm-names: (alarm-names #f)
       state-value: (state-value #f)
       next-token: (next-token #f)
       max-records: (max-records #f))
  (let* ((full-hash (cw-action/hash client "DescribeAlarms"
                      (append
                        (if alarm-names (encode-member-list "AlarmNames" alarm-names) [])
                        (if state-value [["StateValue" :: state-value]] [])
                        (if next-token [["NextToken" :: next-token]] [])
                        (if max-records [["MaxRecords" :: (if (number? max-records)
                                                             (number->string max-records)
                                                             max-records)]] []))))
         ;; Navigate to the result — hash may be wrapped in DescribeAlarmsResult
         (result (or (hash-get full-hash 'DescribeAlarmsResult) full-hash))
         (result (if (hash-table? result) result (hash))))
    (let ((ht (make-hash-table)))
      (hash-put! ht 'MetricAlarms (extract-member-list (hash-get result 'MetricAlarms)))
      (hash-put! ht 'CompositeAlarms (extract-member-list (hash-get result 'CompositeAlarms)))
      ht)))

;; ListDashboards — list CloudWatch dashboards
(def (list-dashboards client
       dashboard-name-prefix: (dashboard-name-prefix #f)
       next-token: (next-token #f))
  (cw-action/items client "ListDashboards" 'DashboardEntries
    (append
      (if dashboard-name-prefix
        [["DashboardNamePrefix" :: dashboard-name-prefix]]
        [])
      (if next-token [["NextToken" :: next-token]] []))
    item-tag: 'member))
