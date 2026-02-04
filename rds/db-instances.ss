;;; -*- Gerbil -*-
;;; RDS DB Instance operations

(import :gerbil-aws/rds/api)
(export describe-db-instances)

;; DescribeDBInstances — list RDS database instances
;; Returns: list of DB instance hashes with symbol keys
(def (describe-db-instances client
       db-instance-identifier: (db-instance-identifier #f)
       max-records: (max-records #f)
       marker: (marker #f))
  (rds-action/items client "DescribeDBInstances" 'DBInstances
    (append
      (if db-instance-identifier
        [["DBInstanceIdentifier" :: db-instance-identifier]]
        [])
      (if max-records
        [["MaxRecords" :: (if (number? max-records)
                             (number->string max-records)
                             max-records)]]
        [])
      (if marker [["Marker" :: marker]] []))
    item-tag: 'DBInstance))
