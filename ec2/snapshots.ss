;;; -*- Gerbil -*-
;;; EC2 EBS Snapshot operations

(import :gerbil-aws/ec2/api
        :gerbil-aws/ec2/params)
(export describe-snapshots create-snapshot delete-snapshot)

(def (describe-snapshots client
       snapshot-ids: (snapshot-ids [])
       owner-ids: (owner-ids [])
       filters: (filters []))
  (ec2-action/items client "DescribeSnapshots" 'snapshotSet
    (params-merge
      (ec2-param-list "SnapshotId" snapshot-ids)
      (ec2-param-list "Owner" owner-ids)
      (ec2-param-filters filters))))

(def (create-snapshot client
       volume-id: volume-id
       description: (description #f)
       tag-specifications: (tag-specifications #f))
  (ec2-action/hash client "CreateSnapshot"
    (params-merge
      [["VolumeId" :: volume-id]]
      (if description [["Description" :: description]] [])
      (if tag-specifications
        (ec2-param-tags "TagSpecification" 1 "snapshot" tag-specifications)
        []))))

(def (delete-snapshot client snapshot-id)
  (ec2-action client "DeleteSnapshot"
    [["SnapshotId" :: snapshot-id]])
  (void))
