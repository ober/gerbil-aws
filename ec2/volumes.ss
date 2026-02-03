;;; -*- Gerbil -*-
;;; EC2 EBS Volume operations

(import :gerbil-aws/ec2/api
        :gerbil-aws/ec2/params)
(export describe-volumes create-volume delete-volume attach-volume detach-volume)

(def (describe-volumes client
       volume-ids: (volume-ids [])
       filters: (filters []))
  (ec2-action/items client "DescribeVolumes" 'volumeSet
    (params-merge
      (ec2-param-list "VolumeId" volume-ids)
      (ec2-param-filters filters))))

(def (create-volume client
       availability-zone: availability-zone
       size: (size #f)
       snapshot-id: (snapshot-id #f)
       volume-type: (volume-type #f)
       iops: (iops #f)
       encrypted: (encrypted #f)
       tag-specifications: (tag-specifications #f))
  (ec2-action/hash client "CreateVolume"
    (params-merge
      [["AvailabilityZone" :: availability-zone]]
      (if size [["Size" :: size]] [])
      (if snapshot-id [["SnapshotId" :: snapshot-id]] [])
      (if volume-type [["VolumeType" :: volume-type]] [])
      (if iops [["Iops" :: iops]] [])
      (if encrypted [["Encrypted" :: "true"]] [])
      (if tag-specifications
        (ec2-param-tags "TagSpecification" 1 "volume" tag-specifications)
        []))))

(def (delete-volume client volume-id)
  (ec2-action client "DeleteVolume"
    [["VolumeId" :: volume-id]])
  (void))

(def (attach-volume client
       volume-id: volume-id
       instance-id: instance-id
       device: device)
  (ec2-action/hash client "AttachVolume"
    [["VolumeId" :: volume-id]
     ["InstanceId" :: instance-id]
     ["Device" :: device]]))

(def (detach-volume client volume-id
       instance-id: (instance-id #f)
       device: (device #f)
       force: (force #f))
  (ec2-action/hash client "DetachVolume"
    (params-merge
      [["VolumeId" :: volume-id]]
      (if instance-id [["InstanceId" :: instance-id]] [])
      (if device [["Device" :: device]] [])
      (if force [["Force" :: "true"]] []))))
