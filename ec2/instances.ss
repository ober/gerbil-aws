;;; -*- Gerbil -*-
;;; EC2 Instance operations

(import :gerbil-aws/ec2/api
        :gerbil-aws/ec2/params)
(export describe-instances run-instances start-instances stop-instances
        terminate-instances reboot-instances
        describe-instance-status get-console-output)

(def (describe-instances client
       instance-ids: (instance-ids [])
       filters: (filters [])
       max-results: (max-results #f)
       next-token: (next-token #f))
  (ec2-action/items client "DescribeInstances" 'reservationSet
    (params-merge
      (ec2-param-list "InstanceId" instance-ids)
      (ec2-param-filters filters)
      (if max-results [["MaxResults" :: max-results]] [])
      (if next-token [["NextToken" :: next-token]] []))))

(def (run-instances client
       image-id: image-id
       instance-type: (instance-type "t2.micro")
       min-count: (min-count "1")
       max-count: (max-count "1")
       key-name: (key-name #f)
       security-group-ids: (security-group-ids [])
       subnet-id: (subnet-id #f)
       user-data: (user-data #f)
       tag-specifications: (tag-specifications #f)
       block-device-mappings: (block-device-mappings []))
  (ec2-action/items client "RunInstances" 'instancesSet
    (params-merge
      [["ImageId" :: image-id]
       ["InstanceType" :: instance-type]
       ["MinCount" :: min-count]
       ["MaxCount" :: max-count]]
      (if key-name [["KeyName" :: key-name]] [])
      (ec2-param-list "SecurityGroupId" security-group-ids)
      (if subnet-id [["SubnetId" :: subnet-id]] [])
      (if user-data [["UserData" :: user-data]] [])
      (if tag-specifications
        (ec2-param-tags "TagSpecification" 1 "instance" tag-specifications)
        [])
      (ec2-param-block-device-mappings "BlockDeviceMapping" block-device-mappings))))

(def (start-instances client instance-ids)
  (ec2-action/items client "StartInstances" 'instancesSet
    (ec2-param-list "InstanceId" instance-ids)))

(def (stop-instances client instance-ids force: (force #f))
  (ec2-action/items client "StopInstances" 'instancesSet
    (params-merge
      (ec2-param-list "InstanceId" instance-ids)
      (if force [["Force" :: "true"]] []))))

(def (terminate-instances client instance-ids)
  (ec2-action/items client "TerminateInstances" 'instancesSet
    (ec2-param-list "InstanceId" instance-ids)))

(def (reboot-instances client instance-ids)
  (ec2-action client "RebootInstances"
    (ec2-param-list "InstanceId" instance-ids))
  (void))

(def (describe-instance-status client
       instance-ids: (instance-ids [])
       filters: (filters [])
       include-all: (include-all #f))
  (ec2-action/items client "DescribeInstanceStatus" 'instanceStatusSet
    (params-merge
      (ec2-param-list "InstanceId" instance-ids)
      (ec2-param-filters filters)
      (if include-all [["IncludeAllInstances" :: "true"]] []))))

(def (get-console-output client instance-id latest: (latest #f))
  (ec2-action/hash client "GetConsoleOutput"
    (params-merge
      [["InstanceId" :: instance-id]]
      (if latest [["Latest" :: "true"]] []))))
