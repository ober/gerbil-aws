;;; -*- Gerbil -*-
;;; EC2 Subnet operations

(import :gerbil-aws/ec2/api
        :gerbil-aws/ec2/params)
(export describe-subnets create-subnet delete-subnet modify-subnet-attribute)

(def (describe-subnets client
       subnet-ids: (subnet-ids [])
       filters: (filters []))
  (ec2-action/items client "DescribeSubnets" 'subnetSet
    (params-merge
      (ec2-param-list "SubnetId" subnet-ids)
      (ec2-param-filters filters))))

(def (create-subnet client
       vpc-id: vpc-id
       cidr-block: cidr-block
       availability-zone: (availability-zone #f)
       tag-specifications: (tag-specifications #f))
  (ec2-action/hash client "CreateSubnet"
    (params-merge
      [["VpcId" :: vpc-id]
       ["CidrBlock" :: cidr-block]]
      (if availability-zone [["AvailabilityZone" :: availability-zone]] [])
      (if tag-specifications
        (ec2-param-tags "TagSpecification" 1 "subnet" tag-specifications)
        []))))

(def (delete-subnet client subnet-id)
  (ec2-action client "DeleteSubnet"
    [["SubnetId" :: subnet-id]])
  (void))

(def (modify-subnet-attribute client subnet-id
       map-public-ip-on-launch: (map-public-ip-on-launch #f))
  (ec2-action client "ModifySubnetAttribute"
    (params-merge
      [["SubnetId" :: subnet-id]]
      (if (not (eq? map-public-ip-on-launch #f))
        [["MapPublicIpOnLaunch.Value" :: (if map-public-ip-on-launch "true" "false")]]
        [])))
  (void))
