;;; -*- Gerbil -*-
;;; EC2 Elastic IP Address operations

(import :gerbil-aws/ec2/api
        :gerbil-aws/ec2/params)
(export describe-addresses allocate-address release-address
        associate-address disassociate-address)

(def (describe-addresses client
       allocation-ids: (allocation-ids [])
       public-ips: (public-ips [])
       filters: (filters []))
  (ec2-action/items client "DescribeAddresses" 'addressesSet
    (params-merge
      (ec2-param-list "AllocationId" allocation-ids)
      (ec2-param-list "PublicIp" public-ips)
      (ec2-param-filters filters))))

(def (allocate-address client
       domain: (domain "vpc")
       tag-specifications: (tag-specifications #f))
  (ec2-action/hash client "AllocateAddress"
    (params-merge
      [["Domain" :: domain]]
      (if tag-specifications
        (ec2-param-tags "TagSpecification" 1 "elastic-ip" tag-specifications)
        []))))

(def (release-address client
       allocation-id: (allocation-id #f)
       public-ip: (public-ip #f))
  (ec2-action client "ReleaseAddress"
    (params-merge
      (if allocation-id [["AllocationId" :: allocation-id]] [])
      (if public-ip [["PublicIp" :: public-ip]] [])))
  (void))

(def (associate-address client
       instance-id: (instance-id #f)
       allocation-id: (allocation-id #f)
       public-ip: (public-ip #f)
       network-interface-id: (network-interface-id #f)
       allow-reassociation: (allow-reassociation #f))
  (ec2-action/hash client "AssociateAddress"
    (params-merge
      (if instance-id [["InstanceId" :: instance-id]] [])
      (if allocation-id [["AllocationId" :: allocation-id]] [])
      (if public-ip [["PublicIp" :: public-ip]] [])
      (if network-interface-id [["NetworkInterfaceId" :: network-interface-id]] [])
      (if allow-reassociation [["AllowReassociation" :: "true"]] []))))

(def (disassociate-address client
       association-id: (association-id #f)
       public-ip: (public-ip #f))
  (ec2-action client "DisassociateAddress"
    (params-merge
      (if association-id [["AssociationId" :: association-id]] [])
      (if public-ip [["PublicIp" :: public-ip]] [])))
  (void))
