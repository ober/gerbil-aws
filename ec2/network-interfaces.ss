;;; -*- Gerbil -*-
;;; EC2 Network Interface operations

(import :gerbil-aws/ec2/api
        :gerbil-aws/ec2/params)
(export describe-network-interfaces create-network-interface delete-network-interface)

(def (describe-network-interfaces client
       network-interface-ids: (network-interface-ids [])
       filters: (filters []))
  (ec2-action/items client "DescribeNetworkInterfaces" 'networkInterfaceSet
    (params-merge
      (ec2-param-list "NetworkInterfaceId" network-interface-ids)
      (ec2-param-filters filters))))

(def (create-network-interface client
       subnet-id: subnet-id
       description: (description #f)
       security-group-ids: (security-group-ids [])
       private-ip-address: (private-ip-address #f)
       tag-specifications: (tag-specifications #f))
  (ec2-action/hash client "CreateNetworkInterface"
    (params-merge
      [["SubnetId" :: subnet-id]]
      (if description [["Description" :: description]] [])
      (ec2-param-list "SecurityGroupId" security-group-ids)
      (if private-ip-address [["PrivateIpAddress" :: private-ip-address]] [])
      (if tag-specifications
        (ec2-param-tags "TagSpecification" 1 "network-interface" tag-specifications)
        []))))

(def (delete-network-interface client network-interface-id)
  (ec2-action client "DeleteNetworkInterface"
    [["NetworkInterfaceId" :: network-interface-id]])
  (void))
