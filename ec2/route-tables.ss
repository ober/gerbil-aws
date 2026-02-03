;;; -*- Gerbil -*-
;;; EC2 Route Table operations

(import :gerbil-aws/ec2/api
        :gerbil-aws/ec2/params)
(export describe-route-tables create-route-table delete-route-table
        create-route delete-route associate-route-table
        disassociate-route-table replace-route)

(def (describe-route-tables client
       route-table-ids: (route-table-ids [])
       filters: (filters []))
  (ec2-action/items client "DescribeRouteTables" 'routeTableSet
    (params-merge
      (ec2-param-list "RouteTableId" route-table-ids)
      (ec2-param-filters filters))))

(def (create-route-table client vpc-id
       tag-specifications: (tag-specifications #f))
  (ec2-action/hash client "CreateRouteTable"
    (params-merge
      [["VpcId" :: vpc-id]]
      (if tag-specifications
        (ec2-param-tags "TagSpecification" 1 "route-table" tag-specifications)
        []))))

(def (delete-route-table client route-table-id)
  (ec2-action client "DeleteRouteTable"
    [["RouteTableId" :: route-table-id]])
  (void))

(def (create-route client
       route-table-id: route-table-id
       destination-cidr-block: (destination-cidr-block #f)
       destination-ipv6-cidr-block: (destination-ipv6-cidr-block #f)
       gateway-id: (gateway-id #f)
       nat-gateway-id: (nat-gateway-id #f)
       network-interface-id: (network-interface-id #f)
       instance-id: (instance-id #f))
  (ec2-action client "CreateRoute"
    (params-merge
      [["RouteTableId" :: route-table-id]]
      (if destination-cidr-block [["DestinationCidrBlock" :: destination-cidr-block]] [])
      (if destination-ipv6-cidr-block [["DestinationIpv6CidrBlock" :: destination-ipv6-cidr-block]] [])
      (if gateway-id [["GatewayId" :: gateway-id]] [])
      (if nat-gateway-id [["NatGatewayId" :: nat-gateway-id]] [])
      (if network-interface-id [["NetworkInterfaceId" :: network-interface-id]] [])
      (if instance-id [["InstanceId" :: instance-id]] [])))
  (void))

(def (delete-route client
       route-table-id: route-table-id
       destination-cidr-block: (destination-cidr-block #f)
       destination-ipv6-cidr-block: (destination-ipv6-cidr-block #f))
  (ec2-action client "DeleteRoute"
    (params-merge
      [["RouteTableId" :: route-table-id]]
      (if destination-cidr-block [["DestinationCidrBlock" :: destination-cidr-block]] [])
      (if destination-ipv6-cidr-block [["DestinationIpv6CidrBlock" :: destination-ipv6-cidr-block]] [])))
  (void))

(def (associate-route-table client
       route-table-id: route-table-id
       subnet-id: subnet-id)
  (ec2-action/hash client "AssociateRouteTable"
    [["RouteTableId" :: route-table-id]
     ["SubnetId" :: subnet-id]]))

(def (disassociate-route-table client association-id)
  (ec2-action client "DisassociateRouteTable"
    [["AssociationId" :: association-id]])
  (void))

(def (replace-route client
       route-table-id: route-table-id
       destination-cidr-block: (destination-cidr-block #f)
       destination-ipv6-cidr-block: (destination-ipv6-cidr-block #f)
       gateway-id: (gateway-id #f)
       nat-gateway-id: (nat-gateway-id #f)
       network-interface-id: (network-interface-id #f)
       instance-id: (instance-id #f))
  (ec2-action client "ReplaceRoute"
    (params-merge
      [["RouteTableId" :: route-table-id]]
      (if destination-cidr-block [["DestinationCidrBlock" :: destination-cidr-block]] [])
      (if destination-ipv6-cidr-block [["DestinationIpv6CidrBlock" :: destination-ipv6-cidr-block]] [])
      (if gateway-id [["GatewayId" :: gateway-id]] [])
      (if nat-gateway-id [["NatGatewayId" :: nat-gateway-id]] [])
      (if network-interface-id [["NetworkInterfaceId" :: network-interface-id]] [])
      (if instance-id [["InstanceId" :: instance-id]] [])))
  (void))
