;;; -*- Gerbil -*-
;;; EC2 NAT Gateway operations

(import :gerbil-aws/ec2/api
        :gerbil-aws/ec2/params)
(export describe-nat-gateways create-nat-gateway delete-nat-gateway)

(def (describe-nat-gateways client
       nat-gateway-ids: (nat-gateway-ids [])
       filters: (filters []))
  (ec2-action/items client "DescribeNatGateways" 'natGatewaySet
    (params-merge
      (ec2-param-list "NatGatewayId" nat-gateway-ids)
      (ec2-param-filters filters))))

(def (create-nat-gateway client
       subnet-id: subnet-id
       allocation-id: allocation-id
       connectivity-type: (connectivity-type #f)
       tag-specifications: (tag-specifications #f))
  (ec2-action/hash client "CreateNatGateway"
    (params-merge
      [["SubnetId" :: subnet-id]
       ["AllocationId" :: allocation-id]]
      (if connectivity-type [["ConnectivityType" :: connectivity-type]] [])
      (if tag-specifications
        (ec2-param-tags "TagSpecification" 1 "natgateway" tag-specifications)
        []))))

(def (delete-nat-gateway client nat-gateway-id)
  (ec2-action/hash client "DeleteNatGateway"
    [["NatGatewayId" :: nat-gateway-id]]))
