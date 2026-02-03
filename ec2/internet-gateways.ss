;;; -*- Gerbil -*-
;;; EC2 Internet Gateway operations

(import :gerbil-aws/ec2/api
        :gerbil-aws/ec2/params)
(export describe-internet-gateways create-internet-gateway delete-internet-gateway
        attach-internet-gateway detach-internet-gateway)

(def (describe-internet-gateways client
       internet-gateway-ids: (internet-gateway-ids [])
       filters: (filters []))
  (ec2-action/items client "DescribeInternetGateways" 'internetGatewaySet
    (params-merge
      (ec2-param-list "InternetGatewayId" internet-gateway-ids)
      (ec2-param-filters filters))))

(def (create-internet-gateway client
       tag-specifications: (tag-specifications #f))
  (ec2-action/hash client "CreateInternetGateway"
    (if tag-specifications
      (ec2-param-tags "TagSpecification" 1 "internet-gateway" tag-specifications)
      [])))

(def (delete-internet-gateway client internet-gateway-id)
  (ec2-action client "DeleteInternetGateway"
    [["InternetGatewayId" :: internet-gateway-id]])
  (void))

(def (attach-internet-gateway client
       internet-gateway-id: internet-gateway-id
       vpc-id: vpc-id)
  (ec2-action client "AttachInternetGateway"
    [["InternetGatewayId" :: internet-gateway-id]
     ["VpcId" :: vpc-id]])
  (void))

(def (detach-internet-gateway client
       internet-gateway-id: internet-gateway-id
       vpc-id: vpc-id)
  (ec2-action client "DetachInternetGateway"
    [["InternetGatewayId" :: internet-gateway-id]
     ["VpcId" :: vpc-id]])
  (void))
