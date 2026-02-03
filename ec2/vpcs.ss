;;; -*- Gerbil -*-
;;; EC2 VPC operations

(import :gerbil-aws/ec2/api
        :gerbil-aws/ec2/params)
(export describe-vpcs create-vpc delete-vpc)

(def (describe-vpcs client
       vpc-ids: (vpc-ids [])
       filters: (filters []))
  (ec2-action/items client "DescribeVpcs" 'vpcSet
    (params-merge
      (ec2-param-list "VpcId" vpc-ids)
      (ec2-param-filters filters))))

(def (create-vpc client
       cidr-block: cidr-block
       amazon-provided-ipv6: (amazon-provided-ipv6 #f)
       instance-tenancy: (instance-tenancy #f)
       tag-specifications: (tag-specifications #f))
  (ec2-action/hash client "CreateVpc"
    (params-merge
      [["CidrBlock" :: cidr-block]]
      (if amazon-provided-ipv6 [["AmazonProvidedIpv6CidrBlock" :: "true"]] [])
      (if instance-tenancy [["InstanceTenancy" :: instance-tenancy]] [])
      (if tag-specifications
        (ec2-param-tags "TagSpecification" 1 "vpc" tag-specifications)
        []))))

(def (delete-vpc client vpc-id)
  (ec2-action client "DeleteVpc"
    [["VpcId" :: vpc-id]])
  (void))
