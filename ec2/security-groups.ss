;;; -*- Gerbil -*-
;;; EC2 Security Group operations

(import :gerbil-aws/ec2/api
        :gerbil-aws/ec2/params)
(export describe-security-groups create-security-group delete-security-group
        authorize-security-group-ingress authorize-security-group-egress
        revoke-security-group-ingress revoke-security-group-egress)

(def (describe-security-groups client
       group-ids: (group-ids [])
       group-names: (group-names [])
       filters: (filters []))
  (ec2-action/items client "DescribeSecurityGroups" 'securityGroupInfo
    (params-merge
      (ec2-param-list "GroupId" group-ids)
      (ec2-param-list "GroupName" group-names)
      (ec2-param-filters filters))))

(def (create-security-group client
       group-name: group-name
       description: description
       vpc-id: (vpc-id #f)
       tag-specifications: (tag-specifications #f))
  (ec2-action/hash client "CreateSecurityGroup"
    (params-merge
      [["GroupName" :: group-name]
       ["GroupDescription" :: description]]
      (if vpc-id [["VpcId" :: vpc-id]] [])
      (if tag-specifications
        (ec2-param-tags "TagSpecification" 1 "security-group" tag-specifications)
        []))))

(def (delete-security-group client group-id: (group-id #f) group-name: (group-name #f))
  (ec2-action client "DeleteSecurityGroup"
    (params-merge
      (if group-id [["GroupId" :: group-id]] [])
      (if group-name [["GroupName" :: group-name]] [])))
  (void))

(def (authorize-security-group-ingress client group-id ip-permissions)
  (ec2-action client "AuthorizeSecurityGroupIngress"
    (params-merge
      [["GroupId" :: group-id]]
      (ec2-param-ip-permissions "IpPermissions" ip-permissions)))
  (void))

(def (authorize-security-group-egress client group-id ip-permissions)
  (ec2-action client "AuthorizeSecurityGroupEgress"
    (params-merge
      [["GroupId" :: group-id]]
      (ec2-param-ip-permissions "IpPermissions" ip-permissions)))
  (void))

(def (revoke-security-group-ingress client group-id ip-permissions)
  (ec2-action client "RevokeSecurityGroupIngress"
    (params-merge
      [["GroupId" :: group-id]]
      (ec2-param-ip-permissions "IpPermissions" ip-permissions)))
  (void))

(def (revoke-security-group-egress client group-id ip-permissions)
  (ec2-action client "RevokeSecurityGroupEgress"
    (params-merge
      [["GroupId" :: group-id]]
      (ec2-param-ip-permissions "IpPermissions" ip-permissions)))
  (void))
