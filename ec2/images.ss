;;; -*- Gerbil -*-
;;; EC2 AMI operations

(import :gerbil-aws/ec2/api
        :gerbil-aws/ec2/params)
(export describe-images create-image deregister-image)

(def (describe-images client
       image-ids: (image-ids [])
       owners: (owners [])
       filters: (filters []))
  (ec2-action/items client "DescribeImages" 'imagesSet
    (params-merge
      (ec2-param-list "ImageId" image-ids)
      (ec2-param-list "Owner" owners)
      (ec2-param-filters filters))))

(def (create-image client
       instance-id: instance-id
       name: name
       description: (description #f)
       no-reboot: (no-reboot #f)
       tag-specifications: (tag-specifications #f))
  (ec2-action/hash client "CreateImage"
    (params-merge
      [["InstanceId" :: instance-id]
       ["Name" :: name]]
      (if description [["Description" :: description]] [])
      (if no-reboot [["NoReboot" :: "true"]] [])
      (if tag-specifications
        (ec2-param-tags "TagSpecification" 1 "image" tag-specifications)
        []))))

(def (deregister-image client image-id)
  (ec2-action client "DeregisterImage"
    [["ImageId" :: image-id]])
  (void))
