;;; -*- Gerbil -*-
;;; EC2 Launch Template operations

(import :gerbil-aws/ec2/api
        :gerbil-aws/ec2/params)
(export describe-launch-templates create-launch-template delete-launch-template)

(def (describe-launch-templates client
       launch-template-ids: (launch-template-ids [])
       launch-template-names: (launch-template-names [])
       filters: (filters [])
       max-results: (max-results #f)
       next-token: (next-token #f))
  (ec2-action/items client "DescribeLaunchTemplates" 'launchTemplates
    (params-merge
      (ec2-param-list "LaunchTemplateId" launch-template-ids)
      (ec2-param-list "LaunchTemplateName" launch-template-names)
      (ec2-param-filters filters)
      (if max-results [["MaxResults" :: max-results]] [])
      (if next-token [["NextToken" :: next-token]] []))))

(def (create-launch-template client
       launch-template-name: launch-template-name
       image-id: (image-id #f)
       instance-type: (instance-type #f)
       key-name: (key-name #f)
       security-group-ids: (security-group-ids [])
       user-data: (user-data #f)
       tag-specifications: (tag-specifications #f))
  (ec2-action/hash client "CreateLaunchTemplate"
    (params-merge
      [["LaunchTemplateName" :: launch-template-name]]
      (if image-id [["LaunchTemplateData.ImageId" :: image-id]] [])
      (if instance-type [["LaunchTemplateData.InstanceType" :: instance-type]] [])
      (if key-name [["LaunchTemplateData.KeyName" :: key-name]] [])
      (let loop ((sgs security-group-ids) (i 1) (acc []))
        (if (null? sgs)
          (reverse acc)
          (loop (cdr sgs) (+ i 1)
                (cons (cons (string-append "LaunchTemplateData.SecurityGroupId." (number->string i))
                            (car sgs))
                      acc))))
      (if user-data [["LaunchTemplateData.UserData" :: user-data]] [])
      (if tag-specifications
        (ec2-param-tags "TagSpecification" 1 "launch-template" tag-specifications)
        []))))

(def (delete-launch-template client
       launch-template-id: (launch-template-id #f)
       launch-template-name: (launch-template-name #f))
  (ec2-action client "DeleteLaunchTemplate"
    (params-merge
      (if launch-template-id [["LaunchTemplateId" :: launch-template-id]] [])
      (if launch-template-name [["LaunchTemplateName" :: launch-template-name]] [])))
  (void))
