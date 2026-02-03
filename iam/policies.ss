;;; -*- Gerbil -*-
;;; IAM Policy operations

(import :gerbil-aws/iam/api)
(export list-policies create-policy delete-policy
        attach-user-policy detach-user-policy
        attach-role-policy detach-role-policy)

(def (list-policies client
       scope: (scope #f)
       only-attached: (only-attached #f)
       path-prefix: (path-prefix #f)
       max-items: (max-items #f)
       marker: (marker #f))
  (iam-action/items client "ListPolicies" 'Policies
    (append
      (if scope [["Scope" :: scope]] [])
      (if only-attached [["OnlyAttached" :: "true"]] [])
      (if path-prefix [["PathPrefix" :: path-prefix]] [])
      (if max-items [["MaxItems" :: max-items]] [])
      (if marker [["Marker" :: marker]] []))
    item-tag: 'member))

(def (create-policy client policy-name policy-document
       path: (path #f)
       description: (description #f))
  (iam-action/hash client "CreatePolicy"
    (append
      [["PolicyName" :: policy-name]
       ["PolicyDocument" :: policy-document]]
      (if path [["Path" :: path]] [])
      (if description [["Description" :: description]] []))))

(def (delete-policy client policy-arn)
  (iam-action client "DeletePolicy"
    [["PolicyArn" :: policy-arn]])
  (void))

(def (attach-user-policy client user-name policy-arn)
  (iam-action client "AttachUserPolicy"
    [["UserName" :: user-name]
     ["PolicyArn" :: policy-arn]])
  (void))

(def (detach-user-policy client user-name policy-arn)
  (iam-action client "DetachUserPolicy"
    [["UserName" :: user-name]
     ["PolicyArn" :: policy-arn]])
  (void))

(def (attach-role-policy client role-name policy-arn)
  (iam-action client "AttachRolePolicy"
    [["RoleName" :: role-name]
     ["PolicyArn" :: policy-arn]])
  (void))

(def (detach-role-policy client role-name policy-arn)
  (iam-action client "DetachRolePolicy"
    [["RoleName" :: role-name]
     ["PolicyArn" :: policy-arn]])
  (void))
