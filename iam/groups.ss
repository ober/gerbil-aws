;;; -*- Gerbil -*-
;;; IAM Group operations

(import :gerbil-aws/iam/api)
(export list-groups create-group delete-group
        add-user-to-group remove-user-from-group)

(def (list-groups client
       path-prefix: (path-prefix #f)
       max-items: (max-items #f)
       marker: (marker #f))
  (iam-action/items client "ListGroups" 'Groups
    (append
      (if path-prefix [["PathPrefix" :: path-prefix]] [])
      (if max-items [["MaxItems" :: max-items]] [])
      (if marker [["Marker" :: marker]] []))
    item-tag: 'member))

(def (create-group client group-name
       path: (path #f))
  (iam-action/hash client "CreateGroup"
    (append
      [["GroupName" :: group-name]]
      (if path [["Path" :: path]] []))))

(def (delete-group client group-name)
  (iam-action client "DeleteGroup"
    [["GroupName" :: group-name]])
  (void))

(def (add-user-to-group client group-name user-name)
  (iam-action client "AddUserToGroup"
    [["GroupName" :: group-name]
     ["UserName" :: user-name]])
  (void))

(def (remove-user-from-group client group-name user-name)
  (iam-action client "RemoveUserFromGroup"
    [["GroupName" :: group-name]
     ["UserName" :: user-name]])
  (void))
