;;; -*- Gerbil -*-
;;; IAM Role operations

(import :gerbil-aws/iam/api)
(export list-roles create-role delete-role get-role)

(def (list-roles client
       path-prefix: (path-prefix #f)
       max-items: (max-items #f)
       marker: (marker #f))
  (iam-action/items client "ListRoles" 'Roles
    (append
      (if path-prefix [["PathPrefix" :: path-prefix]] [])
      (if max-items [["MaxItems" :: max-items]] [])
      (if marker [["Marker" :: marker]] []))
    item-tag: 'member))

(def (create-role client role-name assume-role-policy-document
       path: (path #f)
       description: (description #f)
       max-session-duration: (max-session-duration #f))
  (iam-action/hash client "CreateRole"
    (append
      [["RoleName" :: role-name]
       ["AssumeRolePolicyDocument" :: assume-role-policy-document]]
      (if path [["Path" :: path]] [])
      (if description [["Description" :: description]] [])
      (if max-session-duration [["MaxSessionDuration" :: max-session-duration]] []))))

(def (delete-role client role-name)
  (iam-action client "DeleteRole"
    [["RoleName" :: role-name]])
  (void))

(def (get-role client role-name)
  (iam-action/hash client "GetRole"
    [["RoleName" :: role-name]]))
