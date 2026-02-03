;;; -*- Gerbil -*-
;;; IAM Access Key operations

(import :gerbil-aws/iam/api)
(export list-access-keys create-access-key delete-access-key update-access-key)

(def (list-access-keys client
       user-name: (user-name #f)
       max-items: (max-items #f)
       marker: (marker #f))
  (iam-action/items client "ListAccessKeys" 'AccessKeyMetadata
    (append
      (if user-name [["UserName" :: user-name]] [])
      (if max-items [["MaxItems" :: max-items]] [])
      (if marker [["Marker" :: marker]] []))
    item-tag: 'member))

(def (create-access-key client
       user-name: (user-name #f))
  (iam-action/hash client "CreateAccessKey"
    (if user-name [["UserName" :: user-name]] [])))

(def (delete-access-key client access-key-id
       user-name: (user-name #f))
  (iam-action client "DeleteAccessKey"
    (append
      [["AccessKeyId" :: access-key-id]]
      (if user-name [["UserName" :: user-name]] [])))
  (void))

(def (update-access-key client access-key-id status
       user-name: (user-name #f))
  (iam-action client "UpdateAccessKey"
    (append
      [["AccessKeyId" :: access-key-id]
       ["Status" :: status]]
      (if user-name [["UserName" :: user-name]] [])))
  (void))
