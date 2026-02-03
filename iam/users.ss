;;; -*- Gerbil -*-
;;; IAM User operations

(import :gerbil-aws/iam/api)
(export list-users create-user delete-user get-user)

(def (list-users client
       path-prefix: (path-prefix #f)
       max-items: (max-items #f)
       marker: (marker #f))
  (iam-action/items client "ListUsers" 'Users
    (append
      (if path-prefix [["PathPrefix" :: path-prefix]] [])
      (if max-items [["MaxItems" :: max-items]] [])
      (if marker [["Marker" :: marker]] []))
    item-tag: 'member))

(def (create-user client user-name
       path: (path #f)
       tags: (tags []))
  (iam-action/hash client "CreateUser"
    (append
      [["UserName" :: user-name]]
      (if path [["Path" :: path]] [])
      (let loop ((ts tags) (i 1) (acc []))
        (if (null? ts)
          (reverse acc)
          (loop (cdr ts) (+ i 1)
                (cons (cons (string-append "Tags.member." (number->string i) ".Value") (cdar ts))
                      (cons (cons (string-append "Tags.member." (number->string i) ".Key") (caar ts))
                            acc))))))))

(def (delete-user client user-name)
  (iam-action client "DeleteUser"
    [["UserName" :: user-name]])
  (void))

(def (get-user client user-name: (user-name #f))
  (iam-action/hash client "GetUser"
    (if user-name [["UserName" :: user-name]] [])))
