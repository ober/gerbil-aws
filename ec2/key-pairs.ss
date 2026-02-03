;;; -*- Gerbil -*-
;;; EC2 Key Pair operations

(import :gerbil-aws/ec2/api
        :gerbil-aws/ec2/params)
(export describe-key-pairs create-key-pair delete-key-pair import-key-pair)

(def (describe-key-pairs client
       key-names: (key-names [])
       key-pair-ids: (key-pair-ids [])
       filters: (filters []))
  (ec2-action/items client "DescribeKeyPairs" 'keySet
    (params-merge
      (ec2-param-list "KeyName" key-names)
      (ec2-param-list "KeyPairId" key-pair-ids)
      (ec2-param-filters filters))))

(def (create-key-pair client key-name
       key-type: (key-type #f)
       tag-specifications: (tag-specifications #f))
  (ec2-action/hash client "CreateKeyPair"
    (params-merge
      [["KeyName" :: key-name]]
      (if key-type [["KeyType" :: key-type]] [])
      (if tag-specifications
        (ec2-param-tags "TagSpecification" 1 "key-pair" tag-specifications)
        []))))

(def (delete-key-pair client key-name: (key-name #f) key-pair-id: (key-pair-id #f))
  (ec2-action client "DeleteKeyPair"
    (params-merge
      (if key-name [["KeyName" :: key-name]] [])
      (if key-pair-id [["KeyPairId" :: key-pair-id]] [])))
  (void))

(def (import-key-pair client key-name public-key-material)
  (ec2-action/hash client "ImportKeyPair"
    [["KeyName" :: key-name]
     ["PublicKeyMaterial" :: public-key-material]]))
