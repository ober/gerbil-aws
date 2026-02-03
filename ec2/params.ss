;;; -*- Gerbil -*-
;;; EC2 Query API parameter encoding
;;; Handles dot-notation for lists, filters, tags, block devices, IP permissions

(export ec2-param-list
        ec2-param-filters
        ec2-param-tags
        ec2-param-block-device-mappings
        ec2-param-ip-permissions
        params-merge)

;; Encode a list of values with dot-notation numbering
;; (ec2-param-list "InstanceId" '("i-abc" "i-def"))
;; => (("InstanceId.1" . "i-abc") ("InstanceId.2" . "i-def"))
(def (ec2-param-list prefix values)
  (let loop ((vs values) (i 1) (acc []))
    (if (null? vs)
      (reverse acc)
      (loop (cdr vs) (+ i 1)
            (cons (cons (string-append prefix "." (number->string i))
                        (car vs))
                  acc)))))

;; Encode filters for EC2 describe calls
;; filters is a list of (name . (value ...)) pairs
;; (ec2-param-filters '(("instance-type" "t2.micro" "t3.micro") ("state" "running")))
;; => Filter.1.Name=instance-type&Filter.1.Value.1=t2.micro&Filter.1.Value.2=t3.micro&...
(def (ec2-param-filters filters)
  (let loop ((fs filters) (i 1) (acc []))
    (if (null? fs)
      (reverse acc)
      (let* ((f (car fs))
             (name (car f))
             (values (cdr f))
             (prefix (string-append "Filter." (number->string i)))
             (name-param (cons (string-append prefix ".Name") name))
             (value-params (ec2-param-list (string-append prefix ".Value") values)))
        (loop (cdr fs) (+ i 1)
              (append (reverse (cons name-param value-params)) acc))))))

;; Encode tag specifications for RunInstances etc.
;; tags is an alist of (key . value) pairs
;; resource-type is e.g. "instance", "volume"
;; index is the TagSpecification index (1-based)
(def (ec2-param-tags prefix index resource-type tags)
  (let* ((base (string-append prefix "." (number->string index)))
         (rt-param (cons (string-append base ".ResourceType") resource-type)))
    (let loop ((ts tags) (i 1) (acc [rt-param]))
      (if (null? ts)
        (reverse acc)
        (let ((tag (car ts)))
          (loop (cdr ts) (+ i 1)
                (cons (cons (string-append base ".Tag." (number->string i) ".Value") (cdr tag))
                      (cons (cons (string-append base ".Tag." (number->string i) ".Key") (car tag))
                            acc))))))))

;; Encode block device mappings
;; mappings is a list of alists with keys: device-name, volume-size, volume-type,
;; delete-on-termination, snapshot-id, encrypted
(def (ec2-param-block-device-mappings prefix mappings)
  (let loop ((ms mappings) (i 1) (acc []))
    (if (null? ms)
      (reverse acc)
      (let* ((m (car ms))
             (base (string-append prefix "." (number->string i)))
             (params
              (append
               (if (assoc "device-name" m)
                 [(cons (string-append base ".DeviceName")
                        (cdr (assoc "device-name" m)))]
                 [])
               (if (assoc "volume-size" m)
                 [(cons (string-append base ".Ebs.VolumeSize")
                        (cdr (assoc "volume-size" m)))]
                 [])
               (if (assoc "volume-type" m)
                 [(cons (string-append base ".Ebs.VolumeType")
                        (cdr (assoc "volume-type" m)))]
                 [])
               (if (assoc "snapshot-id" m)
                 [(cons (string-append base ".Ebs.SnapshotId")
                        (cdr (assoc "snapshot-id" m)))]
                 [])
               (if (assoc "delete-on-termination" m)
                 [(cons (string-append base ".Ebs.DeleteOnTermination")
                        (cdr (assoc "delete-on-termination" m)))]
                 [])
               (if (assoc "encrypted" m)
                 [(cons (string-append base ".Ebs.Encrypted")
                        (cdr (assoc "encrypted" m)))]
                 []))))
        (loop (cdr ms) (+ i 1) (append (reverse params) acc))))))

;; Encode IP permissions for security group rules
;; perms is a list of alists with keys: protocol, from-port, to-port, cidr-ipv4, cidr-ipv6
(def (ec2-param-ip-permissions prefix perms)
  (let loop ((ps perms) (i 1) (acc []))
    (if (null? ps)
      (reverse acc)
      (let* ((p (car ps))
             (base (string-append prefix "." (number->string i)))
             (params
              (append
               (if (assoc "protocol" p)
                 [(cons (string-append base ".IpProtocol")
                        (cdr (assoc "protocol" p)))]
                 [])
               (if (assoc "from-port" p)
                 [(cons (string-append base ".FromPort")
                        (cdr (assoc "from-port" p)))]
                 [])
               (if (assoc "to-port" p)
                 [(cons (string-append base ".ToPort")
                        (cdr (assoc "to-port" p)))]
                 [])
               ;; CIDR IPv4 ranges
               (if (assoc "cidr-ipv4" p)
                 (let ((cidrs (cdr (assoc "cidr-ipv4" p))))
                   (if (list? cidrs)
                     (let lp ((cs cidrs) (j 1) (cacc []))
                       (if (null? cs)
                         (reverse cacc)
                         (lp (cdr cs) (+ j 1)
                             (cons (cons (string-append base ".IpRanges." (number->string j) ".CidrIp")
                                         (car cs))
                                   cacc))))
                     [(cons (string-append base ".IpRanges.1.CidrIp") cidrs)]))
                 [])
               ;; CIDR IPv6 ranges
               (if (assoc "cidr-ipv6" p)
                 (let ((cidrs (cdr (assoc "cidr-ipv6" p))))
                   (if (list? cidrs)
                     (let lp ((cs cidrs) (j 1) (cacc []))
                       (if (null? cs)
                         (reverse cacc)
                         (lp (cdr cs) (+ j 1)
                             (cons (cons (string-append base ".Ipv6Ranges." (number->string j) ".CidrIpv6")
                                         (car cs))
                                   cacc))))
                     [(cons (string-append base ".Ipv6Ranges.1.CidrIpv6") cidrs)]))
                 [])
               ;; Security group references
               (if (assoc "group-id" p)
                 (let ((groups (cdr (assoc "group-id" p))))
                   (if (list? groups)
                     (let lp ((gs groups) (j 1) (cacc []))
                       (if (null? gs)
                         (reverse cacc)
                         (lp (cdr gs) (+ j 1)
                             (cons (cons (string-append base ".UserIdGroupPairs." (number->string j) ".GroupId")
                                         (car gs))
                                   cacc))))
                     [(cons (string-append base ".UserIdGroupPairs.1.GroupId") groups)]))
                 []))))
        (loop (cdr ps) (+ i 1) (append (reverse params) acc))))))

;; Merge multiple parameter alists into one
(def (params-merge . param-lists)
  (apply append param-lists))
