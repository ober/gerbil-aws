;;; -*- Gerbil -*-
;;; Tests for EC2 parameter encoding

(import :std/test
        :gerbil-aws/ec2/params)
(export params-test)

(def params-test
  (test-suite "EC2 parameter encoding"

    (test-case "ec2-param-list with empty list"
      (check (ec2-param-list "InstanceId" []) => []))

    (test-case "ec2-param-list with single value"
      (let (result (ec2-param-list "InstanceId" ["i-abc"]))
        (check (length result) => 1)
        (check (caar result) => "InstanceId.1")
        (check (cdar result) => "i-abc")))

    (test-case "ec2-param-list with multiple values"
      (let (result (ec2-param-list "InstanceId" ["i-abc" "i-def" "i-ghi"]))
        (check (length result) => 3)
        (check (caar result) => "InstanceId.1")
        (check (cdar result) => "i-abc")
        (check (car (cadr result)) => "InstanceId.2")
        (check (cdr (cadr result)) => "i-def")
        (check (car (caddr result)) => "InstanceId.3")
        (check (cdr (caddr result)) => "i-ghi")))

    (test-case "ec2-param-filters with empty list"
      (check (ec2-param-filters []) => []))

    (test-case "ec2-param-filters with single filter"
      (let (result (ec2-param-filters (list (list "instance-type" "t2.micro"))))
        (check (length result) => 2)
        (check (cdr (assoc "Filter.1.Name" result)) => "instance-type")
        (check (cdr (assoc "Filter.1.Value.1" result)) => "t2.micro")))

    (test-case "ec2-param-filters with multiple values"
      (let (result (ec2-param-filters (list (list "instance-type" "t2.micro" "t3.micro"))))
        (check (length result) => 3)
        (check (cdr (assoc "Filter.1.Name" result)) => "instance-type")
        (check (cdr (assoc "Filter.1.Value.1" result)) => "t2.micro")
        (check (cdr (assoc "Filter.1.Value.2" result)) => "t3.micro")))

    (test-case "ec2-param-filters with multiple filters"
      (let (result (ec2-param-filters (list (list "instance-type" "t2.micro")
                                            (list "state" "running"))))
        (check (length result) => 4)
        (check (cdr (assoc "Filter.1.Name" result)) => "instance-type")
        (check (cdr (assoc "Filter.2.Name" result)) => "state")))

    (test-case "ec2-param-tags basic"
      (let (result (ec2-param-tags "TagSpecification" 1 "instance"
                     (list (cons "Name" "my-server") (cons "Env" "prod"))))
        (check (cdr (assoc "TagSpecification.1.ResourceType" result)) => "instance")
        (check (cdr (assoc "TagSpecification.1.Tag.1.Key" result)) => "Name")
        (check (cdr (assoc "TagSpecification.1.Tag.1.Value" result)) => "my-server")
        (check (cdr (assoc "TagSpecification.1.Tag.2.Key" result)) => "Env")
        (check (cdr (assoc "TagSpecification.1.Tag.2.Value" result)) => "prod")))

    (test-case "ec2-param-block-device-mappings"
      (let (result (ec2-param-block-device-mappings "BlockDeviceMapping"
                     (list (list (cons "device-name" "/dev/sda1")
                                 (cons "volume-size" "20")
                                 (cons "volume-type" "gp2")))))
        (check (cdr (assoc "BlockDeviceMapping.1.DeviceName" result)) => "/dev/sda1")
        (check (cdr (assoc "BlockDeviceMapping.1.Ebs.VolumeSize" result)) => "20")
        (check (cdr (assoc "BlockDeviceMapping.1.Ebs.VolumeType" result)) => "gp2")))

    (test-case "ec2-param-ip-permissions basic TCP rule"
      (let (result (ec2-param-ip-permissions "IpPermissions"
                     (list (list (cons "protocol" "tcp")
                                 (cons "from-port" "22")
                                 (cons "to-port" "22")
                                 (cons "cidr-ipv4" "0.0.0.0/0")))))
        (check (cdr (assoc "IpPermissions.1.IpProtocol" result)) => "tcp")
        (check (cdr (assoc "IpPermissions.1.FromPort" result)) => "22")
        (check (cdr (assoc "IpPermissions.1.ToPort" result)) => "22")
        (check (cdr (assoc "IpPermissions.1.IpRanges.1.CidrIp" result)) => "0.0.0.0/0")))

    (test-case "params-merge"
      (let (result (params-merge
                     (list (cons "a" "1"))
                     (list (cons "b" "2"))
                     (list (cons "c" "3"))))
        (check (length result) => 3)
        (check (cdr (assoc "a" result)) => "1")
        (check (cdr (assoc "b" result)) => "2")
        (check (cdr (assoc "c" result)) => "3")))

    (test-case "params-merge with empty lists"
      (let (result (params-merge [] (list (cons "a" "1")) []))
        (check (length result) => 1)
        (check (cdr (assoc "a" result)) => "1")))))
