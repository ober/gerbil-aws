;;; -*- Gerbil -*-
;;; CLI entry point for gerbil-aws

(import :std/cli/getopt
        :std/format
        :std/sugar
        :std/text/json
        ;; EC2
        :gerbil-aws/ec2/api
        :gerbil-aws/ec2/instances
        :gerbil-aws/ec2/images
        :gerbil-aws/ec2/security-groups
        :gerbil-aws/ec2/key-pairs
        :gerbil-aws/ec2/vpcs
        :gerbil-aws/ec2/subnets
        :gerbil-aws/ec2/volumes
        :gerbil-aws/ec2/snapshots
        :gerbil-aws/ec2/addresses
        :gerbil-aws/ec2/network-interfaces
        :gerbil-aws/ec2/route-tables
        :gerbil-aws/ec2/internet-gateways
        :gerbil-aws/ec2/nat-gateways
        :gerbil-aws/ec2/tags
        :gerbil-aws/ec2/regions
        :gerbil-aws/ec2/launch-templates
        ;; S3
        :gerbil-aws/s3/api
        :gerbil-aws/s3/buckets
        :gerbil-aws/s3/objects
        ;; STS
        :gerbil-aws/sts/api
        :gerbil-aws/sts/operations
        ;; IAM
        :gerbil-aws/iam/api
        :gerbil-aws/iam/users
        :gerbil-aws/iam/groups
        :gerbil-aws/iam/roles
        :gerbil-aws/iam/policies
        :gerbil-aws/iam/access-keys
        ;; Lambda
        :gerbil-aws/lambda/api
        :gerbil-aws/lambda/functions
        ;; CloudWatch Logs
        :gerbil-aws/logs/api
        :gerbil-aws/logs/operations
        ;; DynamoDB
        :gerbil-aws/dynamodb/api
        :gerbil-aws/dynamodb/operations
        ;; SNS
        :gerbil-aws/sns/api
        :gerbil-aws/sns/operations
        ;; SQS
        :gerbil-aws/sqs/api
        :gerbil-aws/sqs/operations
        ;; CloudFormation
        :gerbil-aws/cfn/api
        :gerbil-aws/cfn/stacks
        ;; CLI
        :gerbil-aws/cli/format)
(export main)

;; Helper: split comma-separated string into list, empty string -> empty list
(def (parse-csv str)
  (if (or (not str) (equal? str ""))
    []
    (string-split str #\,)))

;; Helper: parse filter strings "name=val1,val2;name2=val3" into filter list
(def (parse-filters str)
  (if (or (not str) (equal? str ""))
    []
    (map (lambda (f)
           (let (parts (string-split f #\=))
             (if (>= (length parts) 2)
               (cons (car parts) (string-split (cadr parts) #\,))
               (cons (car parts) []))))
         (string-split str #\;))))

;; Helper: parse tags string "Key1=Value1,Key2=Value2" into alist
(def (parse-tags str)
  (if (or (not str) (equal? str ""))
    []
    (map (lambda (kv)
           (let (parts (string-split kv #\=))
             (if (>= (length parts) 2)
               (cons (car parts) (cadr parts))
               (cons (car parts) ""))))
         (string-split str #\,))))

;; Helper: non-empty string or #f
(def (nonempty str)
  (and str (not (equal? str "")) str))

;; Helper: read a file as u8vector
(def (read-file-bytes path)
  (let* ((p (open-input-file path))
         (buf (make-u8vector 65536)))
    (let loop ((chunks []))
      (let (n (read-u8vector buf p))
        (if (> n 0)
          (loop (cons (subu8vector buf 0 n) chunks))
          (begin
            (close-port p)
            (apply u8vector-append (reverse chunks))))))))

;; Helper: read a file as string
(def (read-file-string path)
  (call-with-input-file path
    (lambda (p)
      (let loop ((lines []))
        (let (line (read-line p))
          (if (eof-object? line)
            (string-join (reverse lines) "\n")
            (loop (cons line lines))))))))

;; Helper: parse params string "Key1=Value1,Key2=Value2" into alist
(def (parse-params str)
  (if (or (not str) (equal? str ""))
    []
    (map (lambda (kv)
           (let (parts (string-split kv #\=))
             (if (>= (length parts) 2)
               (cons (car parts) (cadr parts))
               (cons (car parts) ""))))
         (string-split str #\,))))

;; Shared options
(def profile-opt
  (option 'profile "--profile" "-p"
    default: #f
    help: "AWS profile name"))

(def region-opt
  (option 'region "--region" "-r"
    default: #f
    help: "AWS region"))

(def output-opt
  (option 'output "--output" "-o"
    default: "json"
    help: "Output format: json, text, table"))

;; Create EC2 client from CLI options hash
(def (make-client opt)
  (let-hash opt
    (EC2Client profile: .?profile region: .?region)))

;; Create S3 client from CLI options hash
(def (make-s3-client* opt)
  (let-hash opt
    (S3Client profile: .?profile region: .?region)))

;; Create STS client from CLI options hash
(def (make-sts-client opt)
  (let-hash opt
    (STSClient profile: .?profile region: .?region)))

;; Create IAM client from CLI options hash
(def (make-iam-client opt)
  (let-hash opt
    (IAMClient profile: .?profile region: .?region)))

;; Create Lambda client from CLI options hash
(def (make-lambda-client* opt)
  (let-hash opt
    (LambdaClient profile: .?profile region: .?region)))

;; Create CloudWatch Logs client from CLI options hash
(def (make-logs-client opt)
  (let-hash opt
    (LogsClient profile: .?profile region: .?region)))

;; Create DynamoDB client from CLI options hash
(def (make-dynamodb-client opt)
  (let-hash opt
    (DynamoDBClient profile: .?profile region: .?region)))

;; Create SNS client from CLI options hash
(def (make-sns-client opt)
  (let-hash opt
    (SNSClient profile: .?profile region: .?region)))

;; Create SQS client from CLI options hash
(def (make-sqs-client opt)
  (let-hash opt
    (SQSClient profile: .?profile region: .?region)))

;; Create CloudFormation client from CLI options hash
(def (make-cfn-client opt)
  (let-hash opt
    (CFNClient profile: .?profile region: .?region)))

;;; ---- Command Definitions ----

;; === EC2 Instances ===
(def describe-instances-cmd
  (command 'describe-instances
    help: "Describe EC2 instances"
    profile-opt region-opt output-opt
    (option 'instance-ids "--instance-ids" "-i"
      default: ""
      help: "Comma-separated instance IDs")
    (option 'filters "--filters" "-f"
      default: ""
      help: "Filters: name=val1,val2;name2=val3")))

(def run-instances-cmd
  (command 'run-instances
    help: "Launch new EC2 instances"
    profile-opt region-opt output-opt
    (option 'image-id "--image-id"
      help: "AMI image ID (required)")
    (option 'instance-type "--instance-type" "-t"
      default: "t2.micro"
      help: "Instance type")
    (option 'count "--count" "-c"
      default: "1"
      help: "Number of instances")
    (option 'key-name "--key-name" "-k"
      default: ""
      help: "Key pair name")
    (option 'security-group-ids "--security-group-ids" "-s"
      default: ""
      help: "Comma-separated security group IDs")
    (option 'subnet-id "--subnet-id"
      default: ""
      help: "Subnet ID")))

(def start-instances-cmd
  (command 'start-instances
    help: "Start stopped instances"
    profile-opt region-opt output-opt
    (option 'instance-ids "--instance-ids" "-i"
      help: "Comma-separated instance IDs (required)")))

(def stop-instances-cmd
  (command 'stop-instances
    help: "Stop running instances"
    profile-opt region-opt output-opt
    (option 'instance-ids "--instance-ids" "-i"
      help: "Comma-separated instance IDs (required)")
    (flag 'force "--force"
      help: "Force stop")))

(def terminate-instances-cmd
  (command 'terminate-instances
    help: "Terminate instances"
    profile-opt region-opt output-opt
    (option 'instance-ids "--instance-ids" "-i"
      help: "Comma-separated instance IDs (required)")))

(def reboot-instances-cmd
  (command 'reboot-instances
    help: "Reboot instances"
    profile-opt region-opt output-opt
    (option 'instance-ids "--instance-ids" "-i"
      help: "Comma-separated instance IDs (required)")))

(def describe-instance-status-cmd
  (command 'describe-instance-status
    help: "Describe instance status"
    profile-opt region-opt output-opt
    (option 'instance-ids "--instance-ids" "-i"
      default: ""
      help: "Comma-separated instance IDs")
    (flag 'include-all "--include-all"
      help: "Include all instances")))

(def get-console-output-cmd
  (command 'get-console-output
    help: "Get console output of an instance"
    profile-opt region-opt output-opt
    (option 'instance-id "--instance-id" "-i"
      help: "Instance ID (required)")
    (flag 'latest "--latest"
      help: "Get latest output only")))

(def modify-instance-attribute-cmd
  (command 'modify-instance-attribute
    help: "Modify an instance attribute"
    profile-opt region-opt output-opt
    (option 'instance-id "--instance-id" "-i"
      help: "Instance ID (required)")
    (option 'instance-type "--instance-type" "-t"
      default: ""
      help: "New instance type")))

(def describe-instance-types-cmd
  (command 'describe-instance-types
    help: "Describe available instance types"
    profile-opt region-opt output-opt
    (option 'instance-types "--instance-types"
      default: ""
      help: "Comma-separated instance types")
    (option 'filters "--filters" "-f"
      default: ""
      help: "Filters: name=val1,val2;name2=val3")))

;; === EC2 Images ===
(def describe-images-cmd
  (command 'describe-images
    help: "Describe AMI images"
    profile-opt region-opt output-opt
    (option 'image-ids "--image-ids" "-i"
      default: ""
      help: "Comma-separated image IDs")
    (option 'owners "--owners"
      default: ""
      help: "Comma-separated owner IDs or 'self'")
    (option 'filters "--filters" "-f"
      default: ""
      help: "Filters: name=val1,val2;name2=val3")))

(def create-image-cmd
  (command 'create-image
    help: "Create an AMI from an instance"
    profile-opt region-opt output-opt
    (option 'instance-id "--instance-id" "-i"
      help: "Instance ID (required)")
    (option 'name "--name" "-n"
      help: "Image name (required)")
    (option 'description "--description" "-d"
      default: ""
      help: "Image description")
    (flag 'no-reboot "--no-reboot"
      help: "Don't reboot instance before creating image")))

(def deregister-image-cmd
  (command 'deregister-image
    help: "Deregister an AMI"
    profile-opt region-opt output-opt
    (option 'image-id "--image-id" "-i"
      help: "Image ID (required)")))

;; === EC2 Security Groups ===
(def describe-security-groups-cmd
  (command 'describe-security-groups
    help: "Describe security groups"
    profile-opt region-opt output-opt
    (option 'group-ids "--group-ids" "-g"
      default: ""
      help: "Comma-separated group IDs")
    (option 'group-names "--group-names"
      default: ""
      help: "Comma-separated group names")
    (option 'filters "--filters" "-f"
      default: ""
      help: "Filters: name=val1,val2;name2=val3")))

(def create-security-group-cmd
  (command 'create-security-group
    help: "Create a security group"
    profile-opt region-opt output-opt
    (option 'group-name "--group-name" "-n"
      help: "Group name (required)")
    (option 'description "--description" "-d"
      help: "Group description (required)")
    (option 'vpc-id "--vpc-id"
      default: ""
      help: "VPC ID")))

(def delete-security-group-cmd
  (command 'delete-security-group
    help: "Delete a security group"
    profile-opt region-opt output-opt
    (option 'group-id "--group-id" "-g"
      help: "Security group ID (required)")))

(def authorize-ingress-cmd
  (command 'authorize-security-group-ingress
    help: "Add inbound rule to security group"
    profile-opt region-opt output-opt
    (option 'group-id "--group-id" "-g"
      help: "Security group ID (required)")
    (option 'protocol "--protocol"
      default: "tcp"
      help: "IP protocol (tcp, udp, icmp, -1 for all)")
    (option 'from-port "--from-port"
      help: "Start port (required)")
    (option 'to-port "--to-port"
      help: "End port (required)")
    (option 'cidr "--cidr"
      default: ""
      help: "CIDR block (e.g., 0.0.0.0/0)")))

(def revoke-ingress-cmd
  (command 'revoke-security-group-ingress
    help: "Remove inbound rule from security group"
    profile-opt region-opt output-opt
    (option 'group-id "--group-id" "-g"
      help: "Security group ID (required)")
    (option 'protocol "--protocol"
      default: "tcp"
      help: "IP protocol")
    (option 'from-port "--from-port"
      help: "Start port (required)")
    (option 'to-port "--to-port"
      help: "End port (required)")
    (option 'cidr "--cidr"
      default: ""
      help: "CIDR block")))

(def authorize-egress-cmd
  (command 'authorize-security-group-egress
    help: "Add outbound rule to security group"
    profile-opt region-opt output-opt
    (option 'group-id "--group-id" "-g"
      help: "Security group ID (required)")
    (option 'protocol "--protocol"
      default: "tcp"
      help: "IP protocol (tcp, udp, icmp, -1 for all)")
    (option 'from-port "--from-port"
      help: "Start port (required)")
    (option 'to-port "--to-port"
      help: "End port (required)")
    (option 'cidr "--cidr"
      default: ""
      help: "CIDR block (e.g., 0.0.0.0/0)")))

(def revoke-egress-cmd
  (command 'revoke-security-group-egress
    help: "Remove outbound rule from security group"
    profile-opt region-opt output-opt
    (option 'group-id "--group-id" "-g"
      help: "Security group ID (required)")
    (option 'protocol "--protocol"
      default: "tcp"
      help: "IP protocol")
    (option 'from-port "--from-port"
      help: "Start port (required)")
    (option 'to-port "--to-port"
      help: "End port (required)")
    (option 'cidr "--cidr"
      default: ""
      help: "CIDR block")))

;; === EC2 Key Pairs ===
(def describe-key-pairs-cmd
  (command 'describe-key-pairs
    help: "Describe key pairs"
    profile-opt region-opt output-opt
    (option 'key-names "--key-names" "-n"
      default: ""
      help: "Comma-separated key pair names")
    (option 'filters "--filters" "-f"
      default: ""
      help: "Filters: name=val1,val2;name2=val3")))

(def create-key-pair-cmd
  (command 'create-key-pair
    help: "Create a key pair"
    profile-opt region-opt output-opt
    (option 'key-name "--key-name" "-n"
      help: "Key pair name (required)")
    (option 'key-type "--key-type"
      default: ""
      help: "Key type: rsa, ed25519")))

(def delete-key-pair-cmd
  (command 'delete-key-pair
    help: "Delete a key pair"
    profile-opt region-opt output-opt
    (option 'key-name "--key-name" "-n"
      help: "Key pair name (required)")))

(def import-key-pair-cmd
  (command 'import-key-pair
    help: "Import a public key as a key pair"
    profile-opt region-opt output-opt
    (option 'key-name "--key-name" "-n"
      help: "Key pair name (required)")
    (option 'public-key-material "--public-key-material"
      help: "Base64-encoded public key material (required)")))

;; === EC2 VPCs ===
(def describe-vpcs-cmd
  (command 'describe-vpcs
    help: "Describe VPCs"
    profile-opt region-opt output-opt
    (option 'vpc-ids "--vpc-ids" "-v"
      default: ""
      help: "Comma-separated VPC IDs")
    (option 'filters "--filters" "-f"
      default: ""
      help: "Filters: name=val1,val2;name2=val3")))

(def create-vpc-cmd
  (command 'create-vpc
    help: "Create a VPC"
    profile-opt region-opt output-opt
    (option 'cidr-block "--cidr-block" "-c"
      help: "CIDR block (required)")))

(def delete-vpc-cmd
  (command 'delete-vpc
    help: "Delete a VPC"
    profile-opt region-opt output-opt
    (option 'vpc-id "--vpc-id" "-v"
      help: "VPC ID (required)")))

(def modify-vpc-attribute-cmd
  (command 'modify-vpc-attribute
    help: "Modify a VPC attribute"
    profile-opt region-opt output-opt
    (option 'vpc-id "--vpc-id" "-v"
      help: "VPC ID (required)")
    (flag 'enable-dns-support "--enable-dns-support"
      help: "Enable DNS support")
    (flag 'enable-dns-hostnames "--enable-dns-hostnames"
      help: "Enable DNS hostnames")))

;; === EC2 Subnets ===
(def describe-subnets-cmd
  (command 'describe-subnets
    help: "Describe subnets"
    profile-opt region-opt output-opt
    (option 'subnet-ids "--subnet-ids" "-s"
      default: ""
      help: "Comma-separated subnet IDs")
    (option 'filters "--filters" "-f"
      default: ""
      help: "Filters: name=val1,val2;name2=val3")))

(def create-subnet-cmd
  (command 'create-subnet
    help: "Create a subnet"
    profile-opt region-opt output-opt
    (option 'vpc-id "--vpc-id" "-v"
      help: "VPC ID (required)")
    (option 'cidr-block "--cidr-block" "-c"
      help: "CIDR block (required)")
    (option 'availability-zone "--availability-zone" "-a"
      default: ""
      help: "Availability zone")))

(def delete-subnet-cmd
  (command 'delete-subnet
    help: "Delete a subnet"
    profile-opt region-opt output-opt
    (option 'subnet-id "--subnet-id" "-s"
      help: "Subnet ID (required)")))

(def modify-subnet-attribute-cmd
  (command 'modify-subnet-attribute
    help: "Modify a subnet attribute"
    profile-opt region-opt output-opt
    (option 'subnet-id "--subnet-id" "-s"
      help: "Subnet ID (required)")
    (flag 'map-public-ip "--map-public-ip"
      help: "Enable auto-assign public IP")))

;; === EC2 Volumes ===
(def describe-volumes-cmd
  (command 'describe-volumes
    help: "Describe EBS volumes"
    profile-opt region-opt output-opt
    (option 'volume-ids "--volume-ids" "-v"
      default: ""
      help: "Comma-separated volume IDs")
    (option 'filters "--filters" "-f"
      default: ""
      help: "Filters: name=val1,val2;name2=val3")))

(def create-volume-cmd
  (command 'create-volume
    help: "Create an EBS volume"
    profile-opt region-opt output-opt
    (option 'availability-zone "--availability-zone" "-a"
      help: "Availability zone (required)")
    (option 'size "--size" "-s"
      default: ""
      help: "Volume size in GiB")
    (option 'volume-type "--volume-type" "-t"
      default: ""
      help: "Volume type: gp2, gp3, io1, io2, st1, sc1")
    (option 'snapshot-id "--snapshot-id"
      default: ""
      help: "Snapshot ID to create from")))

(def delete-volume-cmd
  (command 'delete-volume
    help: "Delete an EBS volume"
    profile-opt region-opt output-opt
    (option 'volume-id "--volume-id" "-v"
      help: "Volume ID (required)")))

(def attach-volume-cmd
  (command 'attach-volume
    help: "Attach an EBS volume to an instance"
    profile-opt region-opt output-opt
    (option 'volume-id "--volume-id" "-v"
      help: "Volume ID (required)")
    (option 'instance-id "--instance-id" "-i"
      help: "Instance ID (required)")
    (option 'device "--device" "-d"
      help: "Device name (required, e.g., /dev/sdf)")))

(def detach-volume-cmd
  (command 'detach-volume
    help: "Detach an EBS volume from an instance"
    profile-opt region-opt output-opt
    (option 'volume-id "--volume-id" "-v"
      help: "Volume ID (required)")
    (flag 'force "--force"
      help: "Force detach")))

(def modify-volume-cmd
  (command 'modify-volume
    help: "Modify an EBS volume"
    profile-opt region-opt output-opt
    (option 'volume-id "--volume-id" "-v"
      help: "Volume ID (required)")
    (option 'size "--size" "-s"
      default: ""
      help: "New size in GiB")
    (option 'volume-type "--volume-type" "-t"
      default: ""
      help: "New volume type")))

(def describe-volume-status-cmd
  (command 'describe-volume-status
    help: "Describe EBS volume status"
    profile-opt region-opt output-opt
    (option 'volume-ids "--volume-ids" "-v"
      default: ""
      help: "Comma-separated volume IDs")))

;; === EC2 Snapshots ===
(def describe-snapshots-cmd
  (command 'describe-snapshots
    help: "Describe EBS snapshots"
    profile-opt region-opt output-opt
    (option 'snapshot-ids "--snapshot-ids" "-s"
      default: ""
      help: "Comma-separated snapshot IDs")
    (option 'owner-ids "--owner-ids"
      default: ""
      help: "Comma-separated owner IDs or 'self'")
    (option 'filters "--filters" "-f"
      default: ""
      help: "Filters: name=val1,val2;name2=val3")))

(def create-snapshot-cmd
  (command 'create-snapshot
    help: "Create an EBS snapshot"
    profile-opt region-opt output-opt
    (option 'volume-id "--volume-id" "-v"
      help: "Volume ID (required)")
    (option 'description "--description" "-d"
      default: ""
      help: "Snapshot description")))

(def delete-snapshot-cmd
  (command 'delete-snapshot
    help: "Delete an EBS snapshot"
    profile-opt region-opt output-opt
    (option 'snapshot-id "--snapshot-id" "-s"
      help: "Snapshot ID (required)")))

(def copy-snapshot-cmd
  (command 'copy-snapshot
    help: "Copy an EBS snapshot"
    profile-opt region-opt output-opt
    (option 'source-snapshot-id "--source-snapshot-id"
      help: "Source snapshot ID (required)")
    (option 'source-region "--source-region"
      help: "Source region (required)")
    (option 'description "--description" "-d"
      default: ""
      help: "Description")))

;; === EC2 Addresses ===
(def describe-addresses-cmd
  (command 'describe-addresses
    help: "Describe Elastic IP addresses"
    profile-opt region-opt output-opt
    (option 'allocation-ids "--allocation-ids" "-a"
      default: ""
      help: "Comma-separated allocation IDs")
    (option 'public-ips "--public-ips"
      default: ""
      help: "Comma-separated public IPs")
    (option 'filters "--filters" "-f"
      default: ""
      help: "Filters: name=val1,val2;name2=val3")))

(def allocate-address-cmd
  (command 'allocate-address
    help: "Allocate an Elastic IP address"
    profile-opt region-opt output-opt
    (option 'domain "--domain"
      default: "vpc"
      help: "Domain: vpc or standard")))

(def release-address-cmd
  (command 'release-address
    help: "Release an Elastic IP address"
    profile-opt region-opt output-opt
    (option 'allocation-id "--allocation-id" "-a"
      help: "Allocation ID (required for VPC)")))

(def associate-address-cmd
  (command 'associate-address
    help: "Associate an Elastic IP with an instance"
    profile-opt region-opt output-opt
    (option 'allocation-id "--allocation-id" "-a"
      help: "Allocation ID (required)")
    (option 'instance-id "--instance-id" "-i"
      help: "Instance ID (required)")))

(def disassociate-address-cmd
  (command 'disassociate-address
    help: "Disassociate an Elastic IP"
    profile-opt region-opt output-opt
    (option 'association-id "--association-id" "-a"
      help: "Association ID (required)")))

;; === EC2 Network Interfaces ===
(def describe-network-interfaces-cmd
  (command 'describe-network-interfaces
    help: "Describe network interfaces"
    profile-opt region-opt output-opt
    (option 'network-interface-ids "--network-interface-ids" "-n"
      default: ""
      help: "Comma-separated ENI IDs")
    (option 'filters "--filters" "-f"
      default: ""
      help: "Filters: name=val1,val2;name2=val3")))

(def create-network-interface-cmd
  (command 'create-network-interface
    help: "Create a network interface"
    profile-opt region-opt output-opt
    (option 'subnet-id "--subnet-id" "-s"
      help: "Subnet ID (required)")
    (option 'description "--description" "-d"
      default: ""
      help: "Description")
    (option 'security-group-ids "--security-group-ids" "-g"
      default: ""
      help: "Comma-separated security group IDs")))

(def delete-network-interface-cmd
  (command 'delete-network-interface
    help: "Delete a network interface"
    profile-opt region-opt output-opt
    (option 'network-interface-id "--network-interface-id" "-n"
      help: "Network interface ID (required)")))

;; === EC2 Route Tables ===
(def describe-route-tables-cmd
  (command 'describe-route-tables
    help: "Describe route tables"
    profile-opt region-opt output-opt
    (option 'route-table-ids "--route-table-ids"
      default: ""
      help: "Comma-separated route table IDs")
    (option 'filters "--filters" "-f"
      default: ""
      help: "Filters: name=val1,val2;name2=val3")))

(def create-route-table-cmd
  (command 'create-route-table
    help: "Create a route table"
    profile-opt region-opt output-opt
    (option 'vpc-id "--vpc-id" "-v"
      help: "VPC ID (required)")))

(def delete-route-table-cmd
  (command 'delete-route-table
    help: "Delete a route table"
    profile-opt region-opt output-opt
    (option 'route-table-id "--route-table-id"
      help: "Route table ID (required)")))

(def create-route-cmd
  (command 'create-route
    help: "Create a route in a route table"
    profile-opt region-opt output-opt
    (option 'route-table-id "--route-table-id"
      help: "Route table ID (required)")
    (option 'destination-cidr "--destination-cidr" "-d"
      help: "Destination CIDR block (required)")
    (option 'gateway-id "--gateway-id" "-g"
      default: ""
      help: "Internet gateway ID")
    (option 'nat-gateway-id "--nat-gateway-id" "-n"
      default: ""
      help: "NAT gateway ID")))

(def delete-route-cmd
  (command 'delete-route
    help: "Delete a route from a route table"
    profile-opt region-opt output-opt
    (option 'route-table-id "--route-table-id"
      help: "Route table ID (required)")
    (option 'destination-cidr "--destination-cidr" "-d"
      help: "Destination CIDR block (required)")))

(def associate-route-table-cmd
  (command 'associate-route-table
    help: "Associate a route table with a subnet"
    profile-opt region-opt output-opt
    (option 'route-table-id "--route-table-id"
      help: "Route table ID (required)")
    (option 'subnet-id "--subnet-id" "-s"
      help: "Subnet ID (required)")))

(def disassociate-route-table-cmd
  (command 'disassociate-route-table
    help: "Disassociate a route table"
    profile-opt region-opt output-opt
    (option 'association-id "--association-id" "-a"
      help: "Association ID (required)")))

(def replace-route-cmd
  (command 'replace-route
    help: "Replace a route in a route table"
    profile-opt region-opt output-opt
    (option 'route-table-id "--route-table-id"
      help: "Route table ID (required)")
    (option 'destination-cidr "--destination-cidr" "-d"
      help: "Destination CIDR block (required)")
    (option 'gateway-id "--gateway-id" "-g"
      default: ""
      help: "Internet gateway ID")
    (option 'nat-gateway-id "--nat-gateway-id" "-n"
      default: ""
      help: "NAT gateway ID")))

;; === EC2 Internet Gateways ===
(def describe-internet-gateways-cmd
  (command 'describe-internet-gateways
    help: "Describe internet gateways"
    profile-opt region-opt output-opt
    (option 'internet-gateway-ids "--internet-gateway-ids" "-i"
      default: ""
      help: "Comma-separated IGW IDs")
    (option 'filters "--filters" "-f"
      default: ""
      help: "Filters: name=val1,val2;name2=val3")))

(def create-internet-gateway-cmd
  (command 'create-internet-gateway
    help: "Create an internet gateway"
    profile-opt region-opt output-opt))

(def delete-internet-gateway-cmd
  (command 'delete-internet-gateway
    help: "Delete an internet gateway"
    profile-opt region-opt output-opt
    (option 'internet-gateway-id "--internet-gateway-id" "-i"
      help: "IGW ID (required)")))

(def attach-internet-gateway-cmd
  (command 'attach-internet-gateway
    help: "Attach an internet gateway to a VPC"
    profile-opt region-opt output-opt
    (option 'internet-gateway-id "--internet-gateway-id" "-i"
      help: "IGW ID (required)")
    (option 'vpc-id "--vpc-id" "-v"
      help: "VPC ID (required)")))

(def detach-internet-gateway-cmd
  (command 'detach-internet-gateway
    help: "Detach an internet gateway from a VPC"
    profile-opt region-opt output-opt
    (option 'internet-gateway-id "--internet-gateway-id" "-i"
      help: "IGW ID (required)")
    (option 'vpc-id "--vpc-id" "-v"
      help: "VPC ID (required)")))

;; === EC2 NAT Gateways ===
(def describe-nat-gateways-cmd
  (command 'describe-nat-gateways
    help: "Describe NAT gateways"
    profile-opt region-opt output-opt
    (option 'nat-gateway-ids "--nat-gateway-ids" "-n"
      default: ""
      help: "Comma-separated NAT gateway IDs")
    (option 'filters "--filters" "-f"
      default: ""
      help: "Filters: name=val1,val2;name2=val3")))

(def create-nat-gateway-cmd
  (command 'create-nat-gateway
    help: "Create a NAT gateway"
    profile-opt region-opt output-opt
    (option 'subnet-id "--subnet-id" "-s"
      help: "Subnet ID (required)")
    (option 'allocation-id "--allocation-id" "-a"
      help: "EIP allocation ID (required)")))

(def delete-nat-gateway-cmd
  (command 'delete-nat-gateway
    help: "Delete a NAT gateway"
    profile-opt region-opt output-opt
    (option 'nat-gateway-id "--nat-gateway-id" "-n"
      help: "NAT gateway ID (required)")))

;; === EC2 Tags ===
(def describe-tags-cmd
  (command 'describe-tags
    help: "Describe tags"
    profile-opt region-opt output-opt
    (option 'filters "--filters" "-f"
      default: ""
      help: "Filters: name=val1,val2;name2=val3")))

(def create-tags-cmd
  (command 'create-tags
    help: "Create tags on resources"
    profile-opt region-opt output-opt
    (option 'resources "--resources"
      help: "Comma-separated resource IDs (required)")
    (option 'tags "--tags"
      help: "Tags as Key=Value,Key2=Value2 (required)")))

(def delete-tags-cmd
  (command 'delete-tags
    help: "Delete tags from resources"
    profile-opt region-opt output-opt
    (option 'resources "--resources"
      help: "Comma-separated resource IDs (required)")
    (option 'tags "--tags"
      help: "Tags as Key=Value,Key2=Value2 (required)")))

;; === EC2 Regions ===
(def describe-regions-cmd
  (command 'describe-regions
    help: "Describe AWS regions"
    profile-opt region-opt output-opt
    (flag 'all "--all"
      help: "Include all regions including opt-in")))

(def describe-availability-zones-cmd
  (command 'describe-availability-zones
    help: "Describe availability zones"
    profile-opt region-opt output-opt
    (flag 'all "--all"
      help: "Include all availability zones")))

;; === EC2 Launch Templates ===
(def describe-launch-templates-cmd
  (command 'describe-launch-templates
    help: "Describe launch templates"
    profile-opt region-opt output-opt
    (option 'launch-template-ids "--launch-template-ids"
      default: ""
      help: "Comma-separated launch template IDs")
    (option 'launch-template-names "--launch-template-names"
      default: ""
      help: "Comma-separated launch template names")))

(def create-launch-template-cmd
  (command 'create-launch-template
    help: "Create a launch template"
    profile-opt region-opt output-opt
    (option 'launch-template-name "--launch-template-name" "-n"
      help: "Launch template name (required)")
    (option 'image-id "--image-id"
      default: ""
      help: "AMI image ID")
    (option 'instance-type "--instance-type" "-t"
      default: ""
      help: "Instance type")))

(def delete-launch-template-cmd
  (command 'delete-launch-template
    help: "Delete a launch template"
    profile-opt region-opt output-opt
    (option 'launch-template-id "--launch-template-id"
      default: ""
      help: "Launch template ID")
    (option 'launch-template-name "--launch-template-name" "-n"
      default: ""
      help: "Launch template name")))

;; === S3 Buckets ===
(def s3-list-buckets-cmd
  (command 's3-list-buckets
    help: "List S3 buckets"
    profile-opt region-opt output-opt))

(def s3-create-bucket-cmd
  (command 's3-create-bucket
    help: "Create an S3 bucket"
    profile-opt region-opt output-opt
    (option 'bucket "--bucket" "-b"
      help: "Bucket name (required)")))

(def s3-delete-bucket-cmd
  (command 's3-delete-bucket
    help: "Delete an S3 bucket"
    profile-opt region-opt output-opt
    (option 'bucket "--bucket" "-b"
      help: "Bucket name (required)")))

(def s3-head-bucket-cmd
  (command 's3-head-bucket
    help: "Check if an S3 bucket exists"
    profile-opt region-opt output-opt
    (option 'bucket "--bucket" "-b"
      help: "Bucket name (required)")))

(def s3-get-bucket-location-cmd
  (command 's3-get-bucket-location
    help: "Get S3 bucket location"
    profile-opt region-opt output-opt
    (option 'bucket "--bucket" "-b"
      help: "Bucket name (required)")))

(def s3-get-bucket-versioning-cmd
  (command 's3-get-bucket-versioning
    help: "Get S3 bucket versioning status"
    profile-opt region-opt output-opt
    (option 'bucket "--bucket" "-b"
      help: "Bucket name (required)")))

(def s3-put-bucket-versioning-cmd
  (command 's3-put-bucket-versioning
    help: "Set S3 bucket versioning"
    profile-opt region-opt output-opt
    (option 'bucket "--bucket" "-b"
      help: "Bucket name (required)")
    (option 'status "--status"
      help: "Versioning status: Enabled or Suspended (required)")))

(def s3-get-bucket-tagging-cmd
  (command 's3-get-bucket-tagging
    help: "Get S3 bucket tags"
    profile-opt region-opt output-opt
    (option 'bucket "--bucket" "-b"
      help: "Bucket name (required)")))

(def s3-put-bucket-tagging-cmd
  (command 's3-put-bucket-tagging
    help: "Set S3 bucket tags"
    profile-opt region-opt output-opt
    (option 'bucket "--bucket" "-b"
      help: "Bucket name (required)")
    (option 'tags "--tags"
      help: "Tags as Key=Value,Key2=Value2 (required)")))

(def s3-delete-bucket-tagging-cmd
  (command 's3-delete-bucket-tagging
    help: "Delete S3 bucket tags"
    profile-opt region-opt output-opt
    (option 'bucket "--bucket" "-b"
      help: "Bucket name (required)")))

(def s3-get-bucket-policy-cmd
  (command 's3-get-bucket-policy
    help: "Get S3 bucket policy"
    profile-opt region-opt output-opt
    (option 'bucket "--bucket" "-b"
      help: "Bucket name (required)")))

(def s3-put-bucket-policy-cmd
  (command 's3-put-bucket-policy
    help: "Set S3 bucket policy"
    profile-opt region-opt output-opt
    (option 'bucket "--bucket" "-b"
      help: "Bucket name (required)")
    (option 'policy-file "--policy-file"
      help: "Path to policy JSON file (required)")))

(def s3-delete-bucket-policy-cmd
  (command 's3-delete-bucket-policy
    help: "Delete S3 bucket policy"
    profile-opt region-opt output-opt
    (option 'bucket "--bucket" "-b"
      help: "Bucket name (required)")))

;; === S3 Objects ===
(def s3-list-objects-cmd
  (command 's3-list-objects
    help: "List objects in an S3 bucket"
    profile-opt region-opt output-opt
    (option 'bucket "--bucket" "-b"
      help: "Bucket name (required)")
    (option 'prefix "--prefix"
      default: ""
      help: "Filter by key prefix")
    (option 'delimiter "--delimiter"
      default: ""
      help: "Grouping delimiter (e.g., /)")
    (option 'max-keys "--max-keys"
      default: ""
      help: "Maximum number of keys to return")
    (option 'start-after "--start-after"
      default: ""
      help: "Start listing after this key")))

(def s3-get-object-cmd
  (command 's3-get-object
    help: "Download an S3 object"
    profile-opt region-opt
    (option 'bucket "--bucket" "-b"
      help: "Bucket name (required)")
    (option 'key "--key" "-k"
      help: "Object key (required)")
    (option 'outfile "--outfile" "-o"
      default: ""
      help: "Output file path (default: stdout)")))

(def s3-put-object-cmd
  (command 's3-put-object
    help: "Upload an S3 object"
    profile-opt region-opt output-opt
    (option 'bucket "--bucket" "-b"
      help: "Bucket name (required)")
    (option 'key "--key" "-k"
      help: "Object key (required)")
    (option 'infile "--infile" "-i"
      help: "Input file path (required)")
    (option 'content-type "--content-type"
      default: "application/octet-stream"
      help: "Content type")))

(def s3-delete-object-cmd
  (command 's3-delete-object
    help: "Delete an S3 object"
    profile-opt region-opt output-opt
    (option 'bucket "--bucket" "-b"
      help: "Bucket name (required)")
    (option 'key "--key" "-k"
      help: "Object key (required)")))

(def s3-head-object-cmd
  (command 's3-head-object
    help: "Get S3 object metadata"
    profile-opt region-opt output-opt
    (option 'bucket "--bucket" "-b"
      help: "Bucket name (required)")
    (option 'key "--key" "-k"
      help: "Object key (required)")))

(def s3-copy-object-cmd
  (command 's3-copy-object
    help: "Copy an S3 object (server-side)"
    profile-opt region-opt output-opt
    (option 'bucket "--bucket" "-b"
      help: "Destination bucket name (required)")
    (option 'key "--key" "-k"
      help: "Destination object key (required)")
    (option 'copy-source "--copy-source" "-s"
      help: "Source as bucket/key (required)")))

(def s3-delete-objects-cmd
  (command 's3-delete-objects
    help: "Batch delete S3 objects"
    profile-opt region-opt output-opt
    (option 'bucket "--bucket" "-b"
      help: "Bucket name (required)")
    (option 'keys "--keys" "-k"
      help: "Comma-separated object keys (required)")))

(def s3-list-object-versions-cmd
  (command 's3-list-object-versions
    help: "List object versions"
    profile-opt region-opt output-opt
    (option 'bucket "--bucket" "-b"
      help: "Bucket name (required)")
    (option 'prefix "--prefix"
      default: ""
      help: "Key prefix filter")))

;; === STS ===
(def sts-get-caller-identity-cmd
  (command 'sts-get-caller-identity
    help: "Get STS caller identity"
    profile-opt region-opt output-opt))

(def sts-assume-role-cmd
  (command 'sts-assume-role
    help: "Assume an IAM role"
    profile-opt region-opt output-opt
    (option 'role-arn "--role-arn"
      help: "Role ARN (required)")
    (option 'role-session-name "--role-session-name"
      help: "Session name (required)")
    (option 'duration-seconds "--duration-seconds"
      default: ""
      help: "Duration in seconds")))

(def sts-get-session-token-cmd
  (command 'sts-get-session-token
    help: "Get temporary session token"
    profile-opt region-opt output-opt
    (option 'duration-seconds "--duration-seconds"
      default: ""
      help: "Duration in seconds")))

;; === IAM ===
(def iam-list-users-cmd
  (command 'iam-list-users
    help: "List IAM users"
    profile-opt region-opt output-opt))

(def iam-create-user-cmd
  (command 'iam-create-user
    help: "Create an IAM user"
    profile-opt region-opt output-opt
    (option 'user-name "--user-name" "-u"
      help: "User name (required)")))

(def iam-delete-user-cmd
  (command 'iam-delete-user
    help: "Delete an IAM user"
    profile-opt region-opt output-opt
    (option 'user-name "--user-name" "-u"
      help: "User name (required)")))

(def iam-get-user-cmd
  (command 'iam-get-user
    help: "Get IAM user details"
    profile-opt region-opt output-opt
    (option 'user-name "--user-name" "-u"
      default: ""
      help: "User name (omit for current user)")))

(def iam-list-roles-cmd
  (command 'iam-list-roles
    help: "List IAM roles"
    profile-opt region-opt output-opt))

(def iam-create-role-cmd
  (command 'iam-create-role
    help: "Create an IAM role"
    profile-opt region-opt output-opt
    (option 'role-name "--role-name"
      help: "Role name (required)")
    (option 'assume-role-policy-document "--assume-role-policy-document"
      help: "Trust policy JSON (required)")))

(def iam-delete-role-cmd
  (command 'iam-delete-role
    help: "Delete an IAM role"
    profile-opt region-opt output-opt
    (option 'role-name "--role-name"
      help: "Role name (required)")))

(def iam-get-role-cmd
  (command 'iam-get-role
    help: "Get IAM role details"
    profile-opt region-opt output-opt
    (option 'role-name "--role-name"
      help: "Role name (required)")))

(def iam-list-groups-cmd
  (command 'iam-list-groups
    help: "List IAM groups"
    profile-opt region-opt output-opt))

(def iam-create-group-cmd
  (command 'iam-create-group
    help: "Create an IAM group"
    profile-opt region-opt output-opt
    (option 'group-name "--group-name" "-g"
      help: "Group name (required)")))

(def iam-delete-group-cmd
  (command 'iam-delete-group
    help: "Delete an IAM group"
    profile-opt region-opt output-opt
    (option 'group-name "--group-name" "-g"
      help: "Group name (required)")))

(def iam-add-user-to-group-cmd
  (command 'iam-add-user-to-group
    help: "Add user to IAM group"
    profile-opt region-opt output-opt
    (option 'group-name "--group-name" "-g"
      help: "Group name (required)")
    (option 'user-name "--user-name" "-u"
      help: "User name (required)")))

(def iam-remove-user-from-group-cmd
  (command 'iam-remove-user-from-group
    help: "Remove user from IAM group"
    profile-opt region-opt output-opt
    (option 'group-name "--group-name" "-g"
      help: "Group name (required)")
    (option 'user-name "--user-name" "-u"
      help: "User name (required)")))

(def iam-list-policies-cmd
  (command 'iam-list-policies
    help: "List IAM policies"
    profile-opt region-opt output-opt
    (option 'scope "--scope"
      default: ""
      help: "Policy scope: All, AWS, Local")))

(def iam-attach-role-policy-cmd
  (command 'iam-attach-role-policy
    help: "Attach policy to IAM role"
    profile-opt region-opt output-opt
    (option 'role-name "--role-name"
      help: "Role name (required)")
    (option 'policy-arn "--policy-arn"
      help: "Policy ARN (required)")))

(def iam-detach-role-policy-cmd
  (command 'iam-detach-role-policy
    help: "Detach policy from IAM role"
    profile-opt region-opt output-opt
    (option 'role-name "--role-name"
      help: "Role name (required)")
    (option 'policy-arn "--policy-arn"
      help: "Policy ARN (required)")))

(def iam-attach-user-policy-cmd
  (command 'iam-attach-user-policy
    help: "Attach policy to IAM user"
    profile-opt region-opt output-opt
    (option 'user-name "--user-name" "-u"
      help: "User name (required)")
    (option 'policy-arn "--policy-arn"
      help: "Policy ARN (required)")))

(def iam-detach-user-policy-cmd
  (command 'iam-detach-user-policy
    help: "Detach policy from IAM user"
    profile-opt region-opt output-opt
    (option 'user-name "--user-name" "-u"
      help: "User name (required)")
    (option 'policy-arn "--policy-arn"
      help: "Policy ARN (required)")))

(def iam-list-access-keys-cmd
  (command 'iam-list-access-keys
    help: "List IAM access keys"
    profile-opt region-opt output-opt
    (option 'user-name "--user-name" "-u"
      default: ""
      help: "User name (omit for current user)")))

(def iam-create-access-key-cmd
  (command 'iam-create-access-key
    help: "Create an IAM access key"
    profile-opt region-opt output-opt
    (option 'user-name "--user-name" "-u"
      default: ""
      help: "User name (omit for current user)")))

(def iam-delete-access-key-cmd
  (command 'iam-delete-access-key
    help: "Delete an IAM access key"
    profile-opt region-opt output-opt
    (option 'access-key-id "--access-key-id"
      help: "Access key ID (required)")
    (option 'user-name "--user-name" "-u"
      default: ""
      help: "User name")))

;; === Lambda ===
(def lambda-list-functions-cmd
  (command 'lambda-list-functions
    help: "List Lambda functions"
    profile-opt region-opt output-opt))

(def lambda-get-function-cmd
  (command 'lambda-get-function
    help: "Get Lambda function details"
    profile-opt region-opt output-opt
    (option 'function-name "--function-name" "-f"
      help: "Function name (required)")))

(def lambda-invoke-cmd
  (command 'lambda-invoke
    help: "Invoke a Lambda function"
    profile-opt region-opt output-opt
    (option 'function-name "--function-name" "-f"
      help: "Function name (required)")
    (option 'payload-file "--payload-file"
      default: ""
      help: "Path to JSON payload file")))

(def lambda-delete-function-cmd
  (command 'lambda-delete-function
    help: "Delete a Lambda function"
    profile-opt region-opt output-opt
    (option 'function-name "--function-name" "-f"
      help: "Function name (required)")))

;; === CloudWatch Logs ===
(def logs-describe-log-groups-cmd
  (command 'logs-describe-log-groups
    help: "Describe CloudWatch log groups"
    profile-opt region-opt output-opt
    (option 'log-group-name-prefix "--log-group-name-prefix"
      default: ""
      help: "Log group name prefix")))

(def logs-describe-log-streams-cmd
  (command 'logs-describe-log-streams
    help: "Describe log streams in a log group"
    profile-opt region-opt output-opt
    (option 'log-group-name "--log-group-name" "-g"
      help: "Log group name (required)")
    (option 'log-stream-name-prefix "--log-stream-name-prefix"
      default: ""
      help: "Log stream name prefix")))

(def logs-create-log-group-cmd
  (command 'logs-create-log-group
    help: "Create a CloudWatch log group"
    profile-opt region-opt output-opt
    (option 'log-group-name "--log-group-name" "-g"
      help: "Log group name (required)")))

(def logs-delete-log-group-cmd
  (command 'logs-delete-log-group
    help: "Delete a CloudWatch log group"
    profile-opt region-opt output-opt
    (option 'log-group-name "--log-group-name" "-g"
      help: "Log group name (required)")))

(def logs-get-log-events-cmd
  (command 'logs-get-log-events
    help: "Get log events from a stream"
    profile-opt region-opt output-opt
    (option 'log-group-name "--log-group-name" "-g"
      help: "Log group name (required)")
    (option 'log-stream-name "--log-stream-name" "-s"
      help: "Log stream name (required)")
    (option 'limit "--limit"
      default: ""
      help: "Max events to return")))

(def logs-filter-log-events-cmd
  (command 'logs-filter-log-events
    help: "Filter log events"
    profile-opt region-opt output-opt
    (option 'log-group-name "--log-group-name" "-g"
      help: "Log group name (required)")
    (option 'filter-pattern "--filter-pattern"
      default: ""
      help: "Filter pattern")))

;; === DynamoDB ===
(def dynamodb-list-tables-cmd
  (command 'dynamodb-list-tables
    help: "List DynamoDB tables"
    profile-opt region-opt output-opt))

(def dynamodb-describe-table-cmd
  (command 'dynamodb-describe-table
    help: "Describe a DynamoDB table"
    profile-opt region-opt output-opt
    (option 'table-name "--table-name" "-t"
      help: "Table name (required)")))

(def dynamodb-delete-table-cmd
  (command 'dynamodb-delete-table
    help: "Delete a DynamoDB table"
    profile-opt region-opt output-opt
    (option 'table-name "--table-name" "-t"
      help: "Table name (required)")))

(def dynamodb-scan-cmd
  (command 'dynamodb-scan
    help: "Scan a DynamoDB table"
    profile-opt region-opt output-opt
    (option 'table-name "--table-name" "-t"
      help: "Table name (required)")
    (option 'limit "--limit"
      default: ""
      help: "Max items to return")))

;; === SNS ===
(def sns-list-topics-cmd
  (command 'sns-list-topics
    help: "List SNS topics"
    profile-opt region-opt output-opt))

(def sns-create-topic-cmd
  (command 'sns-create-topic
    help: "Create an SNS topic"
    profile-opt region-opt output-opt
    (option 'name "--name" "-n"
      help: "Topic name (required)")))

(def sns-delete-topic-cmd
  (command 'sns-delete-topic
    help: "Delete an SNS topic"
    profile-opt region-opt output-opt
    (option 'topic-arn "--topic-arn"
      help: "Topic ARN (required)")))

(def sns-publish-cmd
  (command 'sns-publish
    help: "Publish to an SNS topic"
    profile-opt region-opt output-opt
    (option 'topic-arn "--topic-arn"
      help: "Topic ARN (required)")
    (option 'message "--message" "-m"
      help: "Message (required)")
    (option 'subject "--subject"
      default: ""
      help: "Message subject")))

(def sns-subscribe-cmd
  (command 'sns-subscribe
    help: "Subscribe to an SNS topic"
    profile-opt region-opt output-opt
    (option 'topic-arn "--topic-arn"
      help: "Topic ARN (required)")
    (option 'protocol "--protocol"
      help: "Protocol: email, sms, sqs, lambda, http, https (required)")
    (option 'endpoint "--endpoint"
      help: "Endpoint (required)")))

(def sns-unsubscribe-cmd
  (command 'sns-unsubscribe
    help: "Unsubscribe from an SNS topic"
    profile-opt region-opt output-opt
    (option 'subscription-arn "--subscription-arn"
      help: "Subscription ARN (required)")))

(def sns-list-subscriptions-cmd
  (command 'sns-list-subscriptions
    help: "List SNS subscriptions"
    profile-opt region-opt output-opt))

;; === SQS ===
(def sqs-list-queues-cmd
  (command 'sqs-list-queues
    help: "List SQS queues"
    profile-opt region-opt output-opt
    (option 'queue-name-prefix "--queue-name-prefix"
      default: ""
      help: "Queue name prefix")))

(def sqs-create-queue-cmd
  (command 'sqs-create-queue
    help: "Create an SQS queue"
    profile-opt region-opt output-opt
    (option 'queue-name "--queue-name" "-q"
      help: "Queue name (required)")))

(def sqs-delete-queue-cmd
  (command 'sqs-delete-queue
    help: "Delete an SQS queue"
    profile-opt region-opt output-opt
    (option 'queue-url "--queue-url"
      help: "Queue URL (required)")))

(def sqs-get-queue-url-cmd
  (command 'sqs-get-queue-url
    help: "Get SQS queue URL"
    profile-opt region-opt output-opt
    (option 'queue-name "--queue-name" "-q"
      help: "Queue name (required)")))

(def sqs-send-message-cmd
  (command 'sqs-send-message
    help: "Send a message to an SQS queue"
    profile-opt region-opt output-opt
    (option 'queue-url "--queue-url"
      help: "Queue URL (required)")
    (option 'message-body "--message-body" "-m"
      help: "Message body (required)")))

(def sqs-receive-message-cmd
  (command 'sqs-receive-message
    help: "Receive messages from an SQS queue"
    profile-opt region-opt output-opt
    (option 'queue-url "--queue-url"
      help: "Queue URL (required)")
    (option 'max-messages "--max-messages"
      default: ""
      help: "Max number of messages (1-10)")
    (option 'wait-time "--wait-time"
      default: ""
      help: "Long poll wait time in seconds")))

(def sqs-delete-message-cmd
  (command 'sqs-delete-message
    help: "Delete a message from an SQS queue"
    profile-opt region-opt output-opt
    (option 'queue-url "--queue-url"
      help: "Queue URL (required)")
    (option 'receipt-handle "--receipt-handle"
      help: "Receipt handle (required)")))

(def sqs-purge-queue-cmd
  (command 'sqs-purge-queue
    help: "Purge all messages from an SQS queue"
    profile-opt region-opt output-opt
    (option 'queue-url "--queue-url"
      help: "Queue URL (required)")))

;; === CloudFormation ===
(def cfn-list-stacks-cmd
  (command 'cfn-list-stacks
    help: "List CloudFormation stacks"
    profile-opt region-opt output-opt))

(def cfn-describe-stacks-cmd
  (command 'cfn-describe-stacks
    help: "Describe CloudFormation stacks"
    profile-opt region-opt output-opt
    (option 'stack-name "--stack-name" "-s"
      default: ""
      help: "Stack name or ID")))

(def cfn-create-stack-cmd
  (command 'cfn-create-stack
    help: "Create a CloudFormation stack"
    profile-opt region-opt output-opt
    (option 'stack-name "--stack-name" "-s"
      help: "Stack name (required)")
    (option 'template-body "--template-body"
      default: ""
      help: "Template body (JSON/YAML string)")
    (option 'template-url "--template-url"
      default: ""
      help: "Template S3 URL")
    (option 'template-file "--template-file"
      default: ""
      help: "Path to template file")
    (option 'parameters "--parameters"
      default: ""
      help: "Parameters as Key=Value,Key2=Value2")
    (option 'capabilities "--capabilities"
      default: ""
      help: "Comma-separated capabilities")))

(def cfn-update-stack-cmd
  (command 'cfn-update-stack
    help: "Update a CloudFormation stack"
    profile-opt region-opt output-opt
    (option 'stack-name "--stack-name" "-s"
      help: "Stack name (required)")
    (option 'template-body "--template-body"
      default: ""
      help: "Template body")
    (option 'template-url "--template-url"
      default: ""
      help: "Template S3 URL")
    (option 'template-file "--template-file"
      default: ""
      help: "Path to template file")
    (option 'parameters "--parameters"
      default: ""
      help: "Parameters as Key=Value,Key2=Value2")
    (option 'capabilities "--capabilities"
      default: ""
      help: "Comma-separated capabilities")))

(def cfn-delete-stack-cmd
  (command 'cfn-delete-stack
    help: "Delete a CloudFormation stack"
    profile-opt region-opt output-opt
    (option 'stack-name "--stack-name" "-s"
      help: "Stack name (required)")))

(def cfn-describe-stack-events-cmd
  (command 'cfn-describe-stack-events
    help: "Describe CloudFormation stack events"
    profile-opt region-opt output-opt
    (option 'stack-name "--stack-name" "-s"
      help: "Stack name (required)")))

(def cfn-describe-stack-resources-cmd
  (command 'cfn-describe-stack-resources
    help: "Describe CloudFormation stack resources"
    profile-opt region-opt output-opt
    (option 'stack-name "--stack-name" "-s"
      help: "Stack name (required)")))

(def cfn-get-template-cmd
  (command 'cfn-get-template
    help: "Get CloudFormation stack template"
    profile-opt region-opt output-opt
    (option 'stack-name "--stack-name" "-s"
      help: "Stack name (required)")))

(def cfn-validate-template-cmd
  (command 'cfn-validate-template
    help: "Validate a CloudFormation template"
    profile-opt region-opt output-opt
    (option 'template-body "--template-body"
      default: ""
      help: "Template body")
    (option 'template-url "--template-url"
      default: ""
      help: "Template S3 URL")
    (option 'template-file "--template-file"
      default: ""
      help: "Path to template file")))

(def cfn-list-stack-resources-cmd
  (command 'cfn-list-stack-resources
    help: "List CloudFormation stack resources"
    profile-opt region-opt output-opt
    (option 'stack-name "--stack-name" "-s"
      help: "Stack name (required)")))

;;; ---- Dispatch ----

(def (main . args)
  (call-with-getopt gerbil-aws-main args
    program: "gerbil-aws"
    help: "AWS command-line interface"
    ;; EC2 Instances
    describe-instances-cmd
    run-instances-cmd
    start-instances-cmd
    stop-instances-cmd
    terminate-instances-cmd
    reboot-instances-cmd
    describe-instance-status-cmd
    get-console-output-cmd
    modify-instance-attribute-cmd
    describe-instance-types-cmd
    ;; EC2 Images
    describe-images-cmd
    create-image-cmd
    deregister-image-cmd
    ;; EC2 Security Groups
    describe-security-groups-cmd
    create-security-group-cmd
    delete-security-group-cmd
    authorize-ingress-cmd
    revoke-ingress-cmd
    authorize-egress-cmd
    revoke-egress-cmd
    ;; EC2 Key Pairs
    describe-key-pairs-cmd
    create-key-pair-cmd
    delete-key-pair-cmd
    import-key-pair-cmd
    ;; EC2 VPCs
    describe-vpcs-cmd
    create-vpc-cmd
    delete-vpc-cmd
    modify-vpc-attribute-cmd
    ;; EC2 Subnets
    describe-subnets-cmd
    create-subnet-cmd
    delete-subnet-cmd
    modify-subnet-attribute-cmd
    ;; EC2 Volumes
    describe-volumes-cmd
    create-volume-cmd
    delete-volume-cmd
    attach-volume-cmd
    detach-volume-cmd
    modify-volume-cmd
    describe-volume-status-cmd
    ;; EC2 Snapshots
    describe-snapshots-cmd
    create-snapshot-cmd
    delete-snapshot-cmd
    copy-snapshot-cmd
    ;; EC2 Addresses
    describe-addresses-cmd
    allocate-address-cmd
    release-address-cmd
    associate-address-cmd
    disassociate-address-cmd
    ;; EC2 Network Interfaces
    describe-network-interfaces-cmd
    create-network-interface-cmd
    delete-network-interface-cmd
    ;; EC2 Route Tables
    describe-route-tables-cmd
    create-route-table-cmd
    delete-route-table-cmd
    create-route-cmd
    delete-route-cmd
    associate-route-table-cmd
    disassociate-route-table-cmd
    replace-route-cmd
    ;; EC2 Internet Gateways
    describe-internet-gateways-cmd
    create-internet-gateway-cmd
    delete-internet-gateway-cmd
    attach-internet-gateway-cmd
    detach-internet-gateway-cmd
    ;; EC2 NAT Gateways
    describe-nat-gateways-cmd
    create-nat-gateway-cmd
    delete-nat-gateway-cmd
    ;; EC2 Tags
    describe-tags-cmd
    create-tags-cmd
    delete-tags-cmd
    ;; EC2 Regions
    describe-regions-cmd
    describe-availability-zones-cmd
    ;; EC2 Launch Templates
    describe-launch-templates-cmd
    create-launch-template-cmd
    delete-launch-template-cmd
    ;; S3 Buckets
    s3-list-buckets-cmd
    s3-create-bucket-cmd
    s3-delete-bucket-cmd
    s3-head-bucket-cmd
    s3-get-bucket-location-cmd
    s3-get-bucket-versioning-cmd
    s3-put-bucket-versioning-cmd
    s3-get-bucket-tagging-cmd
    s3-put-bucket-tagging-cmd
    s3-delete-bucket-tagging-cmd
    s3-get-bucket-policy-cmd
    s3-put-bucket-policy-cmd
    s3-delete-bucket-policy-cmd
    ;; S3 Objects
    s3-list-objects-cmd
    s3-get-object-cmd
    s3-put-object-cmd
    s3-delete-object-cmd
    s3-head-object-cmd
    s3-copy-object-cmd
    s3-delete-objects-cmd
    s3-list-object-versions-cmd
    ;; STS
    sts-get-caller-identity-cmd
    sts-assume-role-cmd
    sts-get-session-token-cmd
    ;; IAM
    iam-list-users-cmd
    iam-create-user-cmd
    iam-delete-user-cmd
    iam-get-user-cmd
    iam-list-roles-cmd
    iam-create-role-cmd
    iam-delete-role-cmd
    iam-get-role-cmd
    iam-list-groups-cmd
    iam-create-group-cmd
    iam-delete-group-cmd
    iam-add-user-to-group-cmd
    iam-remove-user-from-group-cmd
    iam-list-policies-cmd
    iam-attach-role-policy-cmd
    iam-detach-role-policy-cmd
    iam-attach-user-policy-cmd
    iam-detach-user-policy-cmd
    iam-list-access-keys-cmd
    iam-create-access-key-cmd
    iam-delete-access-key-cmd
    ;; Lambda
    lambda-list-functions-cmd
    lambda-get-function-cmd
    lambda-invoke-cmd
    lambda-delete-function-cmd
    ;; CloudWatch Logs
    logs-describe-log-groups-cmd
    logs-describe-log-streams-cmd
    logs-create-log-group-cmd
    logs-delete-log-group-cmd
    logs-get-log-events-cmd
    logs-filter-log-events-cmd
    ;; DynamoDB
    dynamodb-list-tables-cmd
    dynamodb-describe-table-cmd
    dynamodb-delete-table-cmd
    dynamodb-scan-cmd
    ;; SNS
    sns-list-topics-cmd
    sns-create-topic-cmd
    sns-delete-topic-cmd
    sns-publish-cmd
    sns-subscribe-cmd
    sns-unsubscribe-cmd
    sns-list-subscriptions-cmd
    ;; SQS
    sqs-list-queues-cmd
    sqs-create-queue-cmd
    sqs-delete-queue-cmd
    sqs-get-queue-url-cmd
    sqs-send-message-cmd
    sqs-receive-message-cmd
    sqs-delete-message-cmd
    sqs-purge-queue-cmd
    ;; CloudFormation
    cfn-list-stacks-cmd
    cfn-describe-stacks-cmd
    cfn-create-stack-cmd
    cfn-update-stack-cmd
    cfn-delete-stack-cmd
    cfn-describe-stack-events-cmd
    cfn-describe-stack-resources-cmd
    cfn-get-template-cmd
    cfn-validate-template-cmd
    cfn-list-stack-resources-cmd))

;; Main dispatch handler
(def (gerbil-aws-main cmd opt)
  (let-hash opt
    (let ((out (or .?output "json")))
      (case cmd
        ;; === EC2 Instances ===
        ((describe-instances)
         (let (client (make-client opt))
           (format-output out
             (describe-instances client
               instance-ids: (parse-csv .?instance-ids)
               filters: (parse-filters .?filters)))))
        ((run-instances)
         (let (client (make-client opt))
           (format-output out
             (run-instances client
               image-id: .image-id
               instance-type: .instance-type
               min-count: .count
               max-count: .count
               key-name: (nonempty .?key-name)
               security-group-ids: (parse-csv .?security-group-ids)
               subnet-id: (nonempty .?subnet-id)))))
        ((start-instances)
         (let (client (make-client opt))
           (format-output out
             (start-instances client (parse-csv .instance-ids)))))
        ((stop-instances)
         (let (client (make-client opt))
           (format-output out
             (stop-instances client (parse-csv .instance-ids)
               force: .?force))))
        ((terminate-instances)
         (let (client (make-client opt))
           (format-output out
             (terminate-instances client (parse-csv .instance-ids)))))
        ((reboot-instances)
         (let (client (make-client opt))
           (reboot-instances client (parse-csv .instance-ids))
           (displayln "Reboot initiated")))
        ((describe-instance-status)
         (let (client (make-client opt))
           (format-output out
             (describe-instance-status client
               instance-ids: (parse-csv .?instance-ids)
               include-all: .?include-all))))
        ((get-console-output)
         (let (client (make-client opt))
           (format-output out
             (get-console-output client .instance-id
               latest: .?latest))))
        ((modify-instance-attribute)
         (let (client (make-client opt))
           (modify-instance-attribute client .instance-id
             instance-type: (nonempty .?instance-type))
           (displayln "Instance attribute modified")))
        ((describe-instance-types)
         (let (client (make-client opt))
           (format-output out
             (describe-instance-types client
               instance-types: (parse-csv .?instance-types)
               filters: (parse-filters .?filters)))))

        ;; === EC2 Images ===
        ((describe-images)
         (let (client (make-client opt))
           (format-output out
             (describe-images client
               image-ids: (parse-csv .?image-ids)
               owners: (parse-csv .?owners)
               filters: (parse-filters .?filters)))))
        ((create-image)
         (let (client (make-client opt))
           (format-output out
             (create-image client
               instance-id: .instance-id
               name: .name
               description: (nonempty .?description)
               no-reboot: .?no-reboot))))
        ((deregister-image)
         (let (client (make-client opt))
           (deregister-image client .image-id)
           (displayln "Image deregistered")))

        ;; === EC2 Security Groups ===
        ((describe-security-groups)
         (let (client (make-client opt))
           (format-output out
             (describe-security-groups client
               group-ids: (parse-csv .?group-ids)
               group-names: (parse-csv .?group-names)
               filters: (parse-filters .?filters)))))
        ((create-security-group)
         (let (client (make-client opt))
           (format-output out
             (create-security-group client
               group-name: .group-name
               description: .description
               vpc-id: (nonempty .?vpc-id)))))
        ((delete-security-group)
         (let (client (make-client opt))
           (delete-security-group client group-id: .group-id)
           (displayln "Security group deleted")))
        ((authorize-security-group-ingress)
         (let (client (make-client opt))
           (let (perm (append [(cons "protocol" .protocol)
                               (cons "from-port" .from-port)
                               (cons "to-port" .to-port)]
                              (if (nonempty .?cidr)
                                [(cons "cidr-ipv4" .cidr)]
                                [])))
             (authorize-security-group-ingress client .group-id [perm]))
           (displayln "Ingress rule added")))
        ((revoke-security-group-ingress)
         (let (client (make-client opt))
           (let (perm (append [(cons "protocol" .protocol)
                               (cons "from-port" .from-port)
                               (cons "to-port" .to-port)]
                              (if (nonempty .?cidr)
                                [(cons "cidr-ipv4" .cidr)]
                                [])))
             (revoke-security-group-ingress client .group-id [perm]))
           (displayln "Ingress rule revoked")))
        ((authorize-security-group-egress)
         (let (client (make-client opt))
           (let (perm (append [(cons "protocol" .protocol)
                               (cons "from-port" .from-port)
                               (cons "to-port" .to-port)]
                              (if (nonempty .?cidr)
                                [(cons "cidr-ipv4" .cidr)]
                                [])))
             (authorize-security-group-egress client .group-id [perm]))
           (displayln "Egress rule added")))
        ((revoke-security-group-egress)
         (let (client (make-client opt))
           (let (perm (append [(cons "protocol" .protocol)
                               (cons "from-port" .from-port)
                               (cons "to-port" .to-port)]
                              (if (nonempty .?cidr)
                                [(cons "cidr-ipv4" .cidr)]
                                [])))
             (revoke-security-group-egress client .group-id [perm]))
           (displayln "Egress rule revoked")))

        ;; === EC2 Key Pairs ===
        ((describe-key-pairs)
         (let (client (make-client opt))
           (format-output out
             (describe-key-pairs client
               key-names: (parse-csv .?key-names)
               filters: (parse-filters .?filters)))))
        ((create-key-pair)
         (let (client (make-client opt))
           (format-output out
             (create-key-pair client .key-name
               key-type: (nonempty .?key-type)))))
        ((delete-key-pair)
         (let (client (make-client opt))
           (delete-key-pair client key-name: .key-name)
           (displayln "Key pair deleted")))
        ((import-key-pair)
         (let (client (make-client opt))
           (format-output out
             (import-key-pair client .key-name .public-key-material))))

        ;; === EC2 VPCs ===
        ((describe-vpcs)
         (let (client (make-client opt))
           (format-output out
             (describe-vpcs client
               vpc-ids: (parse-csv .?vpc-ids)
               filters: (parse-filters .?filters)))))
        ((create-vpc)
         (let (client (make-client opt))
           (format-output out
             (create-vpc client cidr-block: .cidr-block))))
        ((delete-vpc)
         (let (client (make-client opt))
           (delete-vpc client .vpc-id)
           (displayln "VPC deleted")))
        ((modify-vpc-attribute)
         (let (client (make-client opt))
           (modify-vpc-attribute client .vpc-id
             enable-dns-support: .?enable-dns-support
             enable-dns-hostnames: .?enable-dns-hostnames)
           (displayln "VPC attribute modified")))

        ;; === EC2 Subnets ===
        ((describe-subnets)
         (let (client (make-client opt))
           (format-output out
             (describe-subnets client
               subnet-ids: (parse-csv .?subnet-ids)
               filters: (parse-filters .?filters)))))
        ((create-subnet)
         (let (client (make-client opt))
           (format-output out
             (create-subnet client
               vpc-id: .vpc-id
               cidr-block: .cidr-block
               availability-zone: (nonempty .?availability-zone)))))
        ((delete-subnet)
         (let (client (make-client opt))
           (delete-subnet client .subnet-id)
           (displayln "Subnet deleted")))
        ((modify-subnet-attribute)
         (let (client (make-client opt))
           (modify-subnet-attribute client .subnet-id
             map-public-ip-on-launch: .?map-public-ip)
           (displayln "Subnet attribute modified")))

        ;; === EC2 Volumes ===
        ((describe-volumes)
         (let (client (make-client opt))
           (format-output out
             (describe-volumes client
               volume-ids: (parse-csv .?volume-ids)
               filters: (parse-filters .?filters)))))
        ((create-volume)
         (let (client (make-client opt))
           (format-output out
             (create-volume client
               availability-zone: .availability-zone
               size: (nonempty .?size)
               volume-type: (nonempty .?volume-type)
               snapshot-id: (nonempty .?snapshot-id)))))
        ((delete-volume)
         (let (client (make-client opt))
           (delete-volume client .volume-id)
           (displayln "Volume deleted")))
        ((attach-volume)
         (let (client (make-client opt))
           (format-output out
             (attach-volume client
               volume-id: .volume-id
               instance-id: .instance-id
               device: .device))))
        ((detach-volume)
         (let (client (make-client opt))
           (format-output out
             (detach-volume client .volume-id
               force: .?force))))
        ((modify-volume)
         (let (client (make-client opt))
           (format-output out
             (modify-volume client .volume-id
               size: (nonempty .?size)
               volume-type: (nonempty .?volume-type)))))
        ((describe-volume-status)
         (let (client (make-client opt))
           (format-output out
             (describe-volume-status client
               volume-ids: (parse-csv .?volume-ids)))))

        ;; === EC2 Snapshots ===
        ((describe-snapshots)
         (let (client (make-client opt))
           (format-output out
             (describe-snapshots client
               snapshot-ids: (parse-csv .?snapshot-ids)
               owner-ids: (parse-csv .?owner-ids)
               filters: (parse-filters .?filters)))))
        ((create-snapshot)
         (let (client (make-client opt))
           (format-output out
             (create-snapshot client
               volume-id: .volume-id
               description: (nonempty .?description)))))
        ((delete-snapshot)
         (let (client (make-client opt))
           (delete-snapshot client .snapshot-id)
           (displayln "Snapshot deleted")))
        ((copy-snapshot)
         (let (client (make-client opt))
           (format-output out
             (copy-snapshot client
               source-snapshot-id: .source-snapshot-id
               source-region: .source-region
               description: (nonempty .?description)))))

        ;; === EC2 Addresses ===
        ((describe-addresses)
         (let (client (make-client opt))
           (format-output out
             (describe-addresses client
               allocation-ids: (parse-csv .?allocation-ids)
               public-ips: (parse-csv .?public-ips)
               filters: (parse-filters .?filters)))))
        ((allocate-address)
         (let (client (make-client opt))
           (format-output out
             (allocate-address client domain: .domain))))
        ((release-address)
         (let (client (make-client opt))
           (release-address client allocation-id: .allocation-id)
           (displayln "Address released")))
        ((associate-address)
         (let (client (make-client opt))
           (format-output out
             (associate-address client
               allocation-id: .allocation-id
               instance-id: .instance-id))))
        ((disassociate-address)
         (let (client (make-client opt))
           (disassociate-address client association-id: .association-id)
           (displayln "Address disassociated")))

        ;; === EC2 Network Interfaces ===
        ((describe-network-interfaces)
         (let (client (make-client opt))
           (format-output out
             (describe-network-interfaces client
               network-interface-ids: (parse-csv .?network-interface-ids)
               filters: (parse-filters .?filters)))))
        ((create-network-interface)
         (let (client (make-client opt))
           (format-output out
             (create-network-interface client
               subnet-id: .subnet-id
               description: (nonempty .?description)
               security-group-ids: (parse-csv .?security-group-ids)))))
        ((delete-network-interface)
         (let (client (make-client opt))
           (delete-network-interface client .network-interface-id)
           (displayln "Network interface deleted")))

        ;; === EC2 Route Tables ===
        ((describe-route-tables)
         (let (client (make-client opt))
           (format-output out
             (describe-route-tables client
               route-table-ids: (parse-csv .?route-table-ids)
               filters: (parse-filters .?filters)))))
        ((create-route-table)
         (let (client (make-client opt))
           (format-output out
             (create-route-table client .vpc-id))))
        ((delete-route-table)
         (let (client (make-client opt))
           (delete-route-table client .route-table-id)
           (displayln "Route table deleted")))
        ((create-route)
         (let (client (make-client opt))
           (create-route client
             route-table-id: .route-table-id
             destination-cidr-block: .destination-cidr
             gateway-id: (nonempty .?gateway-id)
             nat-gateway-id: (nonempty .?nat-gateway-id))
           (displayln "Route created")))
        ((delete-route)
         (let (client (make-client opt))
           (delete-route client
             route-table-id: .route-table-id
             destination-cidr-block: .destination-cidr)
           (displayln "Route deleted")))
        ((associate-route-table)
         (let (client (make-client opt))
           (format-output out
             (associate-route-table client
               route-table-id: .route-table-id
               subnet-id: .subnet-id))))
        ((disassociate-route-table)
         (let (client (make-client opt))
           (disassociate-route-table client .association-id)
           (displayln "Route table disassociated")))
        ((replace-route)
         (let (client (make-client opt))
           (replace-route client
             route-table-id: .route-table-id
             destination-cidr-block: .destination-cidr
             gateway-id: (nonempty .?gateway-id)
             nat-gateway-id: (nonempty .?nat-gateway-id))
           (displayln "Route replaced")))

        ;; === EC2 Internet Gateways ===
        ((describe-internet-gateways)
         (let (client (make-client opt))
           (format-output out
             (describe-internet-gateways client
               internet-gateway-ids: (parse-csv .?internet-gateway-ids)
               filters: (parse-filters .?filters)))))
        ((create-internet-gateway)
         (let (client (make-client opt))
           (format-output out
             (create-internet-gateway client))))
        ((delete-internet-gateway)
         (let (client (make-client opt))
           (delete-internet-gateway client .internet-gateway-id)
           (displayln "Internet gateway deleted")))
        ((attach-internet-gateway)
         (let (client (make-client opt))
           (attach-internet-gateway client
             internet-gateway-id: .internet-gateway-id
             vpc-id: .vpc-id)
           (displayln "Internet gateway attached")))
        ((detach-internet-gateway)
         (let (client (make-client opt))
           (detach-internet-gateway client
             internet-gateway-id: .internet-gateway-id
             vpc-id: .vpc-id)
           (displayln "Internet gateway detached")))

        ;; === EC2 NAT Gateways ===
        ((describe-nat-gateways)
         (let (client (make-client opt))
           (format-output out
             (describe-nat-gateways client
               nat-gateway-ids: (parse-csv .?nat-gateway-ids)
               filters: (parse-filters .?filters)))))
        ((create-nat-gateway)
         (let (client (make-client opt))
           (format-output out
             (create-nat-gateway client
               subnet-id: .subnet-id
               allocation-id: .allocation-id))))
        ((delete-nat-gateway)
         (let (client (make-client opt))
           (format-output out
             (delete-nat-gateway client .nat-gateway-id))))

        ;; === EC2 Tags ===
        ((describe-tags)
         (let (client (make-client opt))
           (format-output out
             (describe-tags client
               filters: (parse-filters .?filters)))))
        ((create-tags)
         (let (client (make-client opt))
           (create-tags client (parse-csv .resources) (parse-tags .tags))
           (displayln "Tags created")))
        ((delete-tags)
         (let (client (make-client opt))
           (delete-tags client (parse-csv .resources) (parse-tags .tags))
           (displayln "Tags deleted")))

        ;; === EC2 Regions ===
        ((describe-regions)
         (let (client (make-client opt))
           (format-output out
             (describe-regions client all-regions: .?all)
             columns: ["regionName" "regionEndpoint"])))
        ((describe-availability-zones)
         (let (client (make-client opt))
           (format-output out
             (describe-availability-zones client all-zones: .?all)
             columns: ["zoneName" "zoneState" "regionName"])))

        ;; === EC2 Launch Templates ===
        ((describe-launch-templates)
         (let (client (make-client opt))
           (format-output out
             (describe-launch-templates client
               launch-template-ids: (parse-csv .?launch-template-ids)
               launch-template-names: (parse-csv .?launch-template-names)))))
        ((create-launch-template)
         (let (client (make-client opt))
           (format-output out
             (create-launch-template client
               launch-template-name: .launch-template-name
               image-id: (nonempty .?image-id)
               instance-type: (nonempty .?instance-type)))))
        ((delete-launch-template)
         (let (client (make-client opt))
           (delete-launch-template client
             launch-template-id: (nonempty .?launch-template-id)
             launch-template-name: (nonempty .?launch-template-name))
           (displayln "Launch template deleted")))

        ;; === S3 Buckets ===
        ((s3-list-buckets)
         (let (s3 (make-s3-client* opt))
           (format-output out (list-buckets s3)
             columns: ["Name" "CreationDate"])))
        ((s3-create-bucket)
         (let (s3 (make-s3-client* opt))
           (create-bucket s3 .bucket)
           (displayln "Bucket created")))
        ((s3-delete-bucket)
         (let (s3 (make-s3-client* opt))
           (delete-bucket s3 .bucket)
           (displayln "Bucket deleted")))
        ((s3-head-bucket)
         (let (s3 (make-s3-client* opt))
           (if (head-bucket s3 .bucket)
             (displayln "Bucket exists")
             (displayln "Bucket does not exist"))))
        ((s3-get-bucket-location)
         (let (s3 (make-s3-client* opt))
           (format-output out (get-bucket-location s3 .bucket))))
        ((s3-get-bucket-versioning)
         (let (s3 (make-s3-client* opt))
           (format-output out (get-bucket-versioning s3 .bucket))))
        ((s3-put-bucket-versioning)
         (let (s3 (make-s3-client* opt))
           (put-bucket-versioning s3 .bucket .status)
           (displayln "Bucket versioning updated")))
        ((s3-get-bucket-tagging)
         (let (s3 (make-s3-client* opt))
           (format-output out (get-bucket-tagging s3 .bucket))))
        ((s3-put-bucket-tagging)
         (let (s3 (make-s3-client* opt))
           (put-bucket-tagging s3 .bucket (parse-tags .tags))
           (displayln "Bucket tags updated")))
        ((s3-delete-bucket-tagging)
         (let (s3 (make-s3-client* opt))
           (delete-bucket-tagging s3 .bucket)
           (displayln "Bucket tags deleted")))
        ((s3-get-bucket-policy)
         (let (s3 (make-s3-client* opt))
           (displayln (get-bucket-policy s3 .bucket))))
        ((s3-put-bucket-policy)
         (let (s3 (make-s3-client* opt))
           (put-bucket-policy s3 .bucket (read-file-string .policy-file))
           (displayln "Bucket policy updated")))
        ((s3-delete-bucket-policy)
         (let (s3 (make-s3-client* opt))
           (delete-bucket-policy s3 .bucket)
           (displayln "Bucket policy deleted")))

        ;; === S3 Objects ===
        ((s3-list-objects)
         (let (s3 (make-s3-client* opt))
           (format-output out
             (list-objects s3 .bucket
               prefix: (nonempty .?prefix)
               delimiter: (nonempty .?delimiter)
               max-keys: (nonempty .?max-keys)
               start-after: (nonempty .?start-after)))))
        ((s3-get-object)
         (let* ((s3 (make-s3-client* opt))
                (data (get-object s3 .bucket .key)))
           (if (nonempty .?outfile)
             (let (p (open-output-file .outfile))
               (write-u8vector data p)
               (close-port p)
               (displayln "Downloaded to " .outfile))
             (write-u8vector data (current-output-port)))))
        ((s3-put-object)
         (let* ((s3 (make-s3-client* opt))
                (data (read-file-bytes .infile)))
           (put-object s3 .bucket .key data
             content-type: .content-type)
           (displayln "Uploaded " .infile " to s3://" .bucket "/" .key)))
        ((s3-delete-object)
         (let (s3 (make-s3-client* opt))
           (delete-object s3 .bucket .key)
           (displayln "Object deleted")))
        ((s3-head-object)
         (let (s3 (make-s3-client* opt))
           (format-output out (head-object s3 .bucket .key))))
        ((s3-copy-object)
         (let (s3 (make-s3-client* opt))
           (copy-object s3 .bucket .key .copy-source)
           (displayln "Object copied")))
        ((s3-delete-objects)
         (let (s3 (make-s3-client* opt))
           (delete-objects s3 .bucket (parse-csv .keys))
           (displayln "Objects deleted")))
        ((s3-list-object-versions)
         (let (s3 (make-s3-client* opt))
           (format-output out
             (list-object-versions s3 .bucket
               prefix: (nonempty .?prefix)))))

        ;; === STS ===
        ((sts-get-caller-identity)
         (let (client (make-sts-client opt))
           (format-output out (get-caller-identity client))))
        ((sts-assume-role)
         (let (client (make-sts-client opt))
           (format-output out
             (assume-role client
               role-arn: .role-arn
               role-session-name: .role-session-name
               duration-seconds: (nonempty .?duration-seconds)))))
        ((sts-get-session-token)
         (let (client (make-sts-client opt))
           (format-output out
             (get-session-token client
               duration-seconds: (nonempty .?duration-seconds)))))

        ;; === IAM ===
        ((iam-list-users)
         (let (client (make-iam-client opt))
           (format-output out (list-users client))))
        ((iam-create-user)
         (let (client (make-iam-client opt))
           (format-output out (create-user client .user-name))))
        ((iam-delete-user)
         (let (client (make-iam-client opt))
           (delete-user client .user-name)
           (displayln "User deleted")))
        ((iam-get-user)
         (let (client (make-iam-client opt))
           (format-output out (get-user client user-name: (nonempty .?user-name)))))
        ((iam-list-roles)
         (let (client (make-iam-client opt))
           (format-output out (list-roles client))))
        ((iam-create-role)
         (let (client (make-iam-client opt))
           (format-output out
             (create-role client .role-name .assume-role-policy-document))))
        ((iam-delete-role)
         (let (client (make-iam-client opt))
           (delete-role client .role-name)
           (displayln "Role deleted")))
        ((iam-get-role)
         (let (client (make-iam-client opt))
           (format-output out (get-role client .role-name))))
        ((iam-list-groups)
         (let (client (make-iam-client opt))
           (format-output out (list-groups client))))
        ((iam-create-group)
         (let (client (make-iam-client opt))
           (format-output out (create-group client .group-name))))
        ((iam-delete-group)
         (let (client (make-iam-client opt))
           (delete-group client .group-name)
           (displayln "Group deleted")))
        ((iam-add-user-to-group)
         (let (client (make-iam-client opt))
           (add-user-to-group client .group-name .user-name)
           (displayln "User added to group")))
        ((iam-remove-user-from-group)
         (let (client (make-iam-client opt))
           (remove-user-from-group client .group-name .user-name)
           (displayln "User removed from group")))
        ((iam-list-policies)
         (let (client (make-iam-client opt))
           (format-output out
             (list-policies client scope: (nonempty .?scope)))))
        ((iam-attach-role-policy)
         (let (client (make-iam-client opt))
           (attach-role-policy client .role-name .policy-arn)
           (displayln "Policy attached to role")))
        ((iam-detach-role-policy)
         (let (client (make-iam-client opt))
           (detach-role-policy client .role-name .policy-arn)
           (displayln "Policy detached from role")))
        ((iam-attach-user-policy)
         (let (client (make-iam-client opt))
           (attach-user-policy client .user-name .policy-arn)
           (displayln "Policy attached to user")))
        ((iam-detach-user-policy)
         (let (client (make-iam-client opt))
           (detach-user-policy client .user-name .policy-arn)
           (displayln "Policy detached from user")))
        ((iam-list-access-keys)
         (let (client (make-iam-client opt))
           (format-output out
             (list-access-keys client user-name: (nonempty .?user-name)))))
        ((iam-create-access-key)
         (let (client (make-iam-client opt))
           (format-output out
             (create-access-key client user-name: (nonempty .?user-name)))))
        ((iam-delete-access-key)
         (let (client (make-iam-client opt))
           (delete-access-key client .access-key-id
             user-name: (nonempty .?user-name))
           (displayln "Access key deleted")))

        ;; === Lambda ===
        ((lambda-list-functions)
         (let (client (make-lambda-client* opt))
           (format-output out (list-functions client))))
        ((lambda-get-function)
         (let (client (make-lambda-client* opt))
           (format-output out (get-function client .function-name))))
        ((lambda-invoke)
         (let* ((client (make-lambda-client* opt))
                (payload (if (nonempty .?payload-file)
                           (let (content (read-file-string .payload-file))
                             (call-with-input-string content read-json))
                           #f)))
           (format-output out
             (invoke-function client .function-name payload: payload))))
        ((lambda-delete-function)
         (let (client (make-lambda-client* opt))
           (delete-function client .function-name)
           (displayln "Function deleted")))

        ;; === CloudWatch Logs ===
        ((logs-describe-log-groups)
         (let (client (make-logs-client opt))
           (format-output out
             (describe-log-groups client
               log-group-name-prefix: (nonempty .?log-group-name-prefix)))))
        ((logs-describe-log-streams)
         (let (client (make-logs-client opt))
           (format-output out
             (describe-log-streams client .log-group-name
               log-stream-name-prefix: (nonempty .?log-stream-name-prefix)))))
        ((logs-create-log-group)
         (let (client (make-logs-client opt))
           (create-log-group client .log-group-name)
           (displayln "Log group created")))
        ((logs-delete-log-group)
         (let (client (make-logs-client opt))
           (delete-log-group client .log-group-name)
           (displayln "Log group deleted")))
        ((logs-get-log-events)
         (let (client (make-logs-client opt))
           (format-output out
             (get-log-events client .log-group-name .log-stream-name
               limit: (let (l (nonempty .?limit))
                        (and l (string->number l)))))))
        ((logs-filter-log-events)
         (let (client (make-logs-client opt))
           (format-output out
             (filter-log-events client .log-group-name
               filter-pattern: (nonempty .?filter-pattern)))))

        ;; === DynamoDB ===
        ((dynamodb-list-tables)
         (let (client (make-dynamodb-client opt))
           (format-output out (list-tables client))))
        ((dynamodb-describe-table)
         (let (client (make-dynamodb-client opt))
           (format-output out (describe-table client .table-name))))
        ((dynamodb-delete-table)
         (let (client (make-dynamodb-client opt))
           (format-output out (delete-table client .table-name))))
        ((dynamodb-scan)
         (let (client (make-dynamodb-client opt))
           (format-output out
             (dynamodb-scan client .table-name
               limit: (let (l (nonempty .?limit))
                        (and l (string->number l)))))))

        ;; === SNS ===
        ((sns-list-topics)
         (let (client (make-sns-client opt))
           (format-output out (list-topics client))))
        ((sns-create-topic)
         (let (client (make-sns-client opt))
           (format-output out (create-topic client .name))))
        ((sns-delete-topic)
         (let (client (make-sns-client opt))
           (delete-topic client .topic-arn)
           (displayln "Topic deleted")))
        ((sns-publish)
         (let (client (make-sns-client opt))
           (format-output out
             (publish client
               topic-arn: .topic-arn
               message: .message
               subject: (nonempty .?subject)))))
        ((sns-subscribe)
         (let (client (make-sns-client opt))
           (format-output out
             (subscribe client
               topic-arn: .topic-arn
               protocol: .protocol
               endpoint: .endpoint))))
        ((sns-unsubscribe)
         (let (client (make-sns-client opt))
           (unsubscribe client .subscription-arn)
           (displayln "Unsubscribed")))
        ((sns-list-subscriptions)
         (let (client (make-sns-client opt))
           (format-output out (list-subscriptions client))))

        ;; === SQS ===
        ((sqs-list-queues)
         (let (client (make-sqs-client opt))
           (format-output out
             (list-queues client
               queue-name-prefix: (nonempty .?queue-name-prefix)))))
        ((sqs-create-queue)
         (let (client (make-sqs-client opt))
           (format-output out (create-queue client .queue-name))))
        ((sqs-delete-queue)
         (let (client (make-sqs-client opt))
           (delete-queue client .queue-url)
           (displayln "Queue deleted")))
        ((sqs-get-queue-url)
         (let (client (make-sqs-client opt))
           (format-output out (get-queue-url client .queue-name))))
        ((sqs-send-message)
         (let (client (make-sqs-client opt))
           (format-output out
             (send-message client .queue-url .message-body))))
        ((sqs-receive-message)
         (let (client (make-sqs-client opt))
           (format-output out
             (receive-message client .queue-url
               max-number-of-messages: (let (m (nonempty .?max-messages))
                                         (and m (string->number m)))
               wait-time-seconds: (let (w (nonempty .?wait-time))
                                    (and w (string->number w)))))))
        ((sqs-delete-message)
         (let (client (make-sqs-client opt))
           (delete-message client .queue-url .receipt-handle)
           (displayln "Message deleted")))
        ((sqs-purge-queue)
         (let (client (make-sqs-client opt))
           (purge-queue client .queue-url)
           (displayln "Queue purged")))

        ;; === CloudFormation ===
        ((cfn-list-stacks)
         (let (client (make-cfn-client opt))
           (format-output out (list-stacks client))))
        ((cfn-describe-stacks)
         (let (client (make-cfn-client opt))
           (format-output out
             (describe-stacks client
               stack-name: (nonempty .?stack-name)))))
        ((cfn-create-stack)
         (let (client (make-cfn-client opt))
           (format-output out
             (create-stack client .stack-name
               template-body: (or (nonempty .?template-body)
                                  (and (nonempty .?template-file)
                                       (read-file-string .template-file)))
               template-url: (nonempty .?template-url)
               parameters: (parse-params .?parameters)
               capabilities: (parse-csv .?capabilities)))))
        ((cfn-update-stack)
         (let (client (make-cfn-client opt))
           (format-output out
             (update-stack client .stack-name
               template-body: (or (nonempty .?template-body)
                                  (and (nonempty .?template-file)
                                       (read-file-string .template-file)))
               template-url: (nonempty .?template-url)
               parameters: (parse-params .?parameters)
               capabilities: (parse-csv .?capabilities)))))
        ((cfn-delete-stack)
         (let (client (make-cfn-client opt))
           (delete-stack client .stack-name)
           (displayln "Stack deletion initiated")))
        ((cfn-describe-stack-events)
         (let (client (make-cfn-client opt))
           (format-output out
             (describe-stack-events client .stack-name))))
        ((cfn-describe-stack-resources)
         (let (client (make-cfn-client opt))
           (format-output out
             (describe-stack-resources client .stack-name))))
        ((cfn-get-template)
         (let (client (make-cfn-client opt))
           (format-output out (get-template client .stack-name))))
        ((cfn-validate-template)
         (let (client (make-cfn-client opt))
           (format-output out
             (validate-template client
               template-body: (or (nonempty .?template-body)
                                  (and (nonempty .?template-file)
                                       (read-file-string .template-file)))
               template-url: (nonempty .?template-url)))))
        ((cfn-list-stack-resources)
         (let (client (make-cfn-client opt))
           (format-output out
             (list-stack-resources client .stack-name))))))))

(def (string-join strs sep)
  (if (null? strs) ""
    (let loop ((rest (cdr strs)) (acc (car strs)))
      (if (null? rest)
        acc
        (loop (cdr rest) (string-append acc sep (car rest)))))))

