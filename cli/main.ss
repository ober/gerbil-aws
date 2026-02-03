;;; -*- Gerbil -*-
;;; CLI entry point for gerbil-ec2

(import :std/cli/getopt
        :std/format
        :std/sugar
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
        :gerbil-aws/s3/api
        :gerbil-aws/s3/buckets
        :gerbil-aws/s3/objects
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

;;; ---- Command Definitions ----

;; Instances
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

;; Images
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

;; Security Groups
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

;; Key Pairs
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

;; VPCs
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

;; Subnets
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

;; Volumes
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

;; Snapshots
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

;; Addresses
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

;; Network Interfaces
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

;; Route Tables
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

;; Internet Gateways
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

;; NAT Gateways
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

;; Tags
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

;; Regions
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

;; S3 Buckets
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

;; S3 Objects
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

;;; ---- Dispatch ----

(def (main . args)
  (call-with-getopt gerbil-ec2-main args
    program: "gerbil-ec2"
    help: "AWS EC2 command-line interface"
    ;; Instances
    describe-instances-cmd
    run-instances-cmd
    start-instances-cmd
    stop-instances-cmd
    terminate-instances-cmd
    reboot-instances-cmd
    ;; Images
    describe-images-cmd
    create-image-cmd
    deregister-image-cmd
    ;; Security Groups
    describe-security-groups-cmd
    create-security-group-cmd
    delete-security-group-cmd
    authorize-ingress-cmd
    revoke-ingress-cmd
    ;; Key Pairs
    describe-key-pairs-cmd
    create-key-pair-cmd
    delete-key-pair-cmd
    ;; VPCs
    describe-vpcs-cmd
    create-vpc-cmd
    delete-vpc-cmd
    ;; Subnets
    describe-subnets-cmd
    create-subnet-cmd
    delete-subnet-cmd
    ;; Volumes
    describe-volumes-cmd
    create-volume-cmd
    delete-volume-cmd
    attach-volume-cmd
    detach-volume-cmd
    ;; Snapshots
    describe-snapshots-cmd
    create-snapshot-cmd
    delete-snapshot-cmd
    ;; Addresses
    describe-addresses-cmd
    allocate-address-cmd
    release-address-cmd
    associate-address-cmd
    disassociate-address-cmd
    ;; Network Interfaces
    describe-network-interfaces-cmd
    ;; Route Tables
    describe-route-tables-cmd
    create-route-table-cmd
    delete-route-table-cmd
    create-route-cmd
    delete-route-cmd
    ;; Internet Gateways
    describe-internet-gateways-cmd
    create-internet-gateway-cmd
    delete-internet-gateway-cmd
    attach-internet-gateway-cmd
    detach-internet-gateway-cmd
    ;; NAT Gateways
    describe-nat-gateways-cmd
    create-nat-gateway-cmd
    delete-nat-gateway-cmd
    ;; Tags
    describe-tags-cmd
    create-tags-cmd
    delete-tags-cmd
    ;; Regions
    describe-regions-cmd
    describe-availability-zones-cmd
    ;; S3 Buckets
    s3-list-buckets-cmd
    s3-create-bucket-cmd
    s3-delete-bucket-cmd
    s3-head-bucket-cmd
    ;; S3 Objects
    s3-list-objects-cmd
    s3-get-object-cmd
    s3-put-object-cmd
    s3-delete-object-cmd
    s3-head-object-cmd
    s3-copy-object-cmd))

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

;; Main dispatch handler
(def (gerbil-ec2-main cmd opt)
  (let-hash opt
    (let ((client (make-client opt))
          (out (or .?output "json")))
      (case cmd
        ;; Instances
        ((describe-instances)
         (format-output out
           (describe-instances client
             instance-ids: (parse-csv .?instance-ids)
             filters: (parse-filters .?filters))))
        ((run-instances)
         (format-output out
           (run-instances client
             image-id: .image-id
             instance-type: .instance-type
             min-count: .count
             max-count: .count
             key-name: (nonempty .?key-name)
             security-group-ids: (parse-csv .?security-group-ids)
             subnet-id: (nonempty .?subnet-id))))
        ((start-instances)
         (format-output out
           (start-instances client (parse-csv .instance-ids))))
        ((stop-instances)
         (format-output out
           (stop-instances client (parse-csv .instance-ids)
             force: .?force)))
        ((terminate-instances)
         (format-output out
           (terminate-instances client (parse-csv .instance-ids))))
        ((reboot-instances)
         (reboot-instances client (parse-csv .instance-ids))
         (displayln "Reboot initiated"))

        ;; Images
        ((describe-images)
         (format-output out
           (describe-images client
             image-ids: (parse-csv .?image-ids)
             owners: (parse-csv .?owners)
             filters: (parse-filters .?filters))))
        ((create-image)
         (format-output out
           (create-image client
             instance-id: .instance-id
             name: .name
             description: (nonempty .?description)
             no-reboot: .?no-reboot)))
        ((deregister-image)
         (deregister-image client .image-id)
         (displayln "Image deregistered"))

        ;; Security Groups
        ((describe-security-groups)
         (format-output out
           (describe-security-groups client
             group-ids: (parse-csv .?group-ids)
             group-names: (parse-csv .?group-names)
             filters: (parse-filters .?filters))))
        ((create-security-group)
         (format-output out
           (create-security-group client
             group-name: .group-name
             description: .description
             vpc-id: (nonempty .?vpc-id))))
        ((delete-security-group)
         (delete-security-group client group-id: .group-id)
         (displayln "Security group deleted"))
        ((authorize-security-group-ingress)
         (let (perm (append [(cons "protocol" .protocol)
                             (cons "from-port" .from-port)
                             (cons "to-port" .to-port)]
                            (if (nonempty .?cidr)
                              [(cons "cidr-ipv4" .cidr)]
                              [])))
           (authorize-security-group-ingress client .group-id [perm]))
         (displayln "Ingress rule added"))
        ((revoke-security-group-ingress)
         (let (perm (append [(cons "protocol" .protocol)
                             (cons "from-port" .from-port)
                             (cons "to-port" .to-port)]
                            (if (nonempty .?cidr)
                              [(cons "cidr-ipv4" .cidr)]
                              [])))
           (revoke-security-group-ingress client .group-id [perm]))
         (displayln "Ingress rule revoked"))

        ;; Key Pairs
        ((describe-key-pairs)
         (format-output out
           (describe-key-pairs client
             key-names: (parse-csv .?key-names)
             filters: (parse-filters .?filters))))
        ((create-key-pair)
         (format-output out
           (create-key-pair client .key-name
             key-type: (nonempty .?key-type))))
        ((delete-key-pair)
         (delete-key-pair client key-name: .key-name)
         (displayln "Key pair deleted"))

        ;; VPCs
        ((describe-vpcs)
         (format-output out
           (describe-vpcs client
             vpc-ids: (parse-csv .?vpc-ids)
             filters: (parse-filters .?filters))))
        ((create-vpc)
         (format-output out
           (create-vpc client cidr-block: .cidr-block)))
        ((delete-vpc)
         (delete-vpc client .vpc-id)
         (displayln "VPC deleted"))

        ;; Subnets
        ((describe-subnets)
         (format-output out
           (describe-subnets client
             subnet-ids: (parse-csv .?subnet-ids)
             filters: (parse-filters .?filters))))
        ((create-subnet)
         (format-output out
           (create-subnet client
             vpc-id: .vpc-id
             cidr-block: .cidr-block
             availability-zone: (nonempty .?availability-zone))))
        ((delete-subnet)
         (delete-subnet client .subnet-id)
         (displayln "Subnet deleted"))

        ;; Volumes
        ((describe-volumes)
         (format-output out
           (describe-volumes client
             volume-ids: (parse-csv .?volume-ids)
             filters: (parse-filters .?filters))))
        ((create-volume)
         (format-output out
           (create-volume client
             availability-zone: .availability-zone
             size: (nonempty .?size)
             volume-type: (nonempty .?volume-type)
             snapshot-id: (nonempty .?snapshot-id))))
        ((delete-volume)
         (delete-volume client .volume-id)
         (displayln "Volume deleted"))
        ((attach-volume)
         (format-output out
           (attach-volume client
             volume-id: .volume-id
             instance-id: .instance-id
             device: .device)))
        ((detach-volume)
         (format-output out
           (detach-volume client .volume-id
             force: .?force)))

        ;; Snapshots
        ((describe-snapshots)
         (format-output out
           (describe-snapshots client
             snapshot-ids: (parse-csv .?snapshot-ids)
             owner-ids: (parse-csv .?owner-ids)
             filters: (parse-filters .?filters))))
        ((create-snapshot)
         (format-output out
           (create-snapshot client
             volume-id: .volume-id
             description: (nonempty .?description))))
        ((delete-snapshot)
         (delete-snapshot client .snapshot-id)
         (displayln "Snapshot deleted"))

        ;; Addresses
        ((describe-addresses)
         (format-output out
           (describe-addresses client
             allocation-ids: (parse-csv .?allocation-ids)
             public-ips: (parse-csv .?public-ips)
             filters: (parse-filters .?filters))))
        ((allocate-address)
         (format-output out
           (allocate-address client domain: .domain)))
        ((release-address)
         (release-address client allocation-id: .allocation-id)
         (displayln "Address released"))
        ((associate-address)
         (format-output out
           (associate-address client
             allocation-id: .allocation-id
             instance-id: .instance-id)))
        ((disassociate-address)
         (disassociate-address client association-id: .association-id)
         (displayln "Address disassociated"))

        ;; Network Interfaces
        ((describe-network-interfaces)
         (format-output out
           (describe-network-interfaces client
             network-interface-ids: (parse-csv .?network-interface-ids)
             filters: (parse-filters .?filters))))

        ;; Route Tables
        ((describe-route-tables)
         (format-output out
           (describe-route-tables client
             route-table-ids: (parse-csv .?route-table-ids)
             filters: (parse-filters .?filters))))
        ((create-route-table)
         (format-output out
           (create-route-table client .vpc-id)))
        ((delete-route-table)
         (delete-route-table client .route-table-id)
         (displayln "Route table deleted"))
        ((create-route)
         (create-route client
           route-table-id: .route-table-id
           destination-cidr-block: .destination-cidr
           gateway-id: (nonempty .?gateway-id)
           nat-gateway-id: (nonempty .?nat-gateway-id))
         (displayln "Route created"))
        ((delete-route)
         (delete-route client
           route-table-id: .route-table-id
           destination-cidr-block: .destination-cidr)
         (displayln "Route deleted"))

        ;; Internet Gateways
        ((describe-internet-gateways)
         (format-output out
           (describe-internet-gateways client
             internet-gateway-ids: (parse-csv .?internet-gateway-ids)
             filters: (parse-filters .?filters))))
        ((create-internet-gateway)
         (format-output out
           (create-internet-gateway client)))
        ((delete-internet-gateway)
         (delete-internet-gateway client .internet-gateway-id)
         (displayln "Internet gateway deleted"))
        ((attach-internet-gateway)
         (attach-internet-gateway client
           internet-gateway-id: .internet-gateway-id
           vpc-id: .vpc-id)
         (displayln "Internet gateway attached"))
        ((detach-internet-gateway)
         (detach-internet-gateway client
           internet-gateway-id: .internet-gateway-id
           vpc-id: .vpc-id)
         (displayln "Internet gateway detached"))

        ;; NAT Gateways
        ((describe-nat-gateways)
         (format-output out
           (describe-nat-gateways client
             nat-gateway-ids: (parse-csv .?nat-gateway-ids)
             filters: (parse-filters .?filters))))
        ((create-nat-gateway)
         (format-output out
           (create-nat-gateway client
             subnet-id: .subnet-id
             allocation-id: .allocation-id)))
        ((delete-nat-gateway)
         (format-output out
           (delete-nat-gateway client .nat-gateway-id)))

        ;; Tags
        ((describe-tags)
         (format-output out
           (describe-tags client
             filters: (parse-filters .?filters))))
        ((create-tags)
         (create-tags client (parse-csv .resources) (parse-tags .tags))
         (displayln "Tags created"))
        ((delete-tags)
         (delete-tags client (parse-csv .resources) (parse-tags .tags))
         (displayln "Tags deleted"))

        ;; Regions
        ((describe-regions)
         (format-output out
           (describe-regions client all-regions: .?all)
           columns: ["regionName" "regionEndpoint"]))
        ((describe-availability-zones)
         (format-output out
           (describe-availability-zones client all-zones: .?all)
           columns: ["zoneName" "zoneState" "regionName"]))

        ;; S3 Buckets
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

        ;; S3 Objects
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
           (displayln "Object copied")))))))
