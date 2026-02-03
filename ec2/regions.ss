;;; -*- Gerbil -*-
;;; EC2 Region and Availability Zone operations

(import :gerbil-aws/ec2/api
        :gerbil-aws/ec2/params)
(export describe-regions describe-availability-zones)

(def (describe-regions client
       region-names: (region-names [])
       filters: (filters [])
       all-regions: (all-regions #f))
  (ec2-action/items client "DescribeRegions" 'regionInfo
    (params-merge
      (ec2-param-list "RegionName" region-names)
      (ec2-param-filters filters)
      (if all-regions [["AllRegions" :: "true"]] []))))

(def (describe-availability-zones client
       zone-names: (zone-names [])
       zone-ids: (zone-ids [])
       filters: (filters [])
       all-zones: (all-zones #f))
  (ec2-action/items client "DescribeAvailabilityZones" 'availabilityZoneInfo
    (params-merge
      (ec2-param-list "ZoneName" zone-names)
      (ec2-param-list "ZoneId" zone-ids)
      (ec2-param-filters filters)
      (if all-zones [["AllAvailabilityZones" :: "true"]] []))))
