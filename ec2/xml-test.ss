;;; -*- Gerbil -*-
;;; Tests for EC2 XML response parsing

(import :std/test
        :gerbil-aws/ec2/xml)
(export xml-test)

(def params-test-xml
  "<DescribeInstancesResponse xmlns=\"http://ec2.amazonaws.com/doc/2016-11-15/\"><reservationSet><item><reservationId>r-123</reservationId><instancesSet><item><instanceId>i-abc</instanceId><instanceType>t2.micro</instanceType></item></instancesSet></item></reservationSet></DescribeInstancesResponse>")

(def regions-test-xml
  "<DescribeRegionsResponse xmlns=\"http://ec2.amazonaws.com/doc/2016-11-15/\"><regionInfo><item><regionName>us-east-1</regionName><regionEndpoint>ec2.us-east-1.amazonaws.com</regionEndpoint></item><item><regionName>us-west-2</regionName><regionEndpoint>ec2.us-west-2.amazonaws.com</regionEndpoint></item></regionInfo></DescribeRegionsResponse>")

(def empty-set-xml
  "<DescribeInstancesResponse xmlns=\"http://ec2.amazonaws.com/doc/2016-11-15/\"><reservationSet/></DescribeInstancesResponse>")

(def error-xml
  "<Response><Errors><Error><Code>InvalidInstanceID.NotFound</Code><Message>The instance ID 'i-bad' does not exist</Message></Error></Errors><RequestID>req-123</RequestID></Response>")

(def xml-test
  (test-suite "EC2 XML parsing"

    (test-case "strip-ns removes ec2 prefix"
      (check (strip-ns 'ec2:instanceId) => 'instanceId)
      (check (strip-ns 'plainName) => 'plainName))

    (test-case "ec2-parse-xml parses with namespace"
      (let (xml (ec2-parse-xml params-test-xml))
        (check (pair? xml) => #t)))

    (test-case "sxml->hash converts leaf text"
      (let* ((xml (ec2-parse-xml params-test-xml))
             (result (ec2-response->hash xml)))
        (check (hash-table? result) => #t)))

    (test-case "sxml->hash handles regions response"
      (let* ((xml (ec2-parse-xml regions-test-xml))
             (result (ec2-response->hash xml)))
        (check (hash-table? result) => #t)
        ;; regionInfo should be a list from the set element
        (let (regions (hash-get result 'regionInfo))
          (check (list? regions) => #t)
          (check (length regions) => 2)
          (let (first-region (car regions))
            (check (hash-table? first-region) => #t)
            (check (hash-get first-region 'regionName) => "us-east-1")
            (check (hash-get first-region 'regionEndpoint) => "ec2.us-east-1.amazonaws.com")))))

    (test-case "sxml->hash handles nested instances"
      (let* ((xml (ec2-parse-xml params-test-xml))
             (result (ec2-response->hash xml)))
        (let (reservations (hash-get result 'reservationSet))
          (check (list? reservations) => #t)
          (check (length reservations) => 1)
          (let* ((reservation (car reservations))
                 (instances (hash-get reservation 'instancesSet)))
            (check (list? instances) => #t)
            (let (instance (car instances))
              (check (hash-get instance 'instanceId) => "i-abc")
              (check (hash-get instance 'instanceType) => "t2.micro"))))))

    (test-case "sxml-text extracts text"
      (check (sxml-text '(foo "bar")) => "bar")
      (check (sxml-text '(foo (bar "baz"))) => #f)
      (check (sxml-text "just a string") => #f))))
