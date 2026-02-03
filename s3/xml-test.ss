;;; -*- Gerbil -*-
;;; Tests for S3 XML response parsing

(import :std/test
        :gerbil-aws/s3/xml)
(export xml-test)

(def list-buckets-xml
  "<ListAllMyBucketsResult xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\"><Owner><ID>owner-id</ID><DisplayName>owner</DisplayName></Owner><Buckets><Bucket><Name>my-bucket</Name><CreationDate>2024-01-15T10:30:00.000Z</CreationDate></Bucket><Bucket><Name>other-bucket</Name><CreationDate>2024-02-20T14:00:00.000Z</CreationDate></Bucket></Buckets></ListAllMyBucketsResult>")

(def list-objects-xml
  "<ListBucketResult xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\"><Name>my-bucket</Name><Prefix>docs/</Prefix><MaxKeys>1000</MaxKeys><IsTruncated>false</IsTruncated><Contents><Key>docs/file1.txt</Key><Size>1024</Size><StorageClass>STANDARD</StorageClass></Contents><Contents><Key>docs/file2.txt</Key><Size>2048</Size><StorageClass>STANDARD</StorageClass></Contents></ListBucketResult>")

(def list-objects-delimiter-xml
  "<ListBucketResult xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\"><Name>my-bucket</Name><Prefix></Prefix><Delimiter>/</Delimiter><MaxKeys>1000</MaxKeys><IsTruncated>false</IsTruncated><CommonPrefixes><Prefix>docs/</Prefix></CommonPrefixes><CommonPrefixes><Prefix>images/</Prefix></CommonPrefixes></ListBucketResult>")

(def error-xml
  "<Error><Code>NoSuchBucket</Code><Message>The specified bucket does not exist</Message><BucketName>bad-bucket</BucketName><RequestId>req-456</RequestId></Error>")

(def empty-bucket-xml
  "<ListBucketResult xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\"><Name>empty-bucket</Name><Prefix></Prefix><MaxKeys>1000</MaxKeys><IsTruncated>false</IsTruncated><KeyCount>0</KeyCount></ListBucketResult>")

(def xml-test
  (test-suite "S3 XML parsing"

    (test-case "strip-ns removes s3 prefix"
      (check (strip-ns 's3:Name) => 'Name)
      (check (strip-ns 'plainName) => 'plainName))

    (test-case "s3-parse-xml parses with namespace"
      (let (xml (s3-parse-xml list-buckets-xml))
        (check (pair? xml) => #t)))

    (test-case "sxml-text extracts text"
      (check (sxml-text '(foo "bar")) => "bar")
      (check (sxml-text '(foo (bar "baz"))) => #f)
      (check (sxml-text "just a string") => #f))

    (test-case "s3-response->hash converts list-buckets response"
      (let* ((xml (s3-parse-xml list-buckets-xml))
             (result (s3-response->hash xml)))
        (check (hash-table? result) => #t)
        ;; Owner should be a hash
        (let (owner (hash-get result 'Owner))
          (check (hash-table? owner) => #t)
          (check (hash-get owner 'ID) => "owner-id")
          (check (hash-get owner 'DisplayName) => "owner"))
        ;; Buckets should contain Bucket entries
        (let (buckets (hash-get result 'Buckets))
          (check (hash-table? buckets) => #t)
          (let (bucket-list (hash-get buckets 'Bucket))
            (check (list? bucket-list) => #t)
            (check (length bucket-list) => 2)
            (check (hash-get (car bucket-list) 'Name) => "my-bucket")
            (check (hash-get (cadr bucket-list) 'Name) => "other-bucket")))))

    (test-case "s3-response->hash converts list-objects response"
      (let* ((xml (s3-parse-xml list-objects-xml))
             (result (s3-response->hash xml)))
        (check (hash-table? result) => #t)
        (check (hash-get result 'Name) => "my-bucket")
        (check (hash-get result 'Prefix) => "docs/")
        (check (hash-get result 'IsTruncated) => "false")
        ;; Contents should be a list (repeated elements)
        (let (contents (hash-get result 'Contents))
          (check (list? contents) => #t)
          (check (length contents) => 2)
          (check (hash-get (car contents) 'Key) => "docs/file1.txt")
          (check (hash-get (car contents) 'Size) => "1024")
          (check (hash-get (cadr contents) 'Key) => "docs/file2.txt"))))

    (test-case "s3-response->hash handles CommonPrefixes"
      (let* ((xml (s3-parse-xml list-objects-delimiter-xml))
             (result (s3-response->hash xml)))
        (check (hash-table? result) => #t)
        (check (hash-get result 'Delimiter) => "/")
        ;; CommonPrefixes should be a list (repeated elements)
        (let (prefixes (hash-get result 'CommonPrefixes))
          (check (list? prefixes) => #t)
          (check (length prefixes) => 2))))

    (test-case "s3-response->hash converts error response"
      (let* ((xml (s3-parse-xml error-xml))
             (result (s3-response->hash xml)))
        (check (hash-table? result) => #t)
        (check (hash-get result 'Code) => "NoSuchBucket")
        (check (hash-get result 'Message) => "The specified bucket does not exist")
        (check (hash-get result 'BucketName) => "bad-bucket")))

    (test-case "s3-response->hash handles empty bucket listing"
      (let* ((xml (s3-parse-xml empty-bucket-xml))
             (result (s3-response->hash xml)))
        (check (hash-table? result) => #t)
        (check (hash-get result 'Name) => "empty-bucket")
        (check (hash-get result 'KeyCount) => "0")
        (check (hash-get result 'IsTruncated) => "false")))))
