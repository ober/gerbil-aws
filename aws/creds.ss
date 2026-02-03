;;; -*- Gerbil -*-
;;; AWS credential resolution
;;; Extracted from :std/net/s3/api for shared use across AWS services

(import :std/sugar
        (only-in :std/srfi/13 string-trim-both string-prefix? string-suffix? string-index))
(export aws-resolve-credentials parse-ini-file ini-get getenv*)

;; Parse a simple INI file into a hash table of sections.
;; Each section is a hash table of key-value pairs.
(def (parse-ini-file path)
  (and (file-exists? path)
       (call-with-input-file path
         (lambda (port)
           (let ((sections (make-hash-table))
                 (current-section #f)
                 (current-hash #f))
             (let loop ()
               (let (line (read-line port))
                 (unless (eof-object? line)
                   (let (trimmed (string-trim-both line))
                     (cond
                       ;; Empty line or comment
                       ((or (string-empty? trimmed)
                            (string-prefix? "#" trimmed)
                            (string-prefix? ";" trimmed))
                        (void))
                       ;; Section header [name] or [profile name]
                       ((and (string-prefix? "[" trimmed)
                             (string-suffix? "]" trimmed))
                        (let* ((section-name (substring trimmed 1 (- (string-length trimmed) 1)))
                               ;; Handle [profile foo] syntax from config file
                               (section-name (if (string-prefix? "profile " section-name)
                                               (substring section-name 8 (string-length section-name))
                                               section-name)))
                          (set! current-section section-name)
                          (set! current-hash (make-hash-table))
                          (hash-put! sections section-name current-hash)))
                       ;; Key = value
                       ((and current-hash (string-index trimmed #\=))
                        (let* ((idx (string-index trimmed #\=))
                               (key (string-trim-both (substring trimmed 0 idx)))
                               (val (string-trim-both (substring trimmed (+ idx 1) (string-length trimmed)))))
                          (hash-put! current-hash key val)))))
                   (loop))))
             sections)))))

;; Get a value from parsed INI sections for a given profile
(def (ini-get sections profile key)
  (and sections
       (alet (section (hash-get sections profile))
         (hash-get section key))))

;; Get environment variable, returning #f if not set or empty
(def (getenv* name)
  (let (val (getenv name #f))
    (and val (not (string-empty? val)) val)))

;; Resolve AWS credentials using standard credential chain:
;; 1. Environment variables
;; 2. ~/.aws/credentials file
;; 3. ~/.aws/config file (for region)
(def (aws-resolve-credentials profile)
  (let* ((profile (or profile (getenv* "AWS_PROFILE") "default"))
         (home (getenv "HOME" ""))
         (creds-file (path-expand ".aws/credentials" home))
         (config-file (path-expand ".aws/config" home))
         (creds (parse-ini-file creds-file))
         (config (parse-ini-file config-file))
         ;; Resolve access key: env -> credentials file
         (access-key (or (getenv* "AWS_ACCESS_KEY_ID")
                         (ini-get creds profile "aws_access_key_id")))
         ;; Resolve secret key: env (both names) -> credentials file
         (secret-key (or (getenv* "AWS_SECRET_ACCESS_KEY")
                         (getenv* "AWS_SECRET_KEY")
                         (ini-get creds profile "aws_secret_access_key")))
         ;; Resolve region: env -> credentials -> config -> default
         (region (or (getenv* "AWS_DEFAULT_REGION")
                     (getenv* "AWS_REGION")
                     (ini-get creds profile "region")
                     (ini-get config profile "region")
                     "us-east-1")))
    (values access-key secret-key region)))
