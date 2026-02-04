;;; -*- Gerbil -*-
;;; S3 XML response parsing
;;; Converts SXML (from :std/markup/xml) to nested hash tables

(import :std/markup/xml
        :std/iter
        (only-in :std/srfi/13 string-prefix? string-contains))
(export s3-parse-xml sxml->hash strip-ns sxml-items sxml-text s3-response->hash)

;; S3 namespace mapping
(def s3-namespaces
  '(("http://s3.amazonaws.com/doc/2006-03-01/" . "s3")
    ("http://doc.s3.amazonaws.com/2006-03-01" . "s3")))

;; Parse an XML response body string into SXML
(def (s3-parse-xml body)
  (read-xml body namespaces: s3-namespaces))

;; Strip namespace prefix from a symbol (s3:foo -> foo)
(def (strip-ns sym)
  (let (s (symbol->string sym))
    (let (pos (string-contains s ":"))
      (if pos
        (string->symbol (substring s (+ pos 1) (string-length s)))
        sym))))

;; Get text content of an SXML element
;; (tag "text") -> "text"
;; (tag (subtag "text")) -> #f (not a leaf)
(def (sxml-text element)
  (and (pair? element)
       (= (length element) 2)
       (string? (cadr element))
       (cadr element)))

;; Extract children that match a given tag name (stripped of namespace)
;; Used for S3 responses where items are named elements (e.g., Bucket, Contents)
;; not generic <item> tags like EC2
(def (sxml-items element (tag #f))
  (if (pair? element)
    (filter (lambda (child)
              (and (pair? child)
                   (symbol? (car child))
                   (if tag
                     (eq? (strip-ns (car child)) tag)
                     ;; Default: look for 'item like EC2
                     (eq? (strip-ns (car child)) 'item))))
            (cdr element))
    []))

;; Convert an SXML element to a hash table recursively
;; - Leaf text nodes become string values
;; - Elements containing repeated children with the same tag become lists
;; - Other nested elements become nested hash tables
;; - Boolean strings "true"/"false" remain as strings (caller can convert)
(def (sxml->hash element)
  (cond
    ;; String leaf
    ((string? element) element)
    ;; Not a proper element
    ((not (pair? element)) #f)
    ;; Skip @-attributes and *TOP* wrapper
    ((memq (car element) '(@ *TOP* *NAMESPACES*)) #f)
    ;; Leaf element with text: (tag "text")
    ((sxml-text element) => (lambda (text) text))
    ;; Empty element: (tag)
    ((null? (cdr element)) "")
    (else
     (let* ((children (filter (lambda (c)
                                (and (pair? c)
                                     (symbol? (car c))
                                     (not (eq? (car c) '@))))
                              (cdr element))))
       (cond
         ;; No children -> empty string
         ((null? children) "")
         ;; Single child that is text
         ((and (= (length (cdr element)) 1)
               (string? (cadr element)))
          (cadr element))
         ;; Multiple children -> hash table
         ;; Detect repeated tags and collect them as lists
         (else
          (let (ht (make-hash-table))
            (for (child children)
              (let* ((key (strip-ns (car child)))
                     (val (sxml->hash child)))
                (when val
                  ;; If key already exists (duplicate elements), make a list
                  (let (existing (hash-get ht key))
                    (if existing
                      (if (list? existing)
                        (hash-put! ht key (append existing [val]))
                        (hash-put! ht key [existing val]))
                      (hash-put! ht key val))))))
            ht)))))))

;; Convert a full S3 XML response, extracting the response element
(def (s3-response->hash xml)
  (let find ((node xml))
    (cond
      ((not (pair? node)) #f)
      ;; Skip metadata nodes entirely
      ((and (pair? node) (symbol? (car node))
            (memq (car node) '(@ *NAMESPACES* *PI*)))
       #f)
      ;; Found a real element
      ((and (pair? node) (symbol? (car node))
            (not (eq? (car node) '*TOP*)))
       (sxml->hash node))
      ;; Recurse into *TOP* or list structure
      (else
       (let loop ((children (if (pair? node) (cdr node) [])))
         (if (null? children)
           #f
           (or (find (car children))
               (loop (cdr children)))))))))
