;;; -*- Gerbil -*-
;;; Shared XML parsing utilities for AWS services
;;; Converts SXML (from :std/markup/xml) to nested hash tables

(import :std/markup/xml
        :std/iter
        (only-in :std/srfi/13 string-prefix? string-contains string-suffix?))
(export aws-parse-xml sxml->hash strip-ns sxml-items sxml-text aws-response->hash)

;; Parse an XML response body string into SXML with given namespace mappings
(def (aws-parse-xml body namespaces: (namespaces []))
  (read-xml body namespaces: namespaces))

;; Strip namespace prefix from a symbol (ns:foo -> foo)
;; Finds the LAST colon to handle full URI namespace prefixes
;; e.g. http://sts.amazonaws.com/doc/2011-06-15/:Account -> Account
(def (strip-ns sym)
  (let* ((s (symbol->string sym))
         (len (string-length s)))
    (let loop ((i (- len 1)))
      (cond
        ((< i 0) sym)
        ((char=? (string-ref s i) #\:)
         (string->symbol (substring s (+ i 1) len)))
        (else (loop (- i 1)))))))

;; Get text content of an SXML element
;; (tag "text") -> "text"
;; (tag (subtag "text")) -> #f (not a leaf)
(def (sxml-text element)
  (and (pair? element)
       (= (length element) 2)
       (string? (cadr element))
       (cadr element)))

;; Check if an element name looks like a set/list container
(def (set-element? name)
  (let (s (symbol->string name))
    (or (string-suffix? "Set" s)
        (string-suffix? "set" s)
        (string-suffix? "Addresses" s)
        (string-suffix? "Groups" s))))

;; Extract children matching a given tag name (stripped of namespace)
;; If tag is #f, looks for 'item (EC2-style)
(def (sxml-items element (tag #f))
  (if (pair? element)
    (filter (lambda (child)
              (and (pair? child)
                   (symbol? (car child))
                   (if tag
                     (eq? (strip-ns (car child)) tag)
                     (eq? (strip-ns (car child)) 'item))))
            (cdr element))
    []))

;; Convert an SXML element to a hash table recursively
(def (sxml->hash element)
  (cond
    ;; String leaf
    ((string? element) element)
    ;; Not a proper element
    ((not (pair? element)) #f)
    ;; Skip @-attributes, *TOP* wrapper, and processing instructions
    ((memq (car element) '(@ *TOP* *NAMESPACES* *PI*)) #f)
    ;; Leaf element with text: (tag "text")
    ((sxml-text element) => (lambda (text) text))
    ;; Empty element: (tag)
    ((null? (cdr element)) "")
    (else
     (let* ((children (filter (lambda (c)
                                (and (pair? c)
                                     (symbol? (car c))
                                     (not (eq? (car c) '@))))
                              (cdr element)))
            ;; Check if this is a set element with <item> children
            (items (sxml-items element)))
       (cond
         ;; Set element with items -> convert to list
         ((not (null? items))
          (map sxml->hash items))
         ;; No children -> empty string
         ((null? children) "")
         ;; Single child that is text
         ((and (= (length (cdr element)) 1)
               (string? (cadr element)))
          (cadr element))
         ;; Multiple children -> hash table
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

;; Convert a full AWS XML response, extracting the response element
(def (aws-response->hash xml)
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
