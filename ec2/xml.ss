;;; -*- Gerbil -*-
;;; EC2 XML response parsing
;;; Converts SXML (from :std/markup/xml) to nested hash tables

(import :std/markup/xml
        :std/iter
        (only-in :std/srfi/13 string-prefix? string-contains))
(export ec2-parse-xml sxml->hash strip-ns sxml-items sxml-text ec2-response->hash)

;; EC2 namespace mapping
(def ec2-namespaces
  '(("http://ec2.amazonaws.com/doc/2016-11-15/" . "ec2")))

;; Parse an XML response body string into SXML
(def (ec2-parse-xml body)
  (read-xml body namespaces: ec2-namespaces))

;; Strip namespace prefix from a symbol (ec2:foo -> foo)
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

;; Check if an element name looks like a set/list container
;; e.g., instancesSet, reservationSet, tagSet, securityGroupSet, ...
(def (set-element? name)
  (let (s (symbol->string name))
    (or (string-suffix? "Set" s)
        (string-suffix? "set" s)
        ;; Some EC2 responses use plural forms instead of Set
        (string-suffix? "Addresses" s)
        (string-suffix? "Groups" s))))

;; Extract <item> children from a set element
;; (ec2:instancesSet (ec2:item ...) (ec2:item ...)) -> list of items
(def (sxml-items element)
  (if (pair? element)
    (filter (lambda (child)
              (and (pair? child)
                   (eq? (strip-ns (car child)) 'item)))
            (cdr element))
    []))

;; Convert an SXML element to a hash table recursively
;; - Leaf text nodes become string values
;; - Set elements (ending in Set/set) become lists of converted items
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

;; Convert a full EC2 XML response, extracting the response element
(def (ec2-response->hash xml)
  (let find ((node xml))
    (cond
      ((not (pair? node)) #f)
      ;; Skip metadata nodes entirely
      ((and (pair? node) (symbol? (car node))
            (memq (car node) '(@ *NAMESPACES*)))
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
