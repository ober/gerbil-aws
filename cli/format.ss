;;; -*- Gerbil -*-
;;; Output formatters: json, text, table

(import :std/text/json
        :std/format
        :std/iter
        :std/sort
        :std/sugar)
(export format-output format-json format-text format-table)

;; Dispatch to the appropriate formatter
(def (format-output mode data columns: (columns #f))
  (cond
    ((equal? mode "json")  (format-json data))
    ((equal? mode "text")  (format-text data))
    ((equal? mode "table") (format-table data columns: columns))
    (else (format-json data))))

;; JSON output — pretty-printed
(def (format-json data)
  (cond
    ((list? data)
     (pretty-json (list->vector (map hash->json-ready data))))
    ((hash-table? data)
     (pretty-json (hash->json-ready data)))
    (else
     (displayln data))))

;; Convert hash table to JSON-friendly form (string keys, recursive)
(def (hash->json-ready data)
  (cond
    ((hash-table? data)
     (let (ht (make-hash-table))
       (hash-for-each
         (lambda (k v)
           (hash-put! ht (if (symbol? k) (symbol->string k) k)
                      (hash->json-ready v)))
         data)
       ht))
    ((list? data)
     (list->vector (map hash->json-ready data)))
    (else data)))

;; Text output — key: value pairs
(def (format-text data)
  (cond
    ((list? data)
     (let loop ((items data) (first? #t))
       (unless (null? items)
         (unless first? (displayln "---"))
         (format-text-item (car items))
         (loop (cdr items) #f))))
    ((hash-table? data)
     (format-text-item data))
    (else
     (displayln data))))

(def (format-text-item item)
  (when (hash-table? item)
    (let (keys (sort (map (lambda (k)
                            (if (symbol? k) (symbol->string k) k))
                          (hash-keys item))
                     string<?))
      (for (k keys)
        (let (v (hash-ref item (string->symbol k)
                          (hash-get item k)))
          (cond
            ((hash-table? v)
             (displayln k ":")
             (format-text-nested v "  "))
            ((list? v)
             (displayln k ":")
             (for (sub v)
               (if (hash-table? sub)
                 (begin (displayln "  -")
                        (format-text-nested sub "    "))
                 (displayln "  - " sub))))
            (else
             (displayln k "\t" v))))))))

(def (format-text-nested item prefix)
  (when (hash-table? item)
    (let (keys (sort (map (lambda (k)
                            (if (symbol? k) (symbol->string k) k))
                          (hash-keys item))
                     string<?))
      (for (k keys)
        (let (v (hash-ref item (string->symbol k)
                          (hash-get item k)))
          (cond
            ((hash-table? v)
             (displayln prefix k ":")
             (format-text-nested v (string-append prefix "  ")))
            ((list? v)
             (displayln prefix k ":")
             (for (sub v)
               (if (hash-table? sub)
                 (begin (displayln prefix "  -")
                        (format-text-nested sub (string-append prefix "    ")))
                 (displayln prefix "  - " sub))))
            (else
             (displayln prefix k "\t" v))))))))

;; Table output — aligned columns
(def (format-table data columns: (columns #f))
  (when (and (list? data) (not (null? data)))
    (let* ((cols (or columns
                     (if (hash-table? (car data))
                       (sort (map (lambda (k)
                                    (if (symbol? k) (symbol->string k) k))
                                  (hash-keys (car data)))
                             string<?)
                       [])))
           ;; Compute column widths
           (widths (map (lambda (col)
                          (apply max
                            (string-length col)
                            (map (lambda (row)
                                   (let (v (table-get-value row col))
                                     (string-length (if (string? v) v (format "~a" v)))))
                                 data)))
                        cols)))
      ;; Header
      (display-row cols widths)
      ;; Separator
      (displayln (string-join (map (lambda (w) (make-string w #\-)) widths) "  "))
      ;; Rows
      (for (row data)
        (display-row
          (map (lambda (col) (let (v (table-get-value row col))
                               (if (string? v) v (format "~a" v))))
               cols)
          widths)))))

(def (table-get-value row col)
  (if (hash-table? row)
    (let (v (or (hash-get row (string->symbol col))
                (hash-get row col)))
      (cond
        ((not v) "")
        ((hash-table? v) "{...}")
        ((list? v) (format "[~a items]" (length v)))
        (else v)))
    ""))

(def (display-row values widths)
  (displayln
    (string-join
      (map (lambda (val width)
             (let* ((s (if (string? val) val (format "~a" val)))
                    (pad (- width (string-length s))))
               (if (> pad 0)
                 (string-append s (make-string pad #\space))
                 s)))
           values widths)
      "  ")))

(def (string-join strs sep)
  (if (null? strs) ""
    (let loop ((rest (cdr strs)) (acc (car strs)))
      (if (null? rest)
        acc
        (loop (cdr rest) (string-append acc sep (car rest)))))))
