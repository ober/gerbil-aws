;;; -*- Gerbil -*-
;;; Persistent HTTPS connections for S3 with HTTP/1.1 keep-alive
;;; Avoids the per-request TLS handshake overhead of :std/net/request

(import :std/net/ssl
        :std/io
        :std/sugar
        :std/format
        :std/text/utf8
        :std/pregexp)
(export s3-conn s3-conn? s3-conn-host s3-conn-port
        s3-conn-open s3-conn-close! s3-conn-alive?
        s3-conn-request!
        s3-response make-s3-response s3-response?
        s3-response-status s3-response-status-text
        s3-response-headers s3-response-body)

;; Persistent HTTPS connection
(defstruct s3-conn (sock reader writer host port)
  final: #t)

;; Lightweight response from a persistent connection request
(defstruct s3-response (status status-text headers body)
  final: #t)

;; Open a persistent TLS connection to host:port
(def (s3-conn-open host (port 443))
  (let* ((addr (format "~a:~a" host port))
         (sock (ssl-connect addr))
         (reader (open-buffered-reader (StreamSocket-reader sock)))
         (writer (open-buffered-writer (StreamSocket-writer sock))))
    (make-s3-conn sock reader writer host port)))

;; Close the connection
(def (s3-conn-close! conn)
  (using (conn :- s3-conn)
    (when conn.sock
      (with-catch void (cut StreamSocket-close conn.sock))
      (set! conn.sock #f)
      (set! conn.reader #f)
      (set! conn.writer #f))))

;; Check if connection is still usable
(def (s3-conn-alive? conn)
  (and conn (s3-conn? conn) (s3-conn-sock conn) #t))

;; Send an HTTP request on the persistent connection and read the full response.
;; method: symbol (GET, PUT, POST, DELETE, HEAD)
;; target: string path+query (e.g. "/bucket/key?param=val")
;; headers: alist of (key . value) pairs — must include Host, Authorization, etc.
;; body: #f or u8vector
;; Returns an s3-response with pre-read body.
(def (s3-conn-request! conn method target headers body)
  (using (conn :- s3-conn)
    (let ((writer conn.writer)
          (reader conn.reader))
      (using ((writer :- BufferedWriter)
              (reader :- BufferedReader))
        ;; Write request line
        (write-line! writer (format "~a ~a HTTP/1.1" method target))
        ;; Write headers — ensure Connection: keep-alive
        (let ((headers (if (assoc "Connection" headers)
                         headers
                         (cons '("Connection" . "keep-alive") headers))))
          (for-each (lambda (h)
                      (write-line! writer (format "~a: ~a" (car h) (cdr h))))
                    headers))
        ;; End of headers
        (write-line! writer "")
        ;; Write body if present
        (when body
          (writer.write body))
        (writer.flush)
        ;; Read response
        (let-values (((status status-text resp-hdrs) (read-response-head! reader)))
          (let ((resp-body (if (eq? method 'HEAD)
                             #f
                             (read-response-body! reader resp-hdrs))))
            (make-s3-response status status-text resp-hdrs resp-body)))))))

;;; --- HTTP/1.1 protocol helpers ---

(def (write-line! writer line)
  (using (writer :- BufferedWriter)
    (writer.write (string->utf8 (string-append line "\r\n")))))

(def status-line-rx
  (pregexp "HTTP/1\\.[01]\\s+([0-9]{3})\\s+(.*)"))

(def header-line-rx
  (pregexp "([^:]+):\\s*(.*)"))

(def (read-response-head! reader)
  (using (reader :- BufferedReader)
    (let* ((status-line (read-crlf-line! reader))
           (m (pregexp-match status-line-rx status-line)))
      (unless m
        (error "Malformed HTTP status line" status-line))
      (let ((status (string->number (cadr m)))
            (status-text (str-trim-right (caddr m)))
            (headers (read-headers! reader)))
        (values status status-text headers)))))

(def (read-headers! reader)
  (let loop ((acc []))
    (let ((line (read-crlf-line! reader)))
      (if (or (string=? line "") (not line))
        (reverse acc)
        (let ((m (pregexp-match header-line-rx line)))
          (if m
            (loop (cons (cons (str-titlecase (cadr m))
                              (str-trim-right (caddr m)))
                        acc))
            (loop acc)))))))

(def (read-response-body! reader headers)
  (using (reader :- BufferedReader)
    (cond
      ;; Chunked transfer encoding
      ((let ((te (hdr-get "Transfer-Encoding" headers)))
         (and te (not (equal? te "identity"))))
       (read-chunked-body! reader))
      ;; Content-Length
      ((hdr-get "Content-Length" headers)
       => (lambda (cl)
            (let* ((len (string->number cl))
                   (buf (make-u8vector len)))
              (when (fx> len 0)
                (reader.read buf 0 len len))
              buf)))
      ;; No body indication — read nothing (keep-alive safe)
      (else #u8()))))

(def (read-chunked-body! reader)
  (using (reader :- BufferedReader)
    (let loop ((chunks []))
      (let* ((size-line (read-crlf-line! reader))
             (size (string->number (car (string-split size-line #\space)) 16)))
        (if (or (not size) (fxzero? size))
          (begin
            ;; Read trailing headers/CRLF after last chunk
            (read-crlf-line! reader)
            (u8vector-concatenate (reverse chunks)))
          (let ((chunk (make-u8vector size)))
            (reader.read chunk 0 size size)
            ;; Read chunk trailing CRLF
            (read-crlf-line! reader)
            (loop (cons chunk chunks))))))))

;; Read a CRLF-terminated line, return as string without CRLF
(def (read-crlf-line! reader)
  (using (reader :- BufferedReader)
    (let loop ((acc []))
      (let ((b (reader.read-u8-inline)))
        (cond
          ((eof-object? b)
           (if (null? acc) ""
               (utf8->string (list->u8vector (reverse acc)))))
          ((fx= b 13) ; CR
           (let ((next (reader.read-u8-inline)))
             (if (and (fixnum? next) (fx= next 10)) ; LF
               (utf8->string (list->u8vector (reverse acc)))
               ;; Not CRLF, keep both bytes
               (loop (cons next (cons 13 acc))))))
          (else
           (loop (cons b acc))))))))

;;; --- String/header helpers ---

(def (hdr-get key headers)
  (cond
    ((assoc key headers) => cdr)
    (else #f)))

(def (str-titlecase str)
  (let* ((parts (string-split str #\-))
         (titled (map (lambda (p)
                        (if (string=? p "") p
                            (string-append
                              (string-upcase (substring p 0 1))
                              (string-downcase (substring p 1 (string-length p))))))
                      parts)))
    (string-join titled "-")))

(def (str-trim-right str)
  (let loop ((i (fx- (string-length str) 1)))
    (if (fx< i 0) ""
        (let ((ch (string-ref str i)))
          (if (or (char=? ch #\space) (char=? ch #\return) (char=? ch #\newline))
            (loop (fx- i 1))
            (substring str 0 (fx+ i 1)))))))
