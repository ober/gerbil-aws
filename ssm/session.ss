;;; -*- Gerbil -*-
;;; AWS SSM Session Manager — native Gerbil implementation
;;; Implements the SSM agent message protocol over WebSocket.

(import :std/sugar
        :std/format
        :std/text/json
        :std/error
        :std/srfi/19
        :std/net/websocket
        :std/crypto/digest
        :gerbil-aws/ssm/api
        :gerbil-aws/ssm/operations)

(export ssm-connect ssm-list-instances)

;; ==================== Binary Encoding Helpers ====================

(def (encode-uint32 n)
  "Encode uint32 as 4 bytes big-endian."
  (let ((buf (make-u8vector 4 0)))
    (u8vector-set! buf 0 (bitwise-and (arithmetic-shift n -24) #xff))
    (u8vector-set! buf 1 (bitwise-and (arithmetic-shift n -16) #xff))
    (u8vector-set! buf 2 (bitwise-and (arithmetic-shift n -8) #xff))
    (u8vector-set! buf 3 (bitwise-and n #xff))
    buf))

(def (decode-uint32 buf offset)
  "Decode uint32 from 4 bytes big-endian."
  (+ (arithmetic-shift (u8vector-ref buf offset) 24)
     (arithmetic-shift (u8vector-ref buf (+ offset 1)) 16)
     (arithmetic-shift (u8vector-ref buf (+ offset 2)) 8)
     (u8vector-ref buf (+ offset 3))))

(def (encode-uint64 n)
  "Encode uint64 as 8 bytes big-endian."
  (let ((buf (make-u8vector 8 0)))
    (u8vector-set! buf 0 (bitwise-and (arithmetic-shift n -56) #xff))
    (u8vector-set! buf 1 (bitwise-and (arithmetic-shift n -48) #xff))
    (u8vector-set! buf 2 (bitwise-and (arithmetic-shift n -40) #xff))
    (u8vector-set! buf 3 (bitwise-and (arithmetic-shift n -32) #xff))
    (u8vector-set! buf 4 (bitwise-and (arithmetic-shift n -24) #xff))
    (u8vector-set! buf 5 (bitwise-and (arithmetic-shift n -16) #xff))
    (u8vector-set! buf 6 (bitwise-and (arithmetic-shift n -8) #xff))
    (u8vector-set! buf 7 (bitwise-and n #xff))
    buf))

(def (decode-uint64 buf offset)
  "Decode uint64 from 8 bytes big-endian."
  (+ (arithmetic-shift (u8vector-ref buf offset) 56)
     (arithmetic-shift (u8vector-ref buf (+ offset 1)) 48)
     (arithmetic-shift (u8vector-ref buf (+ offset 2)) 40)
     (arithmetic-shift (u8vector-ref buf (+ offset 3)) 32)
     (arithmetic-shift (u8vector-ref buf (+ offset 4)) 24)
     (arithmetic-shift (u8vector-ref buf (+ offset 5)) 16)
     (arithmetic-shift (u8vector-ref buf (+ offset 6)) 8)
     (u8vector-ref buf (+ offset 7))))

(def (encode-int64 n)
  "Encode signed int64 as 8 bytes big-endian (two's complement)."
  (encode-uint64 (if (< n 0) (+ (expt 2 64) n) n)))

(def (decode-int64 buf offset)
  "Decode signed int64 from 8 bytes big-endian."
  (let ((v (decode-uint64 buf offset)))
    (if (>= v (expt 2 63))
      (- v (expt 2 64))
      v)))

;; ==================== u8vector Helpers ====================

(def (u8v-copy! dst dst-off src src-off count)
  "Copy count bytes from src[src-off..] to dst[dst-off..]."
  (let loop ((i 0))
    (when (< i count)
      (u8vector-set! dst (+ dst-off i) (u8vector-ref src (+ src-off i)))
      (loop (+ i 1)))))

(def (u8v-append . vecs)
  "Concatenate u8vectors."
  (let* ((total (apply + (map u8vector-length vecs)))
         (result (make-u8vector total 0)))
    (let loop ((vecs vecs) (off 0))
      (if (null? vecs) result
        (let ((v (car vecs)))
          (u8v-copy! result off v 0 (u8vector-length v))
          (loop (cdr vecs) (+ off (u8vector-length v))))))))

(def (u8v-slice buf start end)
  "Extract a slice of a u8vector."
  (let* ((len (- end start))
         (result (make-u8vector len 0)))
    (u8v-copy! result 0 buf start len)
    result))

;; ==================== UUID Generation ====================

(def (generate-uuid)
  "Generate a random UUID v4 as 16-byte u8vector."
  (let ((bytes (random-u8vector 16)))
    ;; Set version 4
    (u8vector-set! bytes 6 (bitwise-ior (bitwise-and (u8vector-ref bytes 6) #x0f) #x40))
    ;; Set variant (10xx)
    (u8vector-set! bytes 8 (bitwise-ior (bitwise-and (u8vector-ref bytes 8) #x3f) #x80))
    bytes))

(def (uuid->string bytes)
  "Format 16-byte UUID as standard hex string."
  (let ((hex (lambda (b) (let ((s (number->string b 16)))
                           (if (< b 16) (string-append "0" s) s)))))
    (string-append
     (hex (u8vector-ref bytes 0)) (hex (u8vector-ref bytes 1))
     (hex (u8vector-ref bytes 2)) (hex (u8vector-ref bytes 3))
     "-"
     (hex (u8vector-ref bytes 4)) (hex (u8vector-ref bytes 5))
     "-"
     (hex (u8vector-ref bytes 6)) (hex (u8vector-ref bytes 7))
     "-"
     (hex (u8vector-ref bytes 8)) (hex (u8vector-ref bytes 9))
     "-"
     (hex (u8vector-ref bytes 10)) (hex (u8vector-ref bytes 11))
     (hex (u8vector-ref bytes 12)) (hex (u8vector-ref bytes 13))
     (hex (u8vector-ref bytes 14)) (hex (u8vector-ref bytes 15)))))

(def (uuid-wire-bytes uuid-bytes)
  "Swap UUID halves for wire format: bytes[8:16] ++ bytes[0:8]."
  (let ((result (make-u8vector 16 0)))
    (u8v-copy! result 0 uuid-bytes 8 8)
    (u8v-copy! result 8 uuid-bytes 0 8)
    result))

(def (uuid-from-wire bytes)
  "Restore UUID from wire format by swapping halves back."
  (uuid-wire-bytes bytes))

;; ==================== Agent Message Protocol ====================
;; 120-byte header + variable payload

(def HEADER-LENGTH 116)  ;; value stored in the header
(def HEADER-SIZE   120)  ;; actual bytes on the wire

;; Message type strings (space-padded to 32 bytes)
(def MSG-INPUT  "input_stream_data")
(def MSG-OUTPUT "output_stream_data")
(def MSG-ACK    "acknowledge")
(def MSG-CLOSED "channel_closed")

;; Payload type codes
(def PAYLOAD-UNDEFINED          0)
(def PAYLOAD-OUTPUT             1)
(def PAYLOAD-SIZE               3)
(def PAYLOAD-HANDSHAKE-REQUEST  5)
(def PAYLOAD-HANDSHAKE-RESPONSE 6)
(def PAYLOAD-HANDSHAKE-COMPLETE 7)
(def PAYLOAD-FLAG              10)
(def PAYLOAD-STDERR            11)
(def PAYLOAD-EXIT-CODE         12)

;; Flags
(def FLAG-DATA 0)
(def FLAG-SYN  1)

(def (pad-message-type msg-type)
  "Pad message type string to 32 bytes with spaces."
  (let* ((bytes (string->bytes msg-type))
         (result (make-u8vector 32 #x20)))
    (u8v-copy! result 0 bytes 0 (min (u8vector-length bytes) 32))
    result))

(def (unpad-message-type bytes offset)
  "Extract message type string from 32-byte field, trimming spaces and nulls."
  (let* ((raw (u8v-slice bytes offset (+ offset 32)))
         (str (bytes->string raw)))
    (let loop ((i (- (string-length str) 1)))
      (if (and (>= i 0)
               (or (char=? (string-ref str i) #\space)
                   (char=? (string-ref str i) #\nul)))
        (loop (- i 1))
        (substring str 0 (+ i 1))))))

(def (current-time-millis)
  "Current time in milliseconds since epoch."
  (let ((t (current-time)))
    (+ (* (time-second t) 1000)
       (quotient (time-nanosecond t) 1000000))))

(def (build-agent-message msg-type seq-num flags msg-id payload-type payload)
  "Build a complete SSM agent message (header + payload)."
  (let* ((payload-bytes (cond ((u8vector? payload) payload)
                              ((string? payload) (string->bytes payload))
                              (else (make-u8vector 0 0))))
         (payload-len (u8vector-length payload-bytes))
         (payload-hash (sha256 payload-bytes))
         (created-date (current-time-millis))
         (uuid-wire (uuid-wire-bytes msg-id))
         (header (make-u8vector HEADER-SIZE 0)))
    ;; HeaderLength (offset 0)
    (let ((hl (encode-uint32 HEADER-LENGTH)))
      (u8v-copy! header 0 hl 0 4))
    ;; MessageType (offset 4)
    (let ((mt (pad-message-type msg-type)))
      (u8v-copy! header 4 mt 0 32))
    ;; SchemaVersion (offset 36)
    (let ((sv (encode-uint32 1)))
      (u8v-copy! header 36 sv 0 4))
    ;; CreatedDate (offset 40)
    (let ((cd (encode-uint64 created-date)))
      (u8v-copy! header 40 cd 0 8))
    ;; SequenceNumber (offset 48)
    (let ((sn (encode-int64 seq-num)))
      (u8v-copy! header 48 sn 0 8))
    ;; Flags (offset 56)
    (let ((fl (encode-uint64 flags)))
      (u8v-copy! header 56 fl 0 8))
    ;; MessageId (offset 64)
    (u8v-copy! header 64 uuid-wire 0 16)
    ;; PayloadDigest (offset 80)
    (u8v-copy! header 80 payload-hash 0 32)
    ;; PayloadType (offset 112)
    (let ((pt (encode-uint32 payload-type)))
      (u8v-copy! header 112 pt 0 4))
    ;; PayloadLength (offset 116)
    (let ((pl (encode-uint32 payload-len)))
      (u8v-copy! header 116 pl 0 4))
    ;; Combine header + payload
    (u8v-append header payload-bytes)))

(defstruct agent-msg (type schema-version created-date seq-num flags
                      message-id payload-type payload-length payload)
  transparent: #t)

(def (parse-agent-message data)
  "Parse a binary agent message into an agent-msg struct."
  (when (< (u8vector-length data) HEADER-SIZE)
    (error "Agent message too short" (u8vector-length data)))
  (let* ((header-len (decode-uint32 data 0))
         (msg-type (unpad-message-type data 4))
         (schema-ver (decode-uint32 data 36))
         (created-date (decode-uint64 data 40))
         (seq-num (decode-int64 data 48))
         (flags (decode-uint64 data 56))
         (uuid-wire (u8v-slice data 64 80))
         (message-id (uuid-from-wire uuid-wire))
         (payload-type (if (>= header-len HEADER-LENGTH)
                         (decode-uint32 data 112)
                         PAYLOAD-UNDEFINED))
         (payload-length (decode-uint32 data 116))
         (payload (if (> payload-length 0)
                    (u8v-slice data HEADER-SIZE
                               (min (+ HEADER-SIZE payload-length)
                                    (u8vector-length data)))
                    (make-u8vector 0 0))))
    (make-agent-msg msg-type schema-ver created-date seq-num flags
                    message-id payload-type payload-length payload)))

;; ==================== Session State ====================

(defstruct ssm-session (ws session-id out-seq-num out-seq-lock running? terminal-set?)
  transparent: #t)

(def (make-session ws session-id)
  (make-ssm-session ws session-id 0 (make-mutex 'seq-lock) #t #f))

(def (next-seq! session)
  "Atomically get and increment the outgoing sequence number."
  (mutex-lock! (ssm-session-out-seq-lock session))
  (let ((n (ssm-session-out-seq-num session)))
    (ssm-session-out-seq-num-set! session (+ n 1))
    (mutex-unlock! (ssm-session-out-seq-lock session))
    n))

;; ==================== WebSocket I/O ====================

(def (ws-send-binary session data)
  (WebSocket-send (ssm-session-ws session)
                  (make-message data 'binary #f)))

(def (ws-send-text session text)
  (WebSocket-send (ssm-session-ws session)
                  (make-message text 'text #f)))

(def (ws-recv session)
  (try (WebSocket-recv (ssm-session-ws session))
       (catch (e) #f)))

;; ==================== Protocol Messages ====================

(def (to-json obj)
  (call-with-output-string (lambda (p) (write-json obj p))))

(def (send-acknowledge session msg)
  "Send an acknowledge for a received agent message."
  (let* ((payload (to-json
                   (hash ("AcknowledgedMessageType" (agent-msg-type msg))
                         ("AcknowledgedMessageId" (uuid->string (agent-msg-message-id msg)))
                         ("AcknowledgedMessageSequenceNumber" (agent-msg-seq-num msg))
                         ("IsSequentialMessage" #t))))
         (ack (build-agent-message MSG-ACK (agent-msg-seq-num msg) 0
                                   (generate-uuid) PAYLOAD-UNDEFINED payload)))
    (ws-send-binary session ack)))

(def (send-input session data)
  "Send user input data to the remote session."
  (let* ((seq (next-seq! session))
         (flags (if (= seq 0) FLAG-SYN FLAG-DATA))
         (msg (build-agent-message MSG-INPUT seq flags (generate-uuid)
                                   PAYLOAD-OUTPUT data)))
    (ws-send-binary session msg)))

(def (send-resize session cols rows)
  "Send terminal resize event."
  (let* ((payload (to-json (hash ("cols" cols) ("rows" rows))))
         (seq (next-seq! session))
         (msg (build-agent-message MSG-INPUT seq FLAG-DATA (generate-uuid)
                                   PAYLOAD-SIZE payload)))
    (ws-send-binary session msg)))

(def (send-handshake-response session)
  "Send handshake response confirming session type."
  (let* ((payload (to-json
                   (hash ("ClientVersion" "1.2.0.0")
                         ("ProcessedClientActions"
                          (vector (hash ("ActionType" "SessionType")
                                       ("ActionStatus" 1)
                                       ("ActionResult" #f)
                                       ("Error" ""))))
                         ("Errors" (vector)))))
         (seq (next-seq! session))
         (msg (build-agent-message MSG-INPUT seq FLAG-DATA (generate-uuid)
                                   PAYLOAD-HANDSHAKE-RESPONSE payload)))
    (ws-send-binary session msg)))

;; ==================== Terminal Handling ====================

(def (get-terminal-size)
  "Get terminal size as (cols . rows). Returns (80 . 24) as fallback."
  (try
    (let* ((output (with-input-from-process
                    (list path: "/bin/stty" arguments: '("size")
                          stdin-redirection: #f)
                    read-line))
           (parts (string-split output #\space))
           (rows (string->number (car parts)))
           (cols (string->number (cadr parts))))
      (cons (or cols 80) (or rows 24)))
    (catch (e) (cons 80 24))))

(def (set-terminal-raw!)
  "Set terminal to raw mode."
  (with-input-from-process
   (list path: "/bin/stty" arguments: '("raw" "-echo" "-icanon" "-isig")
         stdin-redirection: #f)
   read-line))

(def (restore-terminal!)
  "Restore terminal to normal mode."
  (with-input-from-process
   (list path: "/bin/stty" arguments: '("sane")
         stdin-redirection: #f)
   read-line))

;; ==================== Open Data Channel ====================

(def (open-data-channel session token-value)
  "Send the initial OpenDataChannelInput to authenticate the WebSocket."
  (ws-send-text session
    (to-json (hash ("MessageSchemaVersion" "1.0")
                   ("RequestId" (uuid->string (generate-uuid)))
                   ("TokenValue" token-value)
                   ("ClientId" (uuid->string (generate-uuid)))
                   ("ClientVersion" "1.2.0.0")))))

;; ==================== Message Handler ====================

(def (handle-output-message session msg)
  "Handle an output_stream_data message from the agent."
  (let ((ptype (agent-msg-payload-type msg))
        (payload (agent-msg-payload msg)))
    (cond
      ;; Terminal output
      ((or (= ptype PAYLOAD-OUTPUT) (= ptype PAYLOAD-STDERR))
       ;; If handshake never arrived, mark session ready on first output
       (unless (ssm-session-terminal-set? session)
         (ssm-session-terminal-set?-set! session #t)
         (let ((sz (get-terminal-size)))
           (send-resize session (car sz) (cdr sz))))
       (write-subu8vector payload 0 (u8vector-length payload) (current-output-port))
       (force-output (current-output-port)))
      ;; Handshake request
      ((= ptype PAYLOAD-HANDSHAKE-REQUEST)
       (send-handshake-response session))
      ;; Handshake complete — send initial terminal size
      ((= ptype PAYLOAD-HANDSHAKE-COMPLETE)
       (unless (ssm-session-terminal-set? session)
         (ssm-session-terminal-set?-set! session #t)
         (let ((sz (get-terminal-size)))
           (send-resize session (car sz) (cdr sz)))))
      ;; Exit code or terminate flag
      ((or (= ptype PAYLOAD-EXIT-CODE) (= ptype PAYLOAD-FLAG))
       (ssm-session-running?-set! session #f)))
    ;; Always acknowledge output messages
    (send-acknowledge session msg)))

(def (handle-message session msg)
  "Handle a received agent message."
  (let ((mtype (agent-msg-type msg)))
    (cond
      ((string=? mtype MSG-OUTPUT)
       (handle-output-message session msg))
      ((string=? mtype MSG-CLOSED)
       (ssm-session-running?-set! session #f))
      (else (void)))))

;; ==================== Session Threads ====================

(def (start-ws-reader session)
  "Spawn a thread that reads WebSocket messages and dispatches them."
  (spawn
   (lambda ()
     (let loop ()
       (when (ssm-session-running? session)
         (let ((ws-msg (ws-recv session)))
           (if (and ws-msg (message? ws-msg))
             (let ((data (message-data ws-msg))
                   (mtype (message-type ws-msg)))
               (when (and (eq? mtype 'binary)
                          (>= (u8vector-length data) HEADER-SIZE))
                 (let ((amsg (parse-agent-message data)))
                   (handle-message session amsg)))
               (loop))
             (ssm-session-running?-set! session #f))))))))

(def (start-stdin-reader session)
  "Spawn a thread that reads stdin and sends input to the session."
  (spawn
   (lambda ()
     (let loop ()
       (when (ssm-session-running? session)
         (let ((b (read-u8 (current-input-port))))
           (cond
             ((eof-object? b)
              (ssm-session-running?-set! session #f))
             (else
              (send-input session (u8vector b))
              (loop)))))))))

(def (start-resize-poller session)
  "Spawn a thread that polls terminal size and sends resize events."
  (spawn
   (lambda ()
     (let ((last-size (get-terminal-size)))
       (let loop ()
         (when (ssm-session-running? session)
           (thread-sleep! 0.5)
           (let ((new-size (get-terminal-size)))
             (unless (and (= (car new-size) (car last-size))
                          (= (cdr new-size) (cdr last-size)))
               (set! last-size new-size)
               (send-resize session (car new-size) (cdr new-size))))
           (loop)))))))

;; ==================== Public API ====================

(def (ssm-list-instances client)
  "List SSM-managed instances that are online. Returns list of hashes."
  (let* ((filter-online (hash ("Key" "PingStatus") ("Values" (vector "Online"))))
         (resp (describe-instance-information client
                 filters: [filter-online]
                 max-results: 50)))
    (if (hash-table? resp)
      (let ((instances (hash-ref resp "InstanceInformationList" [])))
        (if (list? instances) instances []))
      [])))

(def (ssm-connect client target)
  "Connect to an EC2 instance via SSM Session Manager.
   client: an SSMClient, target: instance ID string."
  (displayln (format "Starting session to ~a..." target))
  (let ((resp (try (start-session client target)
                (catch (Error? e)
                  (fprintf (current-error-port) "Error: ~a\n" (Error-message e))
                  #f))))
    (unless (and resp
                 (hash-table? resp)
                 (hash-get resp "StreamUrl")
                 (hash-get resp "TokenValue"))
      (when (and resp (hash-table? resp))
        (fprintf (current-error-port) "Error: Failed to start SSM session to ~a\n" target))
      (exit 1))

    (let ((stream-url (hash-ref resp "StreamUrl"))
          (token-value (hash-ref resp "TokenValue"))
          (session-id (hash-ref resp "SessionId" "")))
      (displayln (format "Session: ~a" session-id))

      ;; Connect WebSocket
      (let* ((ws (websocket-connect stream-url))
             (session (make-session ws session-id)))
        ;; Authenticate the data channel
        (open-data-channel session token-value)

        ;; Start reader thread (handles handshake and output)
        (start-ws-reader session)

        ;; Wait for handshake to complete
        (let wait-handshake ((attempts 0))
          (cond
            ((ssm-session-terminal-set? session)
             ;; Handshake done, start interactive session
             (displayln "Connected.\n")
             (force-output)
             (set-terminal-raw!)
             (unwind-protect
               (begin
                 (start-stdin-reader session)
                 (start-resize-poller session)
                 ;; Wait for session to end
                 (let loop ()
                   (when (ssm-session-running? session)
                     (thread-sleep! 0.1)
                     (loop))))
               ;; Cleanup
               (begin
                 (restore-terminal!)
                 (displayln "\nSession terminated.")
                 (try (WebSocket-close ws) (catch (e) (void)))
                 (try (terminate-session client session-id)
                      (catch (e) (void))))))
            ((> attempts 100)
             (try (WebSocket-close ws) (catch (e) (void)))
             (error "SSM handshake timeout"))
            ((not (ssm-session-running? session))
             (try (WebSocket-close ws) (catch (e) (void)))
             (error "Session closed during handshake"))
            (else
             (thread-sleep! 0.1)
             (wait-handshake (+ attempts 1)))))))))
