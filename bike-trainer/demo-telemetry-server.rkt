#lang racket/base
(require racket/tcp
         racket/string
         racket/class
         racket/format
         racket/cmdline
         "../etc/al-interactive.rkt"
         "../rkt/sport-charms.rkt"      ; for get-sport-name
         "../rkt/data-frame.rkt")

(define server-port 7500)               ; TCP port for the server

;; Read a CSV file and produce a data-frame%.  This will read CSV files as
;; exported by ActivityLog2, no other promise is made :-)
(define (csv->df file-name)
  (define series-by-name (make-hash))
  (call-with-input-file file-name
    (lambda (in)
      (define header-line (read-line in 'any))
      (define series-names (string-split header-line #px"\\s*,\\s*"))
      (define series-data (for/list ((s series-names)) '()))
      (let loop ((line (read-line in 'any)))
        (unless (eq? line eof)
          (define data (map string->number (string-split line #px"\\s*,\\s*")))
          (set! series-data
                (for/list ((item series-data)
                           (d data))
                  (cons d item)))
          (loop (read-line in 'any))))
      (define series
        (for/list ([name series-names]
                   [data series-data])
          (new data-series% [name name] [data (list->vector (reverse data))])))
      (new data-frame% [series series]))))

;; Send telemetry data from DATA-FRAME to OUTPUT-PORT.  This is sent in real
;; time: a timer is started and inside a loop, the corresponding time is
;; looked up in the "timer" series of the data frame, Heart Rate, Power,
;; Cadence and Speed data is than sent from he corresponding point in the data
;; frame.
(define (send-telemetry data-frame output-port)
  (define start (current-milliseconds))
  ;; We send data from these telemetry series
  (define telemetry
    (for/list ([series '("hr" "pwr" "spd" "cad")]
               #:when (send data-frame contains? series))
      series))
  (format "telemetry: ~a~%" telemetry)

  (let loop ((current (current-milliseconds)))
    ;; NOTE: timer series is in seconds, not milliseconds!
    (let* ((simulation-time (/ (- current start) 1000.0))
           (index (send data-frame get-index "timer" simulation-time)))
      (if index
          (let ()
            (define data (send/apply data-frame ref* index telemetry))
            (define message-parts
              (for/list ((tag telemetry)
                         (val data))
                (format "~a: ~a" (string-upcase tag)
                        (if val (~r val #:precision 4) ""))))
            (define message (string-append "TELEMETRY " (string-join message-parts ";") "\n"))
            ;; (printf message)
            (flush-output (current-output-port))
            (write-string message output-port)
            (flush-output output-port)  ; does it do anything?
            (sleep 0.1)                 ; don't send it too fast
            (loop (current-milliseconds)))
          (close-output-port output-port))))

  (define-values
    (local-host local-port remote-host remote-port)
    (tcp-addresses output-port #t))
  (printf "Send telemetry to ~a:~a complete~%" remote-host remote-port))

;; Start a server, accepting connections on PORT and sending telemetry data to
;; each client from DATA-FRAME
(define (start-server data-frame)
  ;; NOTE: too lazy to check for empty data frame, will crash out anyway...
  (define max-index (sub1 (send data-frame get-row-count)))
  (define max-time (send data-frame ref max-index "timer"))
  (printf "Data frame contains ~a seconds of data~%" max-time)
  (printf "Started server on port ~a, waiting for connections~%" server-port)
  (flush-output (current-output-port))
  (define listener (tcp-listen server-port))
  (let loop ()
    (let-values (((in out) (tcp-accept listener)))
      (define-values
        (local-host local-port remote-host remote-port)
        (tcp-addresses out #t))
      (printf "Accepted connection from ~a:~a~%" remote-host remote-port)
      (flush-output (current-output-port))
      ;; Receive and discard commands from the training application, we're not
      ;; controlling anything.
      (thread (lambda ()
                (let loop ((line #f))
                  (unless (eq? line eof)
                    (loop (read-line in 'any))))))
      (thread (lambda () (send-telemetry data-frame out))))

    (loop)))

(define (get-session-headline sid)
  (let ((row (query-row (current-database) "
select ifnull(S.name, 'unnamed'), S.sport_id, S.sub_sport_id
  from A_SESSION S
 where S.id = ?" sid)))
    (let ((name (vector-ref row 0))
          (sport (vector-ref row 1))
          (sub-sport (vector-ref row 2)))
    (format "~a (~a)" name
            (get-sport-name (if (sql-null? sport) #f sport)
                            (if (sql-null? sub-sport) #f sub-sport))))))

;; Read a data frame from the database session SID and use it to send
;; telemetry data.
(define (start-server/session-id sid)
  (define headline (get-session-headline sid))
  (printf "Reading data frame from SID ~a, \"~a\"~%" sid headline)
  (define data-frame (sid->df sid))
  (start-server data-frame))

;; Read a data frame from the CSV file and use it to send telemetry data.
(define (start-server/csv csv-file)
  (printf "Reading data frame from CSV file ~a~%" csv-file)
  (define data-frame (csv->df csv-file))
  ;; csv->df does not mark series as sorted, we need this one to look up timer
  ;; values.
  (send (send data-frame get-series "timer") set-sorted #t)
  (start-server data-frame))

(module+ main
  (define sid (make-parameter #f))
  (define csv (make-parameter #f))
  (command-line
   #:program "demo-telemetry-server"
   #:once-any
   (("-s" "--session-id") session-id
    "Send telemetry from the database session <session-id>"
    (sid (string->number session-id)))
   (("-f" "--file") file-name
     "Send telemetry from the CSV <file-name>"
     (csv file-name)))
  (cond ((sid)
         (unless (current-database)
           (error "No database. Open a database in ActivityLog2 first"))
         (start-server/session-id (sid)))
        ((csv)
         (start-server/csv (csv)))
        (#t
         (error "Must specify either a database session id or a CSV file (try -h)"))))

