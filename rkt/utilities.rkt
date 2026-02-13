#lang racket/base
;; utilities.rkt -- various utilities
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015, 2019-2021, 2023, 2025 Alex Harsányi <AlexHarsanyi@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

(require errortrace/errortrace-lib
         racket/async-channel
         racket/contract
         racket/format
         racket/list
         racket/match
         racket/math
         racket/port
         the-application)

(provide/contract

 ;; Provided from the-application package
 (data-directory (-> path-string?))
 (preferences-file (-> path-string?))
 (put-pref (-> symbol? any/c any/c))
 (get-pref (-> symbol? any/c any/c))

 (set-dbglog-to-standard-output (-> boolean? any/c))
 (dbglog (->* (any/c) () #:rest (listof any/c) any/c))
 (ignore-errors (->* ((-> any/c)) (#:name string?) any/c))
 (dbglog-exception (-> any/c any/c any/c))
 (thread/dbglog (->* ((-> any/c))
                     (#:name string?
                      #:log-start boolean?
                      #:log-finish boolean?)
                     any/c))

 (set-worker-thread-count (-> (and/c integer? positive?) any/c))
 (queue-task (-> string? procedure? any/c))
 (shutdown-workers (-> any/c))

 (log-event (-> symbol? any/c any/c))
 (make-log-event-source (-> async-channel?))
 (collect-events (-> async-channel? (hash/c symbol? (listof any/c))))
 (shutdown-event-sink-listener-threads (-> any/c))

 (notify-user (->* (symbol? string?) (#:tag (or/c #f symbol?))
                   #:rest (listof any/c) any/c))
 (retract-user-notification (-> symbol? any/c)))

(provide user-notification-logger
         progress-callback/c)

;; Contract for the progress callback passed to db-open
(define progress-callback/c
  (-> string? exact-positive-integer? exact-positive-integer? any/c))

(define log-to-standard-output #f)          ; when #t dbglog also prints to stdout

(define (set-dbglog-to-standard-output flag)
  (set! log-to-standard-output flag)
  ;; Start counting lines on the current output port, so we know when to open
  ;; a fresh line in `dbglog`.
  (when log-to-standard-output
    (unless (port-counts-lines? (current-output-port))
      (port-count-lines! (current-output-port)))))

;; Return the current timestamp as a string.  Includes milliseconds.  It is
;; used to put timestamps in the log messages.
(define (get-current-timestamp)

  (define (fmt val width)
    (~a val #:width width #:align 'right #:pad-string "0"))

  (let ((ts (exact-round (current-inexact-milliseconds))))
    (let-values (([q r] (quotient/remainder ts 1000)))
      (let ((date (seconds->date q)))
        (string-append
         (fmt (date-year date) 4)
         "-"
         (fmt (date-month date) 2)
         "-"
         (fmt (date-day date) 2)
         " "
         (fmt (date-hour date) 2)
         ":"
         (fmt (date-minute date) 2)
         ":"
         (fmt (date-second date) 2)
         "."
         (fmt r 3))))))

;; Internal logger that dbglog sends messages to (ensures logging works
;; correctly when invoked from multiple threads.
(define al2-logger (make-logger 'al2-logger #f))

;; Write a timestamped message to the log file -- the message is constructed
;; by applying `format` to the "FORMAT-STRING" and "ARGS".  Log messages are
;; constructed than sent to `al2-logger` for actual logging, and, when
;; `log-to-standard-output` is #t, they are also printed to the current output
;; port.
(define (dbglog format-string . args)

  (define msg (apply format format-string args))
  (log-message al2-logger 'info 'al2-logger msg #f #f)

  (when log-to-standard-output
    (let ((out (current-output-port)))
      ;; Turn on line counting (if not already on) and write a new line before
      ;; the log message, if needed -- this ensures all log messages are on
      ;; lines of their own...  We also do this here, in case
      ;; `current-output-port` has changed since
      ;; `set-dbglog-to-standard-output` was called.
      (unless (port-counts-lines? out)
        (port-count-lines! out))
      (define-values (_line column _position) (port-next-location out))
      (when (and column (not (zero? column)))
        (write-string "\n" out))
      (write-string (get-current-timestamp) out)
      (write-string " " out)
      (write-string msg out)
      (write-string "\n" out)
      (flush-output out))))

;; The actual logging happens here

(let (;; This is the output port we log messages to.  This file will be closed
      ;; when the current custodian shuts down the application.
      [output (let ([fname (build-path (data-directory) "ActivityLogDbg.log")])
                (open-output-file fname #:mode 'text #:exists 'append))]

      ;; Keep track of the last message printed.  If the same message is
      ;; printed again, we just append a counter to the current log -- this
      ;; reduces spamming.  The timestamp is for the first logged message
      [last-message-semaphore (make-semaphore 1)]
      [last-message #f]
      [last-message-count 0])

  ;; Register a flush callback with the current plumber, so we print the last
  ;; counter (if applicable) and flush our log
  (plumber-add-flush!
   (current-plumber)
   (lambda (_handle)
     (call-with-semaphore
      last-message-semaphore
      (lambda ()
        (when (> last-message-count 1)
          (write-string (format " (~a times)" last-message-count) output))
        (write-string "\n" output)
        (flush-output output)
        (set! last-message #f)
        (set! last-message-count 0)))))

  ;; In a separate thread, receive messages from the al2-logger and write them
  ;; out to the output port, keeping track of duplicate messages.
  (thread
   (lambda ()
     (let ([receiver (make-log-receiver al2-logger 'info)])
       (let loop ([item (sync receiver)])
         (define message (vector-ref item 1))
         (call-with-semaphore
          last-message-semaphore
          (lambda ()
            (if (equal? message last-message)
                (set! last-message-count (add1 last-message-count))
                (begin
                  (when (> last-message-count 1)
                    (write-string (format " (~a times)" last-message-count) output))
                  (unless (equal? last-message #f)
                    ;; Only write a new line if we wrote out a message, this
                    ;; prevents an empty line each time we start logging.
                    (write-string "\n" output))
                  (write-string (get-current-timestamp) output)
                  (write-string " " output)
                  (write-string message output)
                  ;; Flushing the output slows things down, but we don't log
                  ;; that much and it's good to see log results immediately.
                  (flush-output output)
                  (set! last-message message)
                  (set! last-message-count 1)))))
         (loop (sync receiver))))))

  ;; Return void so we don't print the thread id to stdout
  (void))

;; Log an exception, WHO is prepended to the log message, can be the function
;; name that calls `dbglog-exception'
(define (dbglog-exception who e)
  ;; NOTE: 'print-error-trace' will only print a stack trace if the error
  ;; trace library is used.  To use it, remove all .zo files and run "racket
  ;; -l errortrace -t run.rkt"
  (let ((message (if (exn? e) (exn-message e) e))
        (call-stack (if (exn? e)
                        (call-with-output-string
                         (lambda (o) (print-error-trace o e)))
                        "#<no call stack>")))
    (dbglog "~a: ~a ~a" who message call-stack)))

;; Run THUNK, catching all exceptions and logging them.
(define (ignore-errors thunk #:name (name "*unnamed*"))
  (with-handlers
    (((lambda (e) #t)
      (lambda (e) (dbglog (format "thunk <~a>: ~a" name e)))))
    (thunk)))

;; Wrapper around `thread', log a message if THUNK throws an exception and
;; optionally log messages when the thread starts and finishes.
(define (thread/dbglog thunk
                       #:name [name "*unnamed*"]
                       #:log-start [log-start #f]
                       #:log-finish [log-finish #f])
  (thread
   (lambda ()
     (with-handlers
       (((lambda (e) #t)
         (lambda (e) (dbglog (format "thread <~a>: ~a" name e)))))
       (when log-start (dbglog (format "thread <~a> started" name)))
       (thunk)
       (when log-finish (dbglog (format "thread <~a> completed" name)))))))

;; This is a task sent to a worker thread.  It has a name so any un-caught
;; exceptions can be reported.
(struct task (name thunk))

;; Request channel on which tasks are sent to worker threads by QUEUE-TASK.
(define the-request-channel #f)

;; List of worker threads.  We keep them in a list, so they can be terminated
;; by SHUTDOWN-WORKERS.
(define the-workers '())

(define num-worker-threads 5)

;; Set the number of worker threads to use in the application.  The default
;; should be fine, but the tests set this to 1 to be able to check for a few
;; things.
(define (set-worker-thread-count c)
  (unless (= c num-worker-threads)
    (shutdown-workers)
    (set! num-worker-threads c)
    (maybe-init-workers)))

;; Create a worker thread to execute tasks.  Uncaught exceptions from the
;; tasks are logged using dbglog facilities
(define (make-worker-thread id request-channel)
  (thread/dbglog
   #:name (format "worker-~a" id) #:log-start #t #:log-finish #t
   (lambda ()
     (let loop ((task (async-channel-get request-channel)))
       (when (task? task)               ; anything else stops the thread
         (with-handlers
           (((lambda (e) #t)
             (lambda (e) (dbglog-exception (task-name task) e))))
           ((task-thunk task)))
         (loop (async-channel-get request-channel)))))))

;; Create the worker threads and the request channel (if they are not already
;; created).
(define (maybe-init-workers)
  (unless the-request-channel
    (set! the-request-channel (make-async-channel #f))
    ;; NOTE: since racket threads don't really execute in parallel, it is not
    ;; worth starting too many of them.  Theis main purpose is to free up the
    ;; GUI thread from long operations.
    (set! the-workers
          (for/list ([id (in-range num-worker-threads)])
            (make-worker-thread id the-request-channel)))))

;; Queue a task to one of the worker threads. NOTE: since there are multiple
;; threads, tasks might not be executed in the same order they are queued.
(define (queue-task name thunk)
  (maybe-init-workers)
  (async-channel-put the-request-channel (task name thunk)))

;; called to terminate the worker threads (when the application exits)
(define (shutdown-workers)
  (when the-request-channel
    (for ((worker the-workers))
      (async-channel-put the-request-channel #f))
    (for ((worker the-workers))
      (sync/timeout 0.1 (thread-dead-evt worker)))
    (set! the-request-channel #f)
    (set! the-workers '())))

;; List of async channels on which we send events.  We keep the sync channels
;; in weak boxes, so they can be garbage collected.
(define the-event-sinks '())

;; Send an event with TAG and DATA to all the registered channels.
(define (log-event tag data)
  (define have-gced-sinks? #f)
  (for ([sink-box the-event-sinks])
    (let ((sink (weak-box-value sink-box)))
      (if sink
          (async-channel-put sink (list tag data))
          (set! have-gced-sinks? #t))))
  ;; Clean up any garbage collected sinks
  (when have-gced-sinks?
    (set! the-event-sinks
          (for/list ([box the-event-sinks] #:unless (weak-box-value box)) box))))

;; Send a #f value on all our sinks, any threads that listen on them will shut
;; down (because that's how these threads should be written).
(define (shutdown-event-sink-listener-threads)
  (for ([sink-box the-event-sinks])
    (let ((sink (weak-box-value sink-box)))
      (when sink (async-channel-put sink #f)))))

;; Create a source of log-events.  This is an async channel that will receive
;; events logged with `log-event`
(define (make-log-event-source)
  (let* ((sink (make-async-channel #f))
         (sink-box (make-weak-box sink)))
    (set! the-event-sinks (cons sink-box the-event-sinks))
    sink))

;; Collect all events from SOURCE, an async channel.  Returns a hash mapping
;; an event tag to a list of data values for that tag.  Duplicate data items
;; (according to `equal?`) are also removed from the list.
(define (collect-events source)
  (let ((result (make-hash)))
    (let loop ((item (async-channel-try-get source)))
      (when item
        (match-define (list tag data) item)
        (hash-update! result tag
                      (lambda (v) (cons data v))
                      '())
        (loop (async-channel-try-get source))))
    (for/hash (((k v) (in-hash result)))
      (values k (remove-duplicates v)))))

;; A logger where user notifications can be sent, for example by
;; `notify-user`.  The application will need to create a receiver for this
;; logger and do something with the messages (like display them on the screen)
(define user-notification-logger (make-logger 'user-notification #f))

;; Log a message with the intent of displaying it to the user.  The
;; application will create a log sink for this logger that displays messages
;; in a notification banner.
(define (notify-user level format-string #:tag (tag #f) . args)
  (define msg (apply format format-string args))
  (log-message user-notification-logger level #f msg tag #f))

;; Retract a user notification -- this mechanism is used remove notifications
;; from the GUI, but nothing will be logged.
(define (retract-user-notification tag)
  (log-message user-notification-logger 'info #f "" tag #f))

