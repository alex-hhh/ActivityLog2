#lang racket/base
;; utilities.rkt -- various utilities
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015 Alex Harsanyi (AlexHarsanyi@gmail.com)
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

(require racket/contract)

(provide/contract

 (app-version (-> string?))
 (app-commit-id (-> string?))
 (app-build-timestamp (-> string?))
 
 (data-directory (-> path-string?))
 (preferences-file (-> path-string?))
 (put-pref (-> symbol? any/c any/c))
 (get-pref (-> symbol? any/c any/c))

 (set-dbglog-to-standard-output (-> boolean? any/c))
 (dbglog (->* (string?) () #:rest (listof any/c) any/c))
 (ignore-errors (->* ((-> any/c)) (#:name string?) any/c))
 (dbglog-exception (-> string? any/c any/c))
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

 (notify-user (->* (symbol? string?) () #:rest (listof any/c) any/c)))

(provide user-notification-logger)

(require errortrace/errortrace-lib
         racket/async-channel
         racket/file
         racket/format
         racket/math
         racket/port
         racket/match
         racket/list

         (for-syntax
          racket/base
          racket/file
          racket/format
          racket/string
          racket/runtime-path))

(begin-for-syntax
  (define-runtime-path version-id-file "../version.txt"))

;; Evaluate to a string containing the version as stored in the ../version.txt
;; file.
(define-syntax (embedded-version stx)
  (syntax-case stx ()
    [_ #`(quote #,(with-handlers
                    (((lambda (x) #t)
                      (lambda (x)
                        (printf "*** (embedded-version): ~a~%" (exn-message x))
                        "no version")))
                    ;; NOTE: version.txt is loaded from the current directory
                    ;; of the compilation, which is the toplevel directory.
                    (let ((version (string-trim (file->string version-id-file #:mode 'text))))
                      version)))]))

(begin-for-syntax
  (define-runtime-path build-id-file "../build-id.txt"))

;; Evaluate to a string containing the commit id as stored in the
;; ../build-id.txt file.  This file is created by the build process (see
;; build.rkt)
(define-syntax (embedded-commit-id stx)
  (syntax-case stx ()
    [_ #`(quote #,(with-handlers
                    (((lambda (x) #t)
                      (lambda (x)
                        ;; Silently ignore errors.  This file will be missing
                        ;; if build.rkt was not run first.
                        "no commit id")))
                    ;; NOTE: build-id.txt is loaded from the current directory
                    ;; of the compilation, which is the toplevel directory.
                    (string-trim (file->string build-id-file #:mode 'text))))]))

;; Evaluate to a string containing the current timestamp (at the time the
;; macro is evaluated.
(define-syntax (embedded-timestamp stx)
  (syntax-case stx ()
    [_ #`(quote #,(let ((ts (seconds->date (current-seconds))))
                    (string-append
                     (~a (date-year ts))
                     "/"
                     (~a (date-month ts) #:width 2 #:left-pad-string "0" #:align 'right)
                     "/"
                     (~a (date-month ts) #:width 2 #:left-pad-string "0" #:align 'right)
                     " "
                     (~a (date-hour ts) #:width 2 #:left-pad-string "0" #:align 'right)
                     ":"
                     (~a (date-minute ts) #:width 2 #:left-pad-string "0" #:align 'right)
                     ":"
                     (~a (date-second ts) #:width 2 #:left-pad-string "0" #:align 'right))))]))

(define (app-version) (embedded-version))
(define (app-commit-id) (embedded-commit-id))
(define (app-build-timestamp) (embedded-timestamp))

;; Return the default place where data files are stored on this system.
(define (get-pref-dir)
  (if (eq? 'windows (system-type 'os))
      (let ([pref-dir (getenv "LOCALAPPDATA")])
        (if pref-dir
            (string->path pref-dir)
            (find-system-path 'pref-dir)))
      (find-system-path 'pref-dir)))

(define the-data-directory #f)

;; Return the default directory where the application will store its data
;; files.  This directory will be created if it does not exist.
(define (data-directory)
  (unless the-data-directory
    (let ((dir (get-pref-dir)))
      ;; dir might not exist, but make-directory* never fails
      (let ((pref-dir (build-path dir "ActivityLog")))
        (make-directory* pref-dir)
        (set! the-data-directory pref-dir))))
  the-data-directory)

(define the-preferences-file #f)

;; Return the name of the file used to store preferences.
(define (preferences-file)
  (unless the-preferences-file
    (set! the-preferences-file
          (build-path (data-directory) "ActivityLogPrefs.rktd")))
  the-preferences-file)

;; Store VALUE under NAME in the preferences file
(define (put-pref name value)
  (put-preferences (list name)
                   (list value) 
                   (lambda (p) (error 'lock-fail "Failed to get the pref file lock" p))
                   (preferences-file)))

;; Retrieve the value for NAME from the preferences file, or return the value
;; of FAIL-THUNK if it does not exist.
(define (get-pref name fail-thunk)
  (get-preference name fail-thunk 'timestamp (preferences-file)))


(define the-log-port #f)                    ; port to which all log messages go
(define log-to-standard-output #f)          ; when #t dbglog also prints to stdout

(define (set-dbglog-to-standard-output flag)
  (set! log-to-standard-output flag)
  ;; Start counting lines on the current output port, so we know when to open
  ;; a fresh line in `dbglog`.
  (when log-to-standard-output
    (unless (port-counts-lines? (current-output-port))
      (port-count-lines! (current-output-port)))))

;; Open the log file if needed.  We use a single log file in append mode, we
;; don't expect the file to grow too much so we don't recyle it.  If it
;; becomes a problem, we can create a new file for each new invokation (or
;; some other strategy).
(define (maybe-init-the-log-port)
  (unless the-log-port
    (let ((fname (build-path (data-directory) "ActivityLogDbg.log")))
      (set! the-log-port (open-output-file fname #:mode 'text #:exists 'append)))))

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

;; Write MSG to the log file.  A timestamp is prepended and a newline is
;; appended.  The log port is flushed immediately, so it is not particularily
;; efficient to log many things...
(define (dbglog format-string . args)
  (define msg (apply format format-string args))
  (define ts (get-current-timestamp))
  (maybe-init-the-log-port)
  (define (do-log port)
    (write-string (get-current-timestamp) port)
    (write-string " " port)
    (write-string msg port)
    (write-string "\n" port)
    (flush-output port))
  (do-log the-log-port)
  (when log-to-standard-output
    (let ((out (current-output-port)))
      ;; Turn on line counting (if not already on) and write a new line before
      ;; the log message, if needed -- this ensures all log messages are on
      ;; lines of their own...  We also do this here, in case
      ;; `current-output-port` has changed since
      ;; `set-dbglog-to-standard-output` was called.
      (unless (port-counts-lines? out)
        (port-count-lines! out))
      (define-values (line column position) (port-next-location out))
      (when (and column (not (zero? column)))
        (write-string "\n" out))
      (do-log out))))

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
(define user-notification-logger (make-logger 'user-notification (current-logger)))

;; Log a message with the intent of displaying it to the user.  The
;; application will create a log sink for this logger that displays messages
;; in a notification banner.
(define (notify-user level format-string . args)
  (define msg (apply format format-string args))
  (log-message user-notification-logger level #f msg #f #f))

