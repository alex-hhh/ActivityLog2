#lang racket/base
;; dbglog.rkt -- debug logging facilities
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

(require racket/format
         racket/math
         errortrace/errortrace-lib
         racket/port
         "al-prefs.rkt")

(provide dbglog
         dbglog-exception
         thread/dbglog)

(define log-port #f)                    ; port to which all log messages go

;; Open the log file if needed.  We use a single log file in append mode, we
;; don't expect the file to grow too much so we don't recyle it.  If it
;; becomes a problem, we can create a new file for each new invokation (or
;; some other strategy).
(define (maybe-init-log-port)
  (unless log-port
    (let ((fname (build-path (al-get-pref-dir) "ActivityLogDbg.log")))
      (set! log-port (open-output-file fname #:mode 'text #:exists 'append)))))

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
  (maybe-init-log-port)
  (write-string (get-current-timestamp) log-port)
  (write-string " " log-port)
  (write-string msg log-port)
  (write-string "\n" log-port)
  (flush-output log-port))

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
