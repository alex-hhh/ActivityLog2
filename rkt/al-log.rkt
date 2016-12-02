#lang racket/base
;; al-log.rkt -- logging facilities
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

(require pict
         racket/class
         racket/gui/base
         racket/string
         racket/math
         racket/gui
         "widgets.rkt"
         "dbglog.rkt")

(provide log-al-fatal
         log-al-error
         log-al-warning
         log-al-info
         log-al-debug)
(provide make-log-output-window)
(provide make-dbglog-sink)

(define-logger al)
(define al-prefix "al: ")

(define (make-log-output-window parent)

  (define banner (new notification-banner% [parent parent]))
  
  (define (insert-log-messages source)
    (let* ((m (sync source))
           (msg (string-trim (vector-ref m 1) al-prefix #:left? #t #:right? #f)))
      (queue-callback
       (lambda () (send banner notify msg)))
      (insert-log-messages source)))
  (thread (lambda () (insert-log-messages (make-log-receiver al-logger 'info))))
  (void))

(define (make-dbglog-sink)
  (define (write-file source)
    (let* ((log-item (sync source))
           (level (vector-ref log-item 0))
           (message (vector-ref log-item 1))
           (msg (string-trim message al-prefix #:left? #t #:right? #f)))
      (dbglog (format "~a: ~a" level msg)))
    (write-file source))
  (thread (lambda () (write-file (make-log-receiver al-logger 'info)))))
