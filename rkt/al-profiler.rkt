#lang racket/base
;; al-profiler.rkt -- collect timing and call count for instrumented
;; functions.  See `define/profiled'
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2016 Alex Harsanyi (AlexHarsanyi@gmail.com)
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

(require math/statistics racket/format racket/math)

(provide define/profiled
         profile-enable
         profile-reset
         profile-enable-all
         profile-reset-all
         profile-display)

(struct profile-data (name
                      (enabled #:mutable)
                      (ncalls #:mutable)
                      (stats #:mutable))
  #:transparent)

;; Hold information about all profiled functions
(define profile-db (make-hash))

(define (make-profile-data name)
  (let ((data (profile-data name #t 0 empty-statistics)))
    (hash-set! profile-db name data)
    data))

;; Enable/Disable data collection for function NAME.  This must be a function
;; that has been defined with `define/profiled'
(define (profile-enable name flag)
  (let ((data (hash-ref profile-db name #f)))
    (when data
      (set-profile-data-enabled! data flag))))

;; Reset collected data for function NAME.  This must be a function that has
;; been defined with `define/profiled'
(define (profile-reset name)
  (let ((data (hash-ref profile-db name #f)))
    (when data
      (set-profile-data-stats! data empty-statistics))))

;; Enable/Disable data collection for all instrumented functions.
(define (profile-enable-all flag)
  (for ([item (in-hash-values profile-db)])
    (set-profile-data-enabled! item flag)))

;; Reset collected data for all instrumented functions.
(define (profile-reset-all)
  (for ([item (in-hash-values profile-db)])
    (set-profile-data-stats! item empty-statistics)))

;; Display collected statistics.
(define (profile-display (port (current-output-port)))

  (define (pval val)
    (if (or (nan? val) (infinite? val))
        (~a #:min-width 11)
        (~r val #:min-width 11 #:precision 2)))
  
  (let ([names (sort
                (for/list ([k (in-hash-keys profile-db)])
                  (symbol->string k))
                string<?)]
        [maxw 0])
    (for ([n names]) (set! maxw (max (string-length n) maxw)))
    (fprintf port "~a  calls       total         min         max        mean       stdev~%"
             (~a " " #:min-width maxw))
    (for ([n names])
      (let* ((data (hash-ref profile-db (string->symbol n) #f))
             (stats (profile-data-stats data)))
        (fprintf port "~a: ~a ~a ~a ~a ~a ~a~%"
                 (~a n #:min-width maxw)
                 (~r (profile-data-ncalls data) #:min-width 5)
                 (pval (* (profile-data-ncalls data)
                          (statistics-mean stats)))
                 (pval (statistics-min stats))
                 (pval (statistics-max stats))
                 (pval (statistics-mean stats))
                 (pval (statistics-stddev stats)))))))

;; Define an instrumented function.  The body of the function will be wrapped
;; around code that collects timing information about the function.  When the
;; function, data about the duration of the body will be collected.  Profiling
;; data for all functions can be displayed using `profile-display'
(define-syntax define/profiled
  (syntax-rules ()
    [(_ (name . args) body ...)
     (define name
       (let ((pdata (make-profile-data (quote name))))
         (lambda args
           (define start (current-inexact-milliseconds))
           (begin0
               (let () body ...)
             (let ((end (current-inexact-milliseconds)))
               (when (profile-data-enabled pdata)
                 (set-profile-data-ncalls!
                  pdata
                  (+ 1 (profile-data-ncalls pdata)))
                 (set-profile-data-stats!
                  pdata
                  (update-statistics (profile-data-stats pdata) (- end start)))))))))]))
