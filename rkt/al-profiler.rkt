#lang racket/base
;; al-profiler.rkt -- profiling and tracing capabilities
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

;; This is a helped module used to debug this application.  It contains
;; facilities for profiling and tracing functions.
;;
;; For profiling functions, replace `define` with `define/profile` and timing
;; data will be collected for the function.  This also works for class method
;; names, use 'define/public/profile' or 'define/augment/profile' instead.
;; Timing data for all functions can be displayed with `profile-display`.
;; There are also functions for individually enabling/disabling profiling and
;; resetting the counters.
;;
;; For tracing functions, replace `define` with `define/trace` and the
;; function calls with their arguments will be printed out.

(require (for-syntax racket/base
                     racket/syntax)
         math/statistics
         racket/class
         racket/format
         racket/math
         racket/string)

(provide define/profile
         define/public/profile
         define/augment/profile

         profile-enable
         profile-reset
         profile-enable-all
         profile-reset-all
         profile-display

         define/trace
         define/public/trace
         define/augment/trace

         trace-output)

;; Holds timing information about a profiled function.
(struct profile-data (name
                      (enabled #:mutable)
                      (ncalls #:mutable)
                      (stats #:mutable))
  #:transparent)

;; Hold information about all profiled functions.  Maps a name to a
;; `profile-data` instance.
(define profile-db (make-hash))

(define (make-profile-data name)
  (let ((data (profile-data name #t 0 empty-statistics)))
    (hash-set! profile-db name data)
    data))

;; Enable/Disable data collection for function NAME.  This must be a function
;; that has been defined with `define/profile'
(define (profile-enable name flag)
  (let ((data (hash-ref profile-db name #f)))
    (when data
      (set-profile-data-enabled! data flag))))

;; Reset collected data for function NAME.  This must be a function that has
;; been defined with `define/profile'
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
;; around code that collects timing information about the function: when the
;; function is called, data about the duration of the body will be collected.
;; Profiling data for all functions can be displayed using `profile-display'
(define-syntax define/profile
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
                  (add1 (profile-data-ncalls pdata)))
                 (set-profile-data-stats!
                  pdata
                  (update-statistics (profile-data-stats pdata) (- end start)))))))))]))

;; Same as define/profile, but for public methods of a class.
;;
;; LIMITATION: the class name is not recorded and the function shows up in the
;; profile output with the "-profiled" string appended.
(define-syntax (define/public/profile stx)
  (syntax-case stx ()
    [(_ (name . args) body ...)
     (with-syntax*
         ([fname (format-id stx "~a-profiled" (syntax->datum #'name))]
          [fname+args (datum->syntax stx (list* #'fname #'args))]
          [name+args (datum->syntax stx (list* #'name #'args))])
       #'(begin
           (define/profile fname+args body ...)
           (public name)
           (define name+args fname+args)))]))

;; Same as define/public/profile but for augmented methods of a class.  Same
;; limitations too.
(define-syntax (define/augment/profile stx)
  (syntax-case stx ()
    [(_ (name . args) body ...)
     (with-syntax*
         ([fname (format-id stx "~a-profiled" (syntax->datum #'name))]
          [fname+args (datum->syntax stx (list* #'fname #'args))]
          [name+args (datum->syntax stx (list* #'name #'args))])
       #'(begin
           (define/profile fname+args body ...)
           (augment name)
           (define name+args fname+args)))]))


;;.............................................................. tracing ....

;; Current indent level for tracing
(define indent-level (make-parameter 0))

;; Number of spaces by which we increase the indent for each nested function
;; call.
(define indent-amount 2)

;; A pre-prepared padding string for the indent
(define indent-padding (make-parameter ""))

;; Port where tracing output is sent (if #f, it is sent to current-output-port
(define trace-output (make-parameter #f))

;; Define a traced function.  When the function is invoked, its name and the
;; value of its parameters are printed to TRACE-OUTPUT
;;
;; Limitations: rest and keyword arguments are not supported.
(define-syntax (define/trace stx)
  (syntax-case stx ()
    [(_ (name . args) body ...)
     (with-syntax
       ([arg-vals (datum->syntax
                   stx
                   `(list
                     (~s (quote ,(syntax->datum #'name)))
                     ,@(for/list ((item (syntax-e #'args))) `(~s ,item))))])
       #`(define (name . args)
           (let ((out (or (trace-output) (current-output-port)))
                 (text (string-append
                        "T" (~a #:width 3 (indent-level) #:align 'right #:pad-string "0") ":"
                        (indent-padding)
                        (string-join arg-vals #:before-first "(" #:after-last ")")
                        )))
             (display text out)
             (newline out))
           (parameterize* ((indent-level (+ (indent-level) indent-amount))
                           (indent-padding (make-string (indent-level) #\ )))
             body ...)))]))

;; Same as define/trace, but for public methods of a class
(define-syntax (define/public/trace stx)
  (syntax-case stx ()
    [(_ (name . args) body ...)
     (with-syntax ([name+args (datum->syntax stx (list* #'name #'args))])
       #'(begin
           (public name)
           (define/trace name+args body ...)))]))

;; Same as define/trace, but for augmented methods of a class
(define-syntax (define/augment/trace stx)
  (syntax-case stx ()
    [(_ (name . args) body ...)
     (with-syntax ([name+args (datum->syntax stx (list* #'name #'args))])
       #'(begin
           (augment name)
           (define/trace name+args body ...)))]))
