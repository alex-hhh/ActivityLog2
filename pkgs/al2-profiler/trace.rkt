#lang racket/base
;; al2-profiler.rkt -- profiling and tracing capabilities
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2016, 2020 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require (for-syntax racket/base racket/list)
         racket/format
         racket/string)

(provide
 define/trace

 define/public/trace
 define/augment/trace
 define/private/trace
 define/override/trace
 define/augride/trace

 trace-output)

;; Current indent level for tracing
(define indent-level (make-parameter 0))

;; Number of spaces by which we increase the indent for each nested function
;; call.
(define indent-amount 2)

;; A pre-prepared padding string for the indent
(define indent-padding (make-parameter ""))

;; Port where tracing output is sent (if #f, it is sent to
;; current-output-port).
(define trace-output (make-parameter #f))

;; Define a traced function.  When the function is invoked, its name and the
;; value of its parameters are printed to TRACE-OUTPUT and when returning the
;; results are printed out.
(define-syntax (define/trace stx)
  (syntax-case stx ()
    [(_ (name . args) body ...)
     (with-syntax
       ([arg-vals
         (datum->syntax
          stx
          (let ([e (syntax-e #'args)])
            (cond
              [(symbol? e)
               ;; (define (foo . args) ...) case
               `(cons (~s (quote ,(syntax->datum #'name))) (map ~s ,e))]
              [(list? e)
               ;; (define (a b #:keyword k) ...) case
               `(list
                 (~s (quote ,(syntax->datum #'name)))
                 ,@(for/list ([item (in-list e)])
                     (if (keyword? (syntax-e item))
                         (let ([kw (string-append "#:" (keyword->string (syntax-e item)))])
                           `(~a ,kw))
                         `(~s ,item))))]
              [#t
               ;; (define (a b #:keyword k . rest) ...) case
               (define-values (a r)
                 (let loop ([e e]
                            [result '()])
                   (if (cons? e)
                       (loop (cdr e) (cons (car e) result))
                       (values (reverse result) e))))
               `(append
                 (list
                  (~s (quote ,(syntax->datum #'name)))
                  ,@(for/list ([item (in-list a)])
                      (if (keyword? (syntax-e item))
                          (let ([kw (string-append "#:" (keyword->string (syntax-e item)))])
                            `(~a ,kw))
                          `(~s ,item))))
                 (map ~a ,r))])))])

       #`(define (name . args)
           (let ((out (or (trace-output) (current-output-port)))
                 (text (string-append
                        "T" (~a #:width 3 (indent-level) #:align 'right #:pad-string "0") ":"
                        (indent-padding)
                        (string-join arg-vals #:before-first "(" #:after-last ")")
                        )))
             (display text out)
             (newline out))
           (call-with-values
            (lambda ()
              (parameterize* ((indent-level (+ (indent-level) indent-amount))
                              (indent-padding (make-string (indent-level) #\ )))
                body ...))
            (lambda result
              (let* ((out (or (trace-output) (current-output-port)))
                     (sresult (map ~a result))
                     (text (string-append
                            "R" (~a #:width 3 (indent-level) #:align 'right #:pad-string "0") ":"
                            (indent-padding)
                            (string-join sresult #:before-first "(values " #:after-last ")"))))
                (display text out)
                (newline out))
              (apply values result)))))]))

;; Same as define/trace, but for different class methods

(define-syntax (define/public/trace stx)
  (syntax-case stx ()
    [(_ (name . args) body ...)
     (with-syntax ([name+args (datum->syntax stx (list* #'name #'args))])
       #'(begin
           (public name)
           (define/trace name+args body ...)))]))

(define-syntax (define/augment/trace stx)
  (syntax-case stx ()
    [(_ (name . args) body ...)
     (with-syntax ([name+args (datum->syntax stx (list* #'name #'args))])
       #'(begin
           (augment name)
           (define/trace name+args body ...)))]))

(define-syntax (define/private/trace stx)
  (syntax-case stx ()
    [(_ (name . args) body ...)
     (with-syntax ([name+args (datum->syntax stx (list* #'name #'args))])
       #'(begin
           (private name)
           (define/trace name+args body ...)))]))

(define-syntax (define/override/trace stx)
  (syntax-case stx ()
    [(_ (name . args) body ...)
     (with-syntax ([name+args (datum->syntax stx (list* #'name #'args))])
       #'(begin
           (override name)
           (define/trace name+args body ...)))]))

(define-syntax (define/augride/trace stx)
  (syntax-case stx ()
    [(_ (name . args) body ...)
     (with-syntax ([name+args (datum->syntax stx (list* #'name #'args))])
       #'(begin
           (augride name)
           (define/trace name+args body ...)))]))
