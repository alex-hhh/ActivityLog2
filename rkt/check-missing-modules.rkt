#lang racket/base
;; check-missing-modules.rkt -- check modules are not installed
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2020 Alex Harsányi <AlexHarsanyi@gmail.com>
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
;;
;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.

(require (for-syntax racket/base))

(begin-for-syntax

  ;; Return #t if the module identified by SYM can be loaded using a require
  ;; statement, #f otherwise
  (define (check-module sym)
    (with-handlers
      (((lambda (e) #t) (lambda (e) #f)))
      (and ((current-module-name-resolver) sym #f #f #f) #t)))

  ;; Check for any modules in MODULES which are missing and report them using
  ;; the error function
  (define (check-missing modules)
    (define missing (for/list ([m modules] #:unless (check-module m)) m))

    ;; Note: We could print out the modules which are missing, but users will
    ;; just install them using "raco pkg install", which has the potential to
    ;; create other problems later.  Better to just point them to the readme
    ;; file.
    (unless (null? missing)
      (raise-user-error
       "You must install some packages first, see docs/README.md for details"))))

;; A macro to check for missing modules and report them.  This is used by the
;; application to print out a more meaningful message when a required module
;; is missing.  This is used in run.rkt and build.rkt.
(define-syntax (check-missing-modules stx)
  (syntax-case stx ()
    [(_ mod ...)
     (let ([modsyms (syntax->datum #'(mod ...))])
       #`(quote #,(check-missing modsyms)))]))

(provide check-missing-modules)

