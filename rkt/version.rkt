#lang racket/base
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2017 Alex Harsanyi (AlexHarsanyi@gmail.com)
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

(require (for-syntax racket/base
                     racket/date
                     racket/file
                     racket/format
                     racket/string))

(provide app-version
         app-commit-id
         app-build-timestamp)

;; This file contains macros which compute at compile time the application
;; version, commit id and build date. They are available as functions
;; `app-version', `app-commit-id' and `app-build-timestamp'.


;; Evaluate to a string containing the version as stored in the ../version.txt
;; file.
(define-syntax (embedded-version stx)
  (syntax-case stx ()
    [_ #`(quote #,(with-handlers
                    (((lambda (x) #t)
                      (lambda (x)
                        (printf "*** version.rkt(embedded-version): ~a~%" (exn-message x))
                        "no version")))
                    ;; NOTE: version.txt is loaded from the current directory
                    ;; of the compilation, which is the toplevel directory.
                    (let ((version (string-trim (file->string "./version.txt" #:mode 'text))))
                      version)))]))

;; Evaluate to a string containing the commit id as stored in the
;; ../build-id.txt file.  This file is created by the build process (see
;; build.rkt)
(define-syntax (embedded-commit-id stx)
  (syntax-case stx ()
    [_ #`(quote #,(with-handlers
                    (((lambda (x) #t)
                      (lambda (x)
                        (printf "*** version.rkt(embedded-commit-id): ~a~%" (exn-message x))
                        "no commit id")))
                    ;; NOTE: build-id.txt is loaded from the current directory
                    ;; of the compilation, which is the toplevel directory.
                    (string-trim (file->string "./build-id.txt" #:mode 'text))))]))

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
