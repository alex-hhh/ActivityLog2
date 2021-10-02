#lang racket/base

;; app-info.rkt -- application version and related stuff
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2020, 2021 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require (for-syntax racket/base
                     racket/file
                     racket/format
                     racket/runtime-path
                     racket/string)
         racket/contract)

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
                    (let ((version (string-trim (file->string version-id-file #:mode 'text)))
                          ;; Azure Builds store a unique incrementing build id
                          ;; in the BUILD_BUILDID environment variable.
                          ;; Append it to the version number if we have it.
                          ;; We use `string->number` so we won't be tricked by
                          ;; silly environment variable values...
                          (build-id (getenv "BUILD_BUILDID")))
                      (if (and build-id (string->number build-id))
                          (string-append version " build #" build-id)
                          version))))]))

(define-syntax (embedded-build-number stx)
  (syntax-case stx ()
    [_ #`(quote #,(with-handlers
                    (((lambda (x) #t)
                      (lambda (x)
                        (printf "*** (embedded-version): ~a~%" (exn-message x))
                        "no version")))
                    ;; Azure build number
                    (getenv "BUILD_BUILDNUMBER")))]))

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
(define (app-build-number) (embedded-build-number))

(provide/contract
 (app-version (-> string?))
 (app-commit-id (-> string?))
 (app-build-timestamp (-> string?))
 (app-build-number (-> (or/c string? #f))))
