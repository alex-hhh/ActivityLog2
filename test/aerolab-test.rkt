#lang racket/base
;; aerolab-test.rkt -- tests for aerolab functionality
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2023 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require al2-test-runner
         rackunit
         db/base
         "../rkt/utilities.rkt"
         "../rkt/aerolab/aerolab-storage.rkt"
         "test-util.rkt")

(set-dbglog-to-standard-output #t)     ; send dbglog calls to stdout, so we can see them!
;; Use 1 worker thread, so we can determine when tasks finish (See
;; `do-tc-check`)
(set-worker-thread-count 1)

(define aerolab-test-suite
  (test-suite
   "Aerolab"

   (test-case "put, fetch and delete aerolab parameters"
     (with-fresh-database
       (lambda (db)
         (check-equal? (fetch-aerolab-parameters db 1) #f)
         (check-not-exn (lambda () (drop-aerolab-parameters db 1)))
         (define sid (db-import-manual-session db))
         (check-not-exn (lambda () (store-aerolab-parameters db sid (hash))))
         (check-equal? (query-value db "select count(*) from SESSION_AEROLAB") 1)
         (check-equal? (fetch-aerolab-parameters db sid)
                       (hash 'calculation-method 'dew-point 'use-wind? #t))
         ;; Replace parameters for this session...
         (check-not-exn (lambda () (store-aerolab-parameters db sid (hash 'crr 1.0 'cda 1.0))))
         (check-equal? (fetch-aerolab-parameters db sid)
                       (hash 'calculation-method 'dew-point 'use-wind? #t 'crr 1.0 'cda 1.0))
         )))

   ))

(module+ test
  (run-tests #:package "aerolab-test"
             #:results-file "test-results/aerolab-test.xml"
             aerolab-test-suite))
