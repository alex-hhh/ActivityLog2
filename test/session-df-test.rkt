#lang racket/base
;; session-df-test.rkt -- test various data-manipulation functions in
;; session-df and related files
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

(require rackunit
         racket/match
         math/statistics)
(require "../rkt/session-df/session-df.rkt"
         "../rkt/session-df/native-series.rkt"
         "custom-test-runner.rkt")

(define session-df-tests
  (test-suite
   "session-df-test"
   (test-case "get-plot-y-range / no range"
     (check-exn
      exn:fail?
      (lambda ()
        ;; This should report a "bad bounds" error
        (get-plot-y-range empty-statistics axis-hr-bpm))))

   (test-case "get-plot-y-range / min = max"
     ;; min is the same as max, get-plot-y-range should create a range with a
     ;; gap
     (define s (update-statistics empty-statistics 60))
     (match-define (cons low high) (get-plot-y-range s axis-hr-bpm))
     (check-pred > (- high low) 0))

   (test-case "get-plot-y-range / include outliers"
     (define s (update-statistics* empty-statistics (list 10 80)))
     ;; NOTE: the left-right-balance metadata defines an y-range of 45 - 55,
     ;; but our data has values outside this -- ensure we return a range that
     ;; includes these oob values.
     (match-define (cons low high) (get-plot-y-range s axis-left-right-balance))
     (check-pred < low 10)
     (check-pred > low 80))

   (test-case "get-plot-y-range / extend range"
     (define s (update-statistics* empty-statistics (list 49 50 51)))
     ;; NOTE: the left-right-balance metadata defines an y-range of 45 - 55,
     ;; but our data is narrowly inside this range -- ensure we return a range
     ;; that is the full range (45, 55)
     (match-define (cons low high) (get-plot-y-range s axis-left-right-balance))
     (check-pred < low 45)
     (check-pred > high 55))

   ))

(module+ test
  (run-tests #:package "session-df-test"
             #:results-file "test-results/session-df-test.xml"
             session-df-tests))
