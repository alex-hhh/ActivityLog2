#lang racket/base
;; activity-util-test.rkt -- tests for reading fit files
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2019, 2020, 2021 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require
  al2-test-runner
  racket/set
  rackunit)
(require "../rkt/fit-file/activity-util.rkt")

(define one-trackpoint
  '(((timestamp . 2) (heart-rate . 100))))

(define two-trackpoints
  '(((timestamp . 1) (heart-rate . 100))
    ((timestamp . 2) (heart-rate . 101))))

(define mult-trackpoints
  '(((timestamp . 1) (heart-rate . 100))
    ((timestamp . 2) (heart-rate . 101))
    ((timestamp . 2) (heart-rate . 101))
    ((timestamp . 2) (heart-rate . 101))))

(define mult-trackpoints-same-time
  '(((timestamp . 2) (heart-rate . 100))
    ((timestamp . 2) (heart-rate . 100))
    ((timestamp . 2) (heart-rate . 100))
    ((timestamp . 2) (heart-rate . 100))))

(define (contains? act exp)
  (subset?  (list->set exp) (list->set act)))
  

(define activity-util-tests
  (test-suite
   "compute-summary-data"
   [test-case "should compute avg-heart-rate on one heart beat"
              (check-true  (contains?  (compute-summary-data
                                        one-trackpoint '() '() '())
                                       '((max-heart-rate . 100) (avg-heart-rate . 100.0))))]
   [test-case "should compute avg-heart-rate on two heart beats"
              (check-true  (contains?  (compute-summary-data
                                        two-trackpoints '() '() '())
                                       '((max-heart-rate . 101) (avg-heart-rate . 100.5))))]
   [test-case "should compute avg-heart-rate on mult heart beats"
           (check-true  (contains?  (compute-summary-data
                                     mult-trackpoints '() '() '())
                                    '((max-heart-rate . 101) (avg-heart-rate . 100.5))))]
   [test-case "should compute avg-heart-rate on mult heart beats with a same timestamp"
             (check-true  (contains?  (compute-summary-data
                                       mult-trackpoints-same-time '() '() '())
                                      '((max-heart-rate . 100) (avg-heart-rate . 100.0))))]))

(module+ test
  (run-tests #:package "activity-util-test"
             ;#:results-file "test-results/activity-util-test-results.xml"
             activity-util-tests))
