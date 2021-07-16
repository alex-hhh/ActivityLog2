#lang racket/base
;; fit2-test.rkt -- tests for reading fit files
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

(require al2-test-runner rackunit)
(require "../rkt/fit-file/fit-file.rkt"
         "../rkt/fit-file/activity-util.rkt")

(define fit-file-tests
  (test-suite
   "fit-file"
   (test-case "fit-file/fit-device-name"
     ;; Note that these tests also load the `fit-product-defs.json` file and
     ;; therefore validate it...
     (check-equal? (fit-device-name 1 1328 #f) "Garmin FR910XT")
     ;; FR301 is a device with several product ids
     (check-equal? (fit-device-name 1 475 #f) "Garmin FR301")
     (check-equal? (fit-device-name 69 #f 'bike-power) "Stages Power Meter")
     (check-equal? (fit-device-name 69 #f 'non-existent) "Stages non-existent")
     (check-equal? (fit-device-name 69 #f #f) "Stages")
     ;; Note: product 100 does not exist for Stages (69)
     (check-equal? (fit-device-name 69 100 'bike-power) "Stages Power Meter (100)")
     (check-equal? (fit-device-name 69 100 'non-existent) "Stages non-existent (100)")
     (check-equal? (fit-device-name 69 100 #f) "Stages (100)")
     ;; Note: manufacturer 1000 does not exist
     (check-equal? (fit-device-name 1000 100 'bike-power) "1000/100"))

   (test-case "fit-file/session-avg-stride" ; see #73
     ;; In the FIT file for #73, the athlete stopped moving after the last
     ;; lap, but left the timer running, so it recorded a zero distance and
     ;; zero cycles (steps) for the last lap.  Ensure the stride calculation
     ;; behaves for this case.
     (define dummy-session
       '((total-distance . 0)
         (total-cycles . 0)))
     ;; correctly handle total-cycles being 0
     (check-equal? (session-avg-stride dummy-session) #f))

   (test-case "fit-file/session-avg-swolf" ; see #73
     (define dummy-session
       '((total-cycles . 100)
         (avg-speed . 1.0)
         (pool-length . 0)
         (laps . (((avg-cadence . 100)
                   ;; NOTE: lengths is invalid format, but this is not used as
                   ;; of now in the avg SWOLF calculation.
                  (lengths . (foo bar baz)))))))
     ;; correctly handle pool length being 0
     (check-equal? (session-avg-swolf dummy-session) #f))

   ))

(module+ test
  (run-tests #:package "fit2-test"
             #:results-file "test-results/fit-file-test-results.xml"
             fit-file-tests))
