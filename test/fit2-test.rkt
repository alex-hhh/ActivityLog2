#lang racket/base
;; fit2-test.rkt -- tests for reading fit files
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2019 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require rackunit)
(require "../rkt/fit-file/fit-file.rkt")

(define fit-device-name-tests
  (test-suite
   "`fit-device-name` test suite"
   (test-case "`fit-device-name` check that device names are correctly constructed"
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
     (check-equal? (fit-device-name 1000 100 'bike-power) "1000/100"))))

(module+ test
  (require rackunit/text-ui)
  (run-tests fit-device-name-tests))
