#lang racket/base

;; fthr-test.rkt -- test the FTHR dashboard
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

(require al2-test-runner
         rackunit
         data-frame
         racket/match
         pict
         racket/port
         "../rkt/utilities.rkt"
         "../rkt/models/sport-zone.rkt"
         "../rkt/models/fthr.rkt")

(set-dbglog-to-standard-output #t)     ; send dbglog calls to stdout, so we can see them!
;; Use 1 worker thread, so we can determine when tasks finish (See
;; `do-tc-check`)
(set-worker-thread-count 1)

;; Sample data does not change, so we load it once for all tests to use.
(define run-data (df-read/csv "test-data/track-data-2098.csv"))
(df-set-sorted run-data "elapsed" <)
(df-put-property run-data 'sport (vector 1 #f))
(define bike-data (df-read/csv "test-data/track-data-2604.csv"))
(df-set-sorted bike-data "elapsed" <)
(df-put-property bike-data 'sport (vector 2 #f))

(define fthr-test-suite
  (test-suite
   "FTHR Analysis"
   (test-case "fthr-duration"
     (check-equal? (fthr-duration "running" "heart rate") 1200)
     (check-equal? (fthr-duration "running" "speed") 1800)
     (check-equal? (fthr-duration "cycling" "heart rate") 1200)
     (check-equal? (fthr-duration "cycling" "power") 1200)
     (check-exn
      exn?
      (lambda () (fthr-duration "**non existent**" "hr"))))

   (test-case "fthr-zone-definitions"
     (define (check-zones zones)
       (check-pred list? zones)
       (for ([z (in-list zones)])
         (check-pred list? z)
         (check-equal? 3 (length z))
         (match-define (list n t v) z)
         (check-pred string? n)
         (check-true (list? (member t '(absolute percent))))
         (check-pred number? v)))

     (check-zones (fthr-zone-definitions "running" "heart rate"))
     (check-zones (fthr-zone-definitions "running" "speed"))
     (check-zones (fthr-zone-definitions "cycling" "heart rate"))
     (check-zones (fthr-zone-definitions "cycling" "power"))
     (check-exn
      exn?
      (lambda () (fthr-zone-definitions "** non existent **" "heart rate")))
     (check-exn
      exn?
      (lambda () (fthr-zone-definitions "running" "** non existent **"))))

   (test-case "make-sport-zones"

     (define (check-zones z1 z2)
       (check-equal? (sz-count z1) (length z2))
       (for ([v1 (in-vector (sz-boundaries z1))]
             [n1 (in-vector (sz-names z1))]
             [i2 (in-list z2)])
         (match-define (list n2 v2) i2)
         (check-equal? n1 n2)
         (check-= v1 v2 1.0)))

     (define power-sport-zones
       '(("Coasting" absolute 0)
         ("Recovery" absolute 1)
         ("Endurance" percent 0.55)
         ("Tempo" percent 0.76)
         ("Sweetspot" percent 0.88)
         ("Threshold" percent 0.95)
         ("VO2 Max" percent 1.06)
         ("Anaerobic" percent 1.21)
         ("Max" percent 3.0)))

     (define power-best (hash 'series "power" 'best 215
                              'threshold 215
                              'zone-metric 'power
                              'sport 2 'sub-sport #f
                              'zone-definitions power-sport-zones))

     (define expected-power-zones
       '(("Coasting" 0)
         ("Recovery" 1)
         ("Endurance" 118)
         ("Tempo" 163)
         ("Sweetspot" 189)
         ("Threshold" 204)
         ("VO2 Max" 228)
         ("Anaerobic" 260)
         ("Max" 645)))

     (check-zones (make-sport-zones power-best) expected-power-zones)

     (define running-speed-zones
       '(("Recovery" percent 0)
         ("Endurance" percent 0.78)
         ("Tempo" percent 0.88)
         ("Tempo" percent 0.94)
         ("Threshold" percent 1.03)
         ("Zone 5" percent 1.0)))

     (define pace-best (hash 'series "pace" 'best 280 'threshold 280
                             'zone-metric 'pace
                             'sport 1 'sub-sport #f
                             'zone-definitions running-speed-zones))

     (define expected-speed-zones
       '(("Recovery" 0.0)
         ("Endurance" 2.785714285714286)
         ("Tempo" 3.1428571428571432)
         ("Tempo" 3.357142857142857)
         ("Threshold" 3.678571428571429)
         ("Zone 5" 3.5714285714285716)))

     (check-zones (make-sport-zones pace-best) expected-speed-zones))

   (test-case "best-segment"
     (define run-hr-best (best-segment run-data "hr" 1800.0))
     (check-equal? (hash-ref run-hr-best 'series) "hr")
     (check-= (hash-ref run-hr-best 'best -1) 183 1.0)
     (check-= (hash-ref run-hr-best 'duration -1) 1800 0)
     (check-= (hash-ref run-hr-best 'min-value -1) 166 1.0)
     (check-= (hash-ref run-hr-best 'max-value -1) 190 1.0)
     (check-= (hash-ref run-hr-best 'start-index -1) 600 0)
     (check-= (hash-ref run-hr-best 'end-index -1) 2400 0)
     (check-= (hash-ref run-hr-best 'start-position -1) 600 0)
     (check-= (hash-ref run-hr-best 'end-position -1) 2400 0)
     (check-= (hash-ref run-hr-best 'first-half-average -1) 182 1.0)
     (check-= (hash-ref run-hr-best 'second-half-average -1) 185 1.0)

     (define run-pace-best (best-segment run-data "pace" 1500.0))
     (check-equal? (hash-ref run-pace-best 'series) "pace")
     (check-= (hash-ref run-pace-best 'best -1) 285 1.0)
     (check-= (hash-ref run-pace-best 'duration -1) 1500 0)
     (check-= (hash-ref run-pace-best 'min-value -1) 255 1.0)
     (check-= (hash-ref run-pace-best 'max-value -1) 306 1.0)
     (check-= (hash-ref run-pace-best 'start-index -1) 655 0)
     (check-= (hash-ref run-pace-best 'end-index -1) 2155 0)
     (check-= (hash-ref run-pace-best 'start-position -1) 655 0)
     (check-= (hash-ref run-pace-best 'end-position -1) 2155 0)
     (check-= (hash-ref run-pace-best 'first-half-average -1) 282 1.0)
     (check-= (hash-ref run-pace-best 'second-half-average -1) 289 1.0)

     (define bike-hr-best (best-segment bike-data "hr" 1500.0))
     (check-equal? (hash-ref bike-hr-best 'series) "hr")
     (check-= (hash-ref bike-hr-best 'best -1) 173 1.0)
     (check-= (hash-ref bike-hr-best 'duration -1) 1500 0)
     (check-= (hash-ref bike-hr-best 'min-value -1) 142 1.0)
     (check-= (hash-ref bike-hr-best 'max-value -1) 187 1.0)
     (check-= (hash-ref bike-hr-best 'start-index -1) 1302 0)
     (check-= (hash-ref bike-hr-best 'end-index -1) 2802 0)
     (check-= (hash-ref bike-hr-best 'start-position -1) 1302 0)
     (check-= (hash-ref bike-hr-best 'end-position -1) 2802 0)
     (check-= (hash-ref bike-hr-best 'first-half-average -1) 167 1.0)
     (check-= (hash-ref bike-hr-best 'second-half-average -1) 179 1.0)

     (define bike-power-best (best-segment bike-data "pwr" 1500.0))
     (check-equal? (hash-ref bike-power-best 'series) "pwr")
     (check-= (hash-ref bike-power-best 'best -1) 211 1.0)
     (check-= (hash-ref bike-power-best 'duration -1) 1500 0)
     (check-= (hash-ref bike-power-best 'min-value -1) 19 1.0)
     (check-= (hash-ref bike-power-best 'max-value -1) 308 1.0)
     (check-= (hash-ref bike-power-best 'start-index -1) 1206 0)
     (check-= (hash-ref bike-power-best 'end-index -1) 2706 0)
     (check-= (hash-ref bike-power-best 'start-position -1) 1206 0)
     (check-= (hash-ref bike-power-best 'end-position -1) 2706 0)
     (check-= (hash-ref bike-power-best 'first-half-average -1) 197 1.0)
     (check-= (hash-ref bike-power-best 'second-half-average -1) 225 1.0)

     )

   (test-case "best segments 2"

     ;; NOTE: these tests only check if invoking these functions actually
     ;; works and they run w/o exceptions.  Better than nothing, I guess :-)

     (define run-pace (best-pace-segment run-data))
     (define run-hr (best-heart-rate-segment run-data))
     (define bike-power (best-power-segment bike-data))
     (define bike-hr (best-heart-rate-segment bike-data))

     (for ([segment (list run-pace run-hr bike-power bike-hr)])
       (check-pred hash? segment)
       (check-pred pict? (pp-segment/pict segment))
       (check-true (< 0 (string-length (with-output-to-string (lambda () (pp-segment segment))))))))

   ))


(module+ test
  (run-tests #:package "fthr-test"
             #:results-file "test-results/fthr-test.xml"
             fthr-test-suite))
