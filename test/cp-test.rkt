#lang racket/base

;; cp-test.rkt -- test the critical power functionality
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
         "../rkt/utilities.rkt"
         "custom-test-runner.rkt"
         "../rkt/models/critical-power.rkt"
         "../rkt/metrics.rkt")

(set-dbglog-to-standard-output #t)     ; send dbglog calls to stdout, so we can see them!
;; Use 1 worker thread, so we can determine when tasks finish (See
;; `do-tc-check`)
(set-worker-thread-count 1)

(define ammax
  '((2740 3769 10 422.25)
    (2740 3764 15 414.27)
    (2740 3758 30 388.3)
    (2740 3755 36 375.99)
    (2740 3754 43 362.81)
    (2740 3755 45 358.62)
    (2740 3755 52 350.76)
    (2740 3747 60 332.95)
    (2740 3746 62 328.02)
    (2724 2981 74 296.44)
    (2724 2981 75 295.86)
    (2724 2977 89 288.99)
    (2724 2977 90 288.48)
    (2724 2973 107 280.35)
    (2724 2968 120 273.84)
    (2724 2967 128 268.91)
    (2742 1203 154 257.01)
    (2735 1864 180 254.64)
    (2742 1251 185 253.22)
    (2742 1214 222 253.66)
    (2742 1218 266 249.6)
    (2742 1204 300 249.03)
    (2742 1186 319 242.39)
    (2742 1805 383 226.19)
    (2742 1805 460 225.68)
    (2742 1805 552 225.46)
    (2742 1805 600 224.96)
    (2742 1804 662 224.52)
    (2742 1804 794 223.99)
    (2742 2102 900 223.83)
    (2742 2050 953 223.74)
    (2742 1805 1144 223.98)
    (2742 1804 1200 224.53)
    (2742 1632 1373 209.61)
    (2742 1205 1648 206.48)
    (2742 1204 1800 208.53)
    (2742 1057 1948 203.04)
    (2742 755 2248 197.6)
    (2742 457 2548 192.87)
    (2737 726 2700 192.86)
    (2737 638 2848 189.86)
    (2737 336 3148 185.19)
    (2739 1687 3448 182.14)
    (2739 1684 3600 183.24)
    (2739 877 3748 183.12)
    (2739 980 4048 183.93)
    (2739 936 4348 185.45)
    (2739 638 4648 179.83)
    (2739 337 4948 179.47)
    (2739 37 5248 176.1)
    (2728 136 5400 174.22)
    (2728 133 5548 173.76)
    (2728 135 5848 169.73)
    (2728 208 6148 168.53)
    (2728 151 6448 168.14)
    (2728 155 6748 167.67)
    (2728 135 7048 167.06)
    (2728 55 7200 166.39)
    (2728 135 7348 165.75)
    (2728 154 7648 165.29)
    (2728 151 7948 164.42)
    (2728 152 8248 164.34)
    (2728 151 8548 163.28)
    (2728 56 8848 162.37)
    (2728 134 9148 161.48)
    (2728 63 9448 160.58)
    (2728 151 9748 160.33)
    (2728 56 10048 160.13)
    (2718 17 10348 157.24)))

;; neuromuscular range: 15 to 45 seconds
(define-values (nm-start nm-end) (values 15.0 45.0))
;; anaerobic range: 2 to 5 minutes
(define-values (an-start an-end) (values 120.0 300.0))
;; aerobic range 12 to 20 minutes
(define-values (ae-start ae-end) (values 720.0 1200.0))

(define mmax-fn (aggregate-mmax->spline-fn ammax))

(define critical-power-test-suite
  (test-suite
   "Critical Power"
   (test-case "CP3 exhaustive search"
     (define-values (cp3 cp3-results)
       (cp3-fit mmax-fn nm-start nm-end an-start an-end ae-start ae-end))
     (cp3-check-results cp3 cp3-results mmax-fn))
   (test-case "CP3 exhaustive search with progress"
     (define progress '())
     (define (progress-cb val)
       (set! progress (cons val progress)))
     (define-values (cp3 cp3-results)
       (cp3-fit mmax-fn nm-start nm-end an-start an-end ae-start ae-end progress-cb))
     (cp3-check-results cp3 cp3-results mmax-fn)
     (check-true (> (length progress) 0))
     (for ([one (in-list progress)]
           [two (in-list (cdr progress))])
       (check-pred > one two)))
   (test-case "CP2 exhaustive search"
     (define-values (cp2 cp2-results)
       (cp2-fit mmax-fn an-start an-end ae-start ae-end))
     (cp2-check-results cp2 cp2-results mmax-fn))))

(module+ test
  (run-tests #:package "cp-test"
             #:results-file "test-results/cp-test.xml"
             critical-power-test-suite))
