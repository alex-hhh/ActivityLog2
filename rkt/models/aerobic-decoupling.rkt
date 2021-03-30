#lang racket/base

;; aerobic-decoupling.rkt -- Pw:HR and Pa:HR metrics
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2021 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require data-frame
         racket/math
         math/statistics
         racket/contract)

;; Calculate the decoupling between the ratio of two series S1 and S2 in the
;; data frame between the START and STOP indexes.  The decoupling is a
;; percentage indicating how much the ratio of the two values changes between
;; the first and second halves of the segment between START and STOP.
(define (decoupling df s1 s2 #:start (start 0) #:stop (stop (df-row-count df)))
  (let ((half-point (exact-truncate (/ (+ start stop) 2))))
    (let ((stat-s1-1 (df-statistics df s1 #:start start #:stop half-point))
          (stat-s1-2 (df-statistics df s1 #:start half-point #:stop stop))
          (stat-s2-1 (df-statistics df s2 #:start start #:stop half-point))
          (stat-s2-2 (df-statistics df s2 #:start half-point #:stop stop)))
      (and stat-s1-1 stat-s1-2 stat-s2-1 stat-s2-2
           (let ((r1 (/ (statistics-mean stat-s1-1)
                        (statistics-mean stat-s2-1)))
                 (r2 (/ (statistics-mean stat-s1-2)
                        (statistics-mean stat-s2-2))))
             (* 100.0 (/ (- r1 r2) r1)))))))

;; Calculate the decoupling between series S1 and S2 for every lap in the data
;; frame DF.  The lap start timestamps are taken from the 'laps property on
;; the data frame.  The resulting list will contain #f if the lap is too sort
;; to produce a meaningful decoupling value (currently 60 seconds).
(define (decoupling/laps df s1 s2)
  (let* ((laps (df-get-property df 'laps))
         (limit (vector-length laps)))
    (for/list ([idx (in-range 0 (vector-length laps))])
      (define start
        (df-index-of df "timestamp" (vector-ref laps idx)))
      (define stop
        (if (< idx (sub1 limit))
            (df-index-of df "timestamp" (vector-ref laps (+ idx 1)))
            (df-row-count df)))
      ;; don't compute decoupling for short intervals
      (if (and start stop (> (- stop start) 60))
          (decoupling df s1 s2 #:start start #:stop stop)
          #f))))

;; Calculate the aerobic decoupling in the data frame DF between the START and
;; STOP indexes.  For bike sessions, the power to heart rate ratio is used,
;; for running the speed to heart rate.  Returns #f for all other sports, or
;; if there are no data series (e.g. a bike activity has no power data
;; series).
(define (aerobic-decoupling df #:start (start 0) #:stop (stop (df-row-count df)))
  (define sport (df-get-property df 'sport))
  (unless sport
    (error "aerobic-decoupling: no sport property in data frame"))
  (cond ((and (equal? (vector-ref sport 0) 2) ; bike
              (df-contains? df "pwr" "hr"))
         (decoupling df "pwr" "hr" #:start start #:stop stop))
        ((and (equal? (vector-ref sport 0) 1) ; run
              (df-contains? df "spd" "hr"))
         (decoupling df "spd" "hr" #:start start #:stop stop))
        (#t #f)))

;; Same as `aerobic-decoupling` but return a value for each lap in the data
;; frame DF.
(define (aerobic-decoupling/laps df)
  (define sport (df-get-property df 'sport))
  (unless sport
    (error "aerobic-decoupling: no sport property in data frame"))
  (cond ((and (equal? (vector-ref sport 0) 2) ; bike
              (df-contains? df "pwr" "hr"))
         (decoupling/laps df "pwr" "hr"))
        ((and (equal? (vector-ref sport 0) 1) ; run
              (df-contains? df "spd" "hr"))
         (decoupling/laps df "spd" "hr"))
        (#t '())))

(provide/contract
 (aerobic-decoupling (->* (data-frame?)
                          (#:start exact-integer?
                           #:stop exact-nonnegative-integer?)
                          (or/c real? #f)))
 (aerobic-decoupling/laps (-> data-frame? (listof (or/c real? #f)))))
