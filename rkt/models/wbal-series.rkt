#lang racket/base

;; SPDX-License-Identifier: GPL-3.0-or-later
;; wbal-series.rkt -- add the "wbal" series to a data-frame
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2025 Alex Harsányi <AlexHarsanyi@gmail.com>
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
;; Add the W'Bal series to the data frame using the differential method by
;; Andy Froncioni and Dave Clarke.  This is based off the GoldenCheetah
;; implementation, I could not find any reference to this formula on the web.
;;
;; BASE-SERIES is either "pwr" or "spd"
(require data-frame
         racket/match
         racket/contract
         racket/list
         "critical-power.rkt"
         "gap.rkt")

;; Define filtering on the input for power or speed, when calculating W'Bal
;; series. Without filtering, and especially for power, spikes will eat up the
;; available WPrime reserve, resulting in a faster and unrealistic depletion.
;;
;; We use a low pass filter to filter input values before "feeding" them into
;; the W'Bal calculation mechanism. Setting this value to 0 effectively
;; disables filtering and the current power or speed value is used in W'Bal
;; depletion and reconstitution calculations

(define filter-width 15)                ; seconds

;; Add the W'Bal series to the data frame using the differential method by
;; Andy Froncioni and Dave Clarke, but only if the data frame has the required
;; data, does nothing otherwise.
;;
;; This implementation is based off the GoldenCheetah implementation, I could
;; not find any reference to this formula on the web.
;;
;; BASE-SERIES is either "pwr" or "spd"

(define (maybe-add-wbald-series! df base-series)

  (define-values (cp wprime)
    (match (df-get-property df 'critical-power)
      (#f (values #f #f))
      ((cp2 cp wprime) (values cp wprime))
      ((cp3 cp wprime k) (values cp wprime))))

  (when (and (df-contains? df "elapsed" base-series) cp wprime)

    (define wbal wprime)
    (define filtered-v-timestamp #f)
    (define filtered-v #f)

    (df-add-lazy!
     df
     "wbal"
     (list "elapsed" base-series)
     (lambda (current)
       (match-define (list t v) current)
       (when (and t v)
         (if (and filtered-v-timestamp filtered-v)
             (let* ([dt (- t filtered-v-timestamp)]
                    [alpha (/ dt (+ dt filter-width))])
               (set! filtered-v
                     (+ (* v alpha) (* filtered-v (- 1 alpha))))
               (set! filtered-v-timestamp t)
               (if (< filtered-v cp)
                   (let ([rate (/ (- wprime wbal) wprime)]
                         [delta (- cp filtered-v)])
                     ;; MIN prevents a large DT (such as a pause in the ride)
                     ;; to get WBAL above its maximum.  AB#55
                     (set! wbal (min wprime (+ wbal (* delta dt rate)))))
                   (let ((delta (- filtered-v cp)))
                     (set! wbal (- wbal (* delta dt))))))
             (begin
               (set! filtered-v v)
               (set! filtered-v-timestamp t))))
       wbal))))

;; Same as add-wbald-series! but use grade adjusted speed, instead of speed
(define (maybe-add-wbald-series/gap! df)

  (define-values (cp wprime)
    (match (df-get-property df 'critical-power)
      (#f (values #f #f))
      ((cp2 cp wprime) (values cp wprime))
      ((cp3 cp wprime k) (values cp wprime))))

  (cond
    ((and (df-contains? df "elapsed" "spd" "grade") cp wprime)
     (define wbal wprime)
     (define filtered-v-timestamp #f)
     (define filtered-v #f)

     (df-add-lazy!
      df
      "wbal"
      '("elapsed" "spd" "grade")
      (lambda (current)
        (match-define (list t v g) current)
        (when (and t v g)
          (if (and filtered-v-timestamp filtered-v)
              (let* ([m (grade->multiplier g)]
                     [dt (- t filtered-v-timestamp)]
                     [alpha (/ dt (+ dt filter-width))])
                (set! filtered-v
                      (+ (* m v alpha) (* filtered-v (- 1 alpha))))
                (set! filtered-v-timestamp t)
                (if (< filtered-v cp)
                    (let ((rate (/ (- wprime wbal) wprime))
                          (delta (- cp filtered-v)))
                      (set! wbal (min wprime (+ wbal (* delta dt rate)))))
                    (let ((delta (- filtered-v cp)))
                      (set! wbal (- wbal (* delta dt))))))
              (begin
                (set! filtered-v v)
                (set! filtered-v-timestamp t)))
          wbal))))
    (#t
     (maybe-add-wbald-series! df "spd"))))

;; Add the W'Bal series to the data frame using the integral method by
;; Dr. Phil Skiba.  This is based off the GoldenCheetah implementation, see
;; also
;;
;; http://markliversedge.blogspot.com.au/2014/07/wbal-its-implementation-and-optimisation.html
;;
;; and
;;
;; http://markliversedge.blogspot.com.au/2014/10/wbal-optimisation-by-mathematician.html
;;
;; BASE-SERIES is either "pwr" or "spd"
(define (maybe-add-wbali-series! df base-series)

  (define-values (cp wprime)
    (match (df-get-property df 'critical-power)
      (#f (values #f #f))
      ((cp2 cp wprime) (values cp wprime))
      ((cp3 cp wprime k) (values cp wprime))))

  (when (and (df-contains? df "elapsed" base-series) cp wprime)

    (define sum-count
      (df-fold
       df
       (list "elapsed" base-series)
       '(0 0)
       (lambda (accum prev current)
         (if (and prev current)
             (match-let (((list t1 v1) prev)
                         ((list t2 v2) current))
               (if (and t1 v1 t2 v2)
                   (let ((dt (- t2 t1))
                         (v (* 0.5 (+ v1 v2))))
                     (if (< v cp)
                         (match-let (((list sum count) accum))
                           (list (+ sum v) (+ count dt)))
                         accum))
                   accum))
             accum))))

    (define avg-below-cp (/ (first sum-count) (second sum-count)))
    (define tau
      (+ 316.0 (* 546 (exp (- (* 1.0 (- cp avg-below-cp)))))))
    (define integral 0)
    (define wbal wprime)
    (define filtered-v-timestamp #f)
     (define filtered-v #f)

    (df-add-lazy!
     df
     "wbali"
     (list "elapsed" base-series)
     (lambda (current)
       (match-define (list t v) current)
       (when (and t v)
         (if (and filtered-v-timestamp filtered-v)
             (let* ([dt (- t filtered-v-timestamp)]
                    [alpha (/ dt (+ dt filter-width))])
               (set! filtered-v
                     (+ (* v alpha) (* filtered-v (- 1 alpha))))
               (set! filtered-v-timestamp t)
               (when (> filtered-v cp)
                 (set! integral (+ integral (* (exp (/ filtered-v-timestamp tau)) (* (- filtered-v cp) dt)))))
               (set! wbal (- wprime (* integral (exp (- (/ filtered-v-timestamp tau)))))))
             (begin
               (set! filtered-v v)
               (set! filtered-v-timestamp t)))
       wbal)))))

(provide/contract
 (maybe-add-wbald-series! (-> data-frame? string? any/c))
 (maybe-add-wbald-series/gap! (-> data-frame? any/c))
 (maybe-add-wbali-series! (-> data-frame? string? any/c)))
