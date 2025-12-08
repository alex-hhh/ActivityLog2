#lang racket/base

;; coggan.rkt -- Coggan Metrics calculations for a session (NP, IF, TSS)
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2020, 2021, 2025 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require racket/contract
         data-frame
         data/queue
         racket/match)

;;; Commentary

;; The names of these metrics are trademarked, and cannot be used in code that
;; displays the metric to the user.  GoldenCheetah uses the following names
;; instead:
;;
;; IsoPower for Normalized Power (NP)
;; BikeIntensity for Intensity Factor (IF)
;; BikeStress for Training Stress Score (TSS)
;;
;; The `cg-metrics` function in this file calculates all three metrics


;; Calculate the Normalized Power metric, as defined by Andy Coggan.  See
;; `cg-metrics` for the meaning of the arguments.  Also, you probably want to
;; use `cg-metrics` instead, as it returns the intensity factor and TSS as
;; well.
;;
;; The algorithm is explained here
;; https://forum.slowtwitch.com/Slowtwitch_Forums_C1/Triathlon_Forum_F1/Normalized_Power_Formula_or_Calculator..._P3097774/
;;
;; This is the algorithm outline:
;;
;; 1) starting at the 30 s mark, calculate a rolling 30 s average (of the
;; preceeding time points, obviously).
;;
;; 2) raise all the values obtained in step #1 to the 4th power.
;;
;; 3) take the average of all of the values obtained in step #2.
;;
;; 4) take the 4th root of the value obtained in step #3.
;;
;; The original algorithm does not specify how to handle recording pauses,
;; zero values, variable recording intervals and missing data.  This
;; algorithm:
;;
;; * will discard pauses if "timer" is used as a weight series and will
;; include them if "elapsed" is used instead.
;;
;; * will deal with non-regular sampling interval by using the average between
;; two adjacent samples.
;;
;; * will treat missing data as 0
;;
;; The algorithm produces values that are close (within 1 watt) to Golden
;; Cheetah and to what Garmin FR920 produces for sessions w/o dropouts or
;; spikes.  The FR920 produces much higher values if there is missing data +
;; spikes -- not sure how they calculate those values.
;;
;; Note that the function returns #f if the requested interval is too short.
(define (normalized-power
         df
         #:series (series "pwr")
         #:weight-series (weight "timer")
         #:include-partial? (include-partial? #f) ; set to #t for GC style NP calculation
         #:start (start 0)
         #:stop (stop (df-row-count df)))

  (define rolling-window-size 30)       ; seconds

  ;; Holds `rolling-window-size` worth of data samples, to remove them from
  ;; the rolling `work` and `duration`.
  (define samples (make-queue))

  (define work 0)
  (define duration 0)

  (define np-work 0)
  (define np-duration 0)

  ;; We're supposed to start accumulating the NP work and duration only
  ;; `rolling-window-size` seconds into the section.  This flags when can
  ;; start doing that.
  (define primed? include-partial?)

  (df-for-each
   df
   (list weight series)
   (lambda (old new)
     (when old
       (match-define (list t1 p1) old)
       (match-define (list t2 p2) new)

       (when (and p1 p2)

         (define dt (- t2 t1))
         (define p (/ (+ (or p1 0) (or p2 0)) 2))
         (define dw (* p dt))

         (enqueue! samples (cons dw dt))
         (set! work (+ work dw))
         (set! duration (+ duration dt))

         ;; The first time duration reaches the size of the rolling window,
         ;; the algorithm is primed and we can start accumulating the NP work
         ;; and duration.
         (when (>= duration rolling-window-size)
           (set! primed? #t))

         ;; If duration is larger than the rolling window, start dropping
         ;; samples from the back until it is under
         (let loop ()
           (when (> duration rolling-window-size)
             (match-define (cons dw dt) (dequeue! samples))
             (set! duration (- duration dt))
             (set! work (- work dw))
             (loop)))

         (when primed?
           (set! np-work (+ np-work (expt (/ work duration) 4)))
           (set! np-duration (+ np-duration dt)))

         )))
   #:start start #:stop stop)

  (and (> np-duration 0) (expt (/ np-work np-duration) 0.25)))

;; Holds the "Andy Coggan" defined metrics for a session: Functional Threshold
;; Power (FTP), Normalized Power (NP), Intensity Factor (IF) and Training
;; Stress Score (TSS).
(struct cg (ftp np if tss) #:transparent)

;; Calculate and return the Coggan metrics for an activity in the data-frame
;; DF.
;;
;; In addition to the data-frame DF, the following arguments are used:
;;
;; FTP -- the athlete FTP
;;
;; SERIES -- the "power" series on which to calculate the metrics (defaults to
;; "pwr")
;;
;; WEIGHT -- the "time" series used to measure time.  Meaningful values are
;; "timer" for the activity times (without counting stops) or "elapsed" which
;; includes stop.  This could also be "dst" for distance, if you wish to
;; calculate NP on distance, but no one does that.
;;
;; INCLUDE-PARTIAL? -- whether to include a partial rolling average window at
;; the start.  The original algorithm does not (#f), but Golden Cheetah
;; includes it (#t).  Makes a small difference to the resulting metrics.
;;
;; START, STOP -- the start and end positions in the data-frame to use.  This
;; can be used to calculate the metrics for an interval.
(define (cg-metrics
         df
         #:ftp ftp
         #:series (series "pwr")
         #:weight-series (weight "timer")
         #:include-partial? (include-partial? #f) ; set to #t for GC style NP calculation
         #:start (start 0)
         #:stop (stop (df-row-count df)))

  ;; NOTE: the `get-athlete-ftp` call above is not protected by the contract
  ;; below...
  (unless (and (real? ftp) (positive? ftp))
    (raise-argument-error 'cg-metrics "(and/c real? positive?)"
                          1 df ftp series weight include-partial? start stop))

  (define np (normalized-power df
                               #:series series
                               #:weight-series weight
                               #:include-partial? include-partial?
                               #:start start
                               #:stop stop))

  (define ifactor (if np (/ np ftp) #f))
  (define duration (df-ref df (sub1 (df-row-count df)) weight))
  (define tss (if ifactor (* ifactor ifactor (/ duration 3600.0) 100.0) #f))

  (cg ftp np ifactor tss))


;;............................................................. provides ....

(provide (struct-out cg))
(provide/contract
 (cg-metrics (->* (data-frame?
                   #:ftp (and/c real? positive?))
                  (#:series string?
                   #:weight-series string?
                   #:include-partial? boolean?
                   #:start exact-integer?
                   #:stop exact-nonnegative-integer?)
                  cg?)))
