#lang typed/racket/base
;; critical-power.rkt -- Estimate CP2 and CP3 parameters
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2020, 2022 Alex Harsányi <AlexHarsanyi@gmail.com>
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


(require racket/match
         racket/flonum
         racket/math)

(provide (struct-out cp2)
         (struct-out cp2-fit-results)
         cp2-function
         cp2-fit
         cp2-recovery-constant
         cp2-check-results

         (struct-out cp3)
         (struct-out cp3-fit-results)
         cp3-function
         cp3-fit
         cp3-pmax
         cp3-recovery-constant
         cp3-check-results)

;;;; Overview

;; NOTE: while we use the term "Critical Power", exactly the same concept
;; applies to speed when running or swimming.  Basically in ActivityLog2 we
;; use:
;;
;; For Cycling:
;;
;;     CP -- Critical Power (watts), W' (joules), k, Pmax (watts)
;;
;; For Running and Swimming
;;
;;     CV -- Critical Velocity (m/s), D' a distance in meters, k, Vmax (m/s)
;;
;; The code in this file works the same for both Critical Power and Critical
;; Velocity.
;;
;; Critical Power models allow estimating the maximum power which can be
;; maintained for a given duration (the equations can also be re-written to
;; estimate the time to exhaustion at a given power).
;;
;; There are two CP models implemented here, the two parameter, CP2 and three
;; parameter, CP3 models.
;;
;; The CP2 model uses 2 parameters, CP and W' for an equation written as:
;;
;;     P(t) = CP + W' / t
;;
;; The CP3 model uses 3 parameters, CP, W' and k (a negative value) for an
;; equation written as:
;;
;;     P(t) = CP + W' / (t - k)
;;
;; Notes:
;;
;; * CP2 is the same as CP3 when k is 0
;;
;; * CP and W' parameters will be different when CP2 and CP3 are estimated
;;   from the same data set.  CP, W' and k are parameters of the model, not an
;;   intrinsic measurement of an athlete body (like their body weight or
;;   resting heart rate).
;;
;; * The k value in CP3 is a "time offset", but it has little practical value,
;;   however, CP3 can estimate the maximum power as the power that can be
;;   maintained for zero seconds (note again that k is negative)
;;
;;       Pmax = P(0) = CP - W' / k
;;
;;   In the CP2 model, P(0) is infinite.
;;
;; To estimate the CP parameters you'll need:
;;
;; 1) A Mean-Maximal function which provides some empirical power-duration
;;    data, that is a function which returns the max recorded power for a
;;    given duration.
;;
;; 2) Two search ranges, in seconds for the CP2 model and 3 search ranges for
;;    the CP3 model
;;
;; See the `cp2-fit` and `cp3-fit` functions


;;;; Solving the CP2 equation
;;
;; Solving CP, W' is done by looking at the best power (or speed) at two data
;; points.  The following explains how these parameters are determined:
;;
;; The equation:
;;
;;     P(t) = CP + W' / t
;;
;; Can be re-written as
;;
;;     P(t) * t = CP * t + W'
;;
;; Given that P(t) * t is the work performed at time t, CP and W' can be
;; determined given two values for work at two different time points:
;;
;;     W(t1) = CP * t1 + W'
;;     W(t2) = CP * t2 + W'
;;
;; so:
;;
;;     CP = (W(t2) - W(t1)) / (t2 - t1)
;;     W' = W(t1) - CP * t1

;;;; Solving the CP3 equation
;;
;; Solving for CP, W' and k is done by looking at three data points.  Starting
;; with this equation:
;;
;;     P(t) = CP + W' / (t - k)                                        (1)
;;
;; We multiply both sides by (t - k), and note that P(t) * t = W(t), so we
;; obtain:
;;
;;     W(t) - P(t) * k = CP * t - CP * k + W'                          (2)
;;     W(t) = CP * t - CP * k + P(t) * k + W'                          (3)
;;
;; We than find three points on the PD model (t1, t2, t3) and have:
;;
;;    W(t1) = CP * t1 - CP * k + P(t1) * k + W'                        (4)
;;    W(t2) = CP * t2 - CP * k + P(t2) * k + W'                        (5)
;;    W(t3) = CP * t3 - CP * k + P(t3) * k + W'                        (6)
;;
;; And we can solve this by simple substitution.  From (4) we have:
;;
;;     W' = W(t1) - CP * t1 + CP * k - P(t1) * k                       (7)
;;
;;     dW21 = CP * dT21 + k * dP21                                     (8)
;;     dW31 = CP * dT31 + k * dP31                                     (9)
;;
;; Where:
;;
;;     dW31 = W(t3) - W(t1)
;;     dW21 = W(t2) - W(t1)
;;     dP31 = P(t3) - P(t1)
;;     dP21 = P(t2) - P(t1)
;;     dT31 = t3 - t1
;;     dT21 = t2 - t1
;;
;; From (8) we can calculate CP as:
;;
;;     CP = (dW21 - k * dP21) / dT21                                  (10)
;;
;; ... and substitute CP in (9) to get:
;;
;;     dW31 = (dW21 - k * dP21) * dT31 / dT21 + k * dP31              (11)
;;
;; We can rewrite (11) as:
;;
;;     dW31 - dW21 * dT31 / dT21 = k * (dp31 - dP21 * dT31 / dT21)    (12)
;;
;; so k =  (dW31 - dW21 * dT31 / dT21) / (dP31 - dP21 * dT31 / dT21)
;;


(define-type Data-Point (Pair Flonum Flonum))
(define-type Test-Data (Mutable-Vectorof Data-Point))
(define-type Mmax-Function (-> Flonum Flonum))

;; Multiplier used for the "negative" cost in `evaluate-cost` -- this is the
;; cost where the sample data is above the model -- in general we want to
;; penalize this situation, since this means that the data is better than the
;; prediction.  If this is set to 1.0 negative cost weights the same as the
;; positive cost, so we do a "least squares fit" on the data.  A large value
;; means we are doing more of an "envelope fit" where the model is almost
;; above the data for the entire series.
;;
(: negative-cost-multiplier Flonum)
(define negative-cost-multiplier 20.0)

;; Number of samples we take from the mmax function for evaluating the cost of
;; our fit.
;;
(: test-data-samples Positive-Integer)
(define test-data-samples 100)

;; Sample the MMAX-FN at regular intervals between START and END.
;; `test-data-samples` samples are taken.  Returns two flvectors: one for the
;; time points one for the values of the MMAX-FN at these time points.
;;
(: make-test-data (-> Mmax-Function Flonum Flonum (Values FlVector FlVector)))
(define (make-test-data mmax-fn start end)
  ;; NOTE: we take special care to return floating point values here
  (let ((step (/ (- end start) test-data-samples)))
    (define time-points
      (for/flvector #:length test-data-samples
          ([tp (in-range start end step)])
        (exact->inexact tp)))
    (define data-points
      (for/flvector #:length test-data-samples
          ([t (in-flvector time-points)])
        (mmax-fn t)))
    (values time-points data-points)))

;; Evaluate how good the CP3 parameters are against the test data samples a
;; lower value means a better fit.  We do least-squares fitting, except that
;; values where the data is above the model is penalized by
;; `negative-cost-multiplier`.
;;
(: evaluate-cost/cp3 (-> Flonum Flonum Flonum FlVector FlVector Flonum))
(define (evaluate-cost/cp3 cp wprime k time-points data-points)

  (define-values
    (pcost ncost)
    (for/fold ([pcost : Flonum 0.0] [ncost : Flonum 0.0])
              ([t (in-flvector time-points)]
               [value (in-flvector data-points)])
      (let* ((model (+ cp (/ wprime (- t k))))
             (diff (- value model))
             (cost (* diff diff)))
        (if (>= model value)
            (values (+ pcost cost) ncost)
            (values pcost (+ ncost cost))))))

  (+ pcost (* negative-cost-multiplier ncost)))

;; Same as `evaluate-cost/cp3` but for the CP2 model.
;;
(: evaluate-cost/cp2 (-> Flonum Flonum FlVector FlVector Flonum))
(define (evaluate-cost/cp2 cp wprime time-points data-points)

  (define-values
    (pcost ncost)
    (for/fold ([pcost : Flonum 0.0] [ncost : Flonum 0.0])
              ([t (in-flvector time-points)]
               [value (in-flvector data-points)])
      (let* ((model (+ cp (/ wprime t)))
             (diff (- value model))
             (cost (* diff diff)))
        (if (>= model value)
            (values (+ pcost cost) ncost)
            (values pcost (+ ncost cost))))))

  (+ pcost (* negative-cost-multiplier ncost)))

;; Precompute the WORK produced by MMAX-FN at 1 second interval between START
;; and END and return a flvector of these values.
;;
(: pre-compute-work (-> Mmax-Function Flonum Flonum FlVector))
(define (pre-compute-work mmax-fn start end)
  (for/flvector #:length (exact-truncate (add1 (- end start)))
      ((t (in-range start (add1 end))))
    (* t (mmax-fn t))))

;; Precompute the power values produced by MMAX-FN at 1 second intervals
;; between START and END.  Returns a flvector of power values.
;;
(: pre-compute-power (-> Mmax-Function Flonum Flonum FlVector))
(define (pre-compute-power mmax-fn start end)
  (for/flvector #:length (exact-truncate (add1 (- end start)))
      ((t (in-range start (add1 end))))
    (mmax-fn t)))

(: cp3-search (-> Mmax-Function Flonum Flonum Flonum Flonum Flonum Flonum
                  (U False (-> Flonum Any))
                  (Values Flonum Flonum Flonum Flonum Flonum Flonum Flonum)))
(define (cp3-search mmax-fn nm-start nm-end an-start an-end ae-start ae-end progress-callback)

  (unless (< nm-start nm-end an-start an-end ae-start ae-end)
    (raise "cp3: search intervals out of order"))

  (define-values (test-time-points test-data-points)
    (make-test-data mmax-fn nm-start ae-end))

  ;; Pre-compute the work and power values in the anaerobic and aerobic ranges
  (define nwork (pre-compute-work mmax-fn nm-start nm-end))
  (define anwork (pre-compute-work mmax-fn an-start an-end))
  (define aework (pre-compute-work mmax-fn ae-start ae-end))
  (define npower (pre-compute-power mmax-fn nm-start nm-end))
  (define anpower (pre-compute-power mmax-fn an-start an-end))
  (define aepower (pre-compute-power mmax-fn ae-start ae-end))

  (define total-progress (* (- nm-end nm-start) (- an-end an-start)))
  (define last-progress 0.00)

  (when progress-callback
    (progress-callback 0.0))

  (begin0
      (for/fold ([best-cp : Flonum 0.0]
                 [best-wprime : Flonum 0.0]
                 [best-k : Flonum 0.0]
                 [best-t1 : Flonum 0.0]
                 [best-t2 : Flonum 0.0]
                 [best-t3 : Flonum 0.0]
                 [best-cost : Flonum +inf.0])
                ([t1 (in-range nm-start nm-end 1.0)])

        (define i1 (exact-truncate (- t1 nm-start)))
        (define p1 (flvector-ref npower i1))
        (define w1 (flvector-ref nwork i1))

        (for/fold ([best-cp : Flonum best-cp]
                   [best-wprime : Flonum best-wprime]
                   [best-k : Flonum best-k]
                   [best-t1 : Flonum best-t1]
                   [best-t2 : Flonum best-t2]
                   [best-t3 : Flonum best-t3]
                   [best-cost : Flonum best-cost])
                  ([t2 (in-range an-start an-end 1.0)])

          (when progress-callback
            (define progress (/ (* (- t1 nm-start) (- an-end an-start)) total-progress))
            (when (> (- progress last-progress) 0.1)
              (progress-callback progress)
              (set! last-progress progress)))

          (define i2 (exact-truncate (- t2 an-start)))
          (define p2 (flvector-ref anpower i2))
          (define w2 (flvector-ref anwork i2))
          (define delta-w21 (- w2 w1))
          (define delta-p21 (- p2 p1))
          (define delta-t21 (- t2 t1))

          (for/fold ([best-cp : Flonum best-cp]
                     [best-wprime : Flonum best-wprime]
                     [best-k : Flonum best-k]
                     [best-t1 : Flonum best-t1]
                     [best-t2 : Flonum best-t2]
                     [best-t3 : Flonum best-t3]
                     [best-cost : Flonum best-cost])
                    ([t3 (in-range ae-start ae-end 1.0)])

            (define i3 (exact-truncate (- t3 ae-start)))
            (define p3 (flvector-ref aepower i3))
            (define w3 (flvector-ref aework i3))
            (define delta-w31 (- w3 w1))
            (define delta-p31 (- p3 p1))
            (define delta-t31 (- t3 t1))

            (define delta-t (/ delta-t31 delta-t21))

            (define k (/ (- delta-w31 (* delta-t delta-w21))
                         (- delta-p31 (* delta-t delta-p21))))

            (if (< k 0.0)
                (let* ([cp (/ (- delta-w21 (* k delta-p21)) delta-t21)]
                       [wprime (+ (- w1 (* cp t1) (* k p1)) (* cp k))]
                       [cost (evaluate-cost/cp3 cp wprime k test-time-points test-data-points)])
                  (if (< cost best-cost)
                      (values cp wprime k t1 t2 t3 cost)
                      (values best-cp best-wprime best-k best-t1 best-t2 best-t3 best-cost)))
                (values best-cp best-wprime best-k best-t1 best-t2 best-t3 best-cost)))))
    (when progress-callback
      (progress-callback 1.0))))

(: cp2-search (-> Mmax-Function Flonum Flonum Flonum Flonum
                             (Values Flonum Flonum Flonum Flonum Flonum)))
(define (cp2-search mmax-fn an-start an-end ae-start ae-end)

  (unless (< an-start an-end ae-start ae-end)
    (raise "cp3: search intervals out of order"))

  ;; Pre-compute the work values in the anaerobic and aerobic ranges
  (define anwork (pre-compute-work mmax-fn an-start ae-end))
  (define aework (pre-compute-work mmax-fn ae-start ae-end))

  (define-values (test-time-points test-data-points)
    (make-test-data mmax-fn an-start ae-end))

  (for/fold ([best-cp : Flonum 0.0]
             [best-wprime : Flonum 0.0]
             [best-t1 : Flonum 0.0]
             [best-t2 : Flonum 0.0]
             [best-cost : Flonum +inf.0])
            ([t1 (in-range an-start an-end 1.0)])

    (define i1 (exact-truncate (- t1 an-start)))
    (define w1 (flvector-ref anwork i1))

    (for/fold ([best-cp : Flonum best-cp]
               [best-wprime : Flonum best-wprime]
               [best-t1 : Flonum best-t1]
               [best-t2 : Flonum best-t2]
               [best-cost : Flonum best-cost])
              ([t2 (in-range ae-start ae-end 1.0)])

      (define i2 (exact-truncate (- t2 ae-start)))
      (define w2 (flvector-ref aework i2))

      (define cp (/ (- w2 w1) (- t2 t1)))
      (define wprime (- w1 (* cp t1)))
      (define cost (evaluate-cost/cp2 cp wprime test-time-points test-data-points))
      (if (< cost best-cost)
          (values cp wprime t1 t2 cost)
          (values best-cp best-wprime best-t1 best-t2 best-cost)))))


;;.............................................................. CP2 API ....

;; Hold the 2 parameters for the CP2 model
(struct: cp2
  ([cp : Flonum]
   [wprime : Flonum])
  #:transparent)

;; The results of fitting the model onto the empirical data.  Returns the T1
;; and T2 time points where the fit happened as well as the cost for the fit
;; (smaller is better), see `evaluate-cost/cp2`
(struct: cp2-fit-results
  ([t1 : Flonum]
   [t2 : Flonum]
   [cost : Flonum])
  #:transparent)

;; Fit the CP2 model onto the MMAX-FN in the two ranges between AN-START,
;; AN-END and AE-START, AE-END.  Returns two values, a CP2 structure and a
;; CP2-FIT-RESULTS structure.
;;
;; Good values for the AN range are 120 - 300 and for the AE range 720 - 1200.
(: cp2-fit (-> Mmax-Function Real Real Real Real (Values cp2 cp2-fit-results)))
(define (cp2-fit mmax-fn an-start an-end ae-start ae-end)
  (define-values (cp wprime t1 t2 cost)
    (cp2-search mmax-fn
                (real->double-flonum an-start)
                (real->double-flonum an-end)
                (real->double-flonum ae-start)
                (real->double-flonum ae-end)))
  (values (cp2 cp wprime) (cp2-fit-results t1 t2 cost)))

;; Return a function for the CP2 model, that is, build the P(t) = CP + W' / t
;; function
(: cp2-function (-> cp2 (-> Real Real)))
(define (cp2-function cp-params)
  (match-define (cp2 cp wprime) cp-params)
  (lambda (t) (+ cp (/ wprime t))))

;; Return the time in which 63% of the depleted W' will be recovered at 0
;; effort.
;;
;; This is an exponential recovery, so for a recovery constant TAU, if all W'
;; is depleted, 63% of it is recovered after TAU seconds, than 63% of the
;; REMAINING W" is recovered in the next TAU seconds and so on.  This means
;; that, regardless of how much W' is depleted, 63% of it will be recovered in
;; TAU seconds.
(: cp2-recovery-constant (-> cp2 Flonum))
(define (cp2-recovery-constant cp-params)
  (match-define (cp2 cp wprime) cp-params)
  (/ wprime cp))

;; Check the fit results against the mmax-fn -- this is a validation check for
;; the search and verifies that the model matches the data very closely at the
;; fit points t1 and t2
(: cp2-check-results (-> cp2 cp2-fit-results Mmax-Function Void))
(define (cp2-check-results params fit-results mmax-fn)
  (match-define (cp2-fit-results t1 t2 cost) fit-results)
  (define model (cp2-function params))
  (let* ((m1 (model t1))
         (d1 (mmax-fn t1))
         (diff (- m1 d1)))
    (when (> (abs diff) 1e-10)
      (raise (format "cp2-check-results: bad point at ~a, model ~a, data: ~a, diff ~a"
                     t1 m1 d1 diff))))
  (let* ((m2 (model t2))
         (d2 (mmax-fn t2))
         (diff (- m2 d2)))
    (when (> (abs diff) 1e-10)
      (raise (format "cp2-check-results: bad point at ~a, model ~a, data: ~a, diff ~a"
                     t2 m2 d2 diff)))))


;;.............................................................. CP3 API ....

;; Hold the parameters for the CP3 model
(struct: cp3
  ([cp : Flonum]
   [wprime : Flonum]
   [k : Flonum])
  #:transparent)

;; The results of fitting the model onto the empirical data.  Returns the T1
;; T2 and T3 time points where the fit happened as well as the cost for the
;; fit (smaller is better), see `evaluate-cost/cp3`
(struct: cp3-fit-results
  ([t1 : Flonum]
   [t2 : Flonum]
   [t3 : Flonum]
   [cost : Flonum])
  #:transparent)

;; Fit the CP3 model onto the MMAX-FN in the two ranges between NM-START,
;; NM-END, AN-START, AN-END and AE-START, AE-END.  Returns two values, a CP2
;; structure and a CP2-FIT-RESULTS structure.
;;
;; Good values for the NM range are 15 to 45 seconds, AN range are 120 - 300
;; and for the AE range 720 - 1200.
(: cp3-fit (->* (Mmax-Function Real Real Real Real Real Real)
                ((U False (-> Flonum Any)))
                (Values cp3 cp3-fit-results)))
(define (cp3-fit mmax-fn nm-start nm-end an-start an-end ae-start ae-end (progress-callback #f))
  (define-values (cp wprime k t1 t2 t3 cost)
    (cp3-search mmax-fn
                (real->double-flonum nm-start)
                (real->double-flonum nm-end)
                (real->double-flonum an-start)
                (real->double-flonum an-end)
                (real->double-flonum ae-start)
                (real->double-flonum ae-end)
                progress-callback))
  (values (cp3 cp wprime k) (cp3-fit-results t1 t2 t3 cost)))

;; Return a function for the CP2 model, that is, build the following function:
;;
;;     P(t) = CP + W' / (t - k)
;;
(: cp3-function (-> cp3 (-> Real Real)))
(define (cp3-function cp-params)
  (match-define (cp3 cp wprime k) cp-params)
  (lambda (t) (+ cp (/ wprime (- t k)))))

;; Calculate the maximum achievable power from the CP3 model parameters. This
;; is the value at P(0).
(: cp3-pmax (-> cp3 Real))
(define (cp3-pmax cp-params)
  (match-define (cp3 cp wprime k) cp-params)
  (+ cp (/ wprime (- k))))

;; See cp2-recovery-constant
(: cp3-recovery-constant (-> cp2 Flonum))
(define (cp3-recovery-constant cp-params)
  (match-define (cp3 cp wprime k) cp-params)
  (/ wprime cp))

;; Check the fit results against the mmax-fn -- this is a validation check for
;; the search and verifies that the model matches the data very closely at the
;; fit points t1, t2 and t3
(: cp3-check-results (-> cp3 cp3-fit-results Mmax-Function Void))
(define (cp3-check-results params fit-results mmax-fn)
  (match-define (cp3-fit-results t1 t2 t3 cost) fit-results)
  (define model (cp3-function params))
  (let* ((m1 (model t1))
         (d1 (mmax-fn t1))
         (diff (- m1 d1)))
    (when (> (abs diff) 1e-10)
      (raise (format "cp3-check-results: bad point at ~a, model ~a, data: ~a, diff ~a"
                     t1 m1 d1 diff))))
  (let* ((m2 (model t2))
         (d2 (mmax-fn t2))
         (diff (- m2 d2)))
    (when (> (abs diff) 1e-10)
      (raise (format "cp3-check-results: bad point at ~a, model ~a, data: ~a, diff ~a"
                     t2 m2 d2 diff))))
  (let* ((m3 (model t3))
         (d3 (mmax-fn t3))
         (diff (- m3 d3)))
    (when (> (abs diff) 1e-10)
      (raise (format "cp3-check-results: bad point at ~a, model ~a, data: ~a, diff ~a"
                     t3 m3 d3 diff)))))
