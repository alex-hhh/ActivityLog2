#lang racket/base
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2017 Alex Harsanyi (AlexHarsanyi@gmail.com)
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

(require racket/match
         racket/contract
         racket/math
         racket/flonum
         "data-frame/annealing.rkt")


;;................................................. Critical Power Model ....

;; The two-parameter CP model: P(t) = CP + W' / t, where
;;
;; CP is the critical power (if we are dealing with power) or velocity (if we
;; are dealing with speed).
;;
;; WPRIME (W') is the amount of work that can be done above CP before fatigue
;; forcing the athlete to drop below CP.  It is measured in joules if we are
;; dealing with power and meters if we are dealing with speed.
;;
;; FN is a function computing P(t) = CP + W' / t for the given parameters.
;;
;; T1 and T2 are the times on the Best-Avg curve that the CP model was based
;; on, they are present just for debugging purposes.
;;
;; COST is a measure of how well this CP model fits the best-avg data from
;; which it was derived.  Lower is better.  It is present just for debugging
;; purposes.
(struct cp2 (cp wprime fn t1 t2 cost) #:transparent)

(define (make-cp-fn cp wprime)
  (lambda (t) (fl+ cp (fl/ wprime  (exact->inexact t)))))

;; Validate cp2search structure parameters.  The search ranges should be
;; distinct and at least 60 seconds.
(define/contract (cp2search-guard anstart anend aestart aeend struct-name)
  (-> positive? positive? positive? positive? any/c any)
  (unless (and (< anstart anend aestart aeend)
               (> (- anend anstart) 60.0)
               (> (- aeend aestart) 60.0))
    (raise "cp2search parameters invalid"))
  (values anstart anend aestart aeend))

;; Hold search parameters for the CP solver (anaerobic and aerobic search
;; ranges)
(struct cp2search (anstart anend aestart aeend)
  #:transparent
  #:guard cp2search-guard)

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


;; Create a set of data points used to evaluate potential CP functions.  This
;; is done by evaluating BAVGFN at several points in the time intervals
;; [IVL1-START, IVL1-END] and [IVL2-START IVL2-END] and returning a list of
;; TIME, BAVGFN(TIME) pairs.
(define (make-test-data bavgfn ivl1-start ivl1-end ivl2-start ivl2-end)
  ;; NOTE: we take special care to return floating point values here

  ;; number of slices for each interval, higher values don't produce more
  ;; accurate results.
  (define slices 20.0)

  (let ((ivl1-step (/ (- ivl1-end ivl1-start) slices))
        (ivl2-step (/ (- ivl2-end ivl2-start) slices)))
    (for/list ([t (in-sequences (in-range ivl1-start ivl1-end ivl1-step)
                                (in-range ivl2-start ivl2-end ivl2-step))])
      (let ((et (exact->inexact (round t))))
        (cons et (bavgfn et))))))

;; Return a value determining how good this CP, WPRIME pair is.  Smaller
;; values are better.  TEST-DATA is a set of test data points as obtained from
;; MAKE-TEST-DATA.
(define (evaluate-cost cp wprime test-data)

  (define pdmodel (lambda (t) (fl+ cp (fl/ wprime t))))

  ;; Calculate a sum of squares of differences between values produced by
  ;; PDMODEL and the empirical points in TEST-DATA.  Two sums are returned,
  ;; one for values where our PDMODEL is above the test data and one for where
  ;; it is below.  We give different weights to these sums: if the test data
  ;; indicates higher power or speed than the model, this is considered bad,
  ;; while test data where the model is higher than the test data, this could
  ;; be plausible (the athlete didn't have a maximal effort at that time
  ;; point.

  (define-values
    (pcost ncost)
    (for/fold ([pcost 0.0] [ncost 0.0])
              ([item (in-list test-data)])
      (match-define (cons t value) item)
      (let* ((model (pdmodel t))
             (diff (fl- value model))
             (cost (fl* diff diff)))
        (if (>= model value)
            (values (fl+ pcost cost) ncost)
            (values pcost (fl+ ncost cost))))))

  (if (> ncost 0)
      (+ 1.0 (- 1.0 (exp (- ncost))))
      (- 1.0 (exp (- pcost)))))

;; Return a vector of work done by the BAVGFN at each time point between START
;; and END (a flvector is returned).
(define (compute-work bavgfn start end)
  (for/flvector #:length (add1 (- end start))
                ((t (in-range start (add1 end))))
    (* t (bavgfn t))))

;; Generate indexes for traversing a matrix of W x H dimensions in a zig-zag
;; pattern.  For example for a 3 x 3 matrix, the returned indexes will be 0 0,
;; 1 0, 0 1, 0 2, 1 1, 2 0, 2 1, 1 2, and 2 2.
;;
;; This function actually returns a generator function, which when evaluated
;; returns the next index as a pair of values.
(define (make-index-generator w h)
  (unless (> w 0)
    (error "with is too small"))
  (unless (> h 0)
    (error "height is too small"))

  (define done? #f)
  (define x-lim (sub1 w))
  (define y-lim (sub1 h))

  ;; Initial value for x, y, x-dir, y-dir are set up such that the first
  ;; advance brings them to 0, 0
  (define x -1)
  (define y 1)
  (define x-dir 1)
  (define y-dir -1)

  (define (change-direction)
    (set! x-dir (- x-dir))
    (set! y-dir (- y-dir)))

  (lambda ()

    ;; Advance X and Y
    (set! x (+ x x-dir))
    (set! y (+ y y-dir))

    ;; Check if an index reached the edge of the area, and therefore we need
    ;; to adjust the coordinates to bring the indexes back into the area and
    ;; change the scanning direction.
    (cond ((eqv? x -1)
           (set! x 0)
           (when (eqv? y h)
             (set! y y-lim)
             (set! x (add1 x)))
           (change-direction))
          ((eqv? y -1)
           (set! y 0)
           (when (eqv? x w)
             (set! x x-lim)
             (set! y (add1 y)))
           (change-direction))
          ((eqv? x w)
           (set! x x-lim)
           (set! y (add1 (add1 y)))
           (change-direction))
          ((eqv? y h)
           (set! y y-lim)
           (set! x (add1 (add1 x)))
           (change-direction)))
    (if done?
        (values #f #f)
        (begin
          (set! done? (and (eqv? x x-lim) (eqv? y y-lim)))
          (values x y)))))

;; Find CP and W' by an exhaustive search for a two maximal efforts, one in
;; [ANSTART, ANEND] (the anaerobic range) and one in [AESTART, AEEND] (the
;; aerobic range), which are passed in as SEARCH-PARAMS (an CP2SEARCH
;; instance).
;;
;; Return a CP2 structure with the best values (as determined by the
;; EVALUATE-COST function
;;
;; NOTE: good values for the ranges are [120, 300] for the anaerobic range and
;; [420, 1200] for the aerobic range (at least this is what GoldenCheetah uses
;; as default for the search.
;;
;; CP-PRECISION indicates the required precision for the CP value (as number
;; of digits), while WPRIME-PRECISION does the same for the WPRIME.  Good
;; values seem to be 1 and -1 if we are working with power and 3 and 1 if we
;; are dealing with speed.
;;
;; NOTE: this function is slow, it takes about 0.5 to 1 second to run for
;; reasonable values.
;;
(define (search-best-cp/exhausive
         bavgfn search-params
         #:cp-precision cp-precision
         #:wprime-precision wprime-precision)

  (define anstart (cp2search-anstart search-params))
  (define anend (cp2search-anend search-params))
  (define aestart (cp2search-aestart search-params))
  (define aeend (cp2search-aeend search-params))


  ;; Pre-compute the work values in the anaerobic and aerobic ranges
  (define anwork (compute-work bavgfn anstart anend))
  (define aework (compute-work bavgfn aestart aeend))

  (define cpmult (exact->inexact (expt 10 cp-precision)))
  (define wprimemult (exact->inexact (expt 10 wprime-precision)))

  (define test-data (make-test-data bavgfn anstart anend aestart aeend))

  ;; map CP, W' candidates we already considered, to avoid re-evaluating the
  ;; cost unnecessarily (this is the most expensive operation of the search)
  (define considered-candidates (make-hash))

  ;; A function to determine the next time points we look at.
  (define next-index (make-index-generator (add1 (- anend anstart))
                                           (add1 (- aeend aestart))))

  (define best #f)                   ; best CP2 we found so far
  ;; Base time difference between the intervals, used to avoid some
  ;; computations of t1 and t2 from the indexes.
  (define tdiff (->fl (- aestart anstart)))

  (define (search)
    (define-values (an-index ae-index) (next-index))
    (when (and an-index ae-index)
      (let* ((w1 (flvector-ref anwork an-index))
             (w2 (flvector-ref aework ae-index))
             (cp (/ (fl- w2 w1)
                    (fl+ tdiff (->fl (- ae-index an-index)))))
             (wprime (fl- w1 (fl* cp (->fl (+ anstart an-index)))))
             ;; Get integer versions of CP and W' to be used as hash indexes
             (cpi (exact-round (fl* cp cpmult)))
             (wprimei (exact-round (fl* wprime wprimemult))))
        (unless (hash-ref considered-candidates (cons cpi wprimei) #f)
          ;; We haven't considered this candidate yet
          (let ((cost (evaluate-cost cp wprime test-data)))
            (when (or (not best) (< cost (cp2-cost best)))
              (set! best (cp2 cp wprime (make-cp-fn cp wprime) (+ anstart an-index) (+ aestart ae-index) cost))
              (hash-set! considered-candidates (cons cpi wprimei) best)))))
      (search)))

  (search)
  best)

;; Determine CP params using a simulated-anealing algorithm.  This is
;; algorithm is about 4 times faster than SEARCH-BEST-CP/EXHAUSIVE, but since
;; it is probabilistic it gives slightly different results each time it is
;; run.  The results are within 00:01 min/km or 1 watt or less, but sometimes
;; visible.  It could use some refinement.
(define (search-best-cp/probabilistic bavgfn search-params)

  (define anstart (cp2search-anstart search-params))
  (define anend (cp2search-anend search-params))
  (define aestart (cp2search-aestart search-params))
  (define aeend (cp2search-aeend search-params))

  (define test-data (make-test-data bavgfn anstart anend aestart aeend))

  (define (calculate-cp t1 t2)
    (let* ((p1 (bavgfn t1))
           (p2 (bavgfn t2))
           (w1 (* p1 t1))
           (w2 (* p2 t2))
           (cp (/ (- w2 w1) (- t2 t1)))
           (wprime (- w1 (* cp t1)))
           (cost (evaluate-cost cp wprime test-data)))
      (cp2 cp wprime (make-cp-fn cp wprime) t1 t2 cost)))

  (define anrange (/ (- anend anstart) 2))
  (define aerange (/ (- aeend aestart) 2))

  (define (ntime t1 adj tmin tmax)
    (let* ((nadj (if (< adj 1.0) (sgn adj) adj))
           (nt (exact-round (+ t1 adj))))
      (max tmin (min tmax nt))))

  (define (neighbour state temp)
    ;; Find a neighbour for STATE.  New search positions in the anaerobic and
    ;; aerobic intervals are found, the smaller the temperature the lower the
    ;; range for the new values.
    (define r1 (* temp (- (* 2 (random)) 1)))
    (define r2 (* temp (- (* 2 (random)) 1)))
    (define t1 (ntime (cp2-t1 state) (* r1 anrange) anstart anend))
    (define t2 (ntime (cp2-t2 state) (* r2 aerange) aestart aeend))
    (calculate-cp t1 t2))

  (annealing
   ;; We start in the middle of the range
   #:initial (calculate-cp (/ (+ anstart anend) 2) (/ (+ aestart aeend) 2))
   ;; Our goal is the cost function
   #:goal cp2-cost
   #:neighbour neighbour
   #:iterations 5000))

(provide
 (struct-out cp2)
 (struct-out cp2search)
 make-cp-fn
 search-best-cp/exhausive
 search-best-cp/probabilistic)
