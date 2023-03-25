#lang racket/base

;; aerolab.rkt -- Estimate rolling resistance coefficient (Crr) and drag area
;; coefficient (CdA) from power and speed data series.
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2023 Alex Harsányi <AlexHarsanyi@gmail.com>
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

;;; Commentary

;; This file contains routines for estimating the Coefficient of Rolling
;; Resistance (Crr) and Coefficient of Drag Area (CdA) from the power and
;; speed series from an activity.  The calculations are based on the
;; presentation "Estimating CdA with a power meter" presentation by R Chung.
;;
;; This file contains the estimating functionality, the UI is implemented in
;; the "inspect-aerolab.rkt" file.

(require data-frame
         data-frame/private/annealing
         geoid/geodesy
         math/flonum
         math/statistics
         racket/match
         racket/math
         racket/place
         "../utilities.rkt")

(struct exn:fail:aerolab exn:fail (kind param value))

(define (raise-aerolab-exn kind param value message)
  (exn:fail:aerolab message (current-continuation-marks) kind value))


;;............................................................ Annealing ....

(define (prepare-aerolab-data-for-annealing
         df
         [start 0]
         [stop (df-row-count df)])
  (unless (df-contains? df "elapsed")
    ;; Something is seriously wrong -- the elapsed series should be present in
    ;; all activities...
    (raise-aerolab-exn
     'missing-series "elapsed"
     "elapsed series missing from data-frame"))
  (define c (- stop start))
  (define dt (make-shared-flvector c +nan.0))
  (for/fold ([prev #f])
            ([e (in-data-frame df "elapsed" #:start start #:stop stop)]
             [i (in-naturals)])
    (when (and (rational? prev) (rational? e))
      (flvector-set! dt i (real->double-flonum (- e prev))))
    e)
  (unless (df-contains? df "spd")
    (raise-aerolab-exn
     'missing-series "spd"
     "spd series missing from data-frame"))
  (define spd (make-shared-flvector c +nan.0))
  (define acc (make-shared-flvector c +nan.0))
  (for/fold ([prev-e #f]
             [prev-s #f])
            ([(e s) (in-data-frame df "elapsed" "spd" #:start start #:stop stop)]
             [i (in-naturals)])
    (when (rational? s)
      (flvector-set! spd i (real->double-flonum s)))
    (when (and (rational? prev-e) (rational? prev-s) (rational? e))
      (flvector-set! acc i (real->double-flonum (/ (- s prev-s) (- e prev-e)))))
    (values e s))
  (unless (df-contains? df "pwr")
    (raise-aerolab-exn
     'missing-series "pwr"
     "pwr series missing from data-frame"))
  (define pwr (make-shared-flvector c +nan.0))
  (for ([p (in-data-frame df "pwr" #:start start #:stop stop)]
        [i (in-naturals)])
    (when (rational? p)
      (flvector-set! pwr i (real->double-flonum p))))
  (define bearing
    (if (df-contains? df "lat" "lon")
        (let ([bearing (make-shared-flvector c +nan.0)])
          (for/fold ([prev-lat #f]
                     [prev-lon #f])
                    ([(lat lon) (in-data-frame df "lat" "lon" #:start start #:stop stop)]
                     [i (in-naturals)])
            (when (and (rational? prev-lat)
                       (rational? prev-lon)
                       (rational? lat)
                       (rational? lon))
              (let ([b (final-bearing prev-lat prev-lon lat lon)])
                (flvector-set! bearing i (real->double-flonum b))))
            (values lat lon))
          bearing)
        #f))
  (define alt
    (cond ([df-contains? df "calt"]
           (let ([alt (make-shared-flvector c +nan.0)])
             (for ([a (in-data-frame df "calt" #:start start #:stop stop)]
                   [i (in-naturals)])
               (when (rational? a)
                 (flvector-set! alt i (real->double-flonum a))))
             alt))
          ([df-contains? df "alt"]
           (let ([alt (make-shared-flvector c +nan.0)])
             (for ([a (in-data-frame df "alt" #:start start #:stop stop)]
                   [i (in-naturals)])
               (when (rational? a)
                 (flvector-set! alt i (real->double-flonum a))))
             alt))
          (#t #f)))

  (hash
   'dt dt
   'spd spd
   'acc acc
   'pwr pwr
   'bearing bearing
   'alt alt))

;; Check and raise errors if any user supplied parameters to the annealing
;; function are missing or contain invalid values.
(define (validate-user-parameters data)

  (define iterations (hash-ref data 'iterations (lambda () 2000)))
  (define restart-after (hash-ref data 'restart-after (lambda () 400)))
  (define max-net-elevation-gain (hash-ref data 'max-net-elevation-gain (lambda () 5.0)))

  ;; Parameters
  (define total-weight
    (hash-ref data 'total-weight
              (lambda () (raise-argument-error 'total-weight "not present" #f))))
  (define air-density
    (hash-ref data 'air-density
              (lambda () (raise-argument-error 'air-density "not present" #f))))
  (define wind-speed (hash-ref data 'wind-speed (lambda () 0.0)))
  (define wind-direction (hash-ref data 'wind-direction (lambda () 0.0)))
  (define crr (hash-ref data 'crr (lambda () 0.003)))
  (define cda (hash-ref data 'cda (lambda () 0.3)))

  ;; Data samples
  (define spd
    (hash-ref data 'spd
              (lambda () (raise-argument-error 'spd "not present" #f))))
  (define bearing
    (hash-ref data 'bearing
              (lambda () #f))) ; bearing is optional
  (define dt
    (hash-ref data 'dt
              (lambda () (raise-argument-error 'dt "not present" #f))))
  (define pwr
    (hash-ref data 'pwr
              (lambda () (raise-argument-error 'pwr "not present" #f))))
  (define acc
    (hash-ref data 'acc
              (lambda () (raise-argument-error 'acc "not present" #f))))
  ;; altitude can be made optional, but this requires defining another cost
  ;; function (cost between laps) and the user would have to specify the
  ;; number of laps -- for now, we make it mandatory
  (define alt
    (hash-ref data 'alt
              (lambda ()
                (raise-argument-error 'alt "not present" #f))))

  (unless (exact-nonnegative-integer? iterations)
    (raise-argument-error 'iterations "exact-nonnegative-integer?" iterations))
  (unless (or (eq? #f restart-after)
              (exact-nonnegative-integer? restart-after))
    (raise-argument-error 'restart-after "(or/c #f exact-nonnegative-integer?)" restart-after))
  (unless (positive? max-net-elevation-gain)
    (raise-argument-error 'max-net-elevation-gain "positive?" max-net-elevation-gain))
  (unless (and (real? total-weight) (positive? total-weight))
    (raise-argument-error 'total-weight "positive?" total-weight))
  (unless (and (real? air-density) (positive? air-density))
    (raise-argument-error 'air-density "positive?" air-density))
  (unless (or (and (real? wind-speed) (or (positive? wind-speed) (zero? wind-speed)))
              (and (pair? wind-speed)
                   (let ([low-wind-speed (car wind-speed)])
                     (and (real? low-wind-speed) (or (positive? low-wind-speed) (zero? low-wind-speed))))
                   (let ([high-wind-speed (car wind-speed)])
                     (and (real? high-wind-speed) (or (positive? high-wind-speed) (zero? high-wind-speed))))))
    (raise-argument-error 'wind-speed "(or/c (or/c positive? zero?) (cons/c (or/c positive? zero?) (or/c positive? zero?)))" wind-speed))
  (unless (or (and (real? wind-direction) (positive? wind-direction))
              (and (pair? wind-direction)
                   (let ([low-wind-direction (car wind-direction)])
                     (and (real? low-wind-direction)))
                   (let ([high-wind-direction (car wind-direction)])
                     (and (real? high-wind-direction)))))
    (raise-argument-error 'wind-direction "(or/c real? (cons/c real? real?))" wind-direction))
  (unless (or (and (real? crr) (positive? crr))
              (and (pair? crr)
                   (let ([low-crr (car crr)])
                     (and (real? low-crr) (positive? low-crr)))
                   (let ([high-crr (car crr)])
                     (and (real? high-crr) (positive? high-crr)))))
    (raise-argument-error 'crr "(or/c positive? (cons/c positive? positive?))" crr))
  (unless (or (and (real? cda) (positive? cda))
              (and (pair? cda)
                   (let ([low-cda (car cda)])
                     (and (real? low-cda) (positive? low-cda)))
                   (let ([high-cda (car cda)])
                     (and (real? high-cda) (positive? high-cda)))))
    (raise-argument-error 'cda "(or/c positive? (cons/c positive? positive?))" cda))

  ;; Data series must be all flvectors of the same length (some of them are
  ;; optional, in which case they can be #f)
  (unless (flvector? spd)
    (raise-argument-error 'spd "flvector?" spd))
  (define sample-count (flvector-length spd))
  (unless (or (eq? bearing #f)
              (and (flvector? bearing)
                   (= sample-count (flvector-length bearing))))
    (raise-argument-error
     'bearing
     (format "(and/c flvector? (= length ~a)" sample-count)
     bearing))
  (unless (and (flvector? dt)
               (= sample-count (flvector-length dt)))
    (raise-argument-error
     'dt
     (format "(and/c flvector? (= length ~a)" sample-count)
     dt))
  (unless (and (flvector? pwr)
               (= sample-count (flvector-length pwr)))
    (raise-argument-error
     'pwr
     (format "(and/c flvector? (= length ~a)" sample-count)
     pwr))
  (unless (and (flvector? acc)
               (= sample-count (flvector-length acc)))
    (raise-argument-error
     'acc
     (format "(and/c flvector? (= length ~a)" sample-count)
     acc))
  (unless (or (eq? alt #f)
              (and (flvector? alt)
                   (= sample-count (flvector-length alt))))
    (raise-argument-error
     'alt
     (format "(and/c flvector? (= length ~a)" sample-count)
     alt)))

(define (calculate-air-speed spd bearing wind-speed wind-direction air-speed-out)
  (for ([s (in-flvector spd)]
        [b (in-flvector bearing)]
        [i (in-naturals)])
    (let* ([apparent-wdir (+ b wind-direction)]
           [apparent-wspeed (* wind-speed (cos (degrees->radians apparent-wdir)))])
      ;; NOTE regarding use of max: we assume that the wind speed will never
      ;; push a cyclist backwards.  We expect Aerolab tests to be carried out
      ;; in light wind conditions.
      (flvector-set! air-speed-out i (exact->inexact (max 0.0 (- s apparent-wspeed)))))))

(define (calculate-valt0 dt pwr spd acc air-speed crr cda total-weight air-density valt0-out)
  (define g 9.807)                      ; Earth Gravitational Constant (m/s^2)
  (define altitude 0.0)
  (for ([d (in-flvector dt)]
        [p (in-flvector pwr)]
        [s (in-flvector spd)]
        [a (in-flvector acc)]
        [as (in-flvector air-speed)]
        [i (in-naturals)])
    (define slope
      (let* ([term0 (/ p (* total-weight g s))]
             [term1 crr]
             [term2 (/ a g)]
             [term3 (/ (* cda air-density (* as as))
                       (* 2.0 total-weight g))])
        (- term0 term1 term2 term3)))
    (define dh (* slope (* s d)))
    (when (rational? dh)
      (set! altitude (+ altitude dh)))
    (flvector-set! valt0-out i altitude)))

(define (find-best-initial-altitude alt valt0)

  (define a (flvector-ref alt 0))

  (define (goal p)
    ;; Calculate the sum-of-squares of the difference between the actual
    ;; altitude (calt) and the virtual altitude (valt0 offset by the initial
    ;; altitude)
    (define initial-altitude (aparam-value p))
    (for/fold ([cost 0.0])
              ([a (in-flvector alt)]
               [v (in-flvector valt0)])
      (define delta (- a (+ v initial-altitude)))
      (+ cost (* delta delta))))

  (define-values (state cost)
    (annealing
     #:initial (make-aparam "ialt" #:min (- a 20) #:max (+ a 20))
     #:neighbour transition-aparam
     #:goal goal
     #:iterations 100))

  (values (aparam-value state) cost))

;; NOTE: we assume data is already validated, see `validate-user-parameters`
(define (find-aerolab-params
         data
         [progress-callback (lambda (event t cost state) (void))])

  (define iterations (hash-ref data 'iterations (lambda () 2000)))
  (define restart-after (hash-ref data 'restart-after (lambda () 500)))
  (define max-net-elevation-gain (hash-ref data 'max-net-elevation-gain (lambda () 5.0)))

  ;; Parameters
  (define total-weight (hash-ref data 'total-weight))
  (define air-density (hash-ref data 'air-density))
  (define wind-speed (hash-ref data 'wind-speed (lambda () 0.0)))
  (define wind-direction (hash-ref data 'wind-direction (lambda () 0.0)))
  (define crr (hash-ref data 'crr (lambda () 0.003)))
  (define cda (hash-ref data 'cda (lambda () 0.3)))

  ;; Data samples
  (define spd (hash-ref data 'spd))
  (define bearing (hash-ref data 'bearing (lambda () #f))) ; bearing is optional
  (define dt (hash-ref data 'dt))
  (define pwr (hash-ref data 'pwr))
  (define acc (hash-ref data 'acc))
  (define alt (hash-ref data 'alt))     ; TODO: make altitude optional

  ;; These are intermediate and used by the search process
  (define air-speed (make-flvector (flvector-length spd) +nan.0))
  (define valt0 (make-flvector (flvector-length spd) +nan.0))

  ;; We don't have either wind or bearing, air speed is the same as speed.
  (define skip-air-speed-calculation?
    (or (not bearing)
        (and (rational? wind-speed) (zero? wind-speed))))

  (when skip-air-speed-calculation?
    (flvector-copy! air-speed 0 spd))

  (define initial-state
    (list
     +inf.0                               ; initial cost
     0                                    ; initial altidude
     (for/hash ([v (list wind-speed wind-direction crr cda)]
                [n (list "wind-speed" "wind-direction" "crr" "cda")]
                #:when (pair? v))
       (values n (make-aparam n #:min (car v) #:max (cdr v))))))

  (define (ppstate state)
    (match-define (list cost initial-alt group) state)
    (define h
      (for/hash ([v (in-hash-values group)])
        (define m (aparam-meta v))
        (values (ameta-name m) (aparam-value v))))
    (hash-set*
     h
     "initial-altitude" initial-alt
     "cost" cost))

  (define (goal state)
    (list-ref state 0))

  (define (neighbour state t)
    (match-define (list cost initial-alt group) state)
    (define tgroup (transition-aparam-group group t))

    (define (ref n)
      (define v (hash-ref tgroup n (lambda () #f)))
      (and v (aparam-value v)))

    (unless skip-air-speed-calculation?
      (calculate-air-speed
       spd
       bearing
       (or (ref "wind-speed") wind-speed)
       (or (ref "wind-direction") wind-direction)
       air-speed))

    (calculate-valt0
     dt
     pwr
     spd
     acc
     air-speed
     (or (ref "crr") crr)
     (or (ref "cda") cda)
     total-weight
     air-density
     valt0)

    (define net-elevation-gain
      (abs (- (flvector-ref valt0 0)
              (flvector-ref valt0 (sub1 (flvector-length valt0))))))

    (if (< net-elevation-gain max-net-elevation-gain)
        (let-values ([(initial-altitude icost) (find-best-initial-altitude alt valt0)])
          (list icost initial-altitude tgroup))
        ;; Reject states with large virtual elevation gains, avoid calculating a
        ;; meaningless initial altitude for them.  Also +inf.0 ensures that
        ;; these states are always rejected.
        (list +inf.0 0 tgroup)))

  (define-values (state cost)
    (annealing
     #:initial initial-state
     #:neighbour neighbour
     #:progress-callback progress-callback
     #:goal goal
     #:iterations iterations
     #:restart-after restart-after))

  (values (ppstate state) cost))

(define (aerolab-worker i/o)
  (define id (place-channel-get i/o))
  (define data (place-channel-get i/o))
  (define run-count (place-channel-get i/o))
  (with-handlers
    ((exn? (lambda (e) (place-channel-put i/o e))))
    (let/ec return
      (for ([index (in-range run-count)])
        ;; NOTE: the annealing function sends a last 'final-state event with the
        ;; best state, so we ignore the result of `find-aerolab-params` since
        ;; they were already received as a callback
        (define-values (state _cost)
          (find-aerolab-params
           data
           (lambda (_event t _cost _state)
             (place-channel-put
              i/o
              ;; Report progress, t goes form 1 to 0 for the current run.
              (list id index (- 1 t)))
             (define exit-request? (sync/timeout 0 i/o))
             (when exit-request?
               (return #f)))))
        ;; Report the final state, note that the cost is addded as the "cost"
        ;; key to the state.
        (place-channel-put i/o state)))))

(define (make-aerolab-workers worker-count data run-count)
  (define-values (q r) (quotient/remainder run-count worker-count))
  (for/fold ([workers '()]
             [extra-runs r]
             #:result workers)
            ([id (in-range worker-count)])
    (define pl (place input (aerolab-worker input)))
    (place-channel-put pl id)
    (place-channel-put pl data)
    (place-channel-put pl (if (zero? extra-runs) q (add1 q)))
    (values (cons pl workers) (if (zero? extra-runs) 0 (sub1 extra-runs)))))

(define (find-aerolab-params/many
         data
         #:run-count [run-count 20]
         #:worker-count [worker-count (max 1 (sub1 (processor-count)))]
         #:progress-callback [progress-callback (lambda (_percent-complete _state) #t)])
  (validate-user-parameters data) ; will raise exceptions if something is not OK
  (define workers (make-aerolab-workers worker-count data run-count))

  (define (maybe-request-exit continue?)
    (unless continue?
      (for ([worker (in-list workers)])
        (place-channel-put worker #t))))

  (let loop ([params (hash)]
             [percent-complete 0]
             [progress (hash)]
             [remaining (map place-dead-evt workers)])
    (if (null? remaining)
        params
        ;; TODO: we currently wait until all workers have exited -- this can
        ;; be a problem if we miss a notification, or one of the workers
        ;; enters an infinite loop, in which case this can hang forever.  We
        ;; need to implement some kind of a timeout.
        (let ([evt (apply sync (append workers remaining))])
          (cond [(member evt remaining) ; A worker has exited
                 (loop params percent-complete progress (remove evt remaining))]
                [(exn? evt)       ; A worker raised an exception
                 (dbglog "find-aerolab-params/many: exception caught: ~a" evt)
                 (loop params percent-complete progress remaining)]
                [(list? evt)           ; A worker sent us progress information
                 (match-define (list worker-id run-id pct) evt)
                 (define nprogres (hash-set progress (cons worker-id run-id) pct))
                 (define completion (for/sum ([v (in-hash-values nprogres)]) v))
                 (define npercent-complete (/ completion run-count))
                 (define continue? (progress-callback npercent-complete params))
                 (maybe-request-exit continue?)
                 (loop params npercent-complete nprogres remaining)]
                [(hash? evt)            ; A worker sent us a result
                 (define nparams
                   (for/hash ([(k v) (in-hash evt)])
                     (define stats (hash-ref params k (lambda () empty-statistics)))
                     (values k (update-statistics stats v))))
                 (define continue? (progress-callback percent-complete params))
                 (maybe-request-exit continue?)
                 (loop nparams percent-complete progress remaining)])))))

(provide
 prepare-aerolab-data-for-annealing
 validate-user-parameters
 find-aerolab-params
 find-aerolab-params/many)
