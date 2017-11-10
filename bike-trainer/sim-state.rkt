#lang racket/base
(require racket/contract
         racket/math
         racket/class
         racket/match
         "../rkt/map-util.rkt"
         "../rkt/data-frame.rkt")

(provide (struct-out sim-state))

;; Hold information about the state of the simulation -- this includes a GPS
;; track, a TCP output port to control the trainer as well as the current
;; position and slope on the track, along with timing information.
;;
;; The state of the simulation is "read-only" and all the update functions
;; (see below) produce an "updated" state based on an existing one.
(struct sim-state
  (
   ;; data frame containing GPS track
   df
   ;; output port on which we send control commands to the trainer
   control
   ;; is the simulation paused?  if the simulation is paused,
   ;; `sim-state-update` will not advance the distance and the `current-slope`
   ;; will be set to 0.
   paused?
   ;; total time (milliseconds) of the simulation, does not include pauses
   timer
   ;; distance covered so far (does not update if paused), in meters
   distance
   ;; current timestamp of the simulation (milliseconds)
   now
   ;; slope value we send to the trainer, will be set to 0 if paused.  See
   ;; `sim-state-update` on how this is updated.
   current-slope
   ;; timestamp when we sent the last slope value to the trainer.
   current-slope-ts
   )
  #:transparent)

(provide/contract
 (sim-state-make-initial (-> sim-state?))
 (sim-state-delta (-> sim-state? (and/c real? (or/c zero? positive?))))
 (sim-state-update (-> sim-state? real? (and/c real? positive?) sim-state?))
 (sim-state-set-paused (-> sim-state? boolean? sim-state?))
 (sim-state-set-data-frame (-> sim-state? (is-a?/c data-frame%) sim-state?))
 (sim-state-set-control (-> sim-state? output-port? sim-state?))
 (sim-state-reset (-> sim-state? sim-state?))
 (sim-state-current-position (-> sim-state? (or/c #f (vector/c real? real?)))))


;;............................................................ utilities ....

;; Add a map-point data series to the data frame DF.  map-points represent a
;; location in normalized coordinates (0..1).  These are used to interpolate a
;; position by `lookup-position`
(define/contract (add-map-points df)
  (-> (is-a?/c data-frame%) any/c)
  (send df add-derived-series
        "map-point"
        '("lat" "lon")
        (lambda (val)
          (if val
              (match-let (((vector lat lon) val))
                (lat-lon->map-point lat lon))
              #f))))

;; Lookup a GPS position in the data frame DF at distance DST.  The data frame
;; is assumed to have "lat", "lon", a "dst" and a "map-point" (see
;; `add-map-points`) data series.  If DST does not fall on an exact item in
;; the data series, a location is interpolated between two adjacent GPS
;; positions.  If DST is outside the range if the dst series, the first or
;; last position is returned.
(define/contract (lookup-position df dst)
  (-> (is-a?/c data-frame%) real? (vector/c real? real?))
  (define index (send df get-index "dst" dst))

  (cond ((<= index 0)
         (send df ref* 0 "lat" "lon"))
        ((>= index (send df get-row-count))
         (send df ref* (sub1 (send df get-row-count)) "lat" "lon"))
        (#t
         (let* ((pdst (send df ref (sub1 index) "dst"))
                (adst (send df ref index "dst"))
                (prev-pos (send df ref (sub1 index) "map-point"))
                (next-pos (send df ref index "map-point"))
                (factor (/ (- dst pdst) (- adst pdst)))
                (pos (map-point
                      (+ (* factor (map-point-x next-pos))
                         (* (- 1 factor) (map-point-x prev-pos)))
                      (+ (* factor (map-point-y next-pos))
                         (* (- 1 factor) (map-point-y prev-pos))))))
           (let-values (((lat lon) (map-point->lat-lon pos)))
             (vector lat lon))))))

;; Lookup the slope (slope) in the data frame DF at distance DST.  The data
;; frame is assumed to have the "grade" and "dst" series.  If DST is outside
;; the range if the dst series, the first or last slope value is returned.
(define/contract (lookup-slope df dst)
  (-> (is-a?/c data-frame%) real? real?)
  (define index (send df get-index "dst" dst))
  (cond ((>= index (send df get-row-count))
         (send df ref (sub1 (send df get-row-count)) "grade"))
        ((< index 0)
         (send df ref 0 "grade"))
        (#t
         (send df ref index "grade"))))


;;..................................................... simulation state ....

;; Create an initial sim-state object.  It has no data frame and no control
;; port.
(define (sim-state-make-initial)
  (sim-state #f #f #t 0 0 (current-milliseconds) 0 (current-milliseconds)))

;; Send the current slope value to the trainer to "enact" it.  Will do nothing
;; if there is no control port set in STATE.
(define (send-current-slope-to-trainer state)
  (let ((ctl (sim-state-control state))
        (slope (sim-state-current-slope state)))
    (when ctl
      (write-string (format "SET-SLOPE ~a~%" slope) ctl)
      (flush-output ctl))))

;; Return the amount of time elapsed since STATE was last updated.
(define (sim-state-delta state)
  (- (current-milliseconds) (sim-state-now state)))

;; Update the state of the simulation based on the bike traveling at SPEED
;; (meters/second) for DELTA milliseconds.  A new state object is returned
;; with updated distance covered as well as an updated slope value at the
;; current position on the track.  Will do nothing if the simulation is
;; paused.
(define (sim-state-update state speed delta)

  ;; This is the slope of the GPS track at the current position.  It is not
  ;; the slope displayed or set on the trainer.  See
  ;; `update-trainer-resistance` for more details.
  (define (desired-slope distance)
    (let ((df (sim-state-df state)))
      (if df
          (lookup-slope df distance)
          #f)))

  ;; Maximum amount of slope change we are allowed to send to the trainer
  (define max-slope-change 0.2)
  
  ;; Time interval (in milliseconds before sending a new set-slope command
  (define slope-interval 2000)

  ;; Return #t if we can update the slope value.  We are limited by the rate
  ;; at which we can sent updates to the trainer, which is `slope-interval`
  (define (can-update-slope?)
    (let ((delta (- (current-milliseconds) (sim-state-current-slope-ts state))))
      (> delta slope-interval)))

  ;; Update the ACTUAL slope to the DESIRED value.  Returns an updated value
  ;; that is closer to the DESIRED, keeping the restrictions we have on the
  ;; trainer.  Returns ACTUAL if the slope value is not changed.
  (define (update-slope actual desired)
    (if (and desired actual (can-update-slope?))
        (let ((delta (- desired actual)))
              (if (> (abs delta) 0)
                  (+ actual
                     (* (sgn delta) (min max-slope-change (abs delta))))
                  actual))
            actual))

  (if (sim-state-paused? state)
      (struct-copy sim-state state
                   (now (+ (sim-state-now state) delta)))
      (let* ((ntimer (+ (sim-state-timer state) delta))
             (ndistance (+ (sim-state-distance state)
                           (* speed (/ delta 1000.0))))
             (nnow (+ (sim-state-now state) delta))
             (desired (desired-slope ndistance))
             (actual (sim-state-current-slope state))
             (nslope (update-slope actual desired)))
        (if (equal? nslope actual)
            ;; Slope value did not change
            (struct-copy sim-state state
                         (timer ntimer)
                         (distance ndistance)
                         (now nnow))
            ;; Slope did change, we also need to send the new value to the
            ;; trainer.
            (let ((nstate (struct-copy sim-state state
                                       (timer ntimer)
                                       (distance ndistance)
                                       (now nnow)
                                       (current-slope nslope)
                                       (current-slope-ts (current-milliseconds)))))
              (send-current-slope-to-trainer nstate)
              nstate)))))

;; Pause or resume the simulation, will also set the slope to 0 if the
;; simulation is paused and send it to the trainer so the wheel is not locked
;; while paused.  STATE is not changed, instead a new sim-state object is
;; returned.
(define (sim-state-set-paused state flag)
  (let* ((nslope (if flag 0 (sim-state-current-slope state)))
         (nstate (struct-copy sim-state state
                              (paused? flag)
                              (current-slope nslope))))
    (when flag             ; if we are pausing, release the trainer resistance
      (send-current-slope-to-trainer nstate))
    nstate))

;; Set a new data frame from which the GPS data track will be used.  STATE is
;; not changed, instead a new sim-state object is returned.
(define (sim-state-set-data-frame state df)
  (add-map-points df)               ; needed by lookup-position
  (let ((nstate (struct-copy sim-state state
                             (df df)
                             (paused? #t)
                             (timer 0)
                             (distance 0)
                             (current-slope 0)
                             (now (current-milliseconds)))))
    (send-current-slope-to-trainer nstate)
    nstate))

;; Set the control port for the simulation, this is a TCP connection on which
;; we can send commands to the trainer.  STATE is not changed, instead a new
;; sim-state object is returned.
(define (sim-state-set-control state port)
  (let ((nstate (struct-copy sim-state state
                             (control port)
                             (current-slope 0))))
    (send-current-slope-to-trainer nstate)
    nstate))

;; Reset the simulation to the start position on the GPS track.  STATE is not
;; changed, instead a new sim-state object is returned.
(define (sim-state-reset state)
  (let ((nstate (struct-copy sim-state state
                             (paused? #t)
                             (timer 0)
                             (distance 0)
                             (current-slope 0)
                             (now (current-milliseconds)))))
    (send-current-slope-to-trainer nstate)
    nstate))

;; Return the current location (lat, lon) on the track based on the distance
;; covered so far.
(define (sim-state-current-position state)
  (let ((df (sim-state-df state))
        (distance (sim-state-distance state)))
    (if df
        (lookup-position df distance)
        #f)))
