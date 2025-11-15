
#lang racket/base

;; session-df.rkt --create a data-frame% from a session's trackpoints, plus
;; utilities to plot graphs.
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2016, 2018-2025 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require data-frame
         data-frame/private/bsearch
         data-frame/private/rdp-simplify
         db/base
         geoid/geodesy
         math/statistics
         plot/no-gui
         plot/utils
         racket/async-channel
         racket/class
         racket/contract
         racket/draw
         racket/list
         racket/match
         racket/math
         racket/runtime-path
         racket/sequence
         racket/vector
         "../dbutil.rkt"
         "../fmt-util.rkt"
         "../models/aerobic-decoupling.rkt"
         "../models/cp-util.rkt"
         "../models/critical-power.rkt"
         "../models/gap.rkt"
         "../models/grade-series.rkt"
         "../models/sport-zone.rkt"
         "../models/wbal-series.rkt"
         "../sport-charms.rkt"
         "../utilities.rkt"
         "series-metadata.rkt"
         "shifting.rkt"
         "xdata-series.rkt")

;; Check if R is a valid plot range, as produced by `get-plot-y-range` and
;; used by several other functions.  This is used in the `y-range/c` contract.
(define (valid-range? r)
  (and (pair? r)
       (let ((a (car r))
             (b (cdr r)))
         (and (real? a) (not (nan? a)) (not (infinite? a))
              (real? b) (not (nan? b)) (not (infinite? b))
              (> b a)))))

(define y-range/c (flat-named-contract 'y-range/c valid-range?))

(define color/c (or/c (is-a?/c color%) (list/c real? real? real?)))
(define factor-colors/c (listof (cons/c symbol? color/c)))

(provide/contract
 (extract-data (->* (data-frame?
                     (is-a?/c series-metadata%)
                     (is-a?/c series-metadata%))
                    ((or/c zero? positive?) boolean?)
                    ts-data/c))
 (ds-stats (-> ts-data/c statistics?))
 (add-verticals (-> ts-data/c ts-data/c))
 (get-lap-extents (-> data-frame? number? (cons/c number? (or/c number? #f))))
 (get-plot-y-range (-> statistics? (is-a?/c series-metadata%) y-range/c))
 (combine-y-range (-> y-range/c y-range/c y-range/c))

 (make-plot-renderer (->* (ts-data/c y-range/c)
                          (#:color any/c
                           #:width number?
                           #:alpha (or/c #f number?)
                           #:label (or/c #f string?))
                          renderer2d?))
 (make-box-renderer (->* (number? number? number? number?)
                         (any/c)
                         renderer2d?))

 (make-plot-renderer/factors (-> factor-data/c y-range/c factor-colors/c (treeof renderer2d?)))
 (make-plot-renderer/swim-stroke (-> ts-data/c y-range/c (vectorof (or/c #f integer?)) (treeof renderer2d?)))
 (get-series/ordered (-> data-frame? (listof string?)))
 (session-df (-> connection? number? (or/c data-frame? #f)))
 (reorder-sids (-> (listof integer?) (listof integer?))))

(provide y-range/c factor-colors/c)


;;.............................................. make-session-data-frame ....

(define-runtime-path fetch-trackpoins-path "../../sql/queries/trackpoints.sql")
(define fetch-trackpoins (define-sql-statement fetch-trackpoins-path))

(define-runtime-path fetch-trackpoins/swim-path "../../sql/queries/swim-trackpoints.sql")
(define fetch-trackpoins/swim (define-sql-statement fetch-trackpoins/swim-path))

(define-runtime-path fetch-sport-path "../../sql/queries/session-sport.sql")
(define fetch-sport (define-sql-statement fetch-sport-path))

(define-runtime-path fetch-lap-timestamps-path "../../sql/queries/lap-timestamps.sql")
(define fetch-lap-timestamps (define-sql-statement fetch-lap-timestamps-path))

;; Create a data-frame% from the session's trackpoints.  Some data series come
;; from the database (e.g. heart rate), some are calculated (e.g. heart rate
;; zone).  See also `session-df`, which is the function you want to use.
;;
;; NOTE: This function will return an empty data frame if a non-existent
;; session id is supplied.  Unfortunately, some other code relies on this
;; behavior (e.g. when a session is deleted in one thread while another thread
;; tries to read in a list of sessions to compute summary values...)
(define (make-session-data-frame db session-id)

  (let/ec return

    (define sport
      (let ([row (query-maybe-row db (fetch-sport) session-id)])
        (if row
            (match-let (((vector sport-id sub-sport-id) row))
              (vector (if (sql-null? sport-id) #f sport-id)
                      (if (sql-null? sub-sport-id) #f sub-sport-id)))
            ;; There is no session by this ID
            (return #f))))

    (define-values (is-lap-swim? is-ow-swim?)
      (when sport
        (match-define (vector sport-id sub-sport-id) sport)
        (values
         (and (equal? sport-id 5) (equal? sub-sport-id 17))
         (and (equal? sport-id 5) (not (equal? sub-sport-id 17))))))

    (define df
      (df-read/sql
       db
       (if is-lap-swim? (fetch-trackpoins/swim) (fetch-trackpoins))
       session-id))

    (when (and is-lap-swim? (> (df-row-count df) 0))
      ;; Lap swims don't record the distance in the A_TRACKPOINT table, at least
      ;; not reliably.  We reconstruct the "dst" series here from the lengths
      ;; and the "pool_length".  Note that we use the "active" series as a
      ;; helper, which tracks whether a length is active (i.e. traversing the
      ;; pool) or passive (i.e. resting at the end of the pool)
      (match-define (vector span unit)
        (query-row db "select pool_length, pool_length_unit
                       from A_SESSION where id = ?" session-id))
      (unless (sql-null? span)
        ;; NOTE: unfortunately we store the pool length in its original unit,
        ;; which is a mistake.  The DB should store metric values and those
        ;; should be converted for display only...
        (define pool-length (if (or (sql-null? unit) (equal? unit 0))
                                span
                                (* span 0.9144)))
        (define dst 0)
        ;; NOTE: distance is at the end of the length!
        (df-add-derived!
         df
         "dst"
         '("active")
         (lambda (v)
           (define active (car v))
           (set! dst (+ dst (* active pool-length)))
           dst))))

    ;; delete all empty series (e.g "gct" for a cycling activity)
    (for ([series (in-list (df-series-names df))])
      (unless (df-has-non-na? df series)
        (df-del-series! df series)))

    (define-values (cp-data cp-validity)
      (critical-power-for-session session-id #:database db))
    (df-put-property! df 'critical-power cp-data)
    (df-put-property! df 'critical-power-validity cp-validity)

    (df-put-property! df 'is-lap-swim? is-lap-swim?)
    (df-put-property! df 'is-ow-swim? is-ow-swim?)
    (df-put-property! df 'sport sport)
    (df-put-property! df 'session-id session-id)

    (when (df-contains? df "timestamp")
      (df-set-sorted! df "timestamp" <)

      ;; NOTE: the session might contain lap timestamps that have no track
      ;; points, don't put these laps in the data frame
      (let ((row-count (df-row-count df)))
        (if (> row-count 0)
            (let* ([laps (query-list db (fetch-lap-timestamps) session-id)]
                   [maxts (df-ref df (sub1 row-count) "timestamp")]
                   [xlaps (for/vector ([lap laps] #:when (<= lap maxts)) lap)])
              (df-put-property! df 'laps xlaps))
            (df-put-property! df 'laps '()))))

    ;; If we have a "dst" series, mark it as sorted.
    (when (df-contains? df "dst")
      (with-handlers
        (((lambda (e) #t)
          (lambda (e)
            ;; df-set-sorted! will raise an exception if the dst series contains
            ;; NA's or it is not fully sorted.  We fix it in that case and try
            ;; again.
            (define prev-value (df-ref df 0 "dst"))
            (when (df-is-na? df "dst" prev-value)
              (df-set! df 0 0 "dst")
              (set! prev-value 0))
            (for ([index (in-range 1 (df-row-count df))])
              (let ((value (df-ref df index "dst")))
                (if (or (df-is-na? df "dst" value) (<= value prev-value))
                    (df-set! df index prev-value "dst")
                    (set! prev-value value))))
            (df-set-sorted! df "dst" <))))
        (df-set-sorted! df "dst" <)))

    (add-timer-series df)
    (add-elapsed-series df)
    (add-distance-series df)
    (add-temperature-series df)
    (fixup-invalid-zero-values df "gct")
    (fixup-invalid-zero-values df "pgct")
    (smooth-start-of-series df "gct")
    (smooth-start-of-series df "pgct")
    (smooth-start-of-series df "vosc")
    (smooth-start-of-series df "cad")
    (add-speed-series df)
    (add-altitude-series df)
    (add-pace-series df)
    (add-speed-zone-series df)
    (maybe-add-grade-series! df)
    (when (is-runnig? sport)
      (add-gap-series df)             ; needs to be added after the grade series
      (add-gaspd-series df))
    (add-hr-pct-series df)
    (add-hr-zone-series df)
    (add-stride-series df)
    (add-vratio-series df)
    (smooth-start-of-series df "vratio")
    (add-power-zone-series df)
    (add-lppa-series df)                  ; left power phase angle
    (add-lpppa-series df)                 ; left peak power phase angle
    (fixup-pp-series df "lpps")
    (fixup-pp-series df "rpps")
    (fixup-pp-series df "lppps")
    (fixup-pp-series df "rppps")
    (add-rppa-series df)
    (add-rpppa-series df)
    (fixup-lrbal-series df)
    (add-torque-series df)
    (maybe-add-adecl! df)

    (when is-lap-swim?
      (add-swolf-series df))

    (unless is-lap-swim?
      (cond ((df-contains? df "timer")
             (df-set-default-weight-series! df "timer"))
            ((df-contains? df "elapsed")
             (df-set-default-weight-series! df "elapsed"))))

    (when cp-data
      (cond ((eqv? (vector-ref sport 0) 1) ; running
             (maybe-add-wbald-series/gap! df))
            ((eqv? (vector-ref sport 0) 2) ; biking
             (maybe-add-wbald-series! df "pwr"))))

    (read-xdata-series df db)
    (maybe-add-gear-change-series! df db)

    ;; WARNING: don't check for empty series here (or any operation that
    ;; references all the series) as this will materialize all lazy series and
    ;; make this function really slow!

    df))

(define (add-timer-series df)
  (when (df-contains? df "timestamp")
    (define stop-points '())
    (define st empty-statistics)

    (df-add-derived!
     df
     "timer"
     '("timestamp")
     (let ((timer 0))
       (lambda (prev-val val)
         (when prev-val
           (define ptimestamp (car prev-val))
           (define timestamp (car val))
           (define dt (- timestamp ptimestamp))
           ;; Keep track of the average time between samples (open water swims
           ;; can have 40-70 seconds between samples) and consider a stop
           ;; point any sample that is at least 3 times longer than the
           ;; current average...
           (if (and (> (statistics-count st) 3)
                    (> dt (* 3 (statistics-mean st))))
               (set! stop-points (cons ptimestamp stop-points))
               (begin
                 (when (> dt 1)       ; don't count 1 second gaps
                   (set! st (update-statistics st dt)))
                 (set! timer (+ timer dt)))))
         timer)))

    (set! stop-points (reverse stop-points))
    (define teleport-points
      (if (df-contains? df "lat" "lon")
          (filter (lambda (p) (is-teleport? df p)) stop-points)
          '()))

    (df-set-sorted! df "timer" <)
    (df-put-property! df 'stop-points stop-points)
    (df-put-property! df 'teleport-points teleport-points)))

(define (add-elapsed-series df)
  (when (df-contains? df "timestamp")
    (define timestamp0 (df-ref df 0 "timestamp"))
    ;; Lap swiming timestamps record the start of the length.  Elapsed looks
    ;; much nicer if it records the end of the length, so we add the duration
    ;; of the current length to each generated sample.  We need to be
    ;; carefull, as durations are recorded with millisecond precision, but
    ;; timesamps are only with second precision, sometimes time might go
    ;; backwards.
    (if (and (df-get-property df 'is-lap-swim?)
             (df-contains? df "duration"))
        (let ((elapsed 0))
          (df-add-derived!
           df
           "elapsed"
           '("timestamp" "duration")
           (lambda (val)
             (match-define (list timestamp duration) val)
             (let ((nelapsed (+ (- timestamp timestamp0) duration)))
               (when (> nelapsed elapsed)
                 (set! elapsed nelapsed))
               elapsed))))
        (df-add-derived!
         df
         "elapsed"
         '("timestamp")
         (lambda (val)
           (define timestamp (car val))
           (- timestamp timestamp0))))
    (df-set-sorted! df "elapsed" <)))

(define (add-distance-series df)

  (define (distance-km val)
    (define dst (car val))
    (if dst (m->km dst) #f))

  (define (distance-mi val)
    (define dst (car val))
    (if dst (m->mi dst) #f))

  (define (distance-yards val)
    (define dst (car val))
    (if dst (m->yd dst) #f))

  (define (distance-meters val)
    (car val))

  (when (df-contains? df "dst")

    ;; NOTE: the dst series contains #f's on lap swim activities.  Since this
    ;; series is used as a bases for the grahs, we patch the distance series
    ;; to contain valid values.
    (define distance
      (df-map
       df
       '("dst")
       (if (or (df-get-property df 'is-lap-swim?)
               (df-get-property df 'is-ow-swim?))
           (if (eq? (al-pref-measurement-system) 'metric)
               distance-meters distance-yards)
           (if (eq? (al-pref-measurement-system) 'metric)
               distance-km distance-mi))))

    ;; Patch first element
    (or (vector-ref distance 0)
        (vector-set! distance 0 0))
    ;; Patch the rest of the series so that it is increasing (errors in data
    ;; can have non increasing distance)
    (for ([idx (in-range 1 (vector-length distance))])
      (let ((prev-point (vector-ref distance (- idx 1)))
            (current-point (vector-ref distance idx)))
        (when (or (not current-point) (< current-point prev-point))
          (vector-set! distance idx prev-point))))

    (let ((series (make-series "distance" #:data distance #:cmpfn <)))
      (df-add-series! df series))))

(define (add-speed-series df)

  (define (speed-km/h val)
    (define spd (car val))
    (if spd (m/s->km/h spd) spd))
  (define (speed-mi/h val)
    (define spd (car val))
    (if spd (m/s->mi/h spd) #f))
  (when (df-contains? df "spd")
    (df-add-lazy!
     df
     "speed"
     '("spd")
     (if (eq? (al-pref-measurement-system) 'metric)
         speed-km/h speed-mi/h))))

(define (add-altitude-series df)
  (define (altitude-meters val)
    (car val))
  (define (altitude-feet val)
    (define alt (car val))
    (and alt (m->ft alt)))
  (when (df-contains? df "alt")
    (df-add-lazy!
     df
     "altitude"
     '("alt")
     (if (eq? (al-pref-measurement-system) 'metric)
         altitude-meters
         altitude-feet)))
 (when (df-contains? df "calt")
    (df-add-lazy!
     df
     "corrected-altitude"
     '("calt")
     (if (eq? (al-pref-measurement-system) 'metric)
         altitude-meters
         altitude-feet))))

(define (add-temperature-series df)
  (when (df-contains? df "tempe")
    (df-add-lazy!
     df
     "temperature"
     '("tempe")
     (lambda (v)
       (define v0 (car v))
       (and v0 (celsius->temperature v0))))))

;; Smooth the first LIMIT (in seconds) worth of samples from SERIES-NAME.
;; This is used for series that have huge unrealistic values at the start (e.g
;; GCT) using up space on plots and pushing the useful values to a very narrow
;; range.
;;
;; Smoothing is done by proportionally combining each value with the value
;; @LIMIT.
(define (smooth-start-of-series df series-name (limit 30))

  (define (combine val limit-val elapsed)
    ;; If there is no value @ limit, we effectively disable the smoothing.
    ;; This can happen, among other things, if the session is shorter than
    ;; "limit" seconds (e.g. triathlon transitions)
    (if (or (not val) (not limit-val) (> elapsed limit))
        val
        (let ((alpha (/ elapsed limit)))
          (exact->inexact
           (+ (* (- 1 alpha) limit-val)
              (* alpha val))))))

  (when (df-contains? df series-name "elapsed")
    (let* ((limit-index (df-index-of df "elapsed" limit))
           (limit-val (if (< limit-index (df-row-count df))
                          (df-ref df limit-index series-name)
                          #f)))
      ;; If we have no value at INDEX, don't do any smoothing.  We could
      ;; improve this by searching further forward for a valid value, but it
      ;; is not needed for now.
      (when limit-val
        (for ([idx (in-range limit-index)])
          (define val (df-ref df idx series-name))
          (define e (df-ref df idx "elapsed"))
          (define nval (combine val limit-val e))
          (df-set! df idx nval series-name))))))

(define (add-pace-series df)

  ;; Pace is the inverse of speed, and as such, at the start of an activity,
  ;; where speed ramps up from small values, pace values would be
  ;; unrealistically large.  To account for that, at the start of an activity,
  ;; we combine the computed pace with the pace at a point later in the
  ;; acitvity (1-2 minutes). This is done in a proportional manner, so that
  ;; the pace becomes the actual pace the closer we are to the limit.  This
  ;; produces nicer looking pace graphs.

  (define (combine pace limit-pace elapsed limit)
    ;; If there is no pace @ limit, we effectively disable the smoothing.
    ;; This can happen, among other things, if the session is shorter than
    ;; "limit" seconds (e.g. triathlon transitions)
    (if (or (not pace) (not limit-pace) (> elapsed limit))
        pace
        (let ((alpha (/ elapsed limit)))
          (exact->inexact
           (+ (* (- 1 alpha) limit-pace)
              (* alpha pace))))))

  (define (pace-sec/km spd)
    (if (and spd (> spd 0.6)) (m/s->sec/km spd)  #f))

  (define (pace-sec/mi spd)
    (if (and spd (> spd 0.6)) (m/s->sec/mi spd)  #f))

  (define (pace-sec/100m val)
    (define spd (car val))
    (if (and spd (> spd 0.1)) (m/s->sec/100m spd) #f))

  (define (pace-sec/100yd val)
    (define spd (car val))
    (if (and spd (> spd 0.1)) (m/s->sec/100yd spd) #f))

  (when (df-contains? df "spd" "elapsed")

    (if (or (df-get-property df 'is-lap-swim?)
            (df-get-property df 'is-ow-swim?))
        (df-add-lazy!
         df
         "pace" '("spd")
         (if (eq? (al-pref-measurement-system) 'metric)
             pace-sec/100m pace-sec/100yd))
        ;; non lap swim
        (let* ((limit 120)              ; seconds
               (pace-fn (if (eq? (al-pref-measurement-system) 'metric)
                            pace-sec/km pace-sec/mi))
               (pace-limit (pace-fn (df-lookup df "elapsed" "spd" limit))))
          (df-add-lazy!
           df
           "pace"
           '("spd" "elapsed")
           (lambda (val)
             (match-define (list spd elapsed) val)
             (combine (pace-fn spd) pace-limit elapsed limit)))))))

(define (add-speed-zone-series df)
  (define sid (df-get-property df 'session-id))
  (define zones (sport-zones-for-session sid 'pace))
  (when (and zones (df-contains? df "spd"))
    (df-add-lazy!
     df
     "speed-zone"
     '("spd")
     (lambda (val)
       (define spd (car val))
       (if spd (value->zone zones spd) #f)))))

(define (add-hr-pct-series df)
  (define sid (df-get-property df 'session-id))
  (define zones (sport-zones-for-session sid 'heart-rate))
  (when (and zones (df-contains? df "hr"))
    (df-add-lazy!
     df
     "hr-pct"
     '("hr")
     (lambda (val)
       (define hr (car val))
       (if hr (value->pct-of-max zones hr) #f)))))

(define (add-hr-zone-series df)
  (define sid (df-get-property df 'session-id))
  (define zones (sport-zones-for-session sid 'heart-rate))
  (when (and zones (df-contains? df "hr"))
    (df-add-lazy!
     df
     "hr-zone"
     '("hr")
     (lambda (val)
       (define hr (car val))
       (if hr (value->zone zones hr) #f)))))

(define (add-stride-series df)

  (define (stride-m val)
    (match-define (list spd cad) val)
    (if (and spd cad (> cad 0))
        (/ (* spd 60) (* 2 cad))
        #f))

  (define (stride-ft val)
    (let ((s (stride-m val)))
      (if s
          (m->ft s)
          #f)))

  (when (df-contains? df "spd" "cad")
    (df-add-lazy!
     df
     "stride"
     '("spd" "cad")
     (if (eq? (al-pref-measurement-system) 'metric)
         stride-m
         stride-ft))))

(define (add-vratio-series df)

  (define (stride spd cad)
    (if (and spd cad (> cad 0))
        (/ (* spd 60) (* 2 cad))
        #f))

  (when (df-contains? df "spd" "cad" "vosc")
    (df-add-lazy!
     df
     "vratio"
     '("spd" "cad" "vosc")
     (lambda (val)
       (match-define (list spd cad vosc) val)
       (if (and spd cad vosc)
           (let ((st (stride spd cad)))
             (if (and st (> st 0))
                 (let ((vratio (* 100.0 (/ vosc (* st 1000)))))
                   vratio)
                 #f))
           #f)))))

(define (add-power-zone-series df)
  (define sid (df-get-property df 'session-id))
  (define zones (sport-zones-for-session sid 'power))
  (when (and zones (df-contains? df "pwr"))
    (df-add-lazy!
     df
     "pwr-zone"
     '("pwr")
     (lambda (val)
       (define pwr (car val))
       (if pwr (value->zone zones pwr) #f)))))

(define (add-lppa-series df)
  (when (df-contains? df "lpps" "lppe")
    (df-add-lazy!
     df
     "lppa"
     '("lpps" "lppe")
     (lambda (val)
       (match-define (list start end) val)
       (if (and start end)
           (let ((angle (- end start)))
             (if (< angle 0) (+ angle 360) angle))
           #f)))))

;; Change the angle range form 0-360 to -180 .. 180.  This makes it look nicer
;; when angles are arround 0, as they will transition, for example, between
;; -20 and 20 degrees instead of jumping between 20 and 340
(define (fixup-pp-series df series-name)
  (when (df-contains? df series-name)
    ;; Don't use df-add-lazy! here as we enter in a recursive loop because we
    ;; ask for the same series.
    (df-add-derived!
     df
     series-name
     (list series-name)
     (lambda (val)
       (define a (car val))
       (if a (if (> a 180.0) (- a 360) a) #f)))
    ;; Get rid if this series if it became empty
    (unless (df-has-non-na? df series-name)
      (df-del-series! df series-name))))

(define (add-lpppa-series df)
  (when (df-contains? df "lppps" "lpppe")
    (df-add-lazy!
     df
     "lpppa"
     '("lppps" "lpppe")
     (lambda (val)
       (match-define (list start end) val)
       (if (and start end)
           (let ((angle (- end start)))
             (if (< angle 0) (+ angle 360) angle))
           #f)))))

(define (add-rppa-series df)
  (when (df-contains? df "rpps" "rppe")
    (df-add-lazy!
     df
     "rppa"
     '("rpps" "rppe")
     (lambda (val)
       (match-define (list start end) val)
       (if (and start end)
           (let ((angle (- end start)))
             (if (< angle 0) (+ angle 360) angle))
           #f)))))

(define (add-rpppa-series df)
  (when (df-contains? df "rppps" "rpppe")
    (df-add-lazy!
     df
     "rpppa"
     '("rppps" "rpppe")
     (lambda (val)
       (match-define (list start end) val)
       (if (and start end)
           (let ((angle (- end start)))
             (if (< angle 0) (+ angle 360) angle))
           #f)))))

(define (add-swolf-series df)
  (when (df-contains? df "duration" "strokes")
    (df-add-lazy!
     df
     "swolf"
     '("duration" "strokes")
     (lambda (val)
       (match-define (list duration strokes) val)
       (if (and duration strokes)
           (exact-round (+ duration strokes))
           #f)))))

(define (add-torque-series df)

  (define (cadence->torque power cadence)
    (let ((angular-velocity (* (/ cadence 60.0) (* 2 pi))))
      (/ power angular-velocity)))

  (when (df-contains? df "pwr" "cad")
    (df-add-lazy!
     df
     "torque"
     '("pwr" "cad")
     (lambda (val)
       (match-define (list pwr cad) val)
       (if (and pwr cad (> pwr 0) (> cad 0))
           (cadence->torque pwr cad)
           #f)))))

;; Convert some invalid values (e.g 0, 100) to #f.  This is used to replace 0
;; with #f in series like "gct" where a 0 is really invalid and messes up best
;; avg calculations
(define (fixup-invalid-zero-values df series-name)
  (when (df-contains? df series-name)
    (for ([index (in-range (df-row-count df))])
      (define val (df-ref df index series-name))
      (when (and (number? val) (zero? val))
        (df-set! df index #f series-name)))
    ;; Remove this series if it became empty after fixing invalid values.
    (unless (df-has-non-na? df series-name)
      (df-del-series! df series-name))))

;; Replace 0 and 100 with #f in the "lrbal" series -- these are invalid values
;; at the two extremes
(define (fixup-lrbal-series df)
  (when (df-contains? df "lrbal")
    (for ([index (in-range (df-row-count df))])
      (define val (df-ref df index "lrbal"))
      (when (and (number? val) (or (zero? val) (zero? (- 100 val))))
        (df-set! df index #f "lrbal")))
    ;; Remove this series if it became empty after fixing invalid values.
    (unless (df-has-non-na? df "lrbal")
      (df-del-series! df "lrbal"))))

(define (add-gap-series df)
  (define (pace-sec/km spd)
    (if (and spd (> spd 0.6)) (m/s->sec/km spd)  #f))

  (when (df-contains? df "pace" "grade")
    (df-add-lazy!
     df
     "gap"
     '("pace" "grade")
     (lambda (current)
       (match-define (list pace grade) current)
       (and pace grade (adjust-pace-for-grade pace grade))))))

(define (add-gaspd-series df)
  (when (df-contains? df "spd" "grade")
    (df-add-lazy!
     df
     "gaspd"
     '("spd" "grade")
     (lambda (current)
       (match-define (list spd grade) current)
       (and spd grade
            (* spd (grade->multiplier grade)))))))


;;................................................................ other ....

;; Extract data from DATA-FRAME corresponding to X-AXIS and Y-AXIS. The result
;; is intended for plotting on a graph, not for further processing.  A
;; sequence of three values is returned: x value, y value and timestamp (the
;; values are packed in a vector).  Some processing is done on the data, based
;; on the properties of X-AXIS and Y-AXIS:
;;
;; * missing Y values are replaced with the Y-AXIS default missing value, or
;;   dropped if no such value is specified.
;;
;; * data is filtered if Y-AXIS specifies it (FILTER-WIDTH will be used in
;; that case.
;;
;; * Y values will drop to 0 at stop points if the X-AXIS requests it (this
;; produces nicer graphs).
;;
;; * number of points in the data series will be reduced if SIMPLIFY-DATA is
;; #t and adjacent values are almost identical.  This is useful for long runs
;; and rides which would otherwise generate 8000+ data points in a plot,
;; slowing down the drawing but not adding much to the visuals.
;;
(define (extract-data data-frame x-axis y-axis (filter-width 0) (simplify-data #f))
  (let ((xseries (send x-axis series-name))
        (yseries (send y-axis series-name))
        (missing-value (send y-axis missing-value))
        (should-filter? (send y-axis should-filter?))
        (base-filter-width (send x-axis filter-width))
        (stop-detection? (send x-axis has-stop-detection?)))

    (define data
      (df-select* data-frame xseries yseries "timestamp"
                  #:filter (lambda (v)
                             (match-define (vector x y t) v)
                             (and x (or y missing-value) t))))
    ;; Missing values items are just selected but they are still #f, replace
    ;; them now.
    (when missing-value
      (for ([d (in-vector data)] #:unless (vector-ref d 1))
        (vector-set! d 1 missing-value)))

    ;; Filter the data in place, if required.
    (when (and should-filter?
               (> filter-width 0)
               (> (vector-length data) 0)
               (rational? base-filter-width))
      (let ((fw (* base-filter-width filter-width))
            (sp (or (df-get-property data-frame 'stop-points) '()))
            (px (vector-ref (vector-ref data 0) 0))
            (py (vector-ref (vector-ref data 0) 1)))
        (for ((v (in-vector data)))
          (match-define (vector x y t) v)
          (if (or (empty? sp) (>= (car sp) t))
              (let* ((dt (- x px))
                     (alpha (/ dt (+ dt fw)))
                     (ny (+ (* alpha y) (* (- 1 alpha) py))))
                (vector-set! v 1 ny)
                (set! py ny)
                (set! px x))
              (begin
                ;; stop point, reset the filter
                (set! sp (cdr sp))
                (set! px x)
                (set! py y))))))

    (define stop-points
      (if stop-detection? (df-get-property data-frame 'stop-points '()) '()))
    (define stop-indices
      (for/list ([point (in-list stop-points)])
        (lower-bound data point #:key (lambda (v) (vector-ref v 2)))))
    (define limit (vector-length data))

    ;; Mark stop points by setting the values around the stop point to 0. stop
    ;; points are stored in the data-frame when it is loaded up by
    ;; `make-session-data-frame'.  This is done after any filtering to make
    ;; the "edges" on the graph sharp even when filtering with large widths.
    ;;
    ;; If the data set is too small, overriding existing data points to 0 will
    ;; destroy otherwise useful information.  In such a case, we create a copy
    ;; of the data and add extra points, instead of overriding them.  For
    ;; larger data sets, this is not so important as there are adjacent data
    ;; points with the same, or close values.
    (if (and (< (vector-length data) 100) (> (length stop-points) 0))
        (let ((ndata (make-vector (+ (vector-length data) (* 2 (length stop-points))) #f)))
          (let loop ((index 0) (nindex 0) (stop-indices stop-indices))
            (when (< index (vector-length data))
              (vector-set! ndata nindex (vector-ref data index))
              (cond ((null? stop-indices)
                     (loop (add1 index) (+ 1 nindex) stop-indices))
                    ((= index (car stop-indices))
                     (let ((nd (vector-copy (vector-ref data index))))
                       (vector-set! nd 1 0)
                       (vector-set! ndata (+ 1 nindex) nd))
                     (let ((nd (vector-copy (vector-ref data (add1 index)))))
                       (vector-set! nd 1 0)
                       (vector-set! ndata (+ 2 nindex) nd))
                     (loop (add1 index) (+ 3 nindex) (cdr stop-indices)))
                    (#t
                     (loop (add1 index) (add1 nindex) stop-indices)))))
          (set! data ndata))
        (for ([stop (in-list stop-indices)])
          (when (< stop limit)
            (vector-set! (vector-ref data stop) 1 0))
          (when (< (add1 stop) limit)
            (vector-set! (vector-ref data (add1 stop)) 1 0))))

    ;; Reduce the number of points in the data to make it more manageable for
    ;; the plots.  We don't simplify small data sets (1000 points or less)
    (when (and simplify-data (> limit 1000))
      (define yrange
        (let ((stats (for/fold ([stats empty-statistics])
                               ([item (in-vector data)])
                       (update-statistics stats (vector-ref item 1)))))
          (statistics-range stats)))
      (define ychange-per-pixel (/ yrange 250)) ; assume plot is 250 pixels high
      (define xrange
        (- (vector-ref (vector-ref data (sub1 limit)) 0)
           (vector-ref (vector-ref data 0) 0)))
      (define xchange-per-pixel (/ xrange 1000)) ; assume plot is 1000 pixels wide
      (define eps (min xchange-per-pixel ychange-per-pixel))
      ;; rdp-simplify does not accept an epsilon of 0, but it would be useless
      ;; anyway, as this would not remove any data points anyway
      (when (> eps 0)
        (set! data (rdp-simplify data
                                 #:epsilon eps
                                 #:destroy-original? #t
                                 #:keep-positions stop-indices))))

    data))

;; Compute statistics on the Y value of a data-series (as produced by
;; `extract-data').  This is faster than doing the statistics on the
;; data-frame itself.
(define (ds-stats data-series)
  (for/fold ([stats empty-statistics])
            ([item data-series])
    (update-statistics stats (vector-ref item 1))))

;; Duplicate points in DATA (as produced by `extract-data') so that the graphs
;; have a "boxed" appearance.  This is used in all the swim graphs where the
;; data points are for individual lenghts.
(define (add-verticals data)
  (for*/vector ([idx (in-range (vector-length data))]
                [val (in-sequences
                      (if (> idx 0)
                          (let ()
                            (match-define (vector x y t) (vector-ref data (- idx 1)))
                            (match-define (vector nx ny nt) (vector-ref data idx))
                            (list
                             (vector x ny nt)
                             (vector-ref data idx)))
                          (let ()
                            (match-define (vector x y t) (vector-ref data 0))
                            (list
                             (vector 0 y t)
                             (vector x y t)))))])
    val))

;; Return the start and end timestamps for LAP-NUM in DATA-FRAME.  LAP-NUM is
;; one of the recorded laps, 0 being the first one.  Returns (cons START END),
;; where end can be #f for the last lap.
(define (get-lap-extents data-frame lap-num)
  (let* ((laps (df-get-property data-frame 'laps))
         (start (vector-ref laps lap-num))
         (end (if (< (+ lap-num 1) (vector-length laps))
                  (vector-ref laps (+ lap-num 1))
                  #f)))
    (cons start end)))

;; Determine the min/max y values for a plot based on STATS (as collected by
;; `ds-stats') and the Y-AXIS.
(define (get-plot-y-range stats y-axis)
  ;; Start with the y-range defined by Y-AXIS, if any
  (define-values (low high)
    (let ([srange (send y-axis y-range)])
      (if srange
          (values (car srange) (cdr srange))
          (values #f #f))))

  (let ([v (statistics-min stats)])
    (unless (or (nan? v) (infinite? v))
      (set! low (if low (min low v) v))))

  (let ([v (statistics-max stats)])
    (unless (or (nan? v) (infinite? v))
      (set! high (if high (max high v) v))))

  ;; Unclear what to do here, but our callers would have even less of a
  ;; clue...
  (unless (and low high)
    (error
     (format "get-plot-y-range (~a): bad bounds: min ~a, max ~a"
             (send y-axis series-name)
             low
             high)))

  ;; Now check if the range is too small and adjust it.
  (define arange (- high low))

  ;; Ensure a minimum range for the plot, based off the fractional digits.
  ;; This is used when we have nearly constant values in the data series and
  ;; the actual range is zero, or close to zero
  (define mrange (* 2 10 (expt 10 (- (send y-axis fractional-digits)))))

  (define extend (* 0.05 arange))

  (if (> (+ arange (* 2 extend)) mrange)
      (cons (- low extend) (+ high extend))
      ;; Create a small range in the middle of LOW and HIGH
      (let ((middle (* 0.5 (+ low high)))
            (h (* 0.5 mrange)))
        (cons (- middle h) (+ middle h)))))

;; Combine two Y ranges (as produced by `get-plot-y-range')
(define (combine-y-range yr1 yr2)
  (match-define (cons low1 high1) yr1)
  (match-define (cons low2 high2) yr2)
  (cons (min low1 low2) (max high1 high2)))

;; Create a plot renderer which plots DATA-SERIES (a list of [X Y]) using
;; lines (a "normal" plot).  Y-RANGE is either #f or a cons of the min and max
;; Y values of the graph.
;;
(define (make-plot-renderer data-series y-range
                            #:color (color #f)
                            #:width (width 2)
                            #:alpha (alpha #f)
                            #:label (label #f))
  (let ((kwd '()) (val '()))
    (define (add-arg k v) (set! kwd (cons k kwd)) (set! val (cons v val)))
    ;; WARNING: keywords used by keyword-apply have to be sorted

    ;; Use min and max y values for the graphs, if these are specified.
    ;; Otherwise, we let the plot library determine bounds.
    (when y-range
      (add-arg '#:y-min (car y-range))
      (add-arg '#:y-max (cdr y-range)))
    (when width (add-arg '#:width width))
    (when label (add-arg '#:label label))
    (when color (add-arg '#:color color))
    (when alpha (add-arg '#:alpha alpha))
    (keyword-apply lines kwd val data-series '())))

;; Create a plot renderer which plots a box with the bounds START, END,
;; MIN-VAL, MAX-VAL and it is ranged according to AXIS-DEF.  COLOR specifies
;; the color of the box, if #f.
;;
(define (make-box-renderer start end min-val max-val [color "red"])
  (let ((kwd '()) (val '()))
    (define (add-arg k v) (set! kwd (cons k kwd)) (set! val (cons v val)))
    ;; WARNING: keywords used by keyword-apply have to be sorted
    (add-arg '#:y-min min-val)
    (add-arg '#:y-max max-val)
    (when color (add-arg '#:color color))
    (add-arg '#:alpha 0.2)

    (keyword-apply
     lines-interval kwd val
     (list (vector start max-val) (vector end max-val))
     (list (vector start min-val) (vector end min-val))
     '())))

;; Create a plot renderer that plots different points in different colors.
;; DATA is produced by `group-samples/factor' and it is a hash table mapping a
;; factor to the points in that category.  FACTOR-COLORS is a list mapping the
;; factor to a color.
;;
(define (make-plot-renderer/factors data y-range factor-colors)
  (for/list ([elt factor-colors])
    (match-define (cons factor color) elt)
    (let ((fdata (hash-ref data factor '())))
      (points fdata #:sym 'fullcircle #:color color #:y-min (car y-range) #:y-max (cdr y-range)))))

;; Given `series', a sequence of values, return return a list of start, end
;; indexes for consecutive values in series.  For example, given '(1 1 1 2 2 3
;; 3 3), it will return (#(0 3) #(3 5) #(5 8)).  This is used to find the
;; ranges of the same swim stroke for coloring a swim graph.
;;
(define (find-ranges series)
  (if (= (sequence-length series) 0)
      '()
      (let ((start 0)
            (item (sequence-ref series 0)))
        ;; NOTE: index will start at 0, but we already removed the first
        ;; element
        (for/list ([(val index)
                    (in-indexed
                     (in-sequences (sequence-tail series 1) (list (gensym))))]
                   #:unless (equal? item val))
          (begin0
              (vector start (+ 1 index) item)
            (set! start (+ 1 index))
            (set! item val))))))

;; Make a plot renderer than plots DATA colorized by SWIM-STROKES.  Y-RANGE
;; defines the low and high range of the plot, see also `valid-range?`.  DATA
;; has already been processed by `add-verticals', SWIM-STROKES is the swim
;; stroke series from the data frame.
;;
(define (make-plot-renderer/swim-stroke data y-range swim-strokes)
  ;; NOTE: data has verticals added, swim-strokes does not.
  (for/list ([range (find-ranges swim-strokes)])
    (match-define (vector start end stroke) range)
    (let ((color (get-swim-stroke-color stroke))
          (first? (equal? start 0))
          (items (vector-copy
                  data
                  (* 2 start)
                  (min (vector-length data) (* 2 end)))))
      (if first?
          (make-plot-renderer items y-range #:color color)
          (list
           (make-plot-renderer
            (vector-copy data (- (* 2 start) 1) (+ (* 2 start) 1))
            #f #:color "gray" #:width 0.7)
           (make-plot-renderer items y-range #:color color))))))


;;................................................... get-series/ordered ....

;; List of all data series in the order in which we want them exported in the
;; CSV file.
(define all-series
  '("timestamp" "timer" "elapsed" "duration"
    "lat" "lon" "alt" "calt" "grade" "dst" "distance"
    "spd" "speed" "pace" "speed-zone"
    "hr" "hr-pct" "hr-zone"
    "cad" "stride"
    "vosc" "vratio" "gct" "pgct"
    "pwr" "pwr-zone" "wbal" "lrbal" "lteff" "rteff" "lpsmth" "rpsmth" "cpsmth"
    "lpco" "rpco"
    "lpps" "lppe" "rpps" "rppe" "lppa" "rppa"
    "lppps" "lpppe" "rppps" "rpppe" "lpppa" "rpppa"
    "swim_stroke" "strokes" "swolf"))

;; Series names to skip when exporting
(define skip-series
  ;; map-point is used by the map view to speed up interpolated lookups
  '("map-point"))

;; Return a list of data series names, by reordering the items in ALL such
;; that any PREFERRED items come first, and all remaining series come after.
(define (ordered-series preferred all)
  (define base-list
    (for/list ([name (in-list preferred)] #:when (member name all))
      name))
  (define rest
    (for/list ([name (in-list all)]
               #:unless (or (member name skip-series) (member name base-list)))
      name))
  (append base-list rest))

;; Return a list of the series names in DF, a data-frame% ordered in a "nice"
;; way (such that related series, like lat, lon and alt, are close to each
;; other)
(define (get-series/ordered df)
  (ordered-series all-series (df-series-names df)))


;;............................................................ utilities ....

;; Return the time difference and map distance between the point at TIMESTAMP
;; and the next point.  Returns (values -1 -1) if we cannot determine the
;; difference.
(define (delta-time-and-distance df timestamp)
  (let ((index (df-index-of df "timestamp" timestamp))
        (max-index (df-row-count df)))
    (if (and (< index max-index) (< (add1 index) max-index))
        (let ((t1 (df-ref df index "timestamp"))
              (t2 (df-ref df (add1 index) "timestamp")))
          ;; TODO: We have missing values at this timestamp, perhaps we should
          ;; search forward (or backward?) for the a pair of valid points.
          ;; This search can be quite complex however.  For now, we just give
          ;; up if we have missing values.
          (match-define (vector lat1 lon1) (df-ref* df index "lat" "lon"))
          (match-define (vector lat2 lon2) (df-ref* df (add1 index) "lat" "lon"))
          (if (and t1 t2 lat1 lat2 lon1 lon2)
              (values
               (- t2 t1)
               (distance-between lat1 lon1 lat2 lon2))
              (values -1 -1)))
        ;; We landed on the last index in the series...
        (values -1 -1))))

;; Return true if the point at TIMESTAMP is a teleportation point, where the
;; recording was stopped, the user moved to a different spot and started
;; recording again.
(define (is-teleport? df timestamp)
  (let-values (([dt dd] (delta-time-and-distance df timestamp)))
    ;; NOTE that the value below has to account for GPS error as well, the
    ;; user might not have moved 100 meters, but since it stopped, the GPS
    ;; precision has dropped as well.
    (> dd 100)))



;;............................................................. df cache ....

;; A data frame cache, to avoid reading data frames again if we need to
;; compute BAVG values for several series.  This is a two stage cache,
;; allowing us to expire old entries.  See 'session-df' on how this cache is
;; managed.
(define df-cache (make-hash))
(define df-cache2 (make-hash))

;; Number of data frames to keep in df-cache.  NOTE: total data frame count is
;; up to (hash-count df-cache) + (hash-count df-cache2), so in total number of
;; cached data frames can be up to (* 2 df-cache-limit)
(define df-cache-limit 50)

;; Reorder the session ids in SIDS such that the id's that are in df-cache are
;; listed first.  This is used so that we don't invalidate the cache too
;; quickly if we have a large number of SIDS for which we will request a data
;; frame (e.g. when aggregate metrics are calculated).
(define (reorder-sids sids)
  (process-events)

  (define (present-in-cache sid)
    (or (hash-ref df-cache sid #f)
        (hash-ref df-cache2 sid #f)))

  (let ((in-cache '())
        (not-in-cache '()))
    (for ((sid (in-list sids)))
      (if (present-in-cache sid)
          (set! in-cache (cons sid in-cache))
          (set! not-in-cache (cons sid not-in-cache))))
    (append in-cache not-in-cache)))

;; If DF has a 'dirty-wbal property, check if the effective CP params have
;; changed.  If they did, update the WBAL series.  Note that we return a copy
;; of the data frame, as code relies on `eq?` to check for "different" data
;; frames.
(define (refresh-df sid df)
  (if (df-get-property df 'dirty-wbal)
      (let ((new-cp-data (critical-power-for-session sid)))
        (cond
          (new-cp-data
           (define old-cp-data (df-get-property df 'critical-power))
           (if (equal? new-cp-data old-cp-data)
               (begin
                 ;; parameters are the same, no need to change anything
                 (df-del-property! df 'dirty-wbal)
                 df)
               (let ((new-df (df-shallow-copy df))
                     (sport (df-get-property df 'sport)))
                 (df-del-property! new-df 'dirty-wbal)
                 (df-put-property! new-df 'critical-power new-cp-data)
                 (cond ((eqv? (vector-ref sport 0) 1) ; running
                        (maybe-add-wbald-series/gap! new-df))
                       ((eqv? (vector-ref sport 0) 2) ; biking
                        (maybe-add-wbald-series! new-df "pwr")))
                 (hash-set! df-cache sid new-df)
                 new-df)))
          (#t
           ;; We no longer have CP params for this data frame, delete them.
           (let ((new-df (df-shallow-copy df)))
             (df-del-property! new-df 'dirty-wbal)
             (df-del-property! new-df 'critical-power)
             (df-del-series! new-df "wbal")
             (hash-set! df-cache sid new-df)
             new-df))
          ))
      df))

;; Return the data frame for a session id SID.  Data frames are cached in
;; memory, so retrieving the same one again should be fast.
(define (session-df db sid)
  (process-events)
  (cond ((hash-ref df-cache sid #f)
         => (lambda (df) (refresh-df sid df)))
        ((hash-ref df-cache2 sid #f)
         => (lambda (df)
              ;; Promote it to first cache
              (hash-set! df-cache sid df)
              (refresh-df sid df)))
        (#t
         (let ((df (make-session-data-frame db sid)))
           (when df                     ; if the session actually exists
             (hash-set! df-cache sid df)
             (when (> (hash-count df-cache) df-cache-limit)
               ;; Cache limit reached, demote df-cache to df-cache2 (loosing old
               ;; data) and create a fresh df-cache
               (set! df-cache2 df-cache)
               (set! df-cache (make-hash))))
           df))))

;; Remove session id SID from the cache.  If SID is #f, the entire cache is
;; cleared.
(define (clear-session-df-cache (sid #f))
  (if sid
      (begin
        (hash-remove! df-cache sid)
        (hash-remove! df-cache2 sid))
      (begin
        (hash-clear! df-cache)
        (hash-clear! df-cache2))))

;; Mark the "wbal" series as dirty in all cached series.  The wbal series will
;; be refreshed when it is next retrieved by `session-df`
(define (mark-wbal-dirty)
  (define sids
    (append (hash-keys df-cache)
            (hash-keys df-cache2)))
  (for ((sid sids))
    (define df (or (hash-ref df-cache sid #f)
                   (hash-ref df-cache2 sid #f)))
    (when df                            ; might no longer be here
      (df-put-property! df 'dirty-wbal #t))
    (log-event 'session-updated-data sid)))

(define log-event-source (make-log-event-source))

(define (process-events)
  (let loop ((item (async-channel-try-get log-event-source)))
    (when item
      (match-define (list tag data) item)
      (case tag
        ((measurement-system-changed database-opened)
         (clear-session-df-cache))
        ((session-updated session-updated-data session-deleted
                          sport-zone-parameters-changed)
         (clear-session-df-cache data))
        ((critical-power-parameters-changed) (mark-wbal-dirty)))
      (loop (async-channel-try-get log-event-source)))))
