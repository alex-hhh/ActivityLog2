#lang racket/base
;; plot-axis-def.rkt -- helper classes for plotting various data series
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015 Alex Harsanyi (AlexHarsanyi@gmail.com)
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

(require plot
         racket/class
         racket/list
         racket/math
         "activity-util.rkt"
         "al-prefs.rkt"
         "fmt-util.rkt"
         "sport-charms.rkt"
         "utilities.rkt")

(define *blue* '(0 148 255))
(define *green* '(38 127 0))
(define *red* '(127 0 0))
(define *yellow* '(255 140 0))
(define *purple* '(#x8b #x22 #x52))

;; http://www.spycolor.com/w3c-colors
(define *crimson* '(#xdc #x14 #x3c))
(define *coral* '(#xff #x7f #x50))
(define *cornflower-blue* '(#x64 #x95 #xed))
(define *dark-magenta* '(#x8b #x00 #x8b))
(define *sea-green* '(#x2e #x8b #x57))



;; ..................................................... data extractors ....

(define (extract-raw-data tag)
  (lambda (prev current)
    (cond ((assq tag current) => (lambda (result) (cdr result)))
          (#t #f))))

(define extract-time (extract-raw-data 'timestamp))
(provide extract-time)

;; Extract elapsed time since the start of the track.
(define extract-elapsed-time
  ;; NOTE: this function can be called from multiple threads and the value of
  ;; START is shared between subsequent calls to this function, so we make
  ;; sure it has a per-thread value.
  (let ((start (make-thread-cell #f)))
    (lambda (prev next)
      (let ((time (extract-time prev next)))
	(when (not prev)
          (thread-cell-set! start time))
	(- time (or (thread-cell-ref start) 0))))))

(define extract-timer-time
  ;; NOTE: this function can be called from multiple threads and the value of
  ;; TIMER is shared between subsequent calls to this function, so we make
  ;; sure it has a per-thread value.
  (let ((timer (make-thread-cell 0)))
    (lambda (prev next)
      (when (not prev)
	(thread-cell-set! timer 0.0))
      (let ((delta-distance (extract-delta-distance prev next))
            (delta-time (extract-delta-time prev next)))
	(when (and delta-distance delta-time (> delta-time 0))
          ;; NOTE: sometimes there is a small distance recorded at a pause.
          ;; Maybe I should rely on explicit markers for the stops? They are
          ;; after all recoreded in the FIT file...
          (when (> (/ delta-distance delta-time) 0.1)
            ;; target is moving
            (thread-cell-set! timer (+ (thread-cell-ref timer) delta-time)))))
      (thread-cell-ref timer))))

(define (extract-delta-time prev current)
  (if prev
      (let ((t1 (assq 'timestamp current))
            (t2 (assq 'timestamp prev)))
	(if (and t1 t2)
            (- (cdr t1) (cdr t2))
            #f))
      0))

(define (extract-position prev current)
  (let ((lat (assq 'position-lat current))
	(long (assq 'position-long current)))
    (if (and lat long)
	(vector (cdr lat) (cdr long))
	#f)))

(define (extract-distance prev current)
   (if prev
      (let ((d1 (assq1 'distance current))
            (d2 (assq1 'distance prev)))
        (if (and d1 d2 (> d1 d2)) d1 #f)) ; bugs in data can make this happen
      (assq1 'distance current)))

(define (extract-distance-km prev current)
  (cond ((extract-distance prev current) => m->km)
	(#t #f)))

(define (extract-distance-mi prev current)
  (cond ((extract-distance prev current) => m->mi)
	(#t #f)))

(define (extract-delta-distance prev current)
  (if prev
      (let ((d1 (assq 'distance current))
            (d2 (assq 'distance prev)))
	(if (and d1 d2)
            (let ((dd (- (cdr d1) (cdr d2))))
              (if (> dd 0) dd #f))      ; bugs in data can make this happen
            #f))
      0))

(define extract-speed (extract-raw-data 'speed))

(define (extract-computed-speed prev current)
  (if prev
      (let ((delta-distance (extract-delta-distance prev current))
            (delta-time (extract-delta-time prev current)))
	(if (and delta-distance delta-time (> delta-time 0))
            (let ((speed (/ delta-distance delta-time)))
              (if (< speed 0.1)
                  #f
                  speed))
            #f))
      #f))

(define (extract-speed-km/h prev next)
  (cond ((extract-speed prev next) => m/s->km/h)
        (#t #f)))

(define (extract-speed-mi/h prev next)
  (cond ((extract-speed prev next) => m/s->mi/h)
        (#t #f)))

(define (extract-pace-min/km prev next)
  (let ((speed (extract-speed prev next)))
    ;; Slow speeds (0.5 m/s) are filtered out, as these result in
    ;; unrealistically slow pace values, which screw up the scaling of the
    ;; graphs.  0.5 m/s corresponds to a pace of 33 min/km, which is slow even
    ;; for hiking.  These slow speeds are usually at the start of an activity
    ;; when the GPS hasn't initialized completely yet.
    (if (and speed (> speed 0.5))
	(m/s->sec/km speed)
	#f)))

(define (extract-pace-min/mi prev next)
  (let ((speed (extract-speed prev next)))
    ;; Slow speeds (0.5 m/s) are filtered out, as these result in
    ;; unrealistically slow pace values, which screw up the scaling of the
    ;; graphs.  0.5 m/s corresponds to a pace of 33 min/km, which is slow even
    ;; for hiking.  These slow speeds are usually at the start of an activity
    ;; when the GPS hasn't initialized completely yet.
    (if (and speed (> speed 0.5))
	(m/s->sec/mi speed)
	#f)))

(define (extract-computed-speed-km/h prev next)
  (let ((speed (extract-computed-speed prev next)))
    ;; convert from meters/sec to km/h
    (if (and speed (> speed 0))
        (/ (* speed 3600) 1000)
        #f)))

(define (make-speed-zone-extractor session-id)
  (let ((zones (get-session-sport-zones session-id 2)))
    (if zones
        (lambda (prev next)
          (let ((speed (extract-speed prev next)))
            (if speed (val->zone speed zones) #f)))
        (lambda (prev next) #f))))

(define (extract-computed-pace-min/km prev next)
  (let ((speed (extract-computed-speed prev next)))
    ;; convert from meters/sec to sec/km
    (if (and speed (> speed 0))
	(let ((val (/ 1 (/ speed 1000))))
          val)
	#f)))

(define (extract-grade prev current)

  (define (extract-distance prev current)
    (if prev
        (let ((d1 (assq1 'distance current))
              (d2 (assq1 'distance prev)))
          (if (and d1 d2 (> d1 d2)) (- d1 d2) #f)) ; bugs in data can make this happen
        0))

  (if prev
      (let ((a1 (or (assq1 'corrected-altitude prev)
                    (assq1 'altitude prev)))
            (a2 (or (assq1 'corrected-altitude current)
                    (assq1 'altitude current)))
            (d (extract-distance prev current)))
        (if (and d a1 a2 (> d 0))
            (* 100 (/ (- a2 a1) d))
            #f))
      #f))

(define extract-hr (extract-raw-data 'heart-rate))

(define (make-hr-pct-extractor session-id)
  (let ((zones (get-session-sport-zones session-id 1)))
    (if zones
        (lambda (prev next)
          (let ((hr (extract-hr prev next)))
            (if hr (val->pct-of-max hr zones) #f)))
        (lambda (prev next) #f))))

(define (make-hr-zone-extractor session-id)
  (let ((zones (get-session-sport-zones session-id 1)))
    (if zones
        (lambda (prev next)
          (let ((hr (extract-hr prev next)))
            (if hr (val->zone hr zones) #f)))
        (lambda (prev next) #f))))

(define (make-power-zone-extractor session-id)
  (let ((zones (get-session-sport-zones session-id 3)))
    (if zones
        (lambda (prev next)
          (let ((power (assq1 'power next)))
            (if power (val->zone power zones) #f)))
        (lambda (prev next) #f))))

;; NOTE: there is only one crank length defined, but this should realy be per
;; bike...
(define crank-length
  (al-get-pref 'activity-log:bike-crank-length (lambda () 0.1725)))

(define (extract-torque trackpoint)
  (let ((power (assq1 'power trackpoint))
        (cadence (assq1 'cadence trackpoint)))
    (if (and power cadence)
        (let* ((radial-velocity (* (* 2 pi crank-length) (/ cadence 60.0)))
               (force (/ power radial-velocity)))
          (if (and (not (or (nan? force) (infinite? force))) (> force 0))
              (* force crank-length)
              #f))
        #f)))

(define (extract-swim-pace l)
  (let ((speed (length-avg-speed l)))
    (if (and speed (> speed 0))
        (m/s->sec/100m speed)
        0)))


(define (extract-swim-pace-imperial l)
  (let ((speed (length-avg-speed l)))
    (if (and speed (> speed 0))
        (m/s->sec/100yd speed)
        0)))

;; Lengths only record their swim time, we accumulate it to be the time since
;; the session start.
(define (fixup-swim-time data)
  (if (pair? data)
      (let ((base (vector-ref (car data) 1)))
        (cons (car data)
              (map (lambda (d)
                     (set! base (+ base (vector-ref d 1)))
                     (vector-set! d 1 base)
                     d)
                   (cdr data))))
      '()))


;;..................................................... axis definitions ....

;; Provides definitions about an axis on a plot (functions to extract the data
;; and functions to display the data).  For most plots two axis definitions
;; will be used.  For example, to plor speed over time one would use
;; axis-timer-time as the X axis and axis-speed as the Y axis.  See also
;; `extract-data'.
(define axis-definition%
  (class object% (init) (super-new)

    ;; Return a function suitable for using on `for-each-session-trackpoint`
    ;; to extract the data.  The function should be a (lambda (prev next) ...)
    ;; and should return a value.
    ;;
    ;; SESSION-ID is used for zone based extractors which need to provide a
    ;; session specific extractor instance
    (define/public (get-extractor-fn [session-id #f]) #f)

    (define/public (get-fixup-fn) #f)   ; see fixup-swim-time

    ;; When #t, `make-data-extractor` will attempt to detect stop points and
    ;; generate additional data points with 0 Y values, this makes graphs look
    ;; nicer.
    (define/public (has-stop-detection?) #f)

    ;; When #t, `make-data-extractor' will generate an initial datapoint with
    ;; a 0 Y value.  When combined with a low-pass filter, this will produce
    ;; nicer graphs as many data points are erratic at the start of a session.
    (define/public (should-force-zero-start?) #f)

    ;; Whether to filter extracted values using a low pass filter.  Whether
    ;; filtering happens or not is defined on an Y axis (e.g. we may want to
    ;; filter heart rate but not elevation).
    (define/public (should-filter?) #f)

    ;; Determines the width of the low pass filter.  The filter width is
    ;; determined by the X axis (e.g. distance and time will have different
    ;; widths).
    (define/public (get-filter-width) #f)

    (define/public (get-histogram-bucket-slot) 1.0)
    (define/public (inverted-best-avg?) #f)

    (define/public (get-axis-ticks) (linear-ticks))
    (define/public (get-axis-label) (raise "no axis label defined"))
    (define/public (get-series-label) #f)
    (define/public (get-line-color) #f)
    (define/public (get-y-range) #f)

    ))


(define axis-distance
  (new (class axis-definition% (init) (super-new)
         (define/override (get-extractor-fn sid)
           (if (eq? (al-pref-measurement-system) 'metric)
               extract-distance-km extract-distance-mi))
         (define/override (get-filter-width) 0.05)
         (define/override (get-axis-label)
           (if (eq? (al-pref-measurement-system) 'metric)
               "Distance (km)" "Distance (mi)")))))
(provide axis-distance)

(define axis-elapsed-time
  (new (class axis-definition% (init) (super-new)
         (define/override (get-extractor-fn sid) extract-elapsed-time)
         (define/override (get-axis-ticks) (time-ticks #:formats '("~H:~M")))
         (define/override (has-stop-detection?) #t)
         (define/override (get-filter-width) 5.0) ; seconds
         (define/override (get-axis-label) "Elapsed Time (hour:min)")
         )))
(provide axis-elapsed-time)

(define axis-elapsed-time-no-stop-detection
  (new (class axis-definition% (init) (super-new)
         (define/override (get-extractor-fn sid) extract-elapsed-time)
         (define/override (get-axis-ticks) (time-ticks #:formats '("~H:~M")))
         (define/override (has-stop-detection?) #f)
         (define/override (get-filter-width) 5.0) ; seconds
         (define/override (get-axis-label) "Elapsed Time (hour:min)")
         )))
(provide axis-elapsed-time-no-stop-detection)

(define axis-wall-clock-time
  (new (class axis-definition% (init) (super-new)

         (define extractor-fn (extract-raw-data 'timestamp))

         (define/override (get-extractor-fn sid) extractor-fn)
         (define/override (get-axis-ticks) (time-ticks #:formats '("~H:~M")))
         (define/override (has-stop-detection?) #f)
         (define/override (get-filter-width) 5.0)
         (define/override (get-axis-label) "Wall clock (hour:min)"))))
(provide axis-wall-clock-time)

(define axis-timer-time
  (new (class axis-definition% (init) (super-new)
         (define/override (get-extractor-fn sid) extract-timer-time)
         (define/override (get-axis-ticks) (time-ticks #:formats '("~H:~M")))
         (define/override (has-stop-detection?) #f)
         (define/override (get-filter-width) 5.0)
         (define/override (get-axis-label) "Time (hour:min)"))))
(provide axis-timer-time)

(define axis-speed
  (new (class axis-definition% (init) (super-new)
         (define/override (get-extractor-fn sid)
           (if (eq? (al-pref-measurement-system) 'metric)
               extract-speed-km/h extract-speed-mi/h))
         (define/override (get-axis-label)
           (if (eq? (al-pref-measurement-system) 'metric)
               "Speed (km/h)" "Speed (mi/h)"))
         (define/override (should-filter?) #t)
         (define/override (get-histogram-bucket-slot) 0.1)
         (define/override (should-force-zero-start?) #t)
         (define/override (get-line-color) *blue*))))
(provide axis-speed)

(define axis-pace
  (new (class axis-definition% (init) (super-new)
         (define/override (get-extractor-fn sid)
           (if (eq? (al-pref-measurement-system) 'metric)
               extract-pace-min/km extract-pace-min/mi))
         (define/override (get-axis-ticks) (time-ticks #:formats '("~M:~f")))
         (define/override (get-axis-label)
           (if (eq? (al-pref-measurement-system) 'metric)
               "Pace (min/km)" "Pace (min/mi)"))
         (define/override (should-filter?) #t)
         (define/override (should-force-zero-start?) #t)
         ;; (define/override (get-y-range) (cons 0 #f))
         (define/override (get-line-color) *blue*)
         (define/override (inverted-best-avg?) #t))))
(provide axis-pace)

(define axis-speed-zone
  (new (class axis-definition% (init) (super-new)
         (define/override (get-extractor-fn session-id)
           (make-speed-zone-extractor session-id))
         (define/override (get-axis-label) "Speed (zone)")
         (define/override (should-filter?) #t)
         ;; (define/override (get-y-range) (cons 0 #f))
         (define/override (should-force-zero-start?) #t)
         (define/override (get-line-color) *blue*)
         )))
(provide axis-speed-zone)

(define axis-elevation
  (new (class axis-definition% (init) (super-new)
         (define extractor-fn-metric (extract-raw-data 'altitude))
         (define extractor-fn-imperial
           (let ((fn (extract-raw-data 'altitude)))
             (lambda (prev next) (m->ft (fn prev next)))))
         
         (define/override (get-extractor-fn sid)
           (if (eq? (al-pref-measurement-system) 'metric)
               extractor-fn-metric extractor-fn-imperial))
         (define/override (get-axis-label)
           (if (eq? (al-pref-measurement-system) 'metric)
               "Elevation (m)" "Elevation (ft)"))
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *green*)
           
         )))
(provide axis-elevation)

(define axis-corrected-elevation
  (new (class axis-definition% (init) (super-new)
         (define extractor-fn-metric
           (lambda (prev current)
                    (or (assq1 'corrected-altitude current)
                        (assq1 'altitude current))))
         (define extractor-fn-imperial
           (lambda (prev current)
                    (let ((val (or (assq1 'corrected-altitude current)
                                   (assq1 'altitude current))))
                      (if val (m->ft val) #f))))
         (define/override (get-extractor-fn sid)
           (if (eq? (al-pref-measurement-system) 'metric)
               extractor-fn-metric extractor-fn-imperial))
         (define/override (get-axis-label)
           (if (eq? (al-pref-measurement-system) 'metric)
               "Elevation (m)" "Elevation (ft)"))
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *green*)
           
         )))
(provide axis-corrected-elevation)

(define axis-grade
  (new (class axis-definition% (init) (super-new)
         (define/override (get-extractor-fn sid) extract-grade)
         (define/override (should-filter?) #t)
         (define/override (should-force-zero-start?) #t)
         (define/override (get-axis-label) "Grade (%)")
         (define/override (get-line-color) *green*))))
(provide axis-grade)

(define axis-hr-bpm
  (new (class axis-definition% (init) (super-new)
         (define/override (get-extractor-fn sid) extract-hr)
         (define/override (get-axis-label) "Heart Rate (bpm)")
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *crimson*))))
(provide axis-hr-bpm)

(define axis-hr-pct
  (new (class axis-definition% (init) (super-new)
         (define/override (get-extractor-fn session-id)
           (make-hr-pct-extractor session-id))
         (define/override (get-axis-label) "Heart Rate (% of max)")
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *crimson*))))
(provide axis-hr-pct)

(define axis-hr-zone
  (new (class axis-definition% (init) (super-new)
         (define/override (get-extractor-fn session-id)
           (make-hr-zone-extractor session-id))
         (define/override (get-axis-label) "Heart Rate (zone)")
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *crimson*))))
(provide axis-hr-zone)

(define axis-cadence
  (new (class axis-definition% (init) (super-new)
         (define extractor-fn (extract-raw-data 'cadence))
         (define/override (get-extractor-fn sid) extractor-fn)
         (define/override (get-axis-label) "Cadence (spm)")
         (define/override (should-filter?) #t)
         (define/override (should-force-zero-start?) #t)
         (define/override (get-line-color) *coral*))))
(provide axis-cadence)

(define axis-stride
  (new (class axis-definition% (init) (super-new)
         (define extractor-fn-metric
           (lambda (prev current)
                    (let ((cadence (assq1 'cadence current))
                          (speed (assq1 'speed current)))
                      (if (and speed cadence (> cadence 0))
                          (/ (* speed 60) (* 2 cadence))
                          #f))))
         (define extractor-fn-imperial
           (lambda (prev current)
                    (let ((cadence (assq1 'cadence current))
                          (speed (assq1 'speed current)))
                      (if (and speed cadence (> cadence 0))
                          (m->ft (/ (* speed 60) (* 2 cadence)))
                          #f))))
         (define/override (get-extractor-fn sid)
           (if (eq? (al-pref-measurement-system) 'metric)
               extractor-fn-metric extractor-fn-imperial))
         (define/override (get-axis-label)
           (if (eq? (al-pref-measurement-system) 'metric)
               "Stride (m)" "Stride (ft)"))
         (define/override (should-filter?) #t)
         (define/override (should-force-zero-start?) #t)
         (define/override (get-histogram-bucket-slot) 0.1)
         (define/override (get-line-color) *yellow*))))
(provide axis-stride)

(define axis-vertical-oscillation
  (new (class axis-definition% (init) (super-new)
         (define extractor-fn-metric (extract-raw-data 'vertical-oscillation))
         (define extractor-fn-imperial
           (let ((fn (extract-raw-data 'vertical-oscillation)))
                    (lambda (prev next)
                      (cond ((fn prev next) => mm->inch)
                            (#t #f)))))
         (define/override (get-extractor-fn sid)
           (if (eq? (al-pref-measurement-system) 'metric)
               extractor-fn-metric extractor-fn-imperial))
         (define/override (get-axis-label)
           (if (eq? (al-pref-measurement-system) 'metric)
               "Vertical Oscillation (mm)" "Vertical Oscillation (inch)"))
         (define/override (should-filter?) #t)
         (define/override (should-force-zero-start?) #t)
         (define/override (get-line-color) *yellow*)
         (define/override (inverted-best-avg?) #t))))
(provide axis-vertical-oscillation)

(define axis-stance-time
  (new (class axis-definition% (init) (super-new)
         (define extractor-fn (extract-raw-data 'stance-time))
         (define/override (get-extractor-fn sid) extractor-fn)
         (define/override (get-axis-label) "Ground Contact Time (ms)")
         (define/override (should-filter?) #t)
         (define/override (should-force-zero-start?) #t)
         (define/override (get-line-color) *yellow*)
         (define/override (inverted-best-avg?) #t))))
(provide axis-stance-time)

(define axis-stance-time-percent
  (new (class axis-definition% (init) (super-new)
         (define extractor-fn (extract-raw-data 'stance-time-percent))
         (define/override (get-extractor-fn sid) extractor-fn)
         (define/override (get-axis-label) "Ground Contact Time (%)")
         (define/override (should-filter?) #t)
         (define/override (should-force-zero-start?) #t)
         (define/override (get-line-color) *yellow*)
         (define/override (inverted-best-avg?) #t))))
(provide axis-stance-time-percent)

(define axis-power
  (new (class axis-definition% (init) (super-new)
         (define extractor-fn (extract-raw-data 'power))
         (define/override (get-extractor-fn sid) extractor-fn)
         (define/override (get-axis-label) "Power (watts)")
         (define/override (should-filter?) #t)
         (define/override (should-force-zero-start?) #t)
         (define/override (get-line-color) *dark-magenta*))))
(provide axis-power)

(define axis-power-zone
  (new (class axis-definition% (init) (super-new)
         (define/override (get-extractor-fn session-id)
           (make-power-zone-extractor session-id))
         (define/override (get-axis-label) "Power (zone)")
         (define/override (should-filter?) #t)
         (define/override (get-y-range) (cons 0 #f))
         (define/override (should-force-zero-start?) #t)
         (define/override (get-line-color) *dark-magenta*))))
(provide axis-power-zone)

(define axis-torque
  (new (class axis-definition% (init) (super-new)
         (define extractor-fn (lambda (prev current) (extract-torque current)))
         (define/override (get-extractor-fn sid) extractor-fn)
         (define/override (get-axis-label) "Torque (Nm)")
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *purple*))))
(provide axis-torque)

(define axis-left-right-balance
  (new (class axis-definition% (init) (super-new)
         (define extractor-fn (extract-raw-data 'left-right-balance))
         (define/override (get-extractor-fn sid) extractor-fn)
         (define/override (get-axis-label) "Balance (%)")
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *purple*))))
(provide axis-left-right-balance)

(define axis-left-torque-effectiveness
  (new (class axis-definition% (init) (super-new)
         (define extractor-fn (extract-raw-data 'left-torque-effectiveness))
         (define/override (get-extractor-fn sid) extractor-fn)
         (define/override (get-axis-label) "Left Torque Effectiveness (%)")
         (define/override (should-filter?) #t)
         (define/override (get-y-range) (cons 0 100))
         (define/override (get-line-color) *coral*)
         (define/override (get-series-label) "Left Pedal"))))
(provide axis-left-torque-effectiveness)

(define axis-right-torque-effectiveness
  (new (class axis-definition% (init) (super-new)
         (define extractor-fn (extract-raw-data 'right-torque-effectiveness))
         (define/override (get-extractor-fn sid) extractor-fn)
         (define/override (get-axis-label) "Right Torque Effectiveness (%)")
         (define/override (should-filter?) #t)
         (define/override (get-y-range) (cons 0 100))
         (define/override (get-line-color) *cornflower-blue*)
         (define/override (get-series-label) "Right Pedal"))))
(provide axis-right-torque-effectiveness)

(define axis-torque-effectiveness
  (new (class axis-definition% (init) (super-new)
         (define extractor-fn
           (lambda (prev current)
                    (let ((left (assq1 'left-torque-effectiveness current))
                          (right (assq1 'right-torque-effectiveness current)))
                      (if (and left right)
                          (/ (+ left right) 2.0)
                          (or left right)))))
         (define/override (get-extractor-fn sid) extractor-fn)
         (define/override (get-axis-label) "Combined Torque Effectiveness (%)")
         (define/override (should-filter?) #t)
         (define/override (get-y-range) (cons 0 100))
         (define/override (get-line-color) *purple*))))
(provide axis-torque-effectiveness)

(define axis-left-pedal-smoothness
  (new (class axis-definition% (init) (super-new)
         (define extractor-fn (extract-raw-data 'left-pedal-smoothness))
         (define/override (get-extractor-fn sid) extractor-fn)
         (define/override (get-axis-label) "Left Pedal Smoothness (%)")
         (define/override (should-filter?) #t)
         (define/override (get-y-range) (cons 0 50))
         (define/override (get-line-color) *coral*)
         (define/override (get-series-label) "Left Pedal"))))
(provide axis-left-pedal-smoothness)

(define axis-right-pedal-smoothness
  (new (class axis-definition% (init) (super-new)
         (define extractor-fn (extract-raw-data 'right-pedal-smoothness))
         (define/override (get-extractor-fn sid) extractor-fn)
         (define/override (get-axis-label) "Right Pedal Smoothness (%)")
         (define/override (should-filter?) #t)
         (define/override (get-y-range) (cons 0 50))
         (define/override (get-line-color) *cornflower-blue*)
         (define/override (get-series-label) "Right Pedal"))))
(provide axis-right-pedal-smoothness)

(define axis-pedal-smoothness
  (new (class axis-definition% (init) (super-new)
         (define extractor-fn
           (lambda (prev current)
                    (let ((left (assq1 'left-pedal-smoothness current))
                          (right (assq1 'right-pedal-smoothness current)))
                      (if (and left right)
                          (/ (+ left right) 2.0)
                          (or left right)))))
         (define/override (get-extractor-fn sid) extractor-fn)
         (define/override (get-axis-label) "Combined Pedal Smoothness (%)")
         (define/override (should-filter?) #t)
         (define/override (get-y-range) (cons 0 50))
         (define/override (get-line-color) *purple*))))
(provide axis-pedal-smoothness)

(define axis-left-platform-centre-offset
  (new (class axis-definition% (init) (super-new)
         (define extractor-fn (extract-raw-data 'left-pco))
         (define/override (get-extractor-fn sid) extractor-fn)
         (define/override (get-axis-label) "Left Platform Centre Offset (mm)")
         (define/override (should-filter?) #f)
         (define/override (get-line-color) *coral*)
         (define/override (get-series-label) "Left Pedal"))))
(provide axis-left-platform-centre-offset)

(define axis-right-platform-centre-offset
  (new (class axis-definition% (init) (super-new)
         (define extractor-fn (extract-raw-data 'right-pco))
         (define/override (get-extractor-fn sid) extractor-fn)
         (define/override (get-axis-label) "Right Platform Centre Offset (mm)")
         (define/override (should-filter?) #f)
         (define/override (get-line-color) *cornflower-blue*)
         (define/override (get-series-label) "Right Pedal"))))
(provide axis-right-platform-centre-offset)

(define axis-left-power-phase-start
  (new (class axis-definition% (init) (super-new)
         (define extractor-fn (extract-raw-data 'left-pp-start))
         (define/override (get-extractor-fn sid) extractor-fn)
         (define/override (get-axis-label) "Left Power Phase Start")
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *coral*)
         (define/override (get-series-label) "Left Pedal"))))
(provide axis-left-power-phase-start)

(define axis-left-power-phase-end
  (new (class axis-definition% (init) (super-new)
         (define extractor-fn (extract-raw-data 'left-pp-end))
         (define/override (get-extractor-fn sid) extractor-fn)
         (define/override (get-axis-label) "Left Power Phase End")
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *coral*)
         (define/override (get-series-label) "Left Pedal"))))
(provide axis-left-power-phase-end)

(define axis-left-power-phase-angle
  (new (class axis-definition% (init) (super-new)
         (define extractor-fn
           (lambda (prev current)
             (let ((start (assq1 'left-pp-start current))
                   (end (assq1 'left-pp-end current)))
               (if (and start end)
                   (let ((angle (- end start)))
                     (if (< angle 0) (+ angle 360) angle))
                   #f))))
         (define/override (get-extractor-fn sid) extractor-fn)
         (define/override (get-axis-label) "Left Power Phase Angle")
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *coral*)
         (define/override (get-series-label) "Left Pedal"))))
(provide axis-left-power-phase-angle)

(define axis-right-power-phase-start
  (new (class axis-definition% (init) (super-new)
         (define extractor-fn (extract-raw-data 'right-pp-start))
         (define/override (get-extractor-fn sid) extractor-fn)
         (define/override (get-axis-label) "Right Power Phase Start")
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *cornflower-blue*)
         (define/override (get-series-label) "Right Pedal"))))
(provide axis-right-power-phase-start)

(define axis-right-power-phase-end
  (new (class axis-definition% (init) (super-new)
         (define extractor-fn (extract-raw-data 'right-pp-end))
         (define/override (get-extractor-fn sid) extractor-fn)
         (define/override (get-axis-label) "Right Power Phase End")
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *cornflower-blue*)
         (define/override (get-series-label) "Right Pedal"))))
(provide axis-right-power-phase-end)

(define axis-right-power-phase-angle
  (new (class axis-definition% (init) (super-new)
         (define extractor-fn
           (lambda (prev current)
             (let ((start (assq1 'right-pp-start current))
                   (end (assq1 'right-pp-end current)))
               (if (and start end)
                   (let ((angle (- end start)))
                     (if (< angle 0) (+ angle 360) angle))
                   #f))))
         (define/override (get-extractor-fn sid) extractor-fn)
         (define/override (get-axis-label) "Right Power Phase Angle")
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *cornflower-blue*)
         (define/override (get-series-label) "Right Pedal"))))
(provide axis-right-power-phase-angle)

(define axis-left-peak-power-phase-start
  (new (class axis-definition% (init) (super-new)
         (define extractor-fn (extract-raw-data 'left-ppp-start))
         (define/override (get-extractor-fn sid) extractor-fn)
         (define/override (get-axis-label) "Left Peak Power Phase Start")
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *coral*)
         (define/override (get-series-label) "Left Pedal"))))
(provide axis-left-peak-power-phase-start)

(define axis-left-peak-power-phase-end
  (new (class axis-definition% (init) (super-new)
         (define extractor-fn (extract-raw-data 'left-ppp-end))
         (define/override (get-extractor-fn sid) extractor-fn)
         (define/override (get-axis-label) "Left Peak Power Phase End")
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *coral*)
         (define/override (get-series-label) "Left Pedal"))))
(provide axis-left-peak-power-phase-end)

(define axis-left-peak-power-phase-angle
  (new (class axis-definition% (init) (super-new)
         (define extractor-fn
           (lambda (prev current)
             (let ((start (assq1 'left-ppp-start current))
                   (end (assq1 'left-ppp-end current)))
               (if (and start end)
                   (let ((angle (- end start)))
                     (if (< angle 0) (+ angle 360) angle))
                   #f))))
         (define/override (get-extractor-fn sid) extractor-fn)
         (define/override (get-axis-label) "Left Peak Power Phase Angle")
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *coral*)
         (define/override (get-series-label) "Left Pedal"))))
(provide axis-left-peak-power-phase-angle)

(define axis-right-peak-power-phase-start
  (new (class axis-definition% (init) (super-new)
         (define extractor-fn (extract-raw-data 'right-ppp-start))
         (define/override (get-extractor-fn sid) extractor-fn)
         (define/override (get-axis-label) "Right Peak Power Phase Start")
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *cornflower-blue*)
         (define/override (get-series-label) "Right Pedal"))))
(provide axis-right-peak-power-phase-start)

(define axis-right-peak-power-phase-end
  (new (class axis-definition% (init) (super-new)
         (define extractor-fn (extract-raw-data 'right-ppp-end))
         (define/override (get-extractor-fn sid) extractor-fn)
         (define/override (get-axis-label) "Right Peak Power Phase End")
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *cornflower-blue*)
         (define/override (get-series-label) "Right Pedal"))))
(provide axis-right-peak-power-phase-end)

(define axis-right-peak-power-phase-angle
  (new (class axis-definition% (init) (super-new)
         (define extractor-fn
           (lambda (prev current)
             (let ((start (assq1 'right-ppp-start current))
                   (end (assq1 'right-ppp-end current)))
               (if (and start end)
                   (let ((angle (- end start)))
                     (if (< angle 0) (+ angle 360) angle))
                   #f))))
         (define/override (get-extractor-fn sid) extractor-fn)
         (define/override (get-axis-label) "Right Peak Power Phase Angle")
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *cornflower-blue*)
         (define/override (get-series-label) "Right Pedal"))))
(provide axis-right-peak-power-phase-angle)

(define axis-swim-pace
  (new (class axis-definition% (init) (super-new)
         (define/override (get-extractor-fn sid)
           (if (eq? (al-pref-measurement-system) 'metric)
               extract-swim-pace extract-swim-pace-imperial))
         (define/override (get-axis-ticks) (time-ticks #:formats '("~M:~f")))
         (define/override (get-axis-label)
           (if (eq? (al-pref-measurement-system) 'metric)
               "Pace (min/100m)" "Pace (min/100yd)"))
         (define/override (get-y-range) (cons 0 #f))
         (define/override (get-line-color) 'smart))))
(provide axis-swim-pace)

(define axis-swim-swolf
  (new (class axis-definition% (init) (super-new)
         (define/override (get-extractor-fn sid) length-swolf)
         (define/override (get-axis-label) "SWOLF")
         (define/override (get-y-range) (cons 0 #f))
         (define/override (get-line-color) *green*))))
(provide axis-swim-swolf)

(define axis-swim-stroke-count
  (new (class axis-definition% (init) (super-new)
         (define extractor-fn (lambda (l) (or (length-total-cycles l) 0)))
         (define/override (get-extractor-fn sid) extractor-fn)
         (define/override (get-axis-label) "Strokes / Length")
         (define/override (get-y-range) (cons 0 #f))
         (define/override (get-line-color) *red*))))
(provide axis-swim-stroke-count)

(define axis-swim-avg-cadence
  (new (class axis-definition% (init) (super-new)
         (define extractor-fn
           (lambda (l)
             ;; This looks much better if we compute a fractional cadence.
             (let ((cycles (or (length-total-cycles l) 0))
                   (time (length-time l)))
               (if time (* 60.0 (/ cycles time)) #f))))
         (define/override (get-extractor-fn sid) extractor-fn)
         (define/override (get-axis-label) "Strokes / Min")
         (define/override (get-y-range) (cons 0 #f))
         (define/override (get-line-color) *red*))))
(provide axis-swim-avg-cadence)

(define axis-swim-distance
  (new (class axis-definition% (init) (super-new)
         (define extractor-fn-metric length-distance)
         (define extractor-fn-imperial
           (lambda (x) (cond ((length-distance x) => m->yd)
                             (#t #f))))
         (define/override (get-extractor-fn sid)
           (if (eq? (al-pref-measurement-system) 'metric)
               extractor-fn-metric extractor-fn-imperial))
         (define/override (get-axis-label)
           (if (eq? (al-pref-measurement-system) 'metric)
               "Distance (meters)" "Distance (yards)")))))
(provide axis-swim-distance)

(define axis-swim-time
  (new (class axis-definition% (init) (super-new)
         (define/override (get-extractor-fn sid) length-time)
         (define/override (get-fixup-fn) fixup-swim-time)
         (define/override (get-axis-ticks) (time-ticks #:formats '("~H:~M")))
         (define/override (get-axis-label) "Time (hour:min)"))))
(provide axis-swim-time)

