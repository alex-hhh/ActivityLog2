#lang racket/base
;; series-meta.rkt -- helper classes for plotting various data series
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
         "utilities.rkt"
         (prefix-in ct: "color-theme.rkt"))


;;..................................................... axis definitions ....

(define zone-labels '(z0 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10))

(define (zone->label z)
  (define index (max 0 (min (sub1 (length zone-labels)) (exact-truncate z))))
  (list-ref zone-labels index))

;; We use too many ways to represent sport ids :-(
(define (sport-id sport)
  (cond ((number? sport) sport)
        ((vector? sport) (vector-ref sport 0))
        ((cons? sport) (car sport))
        (#t #f)))

(define (sub-sport-id sport)
  (cond ((number? sport) #f)
        ((vector? sport) (vector-ref sport 1))
        ((cons? sport) (cdr sport))
        (#t #f)))

(define (sport-zones sport sid metric)
  (if sid
      (get-session-sport-zones sid metric)
      (get-sport-zones (sport-id sport) (sub-sport-id sport) metric)))

(define (is-runnig? sport)
  (eqv? (sport-id sport) 1))

(define (is-cycling? sport)
  (eqv? (sport-id sport) 2))

(define (is-lap-swimming? sport)
  (and (eqv? (sport-id sport) 5)
       (eqv? (sub-sport-id sport) 17)))

(provide is-runnig? is-cycling? is-lap-swimming?)

;; Provides meta data information about a series in a data frame, mostly
;; related on how to plot values of this series.
(define series-metadata%
  (class object% (init) (super-new)
    ;; When #t, we attempt to detect stop points and generate additional data
    ;; points with 0 Y values, this makes graphs look nicer.
    (define/public (has-stop-detection?) #f)

    ;; Whether to filter values in this series using a low pass filter.
    ;; Whether filtering happens or not is defined on an Y axis (e.g. we may
    ;; want to filter heart rate but not elevation).
    (define/public (should-filter?) #f)

    ;; The width of the low pass filter.  The filter width is determined by
    ;; the X axis (e.g. distance and time will have different widths).
    (define/public (filter-width) #f)

    ;; Return the base multiplier for determining the final bucket width in a
    ;; histogram.  This value is multiplied with the user specified bucket
    ;; size.  It is used when measurements have fractional values which are
    ;; significant, as actual bucket sizes have 1 as the default and minimum
    ;; value.
    (define/public (histogram-bucket-slot) 1.0)

    ;; Returns #t if BEST-AVG values for this series are "better" when they
    ;; are smaller.  This is used for series like pace, where a smaller value
    ;; means you are running faster, or for series which need to be minimized,
    ;; for example ground contact time or vertical oscillation.
    (define/public (inverted-best-avg?) #f)

    ;; Return the ticks to use for plotting values of this series.
    (define/public (plot-ticks) (linear-ticks))

    ;; Returns the text to display for this series when it appears in various
    ;; selection boxes.
    (define/public (headline) (axis-label))

    ;; Return a string to use as the axis label when this series is used in a
    ;; plot.
    (define/public (axis-label) (raise "no axis label defined"))

    ;; Return a string to use as the legend value on a plot for this series
    (define/public (plot-label) #f)

    (define color #f)

    ;; Return the color to use for plotting this series.  We look it up in the
    ;; (series-colors) ALIST for a tag the same as (series-name)
    (define/public (plot-color)
      (unless color
        (let* ((tag (string->symbol (series-name)))
               (color-item (assq tag (ct:series-colors))))
          (when color-item
            (set! color (cdr color-item)))))
      color)


    ;; Return the Y range for ploting this series.  Returns a (cons LOW HIGH),
    ;; either of them can be #f, in which case the range is automatically
    ;; detected.  This can be used to force Y ranges for certain series (for
    ;; example the balance series can be centered arount 50%, regardless of
    ;; the data in the plot).
    (define/public (y-range) #f)

    ;; Return the name of the series in the data frame
    (define/public (series-name) #f)

    ;; Return the number of fractional digits to keep when truncating values
    ;; in this series.
    (define/public (fractional-digits) 0)

    ;; Value to replace missing values in this series
    (define/public (missing-value) 0)

    ;; Return a function that can classify values for this series into
    ;; discrete elements (tags), returns #f if there is no such function
    (define/public (factor-fn sport (sid #f)) #f)

    ;; Return an alist mapping factor names to colors
    (define/public (factor-colors) (ct:factor-colors))

    ))

(provide series-metadata%)


(define axis-distance
  (new (class series-metadata% (init) (super-new)
         (define/override (filter-width) 0.05)
         (define/override (axis-label)
           (if (eq? (al-pref-measurement-system) 'metric)
               "Distance (km)" "Distance (mi)"))
         (define/override (series-name) "distance")
         (define/override (fractional-digits) 2))))

(provide axis-distance)

(define axis-elapsed-time
  (new (class series-metadata% (init) (super-new)
         (define/override (plot-ticks) (time-ticks #:formats '("~H:~M")))
         (define/override (has-stop-detection?) #t)
         (define/override (filter-width) 5.0) ; seconds
         (define/override (axis-label) "Elapsed Time (hour:min)")
         (define/override (series-name) "elapsed"))))
(provide axis-elapsed-time)

(define axis-timer-time
  (new (class series-metadata% (init) (super-new)
         (define/override (plot-ticks) (time-ticks #:formats '("~H:~M")))
         (define/override (has-stop-detection?) #f)
         (define/override (filter-width) 5.0)
         (define/override (axis-label) "Time (hour:min)")
         (define/override (series-name) "timer"))))
(provide axis-timer-time)

(define axis-speed
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label)
           (if (eq? (al-pref-measurement-system) 'metric)
               "Speed (km/h)" "Speed (mi/h)"))
         (define/override (should-filter?) #t)
         (define/override (histogram-bucket-slot) 0.1)
         (define/override (series-name) "speed")
         (define/override (fractional-digits) 2)

         (define/override (factor-fn sport (sid #f))
           (let ((zones (sport-zones sport sid 2))
                 (metric? (eq? (al-pref-measurement-system) 'metric)))
             (if zones
                 ;; NOTE: value passed in is in km/h or mi/h, we need to
                 ;; convert it back to meters/sec before we can find the zone.
                 (lambda (val)
                   (let* ((val-mps (if metric? (/ (* val 1000.0) 3600.0)
                                       (/ (* val 1609.0) 3600.0)))
                          (zone (val->zone val-mps zones)))
                     (zone->label zone)))
                 #f)))

         (define/override (factor-colors) (ct:zone-colors))

         )))
(provide axis-speed)

(define axis-pace
  (new (class series-metadata% (init) (super-new)
         (define/override (plot-ticks) (time-ticks #:formats '("~M:~f")))
         (define/override (axis-label)
           (if (eq? (al-pref-measurement-system) 'metric)
               "Pace (min/km)" "Pace (min/mi)"))
         (define/override (should-filter?) #t)
         ;; (define/override (y-range) (cons 0 #f))
         (define/override (inverted-best-avg?) #t)
         (define/override (series-name) "pace")
         ;; NOTE: speed is stored in the FIT file as m/s * 1000 (and truncated
         ;; to an integer).  When converting to pace, the minimum delta
         ;; between two representable pace values is 0.09 (do the maths!) and
         ;; this assumes that the device writes the speed values with 1 mm
         ;; precision!  Realistically, precision for the pace values is at
         ;; best 1 second / km
         (define/override (histogram-bucket-slot) 1)
         (define/override (factor-fn sport (sid #f))
           (let ((zones (sport-zones sport sid 2))
                 (metric? (eq? (al-pref-measurement-system) 'metric)))
             (if zones
                 ;; NOTE: value passed in is in sec/km or sec/mi (NOT
                 ;; minutes), we need to convert it back to meters/sec before
                 ;; we can find the zone.
                 (lambda (val)
                   (if (and (number? val) (> val 0))
                       (let* ((val-mps (if metric? (/ 1000.0 val) (/ 1609.0 val)))
                              (zone (val->zone val-mps zones)))
                         (zone->label zone))
                       ;; Put invalid values in zone 0
                       (zone->label 0)))
                 #f)))

         (define/override (factor-colors) (ct:zone-colors))


         )))
(provide axis-pace)

(define axis-speed-zone
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label) "Speed (zone)")
         (define/override (should-filter?) #t)
         ;; (define/override (y-range) (cons 0 #f))
         (define/override (series-name) "speed-zone")
         (define/override (fractional-digits) 2)

         (define/override (factor-fn sport (sid #f))
           (lambda (val) (zone->label val)))
         (define/override (factor-colors) (ct:zone-colors))

         )))
(provide axis-speed-zone)

(define axis-elevation
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label)
           (if (eq? (al-pref-measurement-system) 'metric)
               "Elevation (m)" "Elevation (ft)"))
         (define/override (should-filter?) #t)
         (define/override (series-name) "alt")
         (define/override (fractional-digits) 1)
         )))
(provide axis-elevation)

(define axis-corrected-elevation
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label)
           (if (eq? (al-pref-measurement-system) 'metric)
               "Elevation (m)" "Elevation (ft)"))
         (define/override (should-filter?) #t)
         (define/override (series-name) "calt")
         (define/override (fractional-digits) 1)
         )))
(provide axis-corrected-elevation)

(define axis-grade
  (new (class series-metadata% (init) (super-new)
         (define/override (should-filter?) #t)
         (define/override (axis-label) "Grade (%)")
         (define/override (series-name) "grade")
         (define/override (histogram-bucket-slot) 0.01)
         (define/override (fractional-digits) 2)
         )))
(provide axis-grade)

;; A grade definition where lower grades are better.  This is usefull for
;; activities with lots of descent (e.g. Skiiing)
(define axis-grade-inverted
  (new (class series-metadata% (init) (super-new)
         (define/override (should-filter?) #f)
         (define/override (axis-label) "Descent (%)")
         (define/override (series-name) "grade")
         (define/override (fractional-digits) 1)
         (define/override (inverted-best-avg?) #t))))
(provide axis-grade-inverted)

(define axis-hr-bpm
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label) "Heart Rate (bpm)")
         (define/override (should-filter?) #t)
         (define/override (series-name) "hr")

         (define/override (factor-fn sport (sid #f))
           (let ((zones (sport-zones sport sid 1)))
             (if zones
                 (lambda (val)
                   (let ((zone (val->zone val zones)))
                     (zone->label zone)))
                 #f)))

         (define/override (factor-colors) (ct:zone-colors))


         )))
(provide axis-hr-bpm)

(define axis-hr-pct
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label) "Heart Rate (% of max)")
         (define/override (should-filter?) #t)
         (define/override (series-name) "hr-pct")
         (define/override (fractional-digits) 1)
         )))
(provide axis-hr-pct)

(define axis-hr-zone
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label) "Heart Rate (zone)")
         (define/override (should-filter?) #t)
         (define/override (series-name) "hr-zone")
         (define/override (fractional-digits) 2)

         (define/override (factor-fn sport (sid #f))
           (lambda (val) (zone->label val)))
         (define/override (factor-colors) (ct:zone-colors))

         )))
(provide axis-hr-zone)

(define axis-cadence
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label) "Cadence (spm)")
         (define/override (should-filter?) #t)
         (define/override (series-name) "cad")
         (define/override (fractional-digits) 1)
         (define/override (histogram-bucket-slot) 0.1)

         (define (cadence-factor/run cad)
           (define cad1 (* 2 cad))
           (cond ((> cad1 185) 'purple)
                 ((> cad1 174) 'blue)
                 ((> cad1 163) 'green)
                 ((> cad1 151) 'orange)
                 (#t 'red)))

         (define (cadence-factor/bike cad)
           (cond ((> cad 95) 'purple)
                 ((> cad 85) 'blue)
                 ((> cad 75) 'green)
                 ((> cad 65) 'orange)
                 (#t 'red)))

         (define/override (factor-fn sport (sid #f))
           (cond ((is-runnig? sport) cadence-factor/run)
                 ((is-cycling? sport) cadence-factor/bike)
                 (#t #f)))

         )))
(provide axis-cadence)

(define axis-stride
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label)
           (if (eq? (al-pref-measurement-system) 'metric)
               "Stride (m)" "Stride (ft)"))
         (define/override (should-filter?) #t)
         ;; NOTE: cadence is measured with a 0.5 step precision, so the
         ;; precision of the stride calculation is 0.5 / 60 = 0.00833 meters
         (define/override (histogram-bucket-slot) 0.001)
         (define/override (series-name) "stride")
         (define/override (fractional-digits) 2)
         )))
(provide axis-stride)

(define axis-vratio
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label) "Vertical Ratio (%)")
         (define/override (should-filter?) #t)
         (define/override (histogram-bucket-slot) 0.1)
         (define/override (series-name) "vratio")
         (define/override (inverted-best-avg?) #t)
         (define/override (fractional-digits) 2)

         (define (vratio-factor vratio)
           (cond ((> vratio 10.1) 'red)
                 ((> vratio 8.7) 'orange)
                 ((> vratio 7.5) 'green)
                 ((> vratio 6.1) 'blue)
                 (#t 'purple)))

         (define/override (factor-fn sport (sid #f))
           (and (is-runnig? sport) vratio-factor))

         )))
(provide axis-vratio)

(define axis-vertical-oscillation
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label)
           (if (eq? (al-pref-measurement-system) 'metric)
               "Vertical Oscillation (mm)" "Vertical Oscillation (inch)"))
         (define/override (should-filter?) #t)
         (define/override (histogram-bucket-slot) 0.1)
         (define/override (inverted-best-avg?) #t)
         (define/override (series-name) "vosc")
         (define/override (fractional-digits) 1)

         (define (vosc-factor vosc)
           (cond ((> vosc 118) 'red)
                 ((> vosc 111) 'orange)
                 ((> vosc 84) 'green)
                 ((> vosc 67) 'blue)
                 (#t 'purple)))

         (define/override (factor-fn sport (sid #f))
           (and (is-runnig? sport) vosc-factor))

         )))
(provide axis-vertical-oscillation)

(define axis-stance-time
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label) "Ground Contact Time (ms)")
         (define/override (should-filter?) #t)
         (define/override (inverted-best-avg?) #t)
         (define/override (series-name) "gct")

         (define (gct-factor gct)
           (cond ((> gct 305) 'red)
                 ((> gct 273) 'orange)
                 ((> gct 241) 'green)
                 ((> gct 208) 'blue)
                 (#t 'purple)))

         (define/override (factor-fn sport (sid #f))
           (and (is-runnig? sport) gct-factor))

         )))
(provide axis-stance-time)

(define axis-stance-time-percent
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label) "Ground Contact Time (%)")
         (define/override (should-filter?) #t)
         (define/override (histogram-bucket-slot) 0.1)
         (define/override (inverted-best-avg?) #t)
         (define/override (series-name) "pgct")
         (define/override (fractional-digits) 1)
         )))
(provide axis-stance-time-percent)

(define axis-power
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label) "Power (watts)")
         (define/override (should-filter?) #t)
         (define/override (series-name) "pwr")

         (define/override (factor-fn sport (sid #f))
           (let ((zones (sport-zones sport sid 3)))
             (if zones
                 (lambda (val)
                   (let ((zone (val->zone val zones)))
                     (zone->label zone)))
                 #f)))

         (define/override (factor-colors) (ct:zone-colors))

         )))
(provide axis-power)

(define axis-torque
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label) "Torque (N m)")
         (define/override (should-filter?) #t)
         (define/override (series-name) "torque")
         )))
(provide axis-torque)

(define axis-power-zone
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label) "Power (zone)")
         (define/override (should-filter?) #t)
         (define/override (y-range) (cons 0 #f))
         (define/override (series-name) "pwr-zone")
         (define/override (fractional-digits) 1)

         (define/override (factor-fn sport (sid #f))
           (lambda (val) (zone->label val)))
         (define/override (factor-colors) (ct:zone-colors))

         )))
(provide axis-power-zone)

(define axis-left-right-balance
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label) "Balance (%)")
         (define/override (should-filter?) #t)
         (define/override (series-name) "lrbal")
         (define/override (histogram-bucket-slot) 0.1)
         (define/override (fractional-digits) 1)
         ;; Y-Range for left-right balance only makes sense between 45% and
         ;; 55%, if the value is out of that range it is waaay off anyway.
         ;; Keep the chart centered around 50%.
         (define/override (y-range) (cons 45 55))

         (define (lrbal-factors lrbal)
           (cond ((> lrbal 52.2) 'red)
                 ((> lrbal 50.8) 'orange)
                 ((> lrbal 49.2) 'green)
                 ((> lrbal 47.8) 'orange)
                 (#t 'red)))

         (define/override (factor-fn sport (sid #f))
           (if (or (is-runnig? sport) (is-cycling? sport))
               lrbal-factors
               #f))

         )))
(provide axis-left-right-balance)

(define axis-left-torque-effectiveness
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label) "Torque Effectiveness (%)")
         (define/override (should-filter?) #t)
         (define/override (y-range) (cons 0 100))
         (define/override (headline) "Torque Effectiveness, Left Pedal (%)")
         (define/override (plot-label) "Left Pedal")
         (define/override (series-name) "lteff")
         (define/override (fractional-digits) 1)
         )))
(provide axis-left-torque-effectiveness)

(define axis-right-torque-effectiveness
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label) "Torque Effectiveness (%)")
         (define/override (should-filter?) #t)
         (define/override (y-range) (cons 0 100))
         (define/override (headline) "Torque Effectiveness, Right Pedal (%)")
         (define/override (plot-label) "Right Pedal")
         (define/override (series-name) "rteff")
         (define/override (fractional-digits) 1))))

(provide axis-right-torque-effectiveness)

(define axis-left-pedal-smoothness
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Pedal Smoothness, Left Pedal (%)")
         (define/override (axis-label) "Pedal Smoothness (%)")
         (define/override (should-filter?) #t)
         (define/override (y-range) (cons 0 50))
         (define/override (plot-label) "Left Pedal")
         (define/override (series-name) "lpsmth")
         (define/override (fractional-digits) 1)
         )))
(provide axis-left-pedal-smoothness)

(define axis-right-pedal-smoothness
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Pedal Smoothness, Right Pedal (%)")
         (define/override (axis-label) "Pedal Smoothness (%)")
         (define/override (should-filter?) #t)
         (define/override (y-range) (cons 0 50))
         (define/override (plot-label) "Right Pedal")
         (define/override (series-name) "rpsmth")
         (define/override (fractional-digits) 1))))
(provide axis-right-pedal-smoothness)

(define axis-left-platform-centre-offset
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Platform Centre Offset, Left Pedal (mm)")
         (define/override (axis-label) "Platform Centre Offset (mm)")
         (define/override (should-filter?) #t)
         (define/override (plot-label) "Left Pedal")
         (define/override (series-name) "lpco"))))
(provide axis-left-platform-centre-offset)

(define axis-right-platform-centre-offset
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Platform Centre Offset, Right Pedal (mm)")
         (define/override (axis-label) "Platform Centre Offset (mm)")
         (define/override (should-filter?) #t)
         (define/override (plot-label) "Right Pedal")
         (define/override (series-name) "rpco"))))
(provide axis-right-platform-centre-offset)

(define axis-left-power-phase-start
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Power Phase Start, Left Pedal")
         (define/override (axis-label) "Power Phase Start")
         (define/override (should-filter?) #t)
         (define/override (plot-label) "Left Pedal")
         (define/override (series-name) "lpps")
         )))
(provide axis-left-power-phase-start)

(define axis-left-power-phase-end
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Power Phase End, Left Pedal")
         (define/override (axis-label) "Power Phase End")
         (define/override (should-filter?) #t)
         (define/override (plot-label) "Left Pedal")
         (define/override (series-name) "lppe"))))
(provide axis-left-power-phase-end)

(define axis-left-power-phase-angle
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Power Phase Angle, Left Pedal")
         (define/override (axis-label) "Power Phase Angle")
         (define/override (should-filter?) #t)
         (define/override (plot-label) "Left Pedal")
         (define/override (series-name) "lppa"))))
(provide axis-left-power-phase-angle)

(define axis-right-power-phase-start
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Power Phase Start, Right Pedal")
         (define/override (axis-label) "Power Phase Start")
         (define/override (should-filter?) #t)
         (define/override (plot-label) "Right Pedal")
         (define/override (series-name) "rpps")
         )))
(provide axis-right-power-phase-start)

(define axis-right-power-phase-end
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Power Phase End, Right Pedal")
         (define/override (axis-label) "Power Phase End")
         (define/override (should-filter?) #t)
         (define/override (plot-label) "Right Pedal")
         (define/override (series-name) "rppe"))))
(provide axis-right-power-phase-end)

(define axis-right-power-phase-angle
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Power Phase Angle, Right Pedal")
         (define/override (axis-label) "Power Phase Angle")
         (define/override (should-filter?) #t)
         (define/override (plot-label) "Right Pedal")
         (define/override (series-name) "rppa"))))
(provide axis-right-power-phase-angle)

(define axis-left-peak-power-phase-start
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Peak Power Phase Start, Left Pedal")
         (define/override (axis-label) "Peak Power Phase Start")
         (define/override (should-filter?) #t)
         (define/override (plot-label) "Left Pedal")
         (define/override (series-name) "lppps")
         )))
(provide axis-left-peak-power-phase-start)

(define axis-left-peak-power-phase-end
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Peak Power Phase End, Left Pedal")
         (define/override (axis-label) "Peak Power Phase End")
         (define/override (should-filter?) #t)
         (define/override (plot-label) "Left Pedal")
         (define/override (series-name) "lpppe"))))
(provide axis-left-peak-power-phase-end)

(define axis-left-peak-power-phase-angle
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Peak Power Phase Angle, Left Pedal")
         (define/override (axis-label) "Peak Power Phase Angle")
         (define/override (should-filter?) #t)
         (define/override (plot-label) "Left Pedal")
         (define/override (series-name) "lpppa"))))
(provide axis-left-peak-power-phase-angle)

(define axis-right-peak-power-phase-start
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Peak Power Phase Start, Right Pedal")
         (define/override (axis-label) "Peak Power Phase Start")
         (define/override (should-filter?) #t)
         (define/override (plot-label) "Right Pedal")
         (define/override (series-name) "rppps"))))
(provide axis-right-peak-power-phase-start)

(define axis-right-peak-power-phase-end
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Peak Power Phase End, Right Pedal")
         (define/override (axis-label) "Peak Power Phase End")
         (define/override (should-filter?) #t)
         (define/override (plot-label) "Right Pedal")
         (define/override (series-name) "rpppe"))))
(provide axis-right-peak-power-phase-end)

(define axis-right-peak-power-phase-angle
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Peak Power Phase Angle, Right Pedal")
         (define/override (axis-label) "Peak Power Phase Angle")
         (define/override (should-filter?) #t)
         (define/override (plot-label) "Right Pedal")
         (define/override (series-name) "rpppa"))))
(provide axis-right-peak-power-phase-angle)

(define axis-swim-pace
  (new (class series-metadata% (init) (super-new)
         (define/override (plot-ticks) (time-ticks #:formats '("~M:~f")))
         (define/override (axis-label)
           (if (eq? (al-pref-measurement-system) 'metric)
               "Pace (min/100m)" "Pace (min/100yd)"))
         (define/override (y-range) (cons 0 #f))
         (define/override (plot-color) 'smart)
         (define/override (series-name) "pace")
         )))
(provide axis-swim-pace)

(define axis-swim-swolf
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label) "SWOLF")
         (define/override (y-range) (cons 0 #f))
         (define/override (series-name) "swolf")
         )))
(provide axis-swim-swolf)

(define axis-swim-stroke-count
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label) "Strokes / Length")
         (define/override (y-range) (cons 0 #f))
         (define/override (series-name) "strokes")
         )))
(provide axis-swim-stroke-count)

(define axis-swim-stroke-length
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label) "Stride")
         (define/override (y-range) (cons 0 #f))
         (define/override (series-name) "stride")
         (define/override (fractional-digits) 2)
         (define/override (histogram-bucket-slot) 0.01)
         )))
(provide axis-swim-stroke-length)

(define axis-swim-avg-cadence
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label) "Strokes / Min")
         (define/override (y-range) (cons 0 #f))

         (define color #f)
         (define/override (plot-color)
           ;; The swim cadence series is named "cad", but the color for this
           ;; series lives under the 'swim-cad tag, so we have to roll our own
           ;; plot-color method.
           (unless color
             (let* ((tag 'swim-cad)
                    (color-item (assq tag (ct:series-colors))))
               (when color-item
                 (set! color (cdr color-item)))))
           color)

         (define/override (series-name) "cad")
         (define/override (histogram-bucket-slot) 0.1)
         (define/override (fractional-digits) 1)
         )))
(provide axis-swim-avg-cadence)

(define axis-swim-distance
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label)
           (if (eq? (al-pref-measurement-system) 'metric)
               "Distance (meters)" "Distance (yards)"))
         (define/override (series-name) "distance"))))
(provide axis-swim-distance)

(define axis-swim-time
  (new (class series-metadata% (init) (super-new)
         (define/override (plot-ticks) (time-ticks #:formats '("~H:~M")))
         (define/override (axis-label) "Time (hour:min)")
         (define/override (series-name) "elapsed"))))
(provide axis-swim-time)

(define all-series-meta
  (list
   axis-distance
   axis-elapsed-time
   axis-timer-time
   axis-speed
   axis-pace
   axis-speed-zone
   axis-elevation
   axis-corrected-elevation
   axis-grade
   axis-grade-inverted
   axis-hr-bpm
   axis-hr-pct
   axis-hr-zone
   axis-cadence
   axis-stride
   axis-vratio
   axis-vertical-oscillation
   axis-stance-time
   axis-stance-time-percent
   axis-power
   axis-torque
   axis-power-zone
   axis-left-right-balance
   axis-left-torque-effectiveness
   axis-right-torque-effectiveness
   axis-left-pedal-smoothness
   axis-right-pedal-smoothness
   axis-left-platform-centre-offset
   axis-right-platform-centre-offset
   axis-left-power-phase-start
   axis-left-power-phase-end
   axis-left-power-phase-angle
   axis-right-power-phase-start
   axis-right-power-phase-end
   axis-right-power-phase-angle
   axis-left-peak-power-phase-start
   axis-left-peak-power-phase-end
   axis-left-peak-power-phase-angle
   axis-right-peak-power-phase-start
   axis-right-peak-power-phase-end
   axis-right-peak-power-phase-angle
   axis-swim-pace
   axis-swim-swolf
   axis-swim-stroke-count
   axis-swim-stroke-length
   axis-swim-avg-cadence
   axis-swim-distance
   axis-swim-time))

(define (find-meta-for-series name)
  (findf (lambda (meta) (equal? (send meta series-name) name))
         all-series-meta))
(provide find-meta-for-series)
