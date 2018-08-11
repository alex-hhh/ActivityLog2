#lang racket/base
;; series-meta.rkt -- helper classes for plotting various data series
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015, 2018 Alex Harsanyi <AlexHarsanyi@gmail.com>
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

(require plot/no-gui
         racket/class
         racket/list
         racket/math
         racket/draw
         racket/format
         racket/contract
         pict
         "fmt-util.rkt"
         "sport-charms.rkt"
         "pdmodel.rkt"
         (prefix-in ct: "color-theme.rkt"))

;; Fonts and colors for the Power-Duration information pane

(define pd-background (make-object color% #xff #xf8 #xdc 0.75))
(define pd-item-color (make-object color% #x2f #x4f #x4f))
(define pd-label-color (make-object color% #x77 #x88 #x99))
(define pd-title-font (send the-font-list find-or-create-font 12 'default 'normal 'normal))
(define pd-item-font (send the-font-list find-or-create-font 12 'default 'normal 'normal))
(define pd-label-font (send the-font-list find-or-create-font 10 'default 'normal 'normal))
(define pd-title-face (cons pd-item-color pd-title-font))
(define pd-item-face (cons pd-item-color pd-item-font))
(define pd-label-face (cons pd-label-color pd-label-font))

;; Find the actual maximum value for a pd function, starting at TMAX and going
;; downwards.  We assume the function is at least valid at T = 20 seconds.
;; This is used to find the maximum range of an empirical PD function
;; (best-avg), when there are only short activities available.
(define (find-actual-max pd tmax)
  (define min 20)
  (if (pd min)
      (let loop ((amin min)
                 (amax tmax))
        (cond ((pd amax) (floor amax))
              ((<= (- amax amin) 2.0)
               (floor amin))
              (#t
               (let ((mid (/ (+ amin amax) 2.0)))
                 (if (pd mid)
                     (loop mid amax)
                     (loop amin mid))))))
      #f))

;; Return the max speed (in meters/second) at which DISTANCE can be covered
;; according to the Pace-Duration function PD.
;;
;; PD is a function returning the max speed that can be maintained for a
;; duration.  This can be either a Best AVG function or a PD model function.
;;
;; DISTANCE is the distance to be covered in meters
;;
;; TMIN, TMAX are the min and max search times, #f is returned if the distance
;; cannot be covered in this time range.
;;
;; NOTE: this function needs to be in a different file.
(define (estimate-distance pd distance tmin tmax)
  (define (dst t)
    (let ((p (pd t)))
      (if p (* t p) #f)))
  (define dmin (dst tmin))
  (define dmax (dst tmax))
  (if (or (not dmin) (not dmax) (< distance dmin) (> distance dmax))
      #f
      (let loop ((dlow dmin)
                 (tlow tmin)
                 (dhigh dmax)
                 (thigh tmax))
        (if (< (abs (- thigh tlow)) 1.0)
            (pd thigh)
            (let* ((tmid (/ (+ tlow thigh) 2.0))
                   (dmid (dst tmid)))
              (cond ((not dmid)
                     #f)
                    ((< (abs (- dmid distance)) 0.01)
                     (pd tmid))         ; good enough
                    ((< dmid distance)
                     (loop dmid tmid dhigh thigh))
                    (#t
                     (loop dlow tlow dmid tmid))))))))


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

(define (is-swimming? sport)
  (eqv? (sport-id sport) 5))

(provide is-runnig? is-cycling? is-lap-swimming? is-swimming?)

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
    (define/public (inverted-mean-max?) #f)

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
            (set! color (cdr color-item))))
        ;; Could not find it, use a default
        (unless color
          (set! color (make-object color% 0 148 255))))
      color)

    ;; When true, color the plot by swim stroke colors.  Only makes sense for
    ;; swimming activities.
    (define/public (plot-color-by-swim-stroke?) #f)

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

    ;; Return #t if we can estimate Critical Power for this data series.
    (define/public (have-cp-estimate?) #f)
    ;; Estimate CP for this data series given a best-avg function (as returned
    ;; by `aggregate-bavg') and search parameters (a CP2PARAMS instance).
    ;; Returns a CP2 structure.
    (define/public (cp-estimate bavg-fn params) #f)
    ;; Return a PD function for the supplied critical power parameters (a CP2
    ;; instance)
    (define/public (pd-function cp-params) #f)
    ;; Return a pict displaying information about the supplied critical power
    ;; parameters (a CP2 instance).
    (define/public (pd-data-as-pict cp-params bavgfn) #f)

    ;; Return a function (-> number? string?) which formats a value of this
    ;; series into a string.
    (define/public (value-formatter)
      (lambda (p)
        (if (rational? p)
            (~r p #:precision (fractional-digits))
            (~a p))))

    ;; Return the name of the values in this series (e.g. "Pace", "Power",
    ;; etc).
    (define/public (name)
      "Unnamed")

    ))

(provide series-metadata%)


(define axis-distance
  (new (class series-metadata% (init) (super-new)
         (define/override (filter-width) 0.05)
         (define/override (axis-label)
           (if (eq? (al-pref-measurement-system) 'metric)
               "Distance (km)" "Distance (mi)"))
         (define/override (series-name) "distance")
         (define/override (fractional-digits) 2)
         (define/override (name) "Distance")
         (define/override (value-formatter)
           ;; Unfortunate hack!
           (if (eq? (al-pref-measurement-system) 'metric)
               (lambda (x) (distance->string (* x 1000) #t))
               (lambda (x) (distance->string (* x 1609) #t))))
         )))

(provide axis-distance)

(define axis-elapsed-time
  (new (class series-metadata% (init) (super-new)
         (define/override (plot-ticks) (time-ticks #:formats '("~H:~M")))
         (define/override (has-stop-detection?) #t)
         (define/override (filter-width) 5.0) ; seconds
         (define/override (axis-label) "Elapsed Time (hour:min)")
         (define/override (series-name) "elapsed")
         (define/override (name) "Elapsed Time")
         (define/override (value-formatter) duration->string)
         )))
(provide axis-elapsed-time)

(define axis-timer-time
  (new (class series-metadata% (init) (super-new)
         (define/override (plot-ticks) (time-ticks #:formats '("~H:~M")))
         (define/override (has-stop-detection?) #f)
         (define/override (filter-width) 5.0)
         (define/override (axis-label) "Time (hour:min)")
         (define/override (name) "Time")
         (define/override (series-name) "timer")
         (define/override (value-formatter) duration->string)
         )))
(provide axis-timer-time)

(define axis-speed
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label)
           (if (eq? (al-pref-measurement-system) 'metric)
               "Speed (km/h)" "Speed (mi/h)"))
         (define/override (should-filter?) #t)
         (define/override (histogram-bucket-slot) 0.1)
         (define/override (series-name) "speed")
         (define/override (name) "Speed")
         (define/override (fractional-digits) 2)

         (define/override (factor-fn sport (sid #f))
           (let ((zones (sport-zones sport sid 2)))
             (if zones
                 ;; NOTE: value passed in is in km/h or mi/h, we need to
                 ;; convert it back to meters/sec before we can find the zone.
                 (lambda (val)
                   (let* ((val-mps (convert-speed->m/s val))
                          (zone (val->zone val-mps zones)))
                     (zone->label zone)))
                 #f)))

         (define/override (factor-colors) (ct:zone-colors))

         )))
(provide axis-speed)

(define axis-pace%
  (class series-metadata% (init) (super-new)
    (define/override (plot-ticks) (time-ticks #:formats '("~M:~f")))
    (define/override (axis-label)
      (if (eq? (al-pref-measurement-system) 'metric)
          "Pace (min/km)" "Pace (min/mi)"))
    (define/override (should-filter?) #t)
    ;; (define/override (y-range) (cons 0 #f))
    (define/override (inverted-mean-max?) #t)
    (define/override (series-name) "pace")
    (define/override (name) "Pace")
    ;; NOTE: speed is stored in the FIT file as m/s * 1000 (and truncated
    ;; to an integer).  When converting to pace, the minimum delta
    ;; between two representable pace values is 0.09 (do the maths!) and
    ;; this assumes that the device writes the speed values with 1 mm
    ;; precision!  Realistically, precision for the pace values is at
    ;; best 1 second / km
    (define/override (histogram-bucket-slot) 1)
    (define/override (factor-fn sport (sid #f))
      (let ((zones (sport-zones sport sid 2)))
        (if zones
            ;; NOTE: value passed in is in sec/km or sec/mi (NOT
            ;; minutes), we need to convert it back to meters/sec before
            ;; we can find the zone.
            (lambda (val)
              (if (and (number? val) (> val 0))
                  (let* ((val-mps (convert-pace->m/s val))
                         (zone
                          (val->zone val-mps zones)))
                    (zone->label zone))
                  ;; Put invalid values in zone 0
                  (zone->label 0)))
            #f)))

    (define/override (factor-colors) (ct:zone-colors))

    (define/override (have-cp-estimate?) #t)

    (define/override (cp-estimate bavg-fn params)
      (define afn (lambda (t) (convert-pace->m/s (bavg-fn t))))
      ;; Check that the bavg function can provide values for the CP2
      ;; search range.  The cp2search structure is already validated via
      ;; a #:guard, so we only need to check that the bavg function is
      ;; valid at the AEEND point.
      (and (bavg-fn (cp2search-aeend params))
           (search-best-cp/exhausive
            afn params
            #:cp-precision 3 #:wprime-precision 1)))

    (define/override (pd-function cp-params)
      (define fn (cp2-fn cp-params))
      (lambda (t)
        (let ((spd (fn t)))
          (if spd (convert-m/s->pace spd) #f))))

    (define/override (pd-data-as-pict cp-params bavgfn)
      (define metric? (eq? (al-pref-measurement-system) 'metric))
      (define fn (cp2-fn cp-params))
      (define dfn
        (lambda (t)
          (let ((val (bavgfn t)))
            (if val (convert-m/s->pace val) #f))))

      (define title (text "Model" pd-title-face))
      (define dprime (short-distance->string (round (cp2-wprime cp-params))))
      (define cv (pace->string (cp2-cp cp-params)))
      (define picts
        (list (text "CV" pd-label-face)
              (text cv pd-item-face)
              (text (if metric? "min/km" "min/mile") pd-label-face)
              (text "D'" pd-label-face)
              (text dprime pd-item-face)
              (text (if metric? "meters" "yards") pd-label-face)))
      (define p1
        (vc-append 10 title (table 3 picts lc-superimpose cc-superimpose 15 3)))

      (define interval-estimates-time
        (flatten
         (for/list ([t '(("5 min" . 300)
                         ("10 min" . 600)
                         ("15 min" . 900)
                         ("20 min" . 1200))])
           (list (text (car t) pd-label-face)
                 (text (pace->string (fn (cdr t))) pd-item-face)
                 (text (let ((val (dfn (cdr t))))
                         (if val (pace->string val) "N/A"))
                       pd-item-face)))))

      (define actual-max (find-actual-max dfn 7200))

      (define interval-estimates-distance
        (flatten
         (for/list ([t '(("1 km" . 1000.0)
                         ("1 mile" . 1610.0)
                         ("5k" . 5000.0)
                         ("10k" . 10000.0))])
           (define model-pace (estimate-distance fn (cdr t) 10 actual-max))
           (define data-pace (estimate-distance dfn (cdr t) 10 actual-max))
           (list (text (car t) pd-label-face)
                 (text (if model-pace (pace->string model-pace) "N/A") pd-item-face)
                 (text (if data-pace (pace->string data-pace) "N/A") pd-item-face)))))

      (define interval-estimates
        (append
         (list (text "" pd-label-face)
               (text "model" pd-label-face)
               (text "data" pd-label-face))
         interval-estimates-time
         interval-estimates-distance))

      (define title2 (text "Estimates" pd-title-face))
      (define p2
        (vc-append 10 title2 (table 3 interval-estimates lc-superimpose cc-superimpose 15 3)))

      (define p (vc-append 10 p1 p2))

      (cc-superimpose
       (filled-rounded-rectangle (+ (pict-width p) 20) (+ (pict-height p) 20) -0.1
                                 #:draw-border? #f #:color pd-background)
       p))

    (define/override (value-formatter)
      (lambda (p)
        (if (> p 0)
            (pace->string (convert-pace->m/s p))
            "")))
    ))

(define axis-pace (new axis-pace%))
(provide axis-pace)

(define axis-gap
  (new (class axis-pace% (init) (super-new)
         (define/override (series-name) "gap")
         (define/override (name) "GAP")
         (define/override (axis-label)
           (if (eq? (al-pref-measurement-system) 'metric)
               "Grade Adjusted Pace (min/km)" "Grade Adjusted Pace (min/mi)")))))
(provide axis-gap)

(define axis-speed-zone
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label) "Speed (zone)")
         (define/override (should-filter?) #t)
         ;; (define/override (y-range) (cons 0 #f))
         (define/override (series-name) "speed-zone")
         (define/override (fractional-digits) 2)
         (define/override (name) "Speed Zone")

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
         (define/override (name) "Elevation")
         (define/override (fractional-digits) 1)
         ;; Don't replace missing values with anything, strip them out.
         (define/override (missing-value) #f)
         )))
(provide axis-elevation)

(define axis-corrected-elevation
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label)
           (if (eq? (al-pref-measurement-system) 'metric)
               "Elevation (m)" "Elevation (ft)"))
         (define/override (should-filter?) #t)
         (define/override (series-name) "calt")
         (define/override (name) "Elevation")
         (define/override (fractional-digits) 1)
         ;; Don't replace missing values with anything, strip them out.
         (define/override (missing-value) #f)
         )))
(provide axis-corrected-elevation)

(define axis-grade
  (new (class series-metadata% (init) (super-new)
         (define/override (should-filter?) #t)
         (define/override (axis-label) "Grade (%)")
         (define/override (series-name) "grade")
         (define/override (histogram-bucket-slot) 0.01)
         (define/override (name) "Slope")
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
         (define/override (name) "Slope")
         (define/override (inverted-mean-max?) #t))))
(provide axis-grade-inverted)

(define axis-hr-bpm
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label) "Heart Rate (bpm)")
         (define/override (should-filter?) #t)
         (define/override (series-name) "hr")
         (define/override (name) "HR")

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
         (define/override (name) "HR Percent")
         (define/override (fractional-digits) 1)
         )))
(provide axis-hr-pct)

(define axis-hr-zone
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label) "Heart Rate (zone)")
         (define/override (should-filter?) #t)
         (define/override (series-name) "hr-zone")
         (define/override (name) "HR Zone")
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
         (define/override (name) "Cadence")
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
         (define/override (name) "Stride")
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
         (define/override (name) "VRatio")
         (define/override (inverted-mean-max?) #t)
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
         (define/override (inverted-mean-max?) #t)
         (define/override (series-name) "vosc")
         (define/override (name) "VOsc")
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
         (define/override (inverted-mean-max?) #t)
         (define/override (series-name) "gct")
         (define/override (name) "GCT")

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
         (define/override (inverted-mean-max?) #t)
         (define/override (series-name) "pgct")
         (define/override (name) "GCT percent")
         (define/override (fractional-digits) 1)
         )))
(provide axis-stance-time-percent)

(define axis-power
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label) "Power (watts)")
         (define/override (should-filter?) #t)
         (define/override (series-name) "pwr")
         (define/override (name) "Power")

         (define/override (factor-fn sport (sid #f))
           (let ((zones (sport-zones sport sid 3)))
             (if zones
                 (lambda (val)
                   (let ((zone (val->zone val zones)))
                     (zone->label zone)))
                 #f)))

         (define/override (factor-colors) (ct:zone-colors))

         (define/override (have-cp-estimate?) #t)

         (define/override (cp-estimate bavg-fn params)
           ;; Check that the bavg function can provide values for the CP2
           ;; search range.  The cp2search structure is already validated via
           ;; a #:guard, so we only need to check that the bavg function is
           ;; valid at the AEEND point.
           (and (bavg-fn (cp2search-aeend params))
                (search-best-cp/exhausive
                 bavg-fn params
                 #:cp-precision 1 #:wprime-precision -2)))

         (define/override (pd-function cp-params)
           (cp2-fn cp-params))

         (define/override (pd-data-as-pict cp-params bavgfn)
           (define title (text "Model" pd-title-face))
           (define wprime (~r (round (cp2-wprime cp-params)) #:precision 0))
           (define cp (~r (round (cp2-cp cp-params)) #:precision 0))
           (define fn (cp2-fn cp-params))
           (define picts
             (list (text "CP" pd-label-face)
                   (text cp pd-item-face)
                   (text "watts" pd-label-face)
                   (text "W'" pd-label-face)
                   (text wprime pd-item-face)
                   (text "joules" pd-label-face)))
           (define p1
             (vc-append 10 title (table 3 picts lc-superimpose cc-superimpose 15 3)))

           (define interval-estimates-time
             (flatten
              (for/list ([t '(("5 min" . 300)
                              ("10 min" . 600)
                              ("15 min" . 900)
                              ("20 min" . 1200))])
                (list (text (car t) pd-label-face)
                      (text (~r (fn (cdr t)) #:precision 0) pd-item-face)
                      (text (~r (bavgfn (cdr t)) #:precision 0) pd-item-face)))))

           (define interval-estimates
             (append
              (list (text "" pd-label-face)
                    (text "model" pd-label-face)
                    (text "data" pd-label-face))
              interval-estimates-time))

           (define title2 (text "Estimates" pd-title-face))
           (define p2
             (vc-append 10 title2 (table 3 interval-estimates lc-superimpose cc-superimpose 15 3)))

           (define p (vc-append 10 p1 p2))

           (cc-superimpose
            (filled-rounded-rectangle (+ (pict-width p) 15) (+ (pict-height p) 15) -0.1
                                      #:draw-border? #f #:color pd-background)
            p))

         )))
(provide axis-power)

(define axis-torque
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label) "Torque (N m)")
         (define/override (should-filter?) #t)
         (define/override (series-name) "torque")
         (define/override (name) "Torque")
         )))
(provide axis-torque)

(define axis-wbal
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label) "W' Bal")
         (define/override (should-filter?) #f)
         (define/override (series-name) "wbal")
         (define/override (name) "WBal")
         )))
(provide axis-wbal)

(define axis-wbali
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label) "W' Bal (integral)")
         (define/override (should-filter?) #f)
         (define/override (series-name) "wbali")
         (define/override (name) "Wbali")
         )))
(provide axis-wbali)

(define axis-power-zone
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label) "Power (zone)")
         (define/override (should-filter?) #t)
         (define/override (y-range) (cons 0 #f))
         (define/override (series-name) "pwr-zone")
         (define/override (fractional-digits) 1)
         (define/override (name) "Power Zone")

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
         (define/override (name) "Balance")
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
         (define/override (name) "TEff Left")
         (define/override (series-name) "lteff")
         (define/override (fractional-digits) 1)
         )))
(provide axis-left-torque-effectiveness)

(define axis-right-torque-effectiveness
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label) "Torque Effectiveness (%)")
         (define/override (should-filter?) #t)
         (define/override (y-range) (cons 0 100))
         (define/override (name) "TEff Right")
         (define/override (headline) "Torque Effectiveness, Right Pedal (%)")
         (define/override (plot-label) "Right Pedal")
         (define/override (series-name) "rteff")
         (define/override (fractional-digits) 1))))

(provide axis-right-torque-effectiveness)

(define axis-left-pedal-smoothness
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Pedal Smoothness, Left Pedal (%)")
         (define/override (axis-label) "Pedal Smoothness (%)")
         (define/override (name) "PSmth Left")
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
         (define/override (name) "PSmth Right")
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
         (define/override (name) "PCO Left")
         (define/override (series-name) "lpco"))))
(provide axis-left-platform-centre-offset)

(define axis-right-platform-centre-offset
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Platform Centre Offset, Right Pedal (mm)")
         (define/override (axis-label) "Platform Centre Offset (mm)")
         (define/override (should-filter?) #t)
         (define/override (plot-label) "Right Pedal")
         (define/override (name) "PCO Right")
         (define/override (series-name) "rpco"))))
(provide axis-right-platform-centre-offset)

(define axis-left-power-phase-start
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Power Phase Start, Left Pedal")
         (define/override (axis-label) "Power Phase Start")
         (define/override (should-filter?) #t)
         (define/override (plot-label) "Left Pedal")
         (define/override (name) "PP Start Left")
         (define/override (series-name) "lpps")
         )))
(provide axis-left-power-phase-start)

(define axis-left-power-phase-end
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Power Phase End, Left Pedal")
         (define/override (axis-label) "Power Phase End")
         (define/override (should-filter?) #t)
         (define/override (plot-label) "Left Pedal")
         (define/override (name) "PP End Left")
         (define/override (series-name) "lppe"))))
(provide axis-left-power-phase-end)

(define axis-left-power-phase-angle
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Power Phase Angle, Left Pedal")
         (define/override (axis-label) "Power Phase Angle")
         (define/override (should-filter?) #t)
         (define/override (plot-label) "Left Pedal")
         (define/override (name) "PP Left")
         (define/override (series-name) "lppa"))))
(provide axis-left-power-phase-angle)

(define axis-right-power-phase-start
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Power Phase Start, Right Pedal")
         (define/override (axis-label) "Power Phase Start")
         (define/override (should-filter?) #t)
         (define/override (plot-label) "Right Pedal")
         (define/override (name) "PP Start Right")
         (define/override (series-name) "rpps")
         )))
(provide axis-right-power-phase-start)

(define axis-right-power-phase-end
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Power Phase End, Right Pedal")
         (define/override (axis-label) "Power Phase End")
         (define/override (should-filter?) #t)
         (define/override (plot-label) "Right Pedal")
         (define/override (name) "PP End Right")
         (define/override (series-name) "rppe"))))
(provide axis-right-power-phase-end)

(define axis-right-power-phase-angle
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Power Phase Angle, Right Pedal")
         (define/override (axis-label) "Power Phase Angle")
         (define/override (should-filter?) #t)
         (define/override (plot-label) "Right Pedal")
         (define/override (name) "PP Right")
         (define/override (series-name) "rppa"))))
(provide axis-right-power-phase-angle)

(define axis-left-peak-power-phase-start
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Peak Power Phase Start, Left Pedal")
         (define/override (axis-label) "Peak Power Phase Start")
         (define/override (should-filter?) #t)
         (define/override (plot-label) "Left Pedal")
         (define/override (name) "PPP Start Left")
         (define/override (series-name) "lppps")
         )))
(provide axis-left-peak-power-phase-start)

(define axis-left-peak-power-phase-end
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Peak Power Phase End, Left Pedal")
         (define/override (axis-label) "Peak Power Phase End")
         (define/override (should-filter?) #t)
         (define/override (plot-label) "Left Pedal")
         (define/override (name) "PPP End Left")
         (define/override (series-name) "lpppe"))))
(provide axis-left-peak-power-phase-end)

(define axis-left-peak-power-phase-angle
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Peak Power Phase Angle, Left Pedal")
         (define/override (axis-label) "Peak Power Phase Angle")
         (define/override (should-filter?) #t)
         (define/override (name) "PPP Left")
         (define/override (plot-label) "Left Pedal")
         (define/override (series-name) "lpppa"))))
(provide axis-left-peak-power-phase-angle)

(define axis-right-peak-power-phase-start
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Peak Power Phase Start, Right Pedal")
         (define/override (axis-label) "Peak Power Phase Start")
         (define/override (should-filter?) #t)
         (define/override (plot-label) "Right Pedal")
         (define/override (name) "PPP Start Right")
         (define/override (series-name) "rppps"))))
(provide axis-right-peak-power-phase-start)

(define axis-right-peak-power-phase-end
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Peak Power Phase End, Right Pedal")
         (define/override (axis-label) "Peak Power Phase End")
         (define/override (should-filter?) #t)
         (define/override (plot-label) "Right Pedal")
         (define/override (name) "PPP End Right")
         (define/override (series-name) "rpppe"))))
(provide axis-right-peak-power-phase-end)

(define axis-right-peak-power-phase-angle
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Peak Power Phase Angle, Right Pedal")
         (define/override (axis-label) "Peak Power Phase Angle")
         (define/override (should-filter?) #t)
         (define/override (plot-label) "Right Pedal")
         (define/override (name) "PPP Right")
         (define/override (series-name) "rpppa"))))
(provide axis-right-peak-power-phase-angle)

(define axis-swim-pace
  (new (class series-metadata% (init) (super-new)
         (define/override (plot-ticks) (time-ticks #:formats '("~M:~f")))
         (define/override (axis-label)
           (if (eq? (al-pref-measurement-system) 'metric)
               "Pace (min/100m)" "Pace (min/100yd)"))
         (define/override (y-range) (cons 0 #f))
         (define/override (plot-color-by-swim-stroke?) #t)
         (define/override (series-name) "pace")
         (define/override (inverted-mean-max?) #t)
         (define/override (name) "Pace")
         (define/override (value-formatter)
           (lambda (p)
             (if (> p 0)
                 (swim-pace->string (convert-swim-pace->m/s p))
                 "")))

         (define/override (have-cp-estimate?) #t)

         (define/override (cp-estimate bavg-fn params)
           (define afn (lambda (t) (convert-swim-pace->m/s (bavg-fn t))))
           ;; Check that the bavg function can provide values for the CP2
           ;; search range.  The cp2search structure is already validated via
           ;; a #:guard, so we only need to check that the bavg function is
           ;; valid at the AEEND point.
           (and (bavg-fn (cp2search-aeend params))
                (search-best-cp/exhausive
                 afn params
                 #:cp-precision 3 #:wprime-precision 1)))

         (define/override (pd-function cp-params)
           (define fn (cp2-fn cp-params))
           (lambda (t)
             (let ((spd (fn t)))
               (if spd (convert-m/s->swim-pace spd) #f))))

         (define/override (pd-data-as-pict cp-params bavgfn)
           (define metric? (eq? (al-pref-measurement-system) 'metric))
           (define fn (cp2-fn cp-params))
           (define dfn
             (lambda (t)
               (let ((val (bavgfn t)))
                 (if val (convert-m/s->swim-pace val) #f))))

           (define title (text "Model" pd-title-face))
           (define dprime (short-distance->string (round (cp2-wprime cp-params))))
           (define cv (swim-pace->string (cp2-cp cp-params)))
           (define picts
             (list (text "CV" pd-label-face)
                   (text cv pd-item-face)
                   (text (if metric? "min/100m" "min/100yd") pd-label-face)
                   (text "D'" pd-label-face)
                   (text dprime pd-item-face)
                   (text (if metric? "meters" "yards") pd-label-face)))
           (define p1
             (vc-append 10 title (table 3 picts lc-superimpose cc-superimpose 15 3)))

           (define interval-estimates-time
             (flatten
              (for/list ([t '(("5 min" . 300)
                              ("10 min" . 600)
                              ("15 min" . 900)
                              ("20 min" . 1200))])
                (list (text (car t) pd-label-face)
                      (text (swim-pace->string (fn (cdr t))) pd-item-face)
                      (text (let ((val (dfn (cdr t))))
                              (if val (swim-pace->string val) "N/A"))
                            pd-item-face)))))

           (define actual-max (find-actual-max dfn 7200))

           (define interval-estimates-distance
             (flatten
              (for/list ([t '(("50 m" . 50.0)
                              ("100 m" . 100.0)
                              ("200 m" . 200.0)
                              ("400 m" . 400.0)
                              ("1000 m" . 1000.0)
                              ("1500 m" . 1500.0))])
                (define model-pace (estimate-distance fn (cdr t) 10 actual-max))
                (define data-pace (estimate-distance dfn (cdr t) 10 actual-max))
                (list (text (car t) pd-label-face)
                      (text (if model-pace (swim-pace->string model-pace) "N/A") pd-item-face)
                      (text (if data-pace (swim-pace->string data-pace) "N/A") pd-item-face)))))

           (define interval-estimates
             (append
              (list (text "" pd-label-face)
                    (text "model" pd-label-face)
                    (text "data" pd-label-face))
              interval-estimates-time
              interval-estimates-distance))

           (define title2 (text "Estimates" pd-title-face))
           (define p2
             (vc-append 10 title2 (table 3 interval-estimates lc-superimpose cc-superimpose 15 3)))

           (define p (vc-append 10 p1 p2))

           (cc-superimpose
            (filled-rounded-rectangle (+ (pict-width p) 20) (+ (pict-height p) 20) -0.1
                                      #:draw-border? #f #:color pd-background)
            p))

         )))
(provide axis-swim-pace)

(define axis-swim-swolf
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label) "SWOLF")
         (define/override (y-range) (cons 0 #f))
         (define/override (series-name) "swolf")
         (define/override (name) "SWOLF")
         )))
(provide axis-swim-swolf)

(define axis-swim-stroke-count
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label) "Strokes / Length")
         (define/override (y-range) (cons 0 #f))
         (define/override (series-name) "strokes")
         (define/override (name) "Strokes")
         )))
(provide axis-swim-stroke-count)

(define axis-swim-stroke-length
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label) "Stride")
         (define/override (y-range) (cons 0 #f))
         (define/override (series-name) "stride")
         (define/override (fractional-digits) 2)
         (define/override (histogram-bucket-slot) 0.01)
         (define/override (name) "Stroke Length")
         )))
(provide axis-swim-stroke-length)

(define axis-swim-avg-cadence
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label) "Strokes / Min")
         (define/override (y-range) (cons 0 #f))
         (define/override (name) "Cadence")

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
         (define/override (name) "Distance")
         (define/override (axis-label)
           (if (eq? (al-pref-measurement-system) 'metric)
               "Distance (meters)" "Distance (yards)"))
         (define/override (series-name) "distance")
         (define/override (value-formatter)
           ;; This is a hack!
           (if (eq? (al-pref-measurement-system) 'metric)
               (lambda (x) (short-distance->string x #t))
               (lambda (x) (short-distance->string (* x 0.9144) #t))))
         )))
(provide axis-swim-distance)

(define axis-swim-time
  (new (class series-metadata% (init) (super-new)
         (define/override (plot-ticks) (time-ticks #:formats '("~H:~M")))
         (define/override (axis-label) "Time (hour:min)")
         (define/override (name) "Time")
         (define/override (series-name) "elapsed")
         (define/override (value-formatter) duration->string)
         )))
(provide axis-swim-time)

(define all-series-meta
  (list
   axis-distance
   axis-elapsed-time
   axis-timer-time
   axis-speed
   axis-pace
   axis-gap
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

(define swim-series-meta
  (list axis-swim-pace
        axis-swim-swolf
        axis-swim-stroke-count
        axis-swim-stroke-length
        axis-swim-avg-cadence
        axis-swim-distance
        axis-swim-time))

;; NOTE: `findf` returns #f if the series is not found.  We rely on the
;; contract to check that, as this function is always expected to return a
;; valid axis definition, and it should always be passed valid series names.
(define (find-meta-for-series name (is-lap-swimming? #f))
  (findf (lambda (meta) (equal? (send meta series-name) name))
         (if is-lap-swimming? swim-series-meta all-series-meta)))

(provide/contract
 (find-meta-for-series (->* (string?) (boolean?) (is-a?/c series-metadata%))))
