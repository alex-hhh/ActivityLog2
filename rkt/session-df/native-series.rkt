#lang racket/base
;; native-series.rkt -- define series metadata objects for series that are
;; built into the application (like heart rate or power).
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015, 2018, 2020, 2021, 2022, 2023 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require pict
         plot/no-gui
         racket/class
         racket/draw
         racket/format
         racket/list
         racket/math
         racket/match
         (prefix-in ct: "../color-theme.rkt")
         "../fmt-util.rkt"
         "../models/critical-power.rkt"
         "../models/sport-zone.rkt"
         "../sport-charms.rkt"
         "series-metadata.rkt")

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

(define (estimate-critical-power pd params)
  ;; If PARAMS contains a 'progress-callback key, it is used as the progress
  ;; callback for the CP3 fit
  (define progress-callback (hash-ref params 'progress-callback #f))
  (case (hash-ref params 'model 'cp2)
    ((cp2)
     (let ([an-start (hash-ref params 'an-start 120.0)]
           [an-end (hash-ref params 'an-end 300.0)]
           [ae-start (hash-ref params 'ae-start 720.0)]
           [ae-end (hash-ref params 'ae-end 1200.0)])
       (define-values (cp fit-params)
         (cp2-fit pd an-start an-end ae-start ae-end))
       ;; NOTE: Unit tests pass 'verify #t
       (when (hash-ref params 'verify #f)
         (cp2-check-results cp fit-params pd))
       cp))
    ((cp3)
     (let ([nm-start (hash-ref params 'nm-start 15.0)]
           [nm-end (hash-ref params 'nm-end 45.0)]
           [an-start (hash-ref params 'an-start 120.0)]
           [an-end (hash-ref params 'an-end 300.0)]
           [ae-start (hash-ref params 'ae-start 720.0)]
           [ae-end (hash-ref params 'ae-end 1200.0)])
       (define-values (cp fit-params)
         (cp3-fit pd nm-start nm-end an-start an-end ae-start ae-end progress-callback))
       ;; NOTE: Unit tests pass 'verify #t
       (when (hash-ref params 'verify #f)
         (cp3-check-results cp fit-params pd))
       cp))
    (else
     (raise (format "estimate-critical-power: unknown model: ~a" (hash-ref params 'model))))))


;;........................................................ axis-distance ....

(define axis-distance
  (new (class series-metadata% (init) (super-new)
         (define/override (filter-width) 0.05)
         (define/override (axis-label)
           (if (eq? (al-pref-measurement-system) 'metric)
               "Distance (km)" "Distance (mi)"))
         (define/override (series-name) "distance")
         (define/override (fractional-digits) 2)
         (define/override (name) "Distance")
         (define/override (value-formatter sport (sid #f) #:show-unit-label? (label? #t))
           ;; Unfortunate hack!
           (if (eq? (al-pref-measurement-system) 'metric)
               (lambda (x) (distance->string (* x 1000) label?))
               (lambda (x) (distance->string (* x 1609) label?))))
         )))
(register-series-metadata axis-distance)
(provide axis-distance)


;;.................................................... axis-elapsed-time ....

(define axis-elapsed-time
  (new (class series-metadata% (init) (super-new)
         (define/override (plot-ticks) (time-ticks #:formats '("~H:~M")))
         (define/override (has-stop-detection?) #t)
         (define/override (filter-width) 5.0) ; seconds
         (define/override (axis-label) "Elapsed Time (hour:min)")
         (define/override (series-name) "elapsed")
         (define/override (name) "Elapsed Time")
         (define/override (value-formatter sport (sid #f) #:show-unit-label? (label? #f))
           duration->string)
         )))
(register-series-metadata axis-elapsed-time)
(provide axis-elapsed-time)


;;...................................................... axis-timer-time ....

(define axis-timer-time
  (new (class series-metadata% (init) (super-new)
         (define/override (plot-ticks) (time-ticks #:formats '("~H:~M")))
         (define/override (has-stop-detection?) #f)
         (define/override (filter-width) 5.0)
         (define/override (axis-label) "Time (hour:min)")
         (define/override (name) "Time")
         (define/override (series-name) "timer")
         (define/override (value-formatter sport (sid #f) #:show-unit-label? (label? #f))
           duration->string)
         )))
(register-series-metadata axis-timer-time)
(provide axis-timer-time)


;;........................................................... axis-speed ....

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
           (define sz
             (if sid
                 (sport-zones-for-session sid 'pace)
                 (sport-zones-for-sport (sport-id sport) (sub-sport-id sport) 'pace)))
           (and sz
                ;; NOTE: value passed in is in km/h or mi/h, we need to
                ;; convert it back to meters/sec before we can find the zone.
                (lambda (val)
                  (let* ((val-mps (convert-speed->m/s val))
                         (zone (value->zone sz val-mps)))
                    (zone->label zone)))
                 #f))

         (define/override (factor-colors) (ct:zone-colors))

         )))
(register-series-metadata axis-speed)
(provide axis-speed)


;;............................................................ axis-pace ....

(define pace-series-metadata%
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
    (define/override (missing-value) #f)
    ;; NOTE: speed is stored in the FIT file as m/s * 1000 (and truncated
    ;; to an integer).  When converting to pace, the minimum delta
    ;; between two representable pace values is 0.09 (do the maths!) and
    ;; this assumes that the device writes the speed values with 1 mm
    ;; precision!  Realistically, precision for the pace values is at
    ;; best 1 second / km
    (define/override (histogram-bucket-slot) 1)
    (define/override (factor-fn sport (sid #f))
      (define sz
             (if sid
                 (sport-zones-for-session sid 'pace)
                 (sport-zones-for-sport (sport-id sport) (sub-sport-id sport) 'pace)))
      (and sz
           ;; NOTE: value passed in is in sec/km or sec/mi (NOT
           ;; minutes), we need to convert it back to meters/sec before
           ;; we can find the zone.
           (lambda (val)
             (if (and (number? val) (> val 0))
                 (let* ((val-mps (convert-pace->m/s val))
                         (zone (value->zone sz val-mps)))
                   (zone->label zone))
                 ;; Put invalid values in zone 0
                 (zone->label 0)))))

    (define/override (factor-colors) (ct:zone-colors))

    (define/override (have-cp-estimate?) #t)

    (define/override (cp-estimate bavg-fn params)
      (define afn (lambda (t) (convert-pace->m/s (bavg-fn t))))
      (estimate-critical-power afn params))

    (define/override (pd-function cp-params)
      (define fn
        (cond ((cp2? cp-params)
               (cp2-function cp-params))
              ((cp3? cp-params)
               (cp3-function cp-params))
              (#t
               (raise "pd-function: unknown CP model parameters"))))
      (lambda (t)
        (let ((spd (fn t)))
          (if spd (convert-m/s->pace spd) #f))))

    (define/override (pd-data-as-pict cp-params bavgfn)
      (define metric? (eq? (al-pref-measurement-system) 'metric))

      (define-values (cp wprime k pmax fn)
        (match cp-params
          ((cp2 cp wprime) (values cp wprime #f #f (cp2-function cp-params)))
          ((cp3 cp wprime k) (values cp wprime k (cp3-pmax cp-params) (cp3-function cp-params)))))

      (define dfn
        (lambda (t)
          (let ((val (bavgfn t)))
            (if val (convert-m/s->pace val) #f))))

      (define title
        (cond ((cp2? cp-params)
               (text "CP2 Model" pd-title-face))
              ((cp3? cp-params)
               (text "CP3 Model" pd-title-face))
              (#t
               ;; We're going to fail later
               (text "Model" pd-title-face))))

      (define picts
        (list (text "CV" pd-label-face)
              (text (pace->string cp) pd-item-face)
              (text (if metric? "min/km" "min/mile") pd-label-face)
              (text "D'" pd-label-face)
              (text (short-distance->string (round wprime)) pd-item-face)
              (text (if metric? "meters" "yards") pd-label-face)))

      (when pmax
        (set! picts
              (append picts
                      (list (text "Vmax" pd-label-face)
                            (text (pace->string pmax) pd-item-face)
                            (text (if metric? "min/km" "min/mile") pd-label-face)))))
      (when k
        (set! picts
              (append picts
                      (list (text "k" pd-label-face)
                            (text (if (rational? k)
                                      (~r (round k) #:precision 0)
                                      (~a k))
                                  pd-item-face)
                            (text "seconds" pd-label-face)))))

      ;; NOTE: we could have used cp2-recovery-constant and
      ;; cp3-recovery-constant, but they are the same.
      (set! picts
            (append picts
                    (list (text "τ" pd-label-face)
                          (text (let ([tau (round (/ wprime cp))])
                                  (~r tau #:precision 0)
                                  (~a tau))
                                pd-item-face)
                          (text "seconds" pd-label-face))))

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

    (define/override (value-formatter sport (sid #f) #:show-unit-label? (label? #f))
      (lambda (p)
        (if (> p 0)
            (pace->string (convert-pace->m/s p) label?)
            "")))
    ))

(provide pace-series-metadata%)
(define axis-pace (new pace-series-metadata%))
(register-series-metadata axis-pace)
(provide axis-pace)


;;............................................................. axis-gap ....

(define axis-gap
  (new (class pace-series-metadata% (init) (super-new)
         (define/override (series-name) "gap")
         (define/override (name) "GAP")
         (define/override (axis-label)
           (if (eq? (al-pref-measurement-system) 'metric)
               "Grade Adjusted Pace (min/km)" "Grade Adjusted Pace (min/mi)")))))
(register-series-metadata axis-gap)
(provide axis-gap)


;;...................................................... axis-speed-zone ....

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
         (define/override (value-formatter sport (sid #f) #:show-unit-label? (label? #f))
           (define sz
             (if sid
                 (sport-zones-for-session sid 'pace)
                 (sport-zones-for-sport (sport-id sport) (sub-sport-id sport) 'pace)))
           (if sz
               (lambda (n) (if (real? n) (zone->zone-name sz n) #f))
               (lambda (n) (format "Zone ~a" (if (real? n) (exact-truncate n) #f)))))
         )))
(register-series-metadata axis-speed-zone)
(provide axis-speed-zone)


;;....................................................... axis-elevation ....

(define axis-elevation
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label)
           (if (eq? (al-pref-measurement-system) 'metric)
               "Elevation (m)" "Elevation (ft)"))
         (define/override (should-filter?) #f)
         (define/override (series-name) "altitude")
         (define/override (name) "Elevation")
         (define/override (fractional-digits) 1)
         ;; Don't replace missing values with anything, strip them out.
         (define/override (missing-value) #f)
         (define/override (value-formatter sport (sid #f) #:show-unit-label? (label? #f))
           (lambda (v)
             (define v1 (convert-vertical-distance->m v))
             (vertical-distance->string v1 label?)))
         )))
(register-series-metadata axis-elevation)
(register-series-metadata axis-elevation #:series-name "alt")
(provide axis-elevation)


;;............................................. axis-corrected-elevation ....

(define axis-corrected-elevation
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label)
           (if (eq? (al-pref-measurement-system) 'metric)
               "Elevation (m)" "Elevation (ft)"))
         (define/override (should-filter?) #f)
         (define/override (series-name) "corrected-altitude")
         (define/override (name) "Elevation")
         (define/override (fractional-digits) 1)
         ;; Don't replace missing values with anything, strip them out.
         (define/override (missing-value) #f)
         (define/override (value-formatter sport (sid #f) #:show-unit-label? (label? #f))
           (lambda (v)
             (define v1 (convert-vertical-distance->m v))
             (vertical-distance->string v1 label?)))
         )))
(register-series-metadata axis-corrected-elevation)
(register-series-metadata axis-corrected-elevation #:series-name "calt")
(provide axis-corrected-elevation)


;;........................................................... axis-grade ....

(define axis-grade
  (new (class series-metadata% (init) (super-new)
         (define/override (should-filter?) #f)
         (define/override (axis-label) "Grade (%)")
         (define/override (series-name) "grade")
         (define/override (histogram-bucket-slot) 0.01)
         (define/override (name) "Slope")
         (define/override (fractional-digits) 2)
         (define/override (value-formatter sport (sid #f) #:show-unit-label? (label? #f))
           (lambda (v)
             (format "~a~a" (~r v #:precision (fractional-digits))
                     (if label? " %" ""))))
         )))
(register-series-metadata axis-grade)
(provide axis-grade)


;;.................................................. axis-grade-inverted ....


;; A grade definition where lower grades are better.  This is useful for
;; activities with lots of descent (e.g. Skiiing)
(define axis-grade-inverted
  (new (class series-metadata% (init) (super-new)
         (define/override (should-filter?) #f)
         (define/override (axis-label) "Descent (%)")
         (define/override (series-name) "grade")
         (define/override (fractional-digits) 1)
         (define/override (name) "Slope")
         (define/override (inverted-mean-max?) #t)
         (define/override (value-formatter sport (sid #f) #:show-unit-label? (label? #f))
           (lambda (v)
             (format "~a~a" (~r v #:precision (fractional-digits))
                     (if label? " %" "")))))))
;; WARNING! do not register this series!
(provide axis-grade-inverted)


;;.......................................................... axis-hr-bpm ....

(define axis-hr-bpm
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label) "Heart Rate (bpm)")
         (define/override (should-filter?) #t)
         (define/override (series-name) "hr")
         (define/override (name) "HR")

         (define/override (factor-fn sport (sid #f))
           (define sz
             (if sid
                 (sport-zones-for-session sid 'heart-rate)
                 (sport-zones-for-sport (sport-id sport) (sub-sport-id sport) 'heart-rate)))
           (and sz
                (lambda (val)
                  (let ((zone (value->zone sz val)))
                    (zone->label zone)))))

         (define/override (factor-colors) (ct:zone-colors))


         )))
(register-series-metadata axis-hr-bpm)
(register-series-metadata axis-hr-bpm #t) ; for lap swimming as well
(provide axis-hr-bpm)


;;.......................................................... axis-hr-pct ....

(define axis-hr-pct
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label) "Heart Rate (% of max)")
         (define/override (should-filter?) #t)
         (define/override (series-name) "hr-pct")
         (define/override (name) "HR Percent")
         (define/override (fractional-digits) 1)
         )))
(register-series-metadata axis-hr-pct)
(register-series-metadata axis-hr-bpm #t) ; for lap swimming as well
(provide axis-hr-pct)


;;......................................................... axis-hr-zone ....

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
         (define/override (value-formatter sport (sid #f) #:show-unit-label? (label? #f))
           (define sz
             (if sid
                 (sport-zones-for-session sid 'heart-rate)
                 (sport-zones-for-sport (sport-id sport) (sub-sport-id sport) 'heart-rate)))
           (if sz
               (lambda (n) (if (real? n) (zone->zone-name sz n) #f))
               (lambda (n) (format "Zone ~a" (if (real? n) (exact-truncate n) #f)))))
         )))
(register-series-metadata axis-hr-zone)
(register-series-metadata axis-hr-bpm #t) ; for lap swimming as well
(provide axis-hr-zone)


;;..................................................... axis-temperature ....

(define axis-temperature
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label)
           (if (eq? (al-pref-measurement-system) 'metric)
               "Temperature (℃)" "Temperature (℉)"))
         ;; Don't filter temperature -- at it varies very slowly and the
         ;; precision is 1 ℃ anyway
         (define/override (should-filter?) #f)
         (define/override (histogram-bucket-slot) 1.0)
         (define/override (series-name) "temperature")
         (define/override (name) "Temperature")
         (define/override (fractional-digits) 1)

         )))

(register-series-metadata axis-temperature)
(provide axis-temperature)


;;......................................................... axis-cadence ....

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
(register-series-metadata axis-cadence)
(provide axis-cadence)


;;.......................................................... axis-stride ....

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
(register-series-metadata axis-stride)
(provide axis-stride)


;;.......................................................... axis-vratio ....

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
(register-series-metadata axis-vratio)
(provide axis-vratio)


;;............................................ axis-vertical-oscillation ....

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
(register-series-metadata axis-vertical-oscillation)
(provide axis-vertical-oscillation)


;;..................................................... axis-stance-time ....

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
(register-series-metadata axis-stance-time)
(provide axis-stance-time)


;;............................................. axis-stance-time-percent ....

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
(register-series-metadata axis-stance-time-percent)
(provide axis-stance-time-percent)


;;............................................... power-series-metadata% ....

(define power-series-metadata%
  (class series-metadata% (init) (super-new)
    (define/override (axis-label) "Power (watts)")
    (define/override (should-filter?) #t)
    (define/override (series-name) "pwr")
    (define/override (name) "Power")

    (define/override (factor-fn sport (sid #f))
      (define sz
        (if sid
            (sport-zones-for-session sid 'power)
            (sport-zones-for-sport (sport-id sport) (sub-sport-id sport) 'power)))
      (and sz
           (lambda (val)
             (let ((zone (value->zone sz val)))
               (zone->label zone)))))

    (define/override (factor-colors) (ct:zone-colors))

    (define/override (have-cp-estimate?) #t)

    (define/override (cp-estimate bavg-fn params)
      (estimate-critical-power bavg-fn params))

    (define/override (pd-function cp-params)
      (cond ((cp2? cp-params)
             (cp2-function cp-params))
            ((cp3? cp-params)
             (cp3-function cp-params))
            (#t
             (raise "pd-function: unknown CP model parameters"))))

    (define/override (pd-data-as-pict cp-params bavgfn)
      (define title
        (cond ((cp2? cp-params)
               (text "CP2 Model" pd-title-face))
              ((cp3? cp-params)
               (text "CP3 Model" pd-title-face))
              (#t
               ;; We're going to fail later
               (text "Model" pd-title-face))))
      (define-values (cp wprime k pmax fn)
        (match cp-params
          ((cp2 cp wprime) (values cp wprime #f #f (cp2-function cp-params)))
          ((cp3 cp wprime k) (values cp wprime k (cp3-pmax cp-params) (cp3-function cp-params)))))
      (define picts
        (list (text "CP" pd-label-face)
              (text (if (rational? cp)
                        (~r (round cp) #:precision 0)
                        (~a cp))
                    pd-item-face)
              (text "watts" pd-label-face)
              (text "W'" pd-label-face)
              (text (if (rational? wprime)
                        (~r (round wprime) #:precision 0)
                        (~a wprime))
                    pd-item-face)
              (text "joules" pd-label-face)))
      (when pmax
        (set! picts
              (append picts
                      (list (text "Pmax" pd-label-face)
                            (text (if (rational? pmax)
                                      (~r (round pmax) #:precision 0)
                                      (~a pmax))
                                  pd-item-face)
                            (text "watts" pd-label-face)))))
      (when k
        (set! picts
              (append picts
                      (list (text "k" pd-label-face)
                            (text (if (rational? k)
                                      (~r (round k) #:precision 0)
                                      (~a k))
                                  pd-item-face)
                            (text "seconds" pd-label-face)))))

      ;; NOTE: we could have used cp2-recovery-constant and
      ;; cp3-recovery-constant, but they are the same.
      (set! picts
            (append picts
                    (list (text "τ" pd-label-face)
                          (text (let ([tau (round (/ wprime cp))])
                                  (if (rational? tau)
                                      (~r tau #:precision 0)
                                      (~a tau)))
                                pd-item-face)
                          (text "seconds" pd-label-face))))
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

    ))
(define axis-power (new power-series-metadata%))
(register-series-metadata axis-power)
(provide axis-power power-series-metadata%)


;;.......................................................... axis-torque ....

(define axis-torque
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label) "Torque (N m)")
         (define/override (should-filter?) #t)
         (define/override (series-name) "torque")
         (define/override (name) "Torque")
         (define/override (fractional-digits) 2)
         )))
(register-series-metadata axis-torque)
(provide axis-torque)


;;............................................................ axis-wbal ....

(define axis-wbal
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label) "W' Bal")
         (define/override (should-filter?) #f)
         (define/override (series-name) "wbal")
         (define/override (y-range) '(0 . #f))
         (define/override (name) "WBal")
         )))
(register-series-metadata axis-wbal)
(provide axis-wbal)


;;........................................................... axis-wbali ....

(define axis-wbali
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label) "W' Bal (integral)")
         (define/override (should-filter?) #f)
         (define/override (series-name) "wbali")
         (define/override (name) "Wbali")
         )))
(register-series-metadata axis-wbali)
(provide axis-wbali)


;;...................................................... axis-power-zone ....

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
         (define/override (value-formatter sport (sid #f) #:show-unit-label? (label? #f))
           (define sz
             (if sid
                 (sport-zones-for-session sid 'power)
                 (sport-zones-for-sport (sport-id sport) (sub-sport-id sport) 'power)))
           (if sz
               (lambda (n) (if (real? n) (zone->zone-name sz n) #f))
               (lambda (n) (format "Zone ~a" (if (real? n) (exact-truncate n) #f)))))
         )))
(register-series-metadata axis-power-zone)
(provide axis-power-zone)


;;.............................................. axis-left-right-balance ....

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
(register-series-metadata axis-left-right-balance)
(provide axis-left-right-balance)


;;....................................... axis-left-torque-effectiveness ....

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
(register-series-metadata axis-left-torque-effectiveness)
(provide axis-left-torque-effectiveness)


;;...................................... axis-right-torque-effectiveness ....

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
(register-series-metadata axis-right-torque-effectiveness)
(provide axis-right-torque-effectiveness)


;;........................................... axis-left-pedal-smoothness ....

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
(register-series-metadata axis-left-pedal-smoothness)
(provide axis-left-pedal-smoothness)


;;.......................................... axis-right-pedal-smoothness ....

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
(register-series-metadata axis-right-pedal-smoothness)
(provide axis-right-pedal-smoothness)



;;....................................... axis-combined-pedal-smoothness ....

(define axis-combined-pedal-smoothness
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Pedal Smoothness, Both Pedals (%)")
         (define/override (axis-label) "Pedal Smoothness, Combined (%)")
         (define/override (name) "PSmth Combined")
         (define/override (should-filter?) #t)
         (define/override (y-range) (cons 0 50))
         (define/override (plot-label) "Both Pedals")
         (define/override (series-name) "cpsmth")
         (define/override (fractional-digits) 1))))
(register-series-metadata axis-combined-pedal-smoothness)
(provide axis-combined-pedal-smoothness)



;;..................................... axis-left-platform-centre-offset ....

(define axis-left-platform-centre-offset
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Platform Centre Offset, Left Pedal (mm)")
         (define/override (axis-label) "Platform Centre Offset (mm)")
         (define/override (should-filter?) #t)
         (define/override (plot-label) "Left Pedal")
         (define/override (name) "PCO Left")
         (define/override (series-name) "lpco"))))
(register-series-metadata axis-left-platform-centre-offset)
(provide axis-left-platform-centre-offset)


;;.................................... axis-right-platform-centre-offset ....

(define axis-right-platform-centre-offset
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Platform Centre Offset, Right Pedal (mm)")
         (define/override (axis-label) "Platform Centre Offset (mm)")
         (define/override (should-filter?) #t)
         (define/override (plot-label) "Right Pedal")
         (define/override (name) "PCO Right")
         (define/override (series-name) "rpco"))))
(register-series-metadata axis-right-platform-centre-offset)
(provide axis-right-platform-centre-offset)


;;.......................................... axis-left-power-phase-start ....

(define axis-left-power-phase-start
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Power Phase Start, Left Pedal")
         (define/override (axis-label) "Power Phase Start")
         (define/override (should-filter?) #t)
         (define/override (plot-label) "Left Pedal")
         (define/override (name) "PP Start Left")
         (define/override (series-name) "lpps")
         )))
(register-series-metadata axis-left-power-phase-start)
(provide axis-left-power-phase-start)


;;............................................ axis-left-power-phase-end ....

(define axis-left-power-phase-end
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Power Phase End, Left Pedal")
         (define/override (axis-label) "Power Phase End")
         (define/override (should-filter?) #t)
         (define/override (plot-label) "Left Pedal")
         (define/override (name) "PP End Left")
         (define/override (series-name) "lppe"))))
(register-series-metadata axis-left-power-phase-end)
(provide axis-left-power-phase-end)


;;.......................................... axis-left-power-phase-angle ....

(define axis-left-power-phase-angle
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Power Phase Angle, Left Pedal")
         (define/override (axis-label) "Power Phase Angle")
         (define/override (should-filter?) #t)
         (define/override (plot-label) "Left Pedal")
         (define/override (name) "PP Left")
         (define/override (series-name) "lppa"))))
(register-series-metadata axis-left-power-phase-angle)
(provide axis-left-power-phase-angle)


;;......................................... axis-right-power-phase-start ....

(define axis-right-power-phase-start
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Power Phase Start, Right Pedal")
         (define/override (axis-label) "Power Phase Start")
         (define/override (should-filter?) #t)
         (define/override (plot-label) "Right Pedal")
         (define/override (name) "PP Start Right")
         (define/override (series-name) "rpps")
         )))
(register-series-metadata axis-right-power-phase-start)
(provide axis-right-power-phase-start)


;;........................................... axis-right-power-phase-end ....

(define axis-right-power-phase-end
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Power Phase End, Right Pedal")
         (define/override (axis-label) "Power Phase End")
         (define/override (should-filter?) #t)
         (define/override (plot-label) "Right Pedal")
         (define/override (name) "PP End Right")
         (define/override (series-name) "rppe"))))
(register-series-metadata axis-right-power-phase-end)
(provide axis-right-power-phase-end)


;;......................................... axis-right-power-phase-angle ....

(define axis-right-power-phase-angle
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Power Phase Angle, Right Pedal")
         (define/override (axis-label) "Power Phase Angle")
         (define/override (should-filter?) #t)
         (define/override (plot-label) "Right Pedal")
         (define/override (name) "PP Right")
         (define/override (series-name) "rppa"))))
(register-series-metadata axis-right-power-phase-angle)
(provide axis-right-power-phase-angle)


;;..................................... axis-left-peak-power-phase-start ....

(define axis-left-peak-power-phase-start
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Peak Power Phase Start, Left Pedal")
         (define/override (axis-label) "Peak Power Phase Start")
         (define/override (should-filter?) #t)
         (define/override (plot-label) "Left Pedal")
         (define/override (name) "PPP Start Left")
         (define/override (series-name) "lppps")
         )))
(register-series-metadata axis-left-peak-power-phase-start)
(provide axis-left-peak-power-phase-start)


;;....................................... axis-left-peak-power-phase-end ....

(define axis-left-peak-power-phase-end
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Peak Power Phase End, Left Pedal")
         (define/override (axis-label) "Peak Power Phase End")
         (define/override (should-filter?) #t)
         (define/override (plot-label) "Left Pedal")
         (define/override (name) "PPP End Left")
         (define/override (series-name) "lpppe"))))
(register-series-metadata axis-left-peak-power-phase-end)
(provide axis-left-peak-power-phase-end)


;;..................................... axis-left-peak-power-phase-angle ....

(define axis-left-peak-power-phase-angle
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Peak Power Phase Angle, Left Pedal")
         (define/override (axis-label) "Peak Power Phase Angle")
         (define/override (should-filter?) #t)
         (define/override (name) "PPP Left")
         (define/override (plot-label) "Left Pedal")
         (define/override (series-name) "lpppa"))))
(register-series-metadata axis-left-peak-power-phase-angle)
(provide axis-left-peak-power-phase-angle)


;;.................................... axis-right-peak-power-phase-start ....

(define axis-right-peak-power-phase-start
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Peak Power Phase Start, Right Pedal")
         (define/override (axis-label) "Peak Power Phase Start")
         (define/override (should-filter?) #t)
         (define/override (plot-label) "Right Pedal")
         (define/override (name) "PPP Start Right")
         (define/override (series-name) "rppps"))))
(register-series-metadata axis-right-peak-power-phase-start)
(provide axis-right-peak-power-phase-start)


;;...................................... axis-right-peak-power-phase-end ....

(define axis-right-peak-power-phase-end
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Peak Power Phase End, Right Pedal")
         (define/override (axis-label) "Peak Power Phase End")
         (define/override (should-filter?) #t)
         (define/override (plot-label) "Right Pedal")
         (define/override (name) "PPP End Right")
         (define/override (series-name) "rpppe"))))
(register-series-metadata axis-right-peak-power-phase-end)
(provide axis-right-peak-power-phase-end)


;;.................................... axis-right-peak-power-phase-angle ....

(define axis-right-peak-power-phase-angle
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Peak Power Phase Angle, Right Pedal")
         (define/override (axis-label) "Peak Power Phase Angle")
         (define/override (should-filter?) #t)
         (define/override (plot-label) "Right Pedal")
         (define/override (name) "PPP Right")
         (define/override (series-name) "rpppa"))))
(register-series-metadata axis-right-peak-power-phase-angle)
(provide axis-right-peak-power-phase-angle)


;;....................................................... axis-swim-pace ....

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
         (define/override (value-formatter sport (sid #f) #:show-unit-label? (label? #f))
           (lambda (p)
             (if (> p 0)
                 (swim-pace->string (convert-swim-pace->m/s p) label?)
                 "")))

         (define/override (have-cp-estimate?) #t)

         (define/override (cp-estimate bavg-fn params)
           (define afn (lambda (t) (convert-swim-pace->m/s (bavg-fn t))))
           (estimate-critical-power afn params))

         (define/override (pd-function cp-params)
           (define fn
             (cond ((cp2? cp-params)
                    (cp2-function cp-params))
                   ((cp3? cp-params)
                    (cp3-function cp-params))
                   (#t
                    (raise "pd-function: unknown CP model parameters"))))
           (lambda (t)
             (let ((spd (fn t)))
               (if spd (convert-m/s->swim-pace spd) #f))))

         (define/override (pd-data-as-pict cp-params bavgfn)
           (define metric? (eq? (al-pref-measurement-system) 'metric))

           (define-values (cp wprime k pmax fn)
             (match cp-params
               ((cp2 cp wprime) (values cp wprime #f #f (cp2-function cp-params)))
               ((cp3 cp wprime k) (values cp wprime k (cp3-pmax cp-params) (cp3-function cp-params)))))

           (define dfn
             (lambda (t)
               (let ((val (bavgfn t)))
                 (if val (convert-m/s->swim-pace val) #f))))

           (define title
        (cond ((cp2? cp-params)
               (text "CP2 Model" pd-title-face))
              ((cp3? cp-params)
               (text "CP3 Model" pd-title-face))
              (#t
               ;; We're going to fail later
               (text "Model" pd-title-face))))

           (define picts
             (list (text "CV" pd-label-face)
                   (text (swim-pace->string cp) pd-item-face)
                   (text (if metric? "min/100m" "min/100yd") pd-label-face)
                   (text "D'" pd-label-face)
                   (text (short-distance->string (round wprime)) pd-item-face)
                   (text (if metric? "meters" "yards") pd-label-face)))
           (when pmax
             (set! picts
                   (append picts
                           (list (text "Vmax" pd-label-face)
                                 (text (swim-pace->string pmax) pd-item-face)
                                 (text (if metric? "min/100m" "min/100yd") pd-label-face)))))
           (when k
             (set! picts
                   (append picts
                           (list (text "k" pd-label-face)
                                 (text (if (rational? k)
                                           (~r (round k) #:precision 0)
                                           (~a k))
                                       pd-item-face)
                                 (text "seconds" pd-label-face)))))

           ;; NOTE: we could have used cp2-recovery-constant and
           ;; cp3-recovery-constant, but they are the same.
           (set! picts
                 (append picts
                         (list (text "τ" pd-label-face)
                               (text (let ([tau (round (/ wprime cp))])
                                       (if (rational? tau)
                                           (~r tau #:precision 0)
                                           (~a tau)))
                                     pd-item-face)
                               (text "seconds" pd-label-face))))

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
(register-series-metadata axis-swim-pace #t)
(provide axis-swim-pace)


;;...................................................... axis-swim-swolf ....

(define axis-swim-swolf
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label) "SWOLF")
         (define/override (y-range) (cons 0 #f))
         (define/override (series-name) "swolf")
         (define/override (name) "SWOLF")
         )))
(register-series-metadata axis-swim-swolf #t)
(provide axis-swim-swolf)


;;............................................... axis-swim-stroke-count ....

(define axis-swim-stroke-count
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label) "Strokes / Length")
         (define/override (y-range) (cons 0 #f))
         (define/override (series-name) "strokes")
         (define/override (name) "Strokes")
         )))
(register-series-metadata axis-swim-stroke-count #t)
(provide axis-swim-stroke-count)


;;.............................................. axis-swim-stroke-length ....

(define axis-swim-stroke-length
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label) "Stride")
         (define/override (y-range) (cons 0 #f))
         (define/override (series-name) "stride")
         (define/override (fractional-digits) 2)
         (define/override (histogram-bucket-slot) 0.01)
         (define/override (name) "Stroke Length")
         )))
(register-series-metadata axis-swim-stroke-length #t)
(provide axis-swim-stroke-length)


;;................................................ axis-swim-avg-cadence ....

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
(register-series-metadata axis-swim-avg-cadence #t)
(provide axis-swim-avg-cadence)


;;................................................... axis-swim-distance ....

(define axis-swim-distance
  (new (class series-metadata% (init) (super-new)
         (define/override (name) "Distance")
         (define/override (axis-label)
           (if (eq? (al-pref-measurement-system) 'metric)
               "Distance (meters)" "Distance (yards)"))
         (define/override (series-name) "distance")
         (define/override (value-formatter sport (sid #f) #:show-unit-label? (label? #t))
           ;; This is a hack!
           (if (eq? (al-pref-measurement-system) 'metric)
               (lambda (x) (short-distance->string x label?))
               (lambda (x) (short-distance->string (* x 0.9144) label?))))
         )))
(register-series-metadata axis-swim-distance #t)
(provide axis-swim-distance)


;;....................................................... axis-swim-time ....

(define axis-swim-time
  (new (class series-metadata% (init) (super-new)
         (define/override (plot-ticks) (time-ticks #:formats '("~H:~M")))
         (define/override (axis-label) "Time (hour:min)")
         (define/override (name) "Time")
         (define/override (filter-width) 5.0) ; seconds
         (define/override (series-name) "elapsed")
         (define/override (value-formatter sport (sid #f) #:show-unit-label? (label? #f))
           duration->string)
         )))
(register-series-metadata axis-swim-time #t)
(provide axis-swim-time)
