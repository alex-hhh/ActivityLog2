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


;;..................................................... axis definitions ....

;; Provides definitions about an axis on a plot (functions to extract the data
;; and functions to display the data).  For most plots two axis definitions
;; will be used.  For example, to plor speed over time one would use
;; axis-timer-time as the X axis and axis-speed as the Y axis.  See also
;; `extract-data'.
(define axis-definition%
  (class object% (init) (super-new)
    ;; When #t, we attempt to detect stop points and generate additional data
    ;; points with 0 Y values, this makes graphs look nicer.
    (define/public (has-stop-detection?) #f)

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
    (define/public (get-axis-title) (get-axis-label))
    (define/public (get-axis-label) (raise "no axis label defined"))
    (define/public (get-series-label) #f)
    (define/public (get-line-color) #f)
    (define/public (get-y-range) #f)
    (define/public (get-series-name) #f)
    (define/public (get-fractional-digits) 0)
    (define/public (get-missing-value) 0)

    ))

(provide axis-definition%)


(define axis-distance
  (new (class axis-definition% (init) (super-new)
         (define/override (get-filter-width) 0.05)
         (define/override (get-axis-label)
           (if (eq? (al-pref-measurement-system) 'metric)
               "Distance (km)" "Distance (mi)"))
         (define/override (get-series-name) "distance")
         (define/override (get-fractional-digits) 2))))

(provide axis-distance)

(define axis-elapsed-time
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-ticks) (time-ticks #:formats '("~H:~M")))
         (define/override (has-stop-detection?) #t)
         (define/override (get-filter-width) 5.0) ; seconds
         (define/override (get-axis-label) "Elapsed Time (hour:min)")
         (define/override (get-series-name) "elapsed"))))
(provide axis-elapsed-time)

(define axis-elapsed-time-no-stop-detection
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-ticks) (time-ticks #:formats '("~H:~M")))
         (define/override (has-stop-detection?) #f)
         (define/override (get-filter-width) 5.0) ; seconds
         (define/override (get-axis-label) "Elapsed Time (hour:min)")
         (define/override (get-series-name) "elapsed"))))
(provide axis-elapsed-time-no-stop-detection)

(define axis-wall-clock-time
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-ticks) (time-ticks #:formats '("~H:~M")))
         (define/override (has-stop-detection?) #f)
         (define/override (get-filter-width) 5.0)
         (define/override (get-axis-label) "Wall clock (hour:min)")
         (define/override (get-series-name) "timestamp"))))
(provide axis-wall-clock-time)

(define axis-timer-time
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-ticks) (time-ticks #:formats '("~H:~M")))
         (define/override (has-stop-detection?) #f)
         (define/override (get-filter-width) 5.0)
         (define/override (get-axis-label) "Time (hour:min)")
         (define/override (get-series-name) "timer"))))
(provide axis-timer-time)

(define axis-speed
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-label)
           (if (eq? (al-pref-measurement-system) 'metric)
               "Speed (km/h)" "Speed (mi/h)"))
         (define/override (should-filter?) #t)
         (define/override (get-histogram-bucket-slot) 0.1)
         (define/override (get-line-color) *blue*)
         (define/override (get-series-name) "speed")
         (define/override (get-fractional-digits) 2))))
(provide axis-speed)

(define axis-pace
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-ticks) (time-ticks #:formats '("~M:~f")))
         (define/override (get-axis-label)
           (if (eq? (al-pref-measurement-system) 'metric)
               "Pace (min/km)" "Pace (min/mi)"))
         (define/override (should-filter?) #t)
         ;; (define/override (get-y-range) (cons 0 #f))
         (define/override (get-line-color) *blue*)
         (define/override (inverted-best-avg?) #t)
         (define/override (get-series-name) "pace"))))
(provide axis-pace)

(define axis-speed-zone
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-label) "Speed (zone)")
         (define/override (should-filter?) #t)
         ;; (define/override (get-y-range) (cons 0 #f))
         (define/override (get-line-color) *blue*)
         (define/override (get-series-name) "speed-zone")
         (define/override (get-fractional-digits) 2)
         )))
(provide axis-speed-zone)

(define axis-elevation
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-label)
           (if (eq? (al-pref-measurement-system) 'metric)
               "Elevation (m)" "Elevation (ft)"))
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *green*)
         (define/override (get-series-name) "alt")
         (define/override (get-fractional-digits) 1)
         )))
(provide axis-elevation)

(define axis-corrected-elevation
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-label)
           (if (eq? (al-pref-measurement-system) 'metric)
               "Elevation (m)" "Elevation (ft)"))
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *green*)
         (define/override (get-series-name) "calt")
         (define/override (get-fractional-digits) 1)
         )))
(provide axis-corrected-elevation)

(define axis-grade
  (new (class axis-definition% (init) (super-new)
         (define/override (should-filter?) #t)
         (define/override (get-axis-label) "Grade (%)")
         (define/override (get-line-color) *green*)
         (define/override (get-series-name) "grade")
         (define/override (get-fractional-digits) 1)
         )))
(provide axis-grade)

(define axis-hr-bpm
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-label) "Heart Rate (bpm)")
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *crimson*)
         (define/override (get-series-name) "hr")
         )))
(provide axis-hr-bpm)

(define axis-hr-pct
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-label) "Heart Rate (% of max)")
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *crimson*)
         (define/override (get-series-name) "hr-pct")
         (define/override (get-fractional-digits) 1)
         )))
(provide axis-hr-pct)

(define axis-hr-zone
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-label) "Heart Rate (zone)")
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *crimson*)
         (define/override (get-series-name) "hr-zone")
         (define/override (get-fractional-digits) 2)
         )))
(provide axis-hr-zone)

(define axis-cadence
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-label) "Cadence (spm)")
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *coral*)
         (define/override (get-series-name) "cad")
         )))
(provide axis-cadence)

(define axis-stride
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-label)
           (if (eq? (al-pref-measurement-system) 'metric)
               "Stride (m)" "Stride (ft)"))
         (define/override (should-filter?) #t)
         (define/override (get-histogram-bucket-slot) 0.01)
         (define/override (get-line-color) *yellow*)
         (define/override (get-series-name) "stride")
         (define/override (get-fractional-digits) 2)
         )))
(provide axis-stride)

(define axis-vratio
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-label) "Vertical Ratio (%)")
         (define/override (should-filter?) #t)
         (define/override (get-histogram-bucket-slot) 0.1)
         (define/override (get-line-color) *yellow*)
         (define/override (get-series-name) "vratio")
         (define/override (inverted-best-avg?) #t)
         (define/override (get-fractional-digits) 2)
         )))
(provide axis-vratio)

(define axis-vertical-oscillation
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-label)
           (if (eq? (al-pref-measurement-system) 'metric)
               "Vertical Oscillation (mm)" "Vertical Oscillation (inch)"))
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *yellow*)
         (define/override (get-histogram-bucket-slot) 0.1)
         (define/override (inverted-best-avg?) #t)
         (define/override (get-series-name) "vosc")
         )))
(provide axis-vertical-oscillation)

(define axis-stance-time
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-label) "Ground Contact Time (ms)")
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *yellow*)
         (define/override (inverted-best-avg?) #t)
         (define/override (get-series-name) "gct")
         )))
(provide axis-stance-time)

(define axis-stance-time-percent
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-label) "Ground Contact Time (%)")
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *yellow*)
         (define/override (get-histogram-bucket-slot) 0.1)
         (define/override (inverted-best-avg?) #t)
         (define/override (get-series-name) "pgct")
         (define/override (get-fractional-digits) 1)
         )))
(provide axis-stance-time-percent)

(define axis-power
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-label) "Power (watts)")
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *dark-magenta*)
         (define/override (get-series-name) "pwr")
         )))
(provide axis-power)

(define axis-torque
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-label) "Torque (N m)")
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *dark-magenta*)
         (define/override (get-series-name) "torque")
         )))
(provide axis-torque)

(define axis-power-zone
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-label) "Power (zone)")
         (define/override (should-filter?) #t)
         (define/override (get-y-range) (cons 0 #f))
         (define/override (get-line-color) *dark-magenta*)
         (define/override (get-series-name) "pwr-zone")
         (define/override (get-fractional-digits) 1)
         )))
(provide axis-power-zone)

(define axis-left-right-balance
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-label) "Balance (%)")
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *purple*)
         (define/override (get-series-name) "lrbal")
         (define/override (get-fractional-digits) 1)
         )))
(provide axis-left-right-balance)

(define axis-left-torque-effectiveness
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-label) "Torque Effectiveness (%)")
         (define/override (should-filter?) #t)
         (define/override (get-y-range) (cons 0 100))
         (define/override (get-line-color) *coral*)
         (define/override (get-axis-title) "Torque Effectiveness, Left Pedal (%)")
         (define/override (get-series-label) "Left Pedal")
         (define/override (get-series-name) "lteff")
         (define/override (get-fractional-digits) 1)
         )))
(provide axis-left-torque-effectiveness)

(define axis-right-torque-effectiveness
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-label) "Torque Effectiveness (%)")
         (define/override (should-filter?) #t)
         (define/override (get-y-range) (cons 0 100))
         (define/override (get-line-color) *cornflower-blue*)
         (define/override (get-axis-title) "Torque Effectiveness, Right Pedal (%)")
         (define/override (get-series-label) "Right Pedal")
         (define/override (get-series-name) "rteff")
         (define/override (get-fractional-digits) 1))))

(provide axis-right-torque-effectiveness)

(define axis-left-pedal-smoothness
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-title) "Pedal Smoothness, Left Pedal (%)")
         (define/override (get-axis-label) "Pedal Smoothness (%)")
         (define/override (should-filter?) #t)
         (define/override (get-y-range) (cons 0 50))
         (define/override (get-line-color) *coral*)
         (define/override (get-series-label) "Left Pedal")
         (define/override (get-series-name) "lpsmth")
         (define/override (get-fractional-digits) 1)
         )))
(provide axis-left-pedal-smoothness)

(define axis-right-pedal-smoothness
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-title) "Pedal Smoothness, Right Pedal (%)")
         (define/override (get-axis-label) "Pedal Smoothness (%)")
         (define/override (should-filter?) #t)
         (define/override (get-y-range) (cons 0 50))
         (define/override (get-line-color) *cornflower-blue*)
         (define/override (get-series-label) "Right Pedal")
         (define/override (get-series-name) "rpsmth")
         (define/override (get-fractional-digits) 1))))
(provide axis-right-pedal-smoothness)

(define axis-left-platform-centre-offset
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-title) "Platform Centre Offset, Left Pedal (mm)")
         (define/override (get-axis-label) "Platform Centre Offset (mm)")
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *coral*)
         (define/override (get-series-label) "Left Pedal")
         (define/override (get-series-name) "lpco"))))
(provide axis-left-platform-centre-offset)

(define axis-right-platform-centre-offset
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-title) "Platform Centre Offset, Right Pedal (mm)")
         (define/override (get-axis-label) "Platform Centre Offset (mm)")
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *cornflower-blue*)
         (define/override (get-series-label) "Right Pedal")
         (define/override (get-series-name) "rpco"))))
(provide axis-right-platform-centre-offset)

(define axis-left-power-phase-start
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-title) "Power Phase Start, Left Pedal")
         (define/override (get-axis-label) "Power Phase Start")
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *coral*)
         (define/override (get-series-label) "Left Pedal")
         (define/override (get-series-name) "lpps")
         )))
(provide axis-left-power-phase-start)

(define axis-left-power-phase-end
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-title) "Power Phase End, Left Pedal")
         (define/override (get-axis-label) "Power Phase End")
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *coral*)
         (define/override (get-series-label) "Left Pedal")
         (define/override (get-series-name) "lppe"))))
(provide axis-left-power-phase-end)

(define axis-left-power-phase-angle
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-title) "Power Phase Angle, Left Pedal")
         (define/override (get-axis-label) "Power Phase Angle")
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *coral*)
         (define/override (get-series-label) "Left Pedal")
         (define/override (get-series-name) "lppa"))))
(provide axis-left-power-phase-angle)

(define axis-right-power-phase-start
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-title) "Power Phase Start, Right Pedal")
         (define/override (get-axis-label) "Power Phase Start")
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *cornflower-blue*)
         (define/override (get-series-label) "Right Pedal")
         (define/override (get-series-name) "rpps")
         )))
(provide axis-right-power-phase-start)

(define axis-right-power-phase-end
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-title) "Power Phase End, Right Pedal")
         (define/override (get-axis-label) "Power Phase End")
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *cornflower-blue*)
         (define/override (get-series-label) "Right Pedal")
         (define/override (get-series-name) "rppe"))))
(provide axis-right-power-phase-end)

(define axis-right-power-phase-angle
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-title) "Power Phase Angle, Right Pedal")
         (define/override (get-axis-label) "Power Phase Angle")
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *cornflower-blue*)
         (define/override (get-series-label) "Right Pedal")
         (define/override (get-series-name) "rppa"))))
(provide axis-right-power-phase-angle)

(define axis-left-peak-power-phase-start
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-title) "Peak Power Phase Start, Left Pedal")
         (define/override (get-axis-label) "Peak Power Phase Start")
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *coral*)
         (define/override (get-series-label) "Left Pedal")
         (define/override (get-series-name) "lppps")
         )))
(provide axis-left-peak-power-phase-start)

(define axis-left-peak-power-phase-end
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-title) "Peak Power Phase End, Left Pedal")
         (define/override (get-axis-label) "Peak Power Phase End")
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *coral*)
         (define/override (get-series-label) "Left Pedal")
         (define/override (get-series-name) "lpppe"))))
(provide axis-left-peak-power-phase-end)

(define axis-left-peak-power-phase-angle
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-title) "Peak Power Phase Angle, Left Pedal")
         (define/override (get-axis-label) "Peak Power Phase Angle")
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *coral*)
         (define/override (get-series-label) "Left Pedal")
         (define/override (get-series-name) "lpppa"))))
(provide axis-left-peak-power-phase-angle)

(define axis-right-peak-power-phase-start
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-title) "Peak Power Phase Start, Right Pedal")
         (define/override (get-axis-label) "Peak Power Phase Start")
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *cornflower-blue*)
         (define/override (get-series-label) "Right Pedal")
         (define/override (get-series-name) "rppps"))))
(provide axis-right-peak-power-phase-start)

(define axis-right-peak-power-phase-end
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-title) "Peak Power Phase End, Right Pedal")
         (define/override (get-axis-label) "Peak Power Phase End")
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *cornflower-blue*)
         (define/override (get-series-label) "Right Pedal")
         (define/override (get-series-name) "rpppe"))))
(provide axis-right-peak-power-phase-end)

(define axis-right-peak-power-phase-angle
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-title) "Peak Power Phase Angle, Right Pedal")
         (define/override (get-axis-label) "Peak Power Phase Angle")
         (define/override (should-filter?) #t)
         (define/override (get-line-color) *cornflower-blue*)
         (define/override (get-series-label) "Right Pedal")
         (define/override (get-series-name) "rpppa"))))
(provide axis-right-peak-power-phase-angle)

(define axis-swim-pace
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-ticks) (time-ticks #:formats '("~M:~f")))
         (define/override (get-axis-label)
           (if (eq? (al-pref-measurement-system) 'metric)
               "Pace (min/100m)" "Pace (min/100yd)"))
         (define/override (get-y-range) (cons 0 #f))
         (define/override (get-line-color) 'smart)
         (define/override (get-series-name) "pace")
         )))
(provide axis-swim-pace)

(define axis-swim-swolf
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-label) "SWOLF")
         (define/override (get-y-range) (cons 0 #f))
         (define/override (get-line-color) *green*)
         (define/override (get-series-name) "swolf")
         )))
(provide axis-swim-swolf)

(define axis-swim-stroke-count
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-label) "Strokes / Length")
         (define/override (get-y-range) (cons 0 #f))
         (define/override (get-line-color) *red*)
         (define/override (get-series-name) "strokes")
         )))
(provide axis-swim-stroke-count)

(define axis-swim-stroke-length
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-label) "Stride")
         (define/override (get-y-range) (cons 0 #f))
         (define/override (get-line-color) *red*)
         (define/override (get-series-name) "stride")
         (define/override (get-fractional-digits) 2)
         (define/override (get-histogram-bucket-slot) 0.01)
         )))
(provide axis-swim-stroke-length)

(define axis-swim-avg-cadence
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-label) "Strokes / Min")
         (define/override (get-y-range) (cons 0 #f))
         (define/override (get-line-color) *red*)
         (define/override (get-series-name) "cad")
         )))
(provide axis-swim-avg-cadence)

(define axis-swim-distance
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-label)
           (if (eq? (al-pref-measurement-system) 'metric)
               "Distance (meters)" "Distance (yards)"))
         (define/override (get-series-name) "distance"))))
(provide axis-swim-distance)

(define axis-swim-time
  (new (class axis-definition% (init) (super-new)
         (define/override (get-axis-ticks) (time-ticks #:formats '("~H:~M")))
         (define/override (get-axis-label) "Time (hour:min)")
         (define/override (get-series-name) "elapsed"))))
(provide axis-swim-time)

