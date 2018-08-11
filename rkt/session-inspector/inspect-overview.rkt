#lang racket/base
;; inspect-overview.rkt -- overview panel for the session
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

(require db/base
         pict
         racket/class
         racket/gui/base
         (rename-in srfi/48 (format format-48))
         racket/list
         racket/math
         math/statistics
         racket/dict
         "../fit-file/activity-util.rkt"
         "../al-widgets.rkt"
         "../fmt-util.rkt"
         "../widgets/map-widget/map-util.rkt"
         "../sport-charms.rkt"
         "../weather.rkt"
         "../data-frame/df.rkt"
         "../data-frame/statistics.rkt"
         "../widgets/main.rkt")

(provide inspect-overview-panel%)


;;.............................................. badge field definitions ....

(define *color-16* (make-color 238 238 238))
(define *hr-color* (make-color 255 242 243))
(define *elevation-color* (make-color 229 255 233))
(define *cadence-color* (make-color 255 249 229))
(define *timing-color* (make-color 229 247 255))
(define *power-color* (make-color 255 229 255))
(define *weather-color* (make-color 244 236 220))


(struct badge-field-def (name extractor formatter))
(struct badge-def (name priority bg-color fields))

(define *weather-fields*
  (list
   (badge-field-def "Temperature: " session-temperature (lambda (v) (temperature->string v #t)))
   (badge-field-def "Feels like: "
                    (lambda (session)
                      (let ((t (session-temperature session))
                            (dp (session-dew-point session)))
                        (and t dp (cons t dp))))
                    (lambda (v)
                      (temperature->string (humindex (car v) (cdr v)) #t)))
   (badge-field-def "Dew point: " session-dew-point (lambda (v) (temperature->string v #t)))
   (badge-field-def "Humidity: " session-humidity (lambda (v) (humidity->string v #t)))
   (badge-field-def "Wind speed: " session-wind-speed (lambda (v) (speed->string v #t)))
   (badge-field-def "Wind gusts: " session-wind-gusts (lambda (v) (speed->string v #t)))
   (badge-field-def "Wind direction: " session-wind-direction degrees->wind-rose)
   (badge-field-def "Pressure: " session-barometric-pressure (lambda (v) (pressure->string v #t)))
   (badge-field-def "Source: " session-weather-source values)))

(define *hr-fields*
  (list
   (badge-field-def "Avg HR: " 
                    (lambda (s) 
                      (let ((sid (dict-ref s 'database-id #f))
                            (avg-hr (session-avg-hr s)))
                        (if avg-hr (list sid avg-hr) #f)))
                    (lambda (v)
                      (let ((zones (get-session-sport-zones (first v) 1)))
                        (heart-rate->string/full (second v) zones))))
   (badge-field-def "Max HR: " 
                    (lambda (s) 
                      (let ((sid (dict-ref s 'database-id #f))
                            (max-hr (session-max-hr s)))
                        (if max-hr (list sid max-hr) #f)))
                    (lambda (v)
                      (let ((zones (get-session-sport-zones (first v) 1)))
                        (heart-rate->string/full (second v) zones))))
   (badge-field-def "HRV: "
                    session-hrv
                    (lambda (v) (if v (format "~a ms" v) "")))
   (badge-field-def "Aerobic Decoupling: "
                    session-aerobic-decoupling
                    (lambda (v) (if v (pct->string v) "")))))

;; Calculate the min/max elevation for the data frame DF and store it as the
;; 'min-elevation and 'max-elevation properties.
;;
;; The MIN/MAX elevation is not stored in the database (NOTE: perhaps it
;; should be), so we have to calculate it from the session data frame.  The
;; result is stored as properties on the data frame so they don't need to be
;; recalculated while the data frame is in the cache.
(define (setup-min-max-elevation df)
  (define series
    (cond ((df-contains? df "calt") "calt")
          ((df-contains? df "alt") "alt")
          (#t #f)))
  (when series
    (let ((stats (df-statistics df series)))
      (df-put-property df 'min-elevation (statistics-min stats))
      (df-put-property df 'max-elevation (statistics-max stats)))))

;; Return the min-elevation for the session (as retrieved from the data frame
;; DF).  SESSION is not used, and it is present just to satisfy the signature
;; of the badge extractor functions.  The data frame might be #f, as it is
;; loaded in the background and will be available before the first round of
;; snips are displayed.
(define (session-min-elevation session df)
  (if df
      (or (df-get-property df 'min-elevation)
          (begin
            (setup-min-max-elevation df)
            (df-get-property df 'min-elevation)))
      #f))

;; Return the max-elevation for the session, see remarks for
;; `session-min-elevation`
(define (session-max-elevation session df)
  (if df
      (or (df-get-property df 'max-elevation)
          (begin
            (setup-min-max-elevation df)
            (df-get-property df 'max-elevation)))
      #f))

(define *elevation-fields*
  (list
   (badge-field-def "Total Ascent: " session-total-ascent (lambda (v) (vertical-distance->string v #t)))
   (badge-field-def "Total Descent: " session-total-descent (lambda (v) (vertical-distance->string v #t)))
   (badge-field-def "Min Elevation: " session-min-elevation (lambda (v) (vertical-distance->string v #t)))
   (badge-field-def "Max Elevation: " session-max-elevation (lambda (v) (vertical-distance->string v #t)))))

(define *run-summary-fields*
  (list
   (badge-field-def "Distance: " session-distance (lambda (v) (distance->string v #t)))
   (badge-field-def "Time: " session-time duration->string)
   (badge-field-def "Avg Pace: " session-avg-speed (lambda (v) (pace->string v #t)))
   (badge-field-def "Elevation Gain: " session-total-ascent (lambda (v) (vertical-distance->string v #t)))
   (badge-field-def "Calories: " session-calories calories->string)
   (badge-field-def "Training Effect: " session-training-effect number->string)
   (badge-field-def "Effort: " session-training-stress-score 
                    (lambda (v) (format "~a" (exact-round v))))
   (badge-field-def "Intensity: " session-intensity-factor number->string)
   (badge-field-def "RPE: " session-rpe number->string) 
   ))

(define *run-timing-fields*
  (list
   (badge-field-def "Time: " session-time duration->string)
   (badge-field-def "Elapsed Time: " session-elapsed-time duration->string)
   (badge-field-def "Avg Pace: " session-avg-speed (lambda (v) (pace->string v #t)))
   (badge-field-def "Max Pace: " session-max-speed (lambda (v) (pace->string v #t)))
   (badge-field-def "Avg Speed: " session-avg-speed (lambda (v) (speed->string v #t)))
   (badge-field-def "Max Speed: " session-max-speed (lambda (v) (speed->string v #t)))))

(define *run-cadence-fields*
  (list
   (badge-field-def "Avg Run Cadence: " session-avg-cadence (lambda (v) (cadence->string v 'running)))
   (badge-field-def "Max Run Cadence: " session-max-cadence (lambda (v) (cadence->string v 'running)))
   (badge-field-def "Avg Stride Length: " session-avg-stride (lambda (v) (stride->string v #t)))
   (badge-field-def "Avg Vertical Oscillation: " session-avg-vertical-oscillation
                    (lambda (v) (vosc->string v #t)))
   (badge-field-def "Avg Vertical Ratio: " session-avg-vratio pct->string)
   (badge-field-def "Avg Ground Contact: "
                    (lambda (session)
                      (let ((stime (session-avg-stance-time session))
                            (spct (session-avg-stance-time-percent session)))
                        (if (and stime spct)
                            (cons stime spct)
                            #f)))
                    (lambda (v)
                      (stance->string (car v) (cdr v))))
   (badge-field-def "Left-Righ Balance: " session-left-right-balance
                    (lambda (v)
                      (let ((dev (- v 50.0)))
                        (format-48 "~1,1F% (~1,1F% ~a)" v
                                   dev (if (< dev 0) "Left" "Right")))))
   (badge-field-def "Total Vertical Travel: "
                    session-total-vertical-travel
                    (lambda (v) (vertical-distance->string v #t)))))

(define *run-badge-definitions*
  (list
   (badge-def "Summary" 1 *color-16* *run-summary-fields*)
   (badge-def "Timing"  2 *timing-color* *run-timing-fields*)
   (badge-def "Elevation"  3 *elevation-color* *elevation-fields*)
   (badge-def "Heart Rate" 4 *hr-color* *hr-fields*)
   (badge-def "Run Cadence" 5 *cadence-color* *run-cadence-fields*)
   (badge-def "Weather" 6 *weather-color* *weather-fields*)))


;;................................................... info fields biking ....

(define *bike-summary-fields*
  (list
   (badge-field-def "Distance: " session-distance (lambda (v) (distance->string v #t)))
   (badge-field-def "Time: " session-time duration->string)
   (badge-field-def "Avg Speed: " session-avg-speed (lambda (v) (speed->string v #t)))
   (badge-field-def "Elevation Gain: " session-total-ascent (lambda (v) (vertical-distance->string v #t)))
   (badge-field-def "Calories: " session-calories calories->string)
   (badge-field-def "Training Effect: " session-training-effect number->string)
   (badge-field-def "Effort: " session-training-stress-score 
                    (lambda (v) (format "~a" (exact-round v))))
   (badge-field-def "Intensity: " session-intensity-factor number->string)
   (badge-field-def "RPE: " session-rpe number->string)))

(define *bike-timing-fields*
  (list
   (badge-field-def "Time: " session-time duration->string)
   (badge-field-def "Elapsed Time: " session-elapsed-time duration->string)
   (badge-field-def "Avg Speed: " session-avg-speed (lambda (v) (speed->string v #t)))
   (badge-field-def "Max Speed: " session-max-speed (lambda (v) (speed->string v #t)))))

(define *bike-cadence-fields*
  (list
   (badge-field-def "Avg Bike Cadence: " session-avg-cadence (lambda (v) (cadence->string v 'biking)))
   (badge-field-def "Max Bike Cadence: " session-max-cadence (lambda (v) (cadence->string v 'biking)))))

(define *bike-power-fields*
  (list
   (badge-field-def "Avg Power: " session-avg-power (lambda (v) (power->string v #t)))
   (badge-field-def "Max Power: " session-max-power (lambda (v) (power->string v #t)))
   (badge-field-def "Weighted Power: " session-normalized-power (lambda (v) (power->string v #t)))
   (badge-field-def "Left-Righ Balance: " session-left-right-balance
                    (lambda (v)
                      (let ((dev (- v 50.0)))
                        (format-48 "~1,1F% (~1,1F% ~a)" v
                                   dev (if (< dev 0) "Left" "Right")))))
   (badge-field-def "Torque Effectiveness: " 
                    (lambda (session)
                      (let ((left (session-avg-left-torque-effectiveness session))
                            (right (session-avg-right-torque-effectiveness session)))
                        (if (and left right)
                            (cons left right)
                            #f)))
                    (lambda (v)
                      (format-48 "~1,1F% L, ~1,1F% R" (car v) (cdr v))))
   (badge-field-def "Pedal Smoothness: " 
                    (lambda (session)
                      (let ((left (session-avg-left-pedal-smoothness session))
                            (right (session-avg-right-pedal-smoothness session)))
                        (if (and left right)
                            (cons left right)
                            #f)))
                    (lambda (v)
                      (format-48 "~1,1F% L, ~1,1F% R" (car v) (cdr v))))
   (badge-field-def "Platform Centre Offset: "
                    (lambda (session)
                      (let ((left (session-avg-left-pco session))
                            (right (session-avg-right-pco session)))
                        (if (and left right)
                            (cons left right)
                            #f)))
                    (lambda (v)
                      (string-append (pco->string (car v) #t) " L, " (pco->string (cdr v) #t) " R")))
   (badge-field-def "Left Power Phase: "
                    (lambda (session)
                      (let ((start (session-avg-left-pp-start session))
                            (end (session-avg-left-pp-end session))
                            (peak-start (session-avg-left-ppp-start session))
                            (peak-end (session-avg-left-ppp-end session)))
                        (if (and start end peak-start peak-end)
                            (list start end peak-start peak-end)
                            #f)))
                    (lambda (v)
                      (string-append
                       (power-phase->string (list-ref v 0) (list-ref v 1))
                       "  peak "
                       (power-phase->string (list-ref v 2) (list-ref v 3)))))
   (badge-field-def "Right Power Phase: "
                    (lambda (session)
                      (let ((start (session-avg-right-pp-start session))
                            (end (session-avg-right-pp-end session))
                            (peak-start (session-avg-right-ppp-start session))
                            (peak-end (session-avg-right-ppp-end session)))
                        (if (and start end peak-start peak-end)
                            (list start end peak-start peak-end)
                            #f)))
                    (lambda (v)
                      (string-append
                       (power-phase->string (list-ref v 0) (list-ref v 1))
                       "  peak "
                       (power-phase->string (list-ref v 2) (list-ref v 3)))))))


(define *bike-badge-definitions*
  (list
   (badge-def "Summary" 1 *color-16* *bike-summary-fields*)
   (badge-def "Timing" 2 *timing-color* *bike-timing-fields*)
   (badge-def "Elevation" 3 *elevation-color* *elevation-fields*)
   (badge-def "Heart Rate" 4 *hr-color* *hr-fields*)
   (badge-def "Bike Cadence" 5 *cadence-color* *bike-cadence-fields*)
   (badge-def "Power" 6 *power-color* *bike-power-fields*)
   (badge-def "Weather" 7 *weather-color* *weather-fields*)))


;;................................................. info fields swimming ....

(define *swim-summary-fields*
  (list
   (badge-field-def "Distance: " session-distance (lambda (v) (short-distance->string v #t)))
   (badge-field-def "Pool Length: " session-pool-length (lambda (v) (short-distance->string v #t)))
   (badge-field-def "Time: " session-elapsed-time duration->string)
   (badge-field-def "Avg Pace: " session-avg-speed (lambda (v) (swim-pace->string v #t)))
   (badge-field-def "Calories: " session-calories calories->string)
   (badge-field-def "Avg SWOLF: " session-avg-swolf number->string)
   (badge-field-def "Training Effect: " session-training-effect number->string)
   (badge-field-def "Effort: " session-training-stress-score 
                    (lambda (v) (format "~a" (exact-round v))))
   (badge-field-def "Intensity: " session-intensity-factor number->string)
   (badge-field-def "RPE: " session-rpe number->string)))

(define *swim-timing-fields*
  (list
   (badge-field-def "Time: " session-time duration->string)
   (badge-field-def "Elapsed Time: " session-elapsed-time duration->string)
   (badge-field-def "Moving Time: " session-moving-time duration->string)
   (badge-field-def "Avg Pace: " session-avg-speed (lambda (v) (swim-pace->string v #t)))
   (badge-field-def "Best Pace: " session-max-speed (lambda (v) (swim-pace->string v #t)))))

(define *swim-cadence-fields*
  (list
   (badge-field-def "Avg Cadence: " session-avg-cadence (lambda (v) (cadence->string v 'swimming)))
   (badge-field-def "Avg Strokes: " session-avg-strokes-per-length (lambda (v) (format "~a strokes/length" v)))
   (badge-field-def "Total Strokes: " session-total-cycles number->string)))

(define *swim-badge-definitions*
  (list
   (badge-def "Summary" 1 *color-16* *swim-summary-fields*)
   (badge-def "Timing" 2 *timing-color* *swim-timing-fields*)
   (badge-def "Strokes" 3 *cadence-color* *swim-cadence-fields*)
   (badge-def "Weather" 4 *weather-color* *weather-fields*)))


;............................................. info fields other sports ....

(define *other-summary-fields*
  (list
   (badge-field-def "Distance: " session-distance (lambda (v) (distance->string v #t)))
   (badge-field-def "Time: " session-time duration->string)
   (badge-field-def "Avg Pace: " session-avg-speed (lambda (v) (pace->string v #t)))
   (badge-field-def "Elevation Gain: " session-total-ascent (lambda (v) (vertical-distance->string v #t)))
   (badge-field-def "Calories: " session-calories calories->string)
   (badge-field-def "Training Effect: " session-training-effect number->string)
   (badge-field-def "Effort: " session-training-stress-score 
                    (lambda (v) (format "~a" (exact-round v))))
   (badge-field-def "Intensity: " session-intensity-factor number->string)
   (badge-field-def "RPE: " session-rpe number->string)))

(define *other-timing-fields*
  (list
   (badge-field-def "Time: " session-time duration->string)
   (badge-field-def "Elapsed Time: " session-elapsed-time duration->string)
   (badge-field-def "Avg Pace: " session-avg-speed (lambda (v) (pace->string v #t)))
   (badge-field-def "Max Pace: " session-max-speed (lambda (v) (pace->string v #t)))
   (badge-field-def "Avg Speed: " session-avg-speed (lambda (v) (speed->string v #t)))
   (badge-field-def "Max Speed: " session-max-speed (lambda (v) (speed->string v #t)))))

(define *other-cadence-fields*
  (list
   (badge-field-def "Avg Cadence: " session-avg-cadence (lambda (v) (cadence->string v 'running)))
   (badge-field-def "Max Cadence: " session-max-cadence (lambda (v) (cadence->string v 'running)))))

(define *other-badge-definitions*
  (list
   (badge-def "Summary" 1 *color-16* *other-summary-fields*)
   (badge-def "Timing" 2 *timing-color* *other-timing-fields*)
   (badge-def "Elevation" 3 *elevation-color* *elevation-fields*)
   (badge-def "Heart Rate" 4 *hr-color* *hr-fields*)
   (badge-def "Cadence" 5 *cadence-color* *other-cadence-fields*)
   (badge-def "Weather" 6 *weather-color* *weather-fields*)))

(define *sport-badge-definitions*
  (list
   (cons 1 *run-badge-definitions*)
   (cons 2 *bike-badge-definitions*)
   (cons 5 *swim-badge-definitions*)))

(define *default-badge-definitions* *other-badge-definitions*)

(define (get-badge-definitions sport)
  (cond ((assoc sport *sport-badge-definitions*) => cdr)
	(#t *default-badge-definitions*)))

;; Make PICT to have TARGET-WIDTH by adding space to the right.  Do nothing if
;; PICT is wider than TARGET-WIDTH.
(define (pad-right pict target-width)
  (let ((w (pict-width pict)))
    (if (> (- target-width w) 0)
        (inset pict 0 0 (- target-width w) 0)
        pict)))

;; Merge two columns of picts, COL1 and COL2 as row pairs, ensuring that they
;; have uniform width.
(define (merge-as-rows col1 col2)
  (let ((col1-width (* 1.10 (apply max (map pict-width col1))))
	(col2-width (* 1.05 (apply max (map pict-width col2)))))
    (apply
     vl-append
     5
     (for/list ((item1 (in-list col1))
                (item2 (in-list col2)))
       (hc-append
        (pad-right item1 col1-width)
        (pad-right item2 col2-width))))))

(define (add-title-and-bg title color data-badge)
  (let ((title-pict (text title (cons 'bold "Helvetica") 14)))
    (let ((b (vl-append 10 title-pict data-badge)))
      (let ((r (filled-rounded-rectangle (+ (pict-width b) 25)
                                         (+ (pict-height b) 25)
                                         -0.05)))
	(cc-superimpose (colorize r color) b)))))

;; Make a badge pict according to BDEF (a badge-def structure), using data
;; from SESSION.
(define (make-badge bdef session df)
  (let ((labels '())
	(values '()))
    (for ((fdef (in-list (badge-def-fields bdef))))
      (let* ((extractor-fn (badge-field-def-extractor fdef))
             ;; extractor functions have one argument, which is the session,
             ;; or two arguments which are the session and the data frame.
             (data (if (eq? (procedure-arity extractor-fn) 2)
                       (extractor-fn session df)
                       (extractor-fn session))))
        (when data
          (set! labels (cons (text (badge-field-def-name fdef)) labels))
          (set! values (cons (text ((badge-field-def-formatter fdef) data)) values)))))
    (if (null? labels)
        #f
	(add-title-and-bg
         (badge-def-name bdef)
         (badge-def-bg-color bdef)
         (merge-as-rows (reverse labels) (reverse values))))))


;;.......................................................... badge-snip% ....

(define badge-snip-class
  (make-object
   (class snip-class% (super-new) (send this set-classname "badge-snip"))))

(send (get-the-snip-class-list) add badge-snip-class)

(define badge-snip%
  (class snip%
    (init badge-definition session data-frame)
    (super-new)
    (inherit get-admin set-flags get-flags set-snipclass set-count)

    (set-snipclass badge-snip-class)
    (set-count 1)

    ;; NOTE: badge-pict will be #f if SESSION has none of the fields in
    ;; BADGE-DEFINITION.
    (define bdef badge-definition)
    (define priority (badge-def-priority bdef))
    (define bpict (make-badge badge-definition session data-frame))
    (define target-width (if bpict (pict-width bpict) 0))

    (define decorated-bpict #f)
    (define draw-fn #f)

    (define (decorate pict color width)
      (let ((pict (pad-right pict width)))
        (let ((r (filled-rounded-rectangle
                  (+ (pict-width pict) 0)
                  (+ (pict-height pict) 0) -0.05)))
          (cc-superimpose (colorize r color) pict))))

    (define (invalidate-draw-fn)
      (set! decorated-bpict #f)
      (set! draw-fn #f))

    ;; Attempt to make a draw function (and a decorated-bpict).  Note that
    ;; draw-fn might still be #f after calling this methtod
    (define (make-draw-fn)
      (set! decorated-bpict #f)
      (set! draw-fn #f)
      (when bpict
        (set! decorated-bpict (decorate bpict (badge-def-bg-color bdef) target-width))
        ;;(printf "target-width ~a, actual ~a~%" target-width (pict-width decorated-bpict))
        (set! draw-fn (make-pict-drawer decorated-bpict))))

    (define/override (get-extent dc x y w h descent space lspace rspace)
      (unless draw-fn (make-draw-fn))
      (when w (set-box! w (if decorated-bpict (pict-width decorated-bpict) 0)))
      (when h (set-box! h (if decorated-bpict (pict-height decorated-bpict) 0)))
      (when descent (set-box! descent (if decorated-bpict (pict-descent decorated-bpict) 0)))
      (when space (set-box! space 10.0))
      (when lspace (set-box! lspace 10.0))
      (when rspace (set-box! rspace 10.0)))

    (define/override (draw dc x y . other)
      (unless draw-fn (make-draw-fn))
      (when draw-fn (draw-fn dc x y)))

    (define/public (set-target-width w)
      (set! target-width w)
      (invalidate-draw-fn)
      (send (get-admin) resized this #t))

    (define/public (get-actual-width)
      (unless draw-fn (make-draw-fn))
      (if decorated-bpict (pict-width decorated-bpict) 0))

    (define/public (get-actual-height)
      (unless draw-fn (make-draw-fn))
      (if decorated-bpict (pict-height decorated-bpict) 0))

    (define/public (good?) (if bpict #t #f))
    (define/public (get-priority) priority)

    ))


;;.................................................... badge-pasteboard% ....

(define badge-pb%
  (class pasteboard%
    (init)
    (super-new)
    (inherit get-canvas move-to find-first-snip insert delete set-before
             begin-edit-sequence end-edit-sequence erase)

    (define (with-edit-sequence thunk)
      (begin-edit-sequence)
      (thunk)
      (end-edit-sequence))

    ;; (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
    ;;   (when before?
    ;;     (send dc clear)

    ;;     (shade-some-cells dc)
    ;;     (draw-headers dc)
    ;;     (draw-grid dc)
    ;;     (draw-totals dc)
    ;;     (draw-day-labels dc)))

    ;; (define/augment (can-insert? snip . rest)
    ;;   (let ((start-time (send snip get-start-time)))
    ;;     (number? (seconds->day-index start-time))))

    (define/augment (after-insert snip before x y)
      #f)

    (define removing-snips? #f)
    (define move-allowed? #f)

    (define/augment (can-delete? snip)
      removing-snips?)

    (define/augment (can-move-to? snip x y dragging?)
      (or dragging? move-allowed?))

    (define/augment (after-delete snip)
      #f)

    (define/augment (after-move-to snip x y dragging?)
      #f)

    (define/augment (on-select snip on?)
      #f)

    (define (remove-all-snips)
      (with-edit-sequence
       (lambda ()
           (set! removing-snips? #t)
           (erase)
           (set! removing-snips? #f))))

    (define (get-snip-list)
      (let ((snip-list (let loop ((snip (find-first-snip)) (snip-list '()))
                         (if snip
                             (loop (send snip next) (cons snip snip-list))
                             snip-list)))
            (sort-key (lambda (s) (send s get-priority))))
        (sort snip-list < #:key sort-key)))

    (define (arrange-badges)

      (define (get-max-width badges)
        (foldl (lambda (a b)
                 (max (send a get-actual-width) b))
               0 badges))
      
      (define (get-total-height badges spacing)
        (foldl (lambda (a b)
                 (+ (send a get-actual-height) b spacing))
               spacing badges))

      (set! move-allowed? #t)
      (let ((dc (send (get-canvas) get-dc))
            (spacing 5))
        (let-values (([w h] (send dc get-size)))
          (let* ((badges (get-snip-list))
                 (target-width (get-max-width badges))
                 (total-height (get-total-height badges spacing))
                 (num-columns (min 2 (exact-ceiling (/ total-height h)))))

            (for ((badge (in-list badges)))
              (send badge set-target-width target-width))

            (let loop ((b badges) (y spacing) (c 0))
              (unless (null? b)
                (let* ((bheight (send (car b) get-actual-height))
                       (next-y (+ y bheight spacing)))
                  (if (and (> y spacing) (> next-y h) (< c num-columns))
                      (loop b spacing (+ c 1))
                      (begin
                        (move-to (car b) (+ spacing (* c (+ spacing target-width))) y)
                        (loop (cdr b) next-y c))))))
            (send (get-canvas) min-width 
                  (exact-ceiling (+ (* num-columns target-width) (* spacing (+ 1 num-columns)))))
            )))
      (set! move-allowed? #f))

    (define/augment (on-display-size)
      (arrange-badges))

    (define/public (set-session session df)
      (with-edit-sequence
       (lambda ()
         (remove-all-snips)
         (for ((bdef (in-list (get-badge-definitions (session-sport session)))))
           (let ((snip (new badge-snip% [badge-definition bdef] [session session] [data-frame df])))
             (when (send snip good?)
               (insert snip))))
         (arrange-badges))))

    (send this set-selection-visible #f)

    ))


;;.............................................. inspect-overview-panel% ....

(define headline-font
  (send the-font-list find-or-create-font 16 'default 'normal 'normal))

(define inspect-overview-panel%
  (class object%
    (init parent database)
    (super-new)

    (define the-database database)
    (define the-session #f)

    (define panel (new horizontal-panel% [parent parent] [spacing 5]))

    (define badge-pb
      (let ((badge-pb (new badge-pb%)))
        (new editor-canvas%
             [parent panel]
             [editor badge-pb]
             [style '(auto-hscroll auto-vscroll)]
             [stretchable-width #f]
             [horizontal-inset 0]
             [vertical-inset 0])
        badge-pb))

    (define desc-panel (new vertical-pane% [parent panel] [alignment '(center top)]))

    (new message%
         [parent desc-panel]
         [stretchable-height #f]
         [stretchable-width #t]
         [font headline-font]
         [label "Labels"])

    (define labels-field
      (new label-input-field% [parent desc-panel]
           [callback (lambda (x) (update-session-labels))]))

    (new message%
         [parent desc-panel]
         [stretchable-height #f]
         [stretchable-width #t]
         [font headline-font]
         [label "Equipment"])

    (define equipment-field
      (new equipment-input-field% [parent desc-panel]
           [callback (lambda (x) (update-session-equipment))]))

    (define description-field
      (new notes-input-field% [parent desc-panel]
           [on-save-callback (lambda (text) (update-session-description text))]))
      
    (define (update-session-labels)
      (when the-session
        (let ((sid (dict-ref the-session 'database-id #f)))
          (when sid
            (send labels-field update-session-tags sid)))))

    (define (update-session-equipment)
      (when the-session
        (let ((sid (dict-ref the-session 'database-id #f)))
          (when sid
            (send equipment-field update-session-tags sid)))))

    ;; WARNING: the description text is currently saved when the description
    ;; field looses focus.  This might result in unwanted changes and
    ;; potential loss of information.  Maybe we can do better?
    (define (update-session-description description)
      (when the-session
        (let ((sid (dict-ref the-session 'database-id #f)))
          (when sid
            (call-with-transaction
             the-database
             (lambda ()
               (query-exec the-database
                           "update A_SESSION set description = ? where id = ?"
                           description sid)))))))

    (define/public (set-session session df)
      (set! the-session #f) ; prevent saving of data to the wrong session
      (send badge-pb set-session session df)
      (let ((session-id (dict-ref session 'database-id #f)))
        (when session-id
          (send labels-field setup-for-session the-database session-id)
          (send equipment-field setup-for-session the-database session-id)
          (let ((desc (dict-ref session 'description #f)))
            (send description-field set-contents desc)))
        (set! the-session session)))

    (define/public (unsaved-edits?)
      (send description-field unsaved-edits?))

    ))
