#lang typed/racket/base
;; fmt-util.rkt -- formatting utilities
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

;;; Comentary:
;;
;; Utility functions to convert values (speeds, pace, durations, etc) to
;; strings.  Internally, activity log uses the metric system to store and
;; manipulate values, these functions are used to display data to the user in
;; the GUI using the appropiate measurement system (metric or statute)

(require/typed
    (rename-in srfi/48 (format format-48))
  [format-48 (-> String Any * String)])
(require/typed
    "sport-charms.rkt"
  [val->pct-of-max (-> Real (Listof Real) Real)]
  [val->zone (-> Real (Listof Real) Real)])
(require/typed
    "al-prefs.rkt"
  [al-get-pref (-> Symbol (-> Any) Any)]
  [al-put-pref (-> Symbol Any Void)])

(require racket/date
         racket/list
         racket/math
         racket/string
         "map-util.rkt")

(: identity (All (a) (-> a a)))
(define (identity x) x)

(provide wind->string
         temperature->string
         humidity->string
         pressure->string
         speed->string
         pace->string
         swim-pace->string
         distance->string
         short-distance->string
         vertical-distance->string
         duration->string
         calendar-date->string
         time-of-day->string
         date-time->string
         n->string
         pct->string
         stride->string
         vosc->string
         vratio->string
         stance-time->string
         stance-time-pct->string
         stance->string
         cadence->string
         heart-rate->string/bpm
         heart-rate->string/pct
         heart-rate->string/zone
         heart-rate->string/full
         calories->string
         power->string
         weight->string
         pco->string
         power-phase->string)



;;........................................ converters to different units ....

(provide
 m/s->km/h
 m/s->mi/h
 m/s->sec/km
 m/s->sec/mi
 m/s->sec/100m
 m/s->sec/100yd
 m/s->swim-pace
 m->km
 m->mi
 m->yd
 m->ft
 mm->inch)

(: m/s->km/h (-> Real Real))
(define (m/s->km/h speed) (/ (* speed 3600.0) 1000.0))

(: m/s->mi/h (-> Real Real))
(define (m/s->mi/h speed) (/ (* speed 3600.0) 1609.0))

(: m/s->sec/km (-> Positive-Real Real))
(define (m/s->sec/km speed) (/ 1000.0 speed))

(: m/s->sec/mi (-> Positive-Real Real))
(define (m/s->sec/mi speed) (/ 1609.0 speed))

(: m/s->sec/100m (-> Positive-Real Real))
(define (m/s->sec/100m speed) (/ 100.0 speed))

(: m/s->sec/100yd (-> Positive-Real Real))
(define (m/s->sec/100yd speed) (/ 91.44 speed))

(: m->km (-> Real Real))
(define (m->km m) (/ m 1000))

(: m->mi (-> Real Real))
(define (m->mi m) (/ m 1609))

(: m->yd (-> Real Real))
(define (m->yd m) (/ m 0.9144))

(: m->ft (-> Real Real))
(define (m->ft m) (* m 3.2808))

(: celsius->fahrenheit (-> Real Real))
(define (celsius->fahrenheit c) (+ 32 (* c 1.8)))

(: mm->inch (-> Real Real))
(define (mm->inch mm) (* mm 0.0393700787))

(: kg->lb (-> Real Real))
(define (kg->lb kg) (* kg 2.20462262))

;; The actual converter function, used by the formatters.  They are set up by
;; `setup-measurement-system' based on the value of `measurement-system'

(define m/s->speed m/s->km/h)
(provide m/s->speed)
(define speed-label "km/h")
(define m/s->pace m/s->sec/km)
(provide m/s->pace)
(define pace-label "min/km")
(define m/s->swim-pace m/s->sec/100m)
(define swim-pace-label "min/100m")
(define celsius->temperature celsius->fahrenheit) ; !!! 
(define temperature-label "℃")
(define m->distance m->km)
(define distance-label "km")
(define m->short-distance m->yd)        ; !!!
(define short-distance-label "yd")
(define m->vertical-distance m->ft)
(define vertical-distance-label "ft")
(define m->vertical-oscillation mm->inch) ; !!!
(define vertical-oscillation-label "in")
(define m->weight kg->lb)
(define weight-label "kg")

(: setup-measurement-system (-> Symbol Void))
(define (setup-measurement-system mu)
  (if (eq? mu 'statute)
      (begin
        (set! m/s->speed m/s->mi/h)
        (set! speed-label "mi/h")
        (set! m/s->pace m/s->sec/mi)
        (set! pace-label "min/mi")
        (set! m/s->swim-pace m/s->sec/100yd)
        (set! swim-pace-label "min/100yd")
        (set! celsius->temperature celsius->fahrenheit)
        (set! temperature-label "℉")
        (set! m->distance m->mi)
        (set! distance-label "mi")
        (set! m->short-distance m->yd)
        (set! short-distance-label "yd")
        (set! m->vertical-distance m->ft)
        (set! vertical-distance-label "ft")
        (set! m->vertical-oscillation mm->inch)
        (set! vertical-oscillation-label "in")
        (set! m->weight kg->lb)
        (set! weight-label "lb"))
      
      (begin
        (set! m/s->speed m/s->km/h)
        (set! speed-label "km/h")
        (set! m/s->pace m/s->sec/km)
        (set! pace-label "min/km")
        (set! m/s->swim-pace m/s->sec/100m)
        (set! swim-pace-label "min/100m")
        (set! celsius->temperature identity)
        (set! temperature-label "℃")
        (set! m->distance m->km)
        (set! distance-label "km")
        (set! m->short-distance identity)
        (set! short-distance-label "m")
        (set! m->vertical-distance identity)
        (set! vertical-distance-label "m")
        (set! m->vertical-oscillation identity)
        (set! vertical-oscillation-label "mm")
        (set! m->weight identity)
        (set! weight-label "kg")
        )))

(: al-pref-measurement-system (Parameterof Symbol))
(define al-pref-measurement-system
  (let* ((tag 'activity-log:measurement-system)
         (val (al-get-pref tag (lambda () 'metric))))
    (make-parameter
     (if (symbol? val) val 'metric)
     (lambda ([new-val : Symbol])
       ;; Write the value back to the store
       (al-put-pref tag new-val)
       ;; Update the value
       (setup-measurement-system new-val)
       new-val))))
(provide al-pref-measurement-system)

(setup-measurement-system (al-pref-measurement-system))



;;......................................................... weather data ....

;; NOTE: SPEED is in meters/second
(: wind->string (-> Real Real String))
(define (wind->string speed direction)
  (if (> speed 0)
      (format-48 "~1,1F ~a ~a" (m/s->speed speed) speed-label (degrees->wind-rose direction))
      ""))

;; NOTE: DEGC is always in degrees celsius
(: temperature->string (->* (Real) (Boolean) String))
(define (temperature->string degc [unit-label #f])
  (if (> degc -1000)
      (string-append
       (format-48 "~1,1F" (celsius->temperature degc))
       (if unit-label (string-append " " temperature-label) ""))
      ""))

;; NOTE: humidity is between 0 and 100
(: humidity->string (->* (Real) (Boolean) String))
(define (humidity->string hum [unit-label #f])
  (if (and (> hum 0) (<= hum 100))
      (string-append
       (format-48 "~F" (exact-round hum))
       (if unit-label " %" ""))
      ""))

(: pressure->string (->* (Real) (Boolean) String))
(define (pressure->string p [unit-label #f])
  (if (> p 0)
      (string-append
       (format-48 "~1,1F" (exact->inexact p))
       (if unit-label " hPa" ""))
      ""))


;;..................................................... speeds and paces ....

(: speed->string (->* (Real) (Boolean) String))
(define (speed->string speed/mps [unit-label #f])
  (let ((speed (m/s->speed speed/mps)))
    (format-48 "~1,2F~a" speed (if unit-label (string-append " " speed-label) ""))))

(: pace->string (->* (Real) (Boolean) String))
(define (pace->string speed/mps [unit-label #f])
  ;; NOTE: not sure if we should just force a Positive-Real on the SPEED/MPS
  ;; and not return an empty string...
  (if (> speed/mps 0)
      (let* ((sec/km (m/s->pace speed/mps))
             (min (exact-truncate (/ sec/km 60.0)))
             (sec (exact-truncate (- sec/km (* min 60.0)))))
        (string-append
         (string-replace (format-48 "~2F:~2F" min sec) " " "0")
         (if unit-label (string-append " " pace-label) "")))
      ""))

(: swim-pace->string (->* (Real) (Boolean) String))
(define (swim-pace->string speed/mps [unit-label #f])
  (if (> speed/mps 0)
      (let* ((sec/100m (m/s->swim-pace speed/mps))
             (min (exact-truncate (/ sec/100m 60.0)))
             (sec (exact-truncate (- sec/100m (* min 60.0)))))
        (string-append
         (string-replace (format-48 "~2F:~2F" min sec) " " "0")
         (if unit-label (string-append " " swim-pace-label) "")))
      ""))


;;.............................................. distances and durations ....

(: distance->string (->* (Real) (Boolean) String))
(define (distance->string distance/m [unit-label #f])
  (string-append
   (format-48 "~1,2F" (m->distance distance/m))
   (if unit-label (string-append " " distance-label) "")))

(: short-distance->string (->* (Real) (Boolean) String))
(define (short-distance->string distance/m [unit-label #f])
  (string-append
   (format-48 "~F" (inexact->exact (truncate (m->short-distance distance/m))))
   (if unit-label (string-append " " short-distance-label)  "")))

(: vertical-distance->string (->* (Real) (Boolean) String))
(define (vertical-distance->string distance/m [unit-label #f])
  (string-append
   (format-48 "~F" (inexact->exact (truncate (m->vertical-distance distance/m))))
   (if unit-label (string-append " " vertical-distance-label)  "")))

(: duration->string (->* (Real) (Boolean) String))
(define (duration->string seconds [high-precision? #f])
  (let* ((seconds (if high-precision? seconds (round seconds)))
         (h (exact-truncate (/ seconds 3600.0)))
         (m (exact-truncate (/ (- seconds (* h 3600.0)) 60.0)))
         (s (exact-truncate (- seconds (* h 3600.0) (* m 60.0))))
         (ms (exact-truncate (* 10 (- seconds s (* h 3600.0) (* m 60.0))))))
    (string-replace
     (if high-precision?
         (if (> h 0)
             (format-48 "~2F:~2F:~2F.~F" h m s ms)
             (format-48 "~2F:~2F.~F" m s ms))
         (if (> h 0)
             (format-48 "~2F:~2F:~2F" h m s)
             (format-48 "~2F:~2F" m s)))
     " " "0")))


;;........................................................ calendar time ....

(: calendar-date->string (-> (U date Integer) String))
(define (calendar-date->string d)

  (: month (Vectorof String))
  (define month (vector "XXX" "Jan" "Feb" "Mar" "Apr" "May" "Jun"
                        "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

  (let ((d1 (if (date? d) d (seconds->date d #t))))
    (format-48 "~a ~a ~a" (date-day d1)
               (vector-ref month (date-month d1))
               (date-year d1))))

(: time-of-day->string (->* ((U date Integer)) (Boolean) String))
(define (time-of-day->string d [include-seconds? #t])
  (let ((d1 (if (date? d) d (seconds->date d #t))))
    (string-replace
     (if include-seconds?
         (format-48 "~2F:~2F:~2F"
                    (date-hour d1) (date-minute d1) (date-second d1))
         (format-48 "~2F:~2F"
                    (date-hour d1) (date-minute d1)))
         " " "0")))

(: date-time->string (->* ((U date Integer)) (Boolean) String))
(define (date-time->string d [include-seconds? #f])
  (let ((d1 (if (date? d) d (seconds->date d #t))))
    (string-append (calendar-date->string d1) 
                   " " 
                   (time-of-day->string d1 include-seconds?))))


;;................................................... cadence and stride ....

(: stride->string (->* (Real) (Boolean) String))
(define (stride->string stride [unit-label #f])
  (if (> stride 0) 
      (format-48 "~1,2F~a" (m->short-distance stride)
                 (if unit-label (string-append " " short-distance-label) ""))
      ""))

(: vosc->string (->* (Real) (Boolean) String))
(define (vosc->string vosc [unit-label #f])
  (if (> vosc 0)
      (format-48 "~1,1F~a"
                 (m->vertical-oscillation vosc)
                 (if unit-label (string-append " " vertical-oscillation-label) ""))
      ""))

(: vratio->string (->* (Real) (Boolean) String))
(define (vratio->string vratio [unit-label #f])
  (if (> vratio 0)
      (format-48 "~1,1F~a" vratio (if unit-label " %" ""))
      ""))

(: stance-time->string (->* (Real) (Boolean) String))
(define (stance-time->string stime [unit-label #f])
  (if (> stime 0)
      (format-48 "~F~a" (exact-round stime) (if unit-label " ms" ""))
      ""))

(: stance-time-pct->string (->* (Real) (Boolean) String))
(define (stance-time-pct->string pct [unit-label #f])
  (if (> pct 0)
      (format-48 "~1,1F~a" pct (if unit-label " %" ""))
      ""))

(: stance->string (-> Real Real String))
(define (stance->string stime pct)
  (if (and (> pct 0) (> stime 0))
      (format-48 "~F ms (~F %)" (exact-round stime) (exact-round pct))
      ""))

(: cadence->string (->* (Real (U Symbol Nonnegative-Integer)) (Boolean) String))
(define (cadence->string cadence sport [unit-label #f])
  (if (> cadence 0)
      (format-48 "~F ~a" (exact-truncate cadence)
                 (case sport
                   ((running 1) "SPM")
                   ((biking 2) "RPM")
                   ((swimming 5) "strokes/min")
                   (else "SPM")))
      ""))


;;.............................................. heart rate and calories ....

(: heart-rate->string/bpm (-> Real String))
(define (heart-rate->string/bpm bpm)
  (format-48 "~F bpm" (exact-truncate bpm)))

(: heart-rate->string/pct (-> Real (Listof Real) String))
(define (heart-rate->string/pct bpm zones)
  (let ((pct (val->pct-of-max bpm zones)))
    (format-48 "~F% of Max" (exact-truncate pct))))

(: heart-rate->string/zone (-> Real (Listof Real) String))
(define (heart-rate->string/zone bpm zones)
  (let ((zone (val->zone bpm zones)))
    (format-48 "~1,1Fz" zone)))

(: heart-rate->string/full (->* (Real) ((U False (Listof Real))) String))
(define (heart-rate->string/full bpm [zones #f])
  (if zones
      (format "~a (~a, ~a)"
              (heart-rate->string/bpm bpm)
              (heart-rate->string/pct bpm zones)
              (heart-rate->string/zone bpm zones))
      (heart-rate->string/bpm bpm)))

(: calories->string (-> Real String))
(define (calories->string cal)
  (format-48 "~F C" (exact-truncate cal)))

(: power->string (->* (Real) (Boolean) String))
(define (power->string p [unit-label #f])
  (if (> p 0)
      (format-48 "~F~a" (exact-round p) (if unit-label " watts" ""))
      ""))

(: weight->string (->* (Real) (Boolean) String))
(define (weight->string w [unit-label #f])
  (if (> w 0)
      (format-48 "~1,1F ~a"
                 (m->weight w)
                 (if unit-label weight-label "")) ""))

(: pco->string (->* (Real) (Boolean) String))
(define (pco->string vosc [unit-label #f])
  (format-48 "~1,1F~a"
             (m->vertical-oscillation vosc)
             (if unit-label (string-append " " vertical-oscillation-label) "")))


(: power-phase->string (-> Real Real String))
(define (power-phase->string start end)
  (format-48 "~F° - ~F°" (exact-round start) (exact-round end)))


;;................................................................ other ....

(: n->string (-> Real String))
(define (n->string val)
  (if (= val 0) "" (number->string (exact-round val))))

(: pct->string (-> Real String))
(define (pct->string val)
  (format-48 "~1,1F %" val))
