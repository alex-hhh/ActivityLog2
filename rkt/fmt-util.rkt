#lang racket/base
;; fmt-util.rkt -- formatting utilities
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015, 2020-2025 Alex Harsányi <AlexHarsanyi@gmail.com>
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

;;; Commentary:
;;
;; Utility functions to convert values (speeds, pace, durations, etc) to
;; strings.  Internally, activity log uses the metric system to store and
;; manipulate values, these functions are used to display data to the user in
;; the GUI using the appropiate measurement system (metric or statute)

(require "utilities.rkt")

(require racket/format
         racket/math
         racket/string)

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
         vertical-speed->string
         duration->string
         n->string
         pct->string
         stride->string
         vosc->string
         vratio->string
         stance-time->string
         stance-time-pct->string
         stance->string
         cadence->string
         calories->string
         power->string
         work->string
         weight->string
         pco->string
         power-phase->string
         run-pace-string->mps
         swim-pace-string->mps

         pace-label
         swim-pace-label

         degrees->wind-rose)


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
 mm->inch
 celsius->temperature
 km/h->m/s)

;; (: m/s->km/h (-> Real Real))
(define (m/s->km/h speed) (/ (* speed 3600.0) 1000.0))

;; (: km/h->m/s (-> Real Real))
(define (km/h->m/s speed) (/ (* speed 1000.0) 3600.0))

;; (: m/s->mi/h (-> Real Real))
(define (m/s->mi/h speed) (/ (* speed 3600.0) 1609.0))

;; (: mi/h->m/s (-> Real Real))
(define (mi/h->m/s speed) (/ (* speed 1609.0) 3600.0))

;; (: m/s->sec/km (-> Positive-Real Real))
(define (m/s->sec/km speed) (/ 1000.0 speed))

;; (: sec/km->m/s (-> Positive-Real Real))
(define (sec/km->m/s pace) (/ 1000.0 pace))

;; (: m/s->sec/mi (-> Positive-Real Real))
(define (m/s->sec/mi speed) (/ 1609.0 speed))

;; (: sec/mi->m/s (-> Positive-Real Real))
(define (sec/mi->m/s pace) (/ 1609.0 pace))

;; (: m/s->sec/100m (-> Positive-Real Real))
(define (m/s->sec/100m speed) (/ 100.0 speed))

;; (: sec/100m->m/s (-> Positive-Real Real))
(define (sec/100m->m/s pace) (/ 100.0 pace))

;; (: m/s->sec/100yd (-> Positive-Real Real))
(define (m/s->sec/100yd speed) (/ 91.44 speed))

;; (: sec/100yd->m/s (-> Positive-Real Real))
(define (sec/100yd->m/s pace) (/ 91.44 pace))

;; (: m->km (-> Real Real))
(define (m->km m) (/ m 1000))

;; (: m->mi (-> Real Real))
(define (m->mi m) (/ m 1609))

;; (: m->yd (-> Real Real))
(define (m->yd m) (/ m 0.9144))

;; (: m->ft (-> Real Real))
(define (m->ft m) (* m 3.2808))

(define (ft->m m) (/ m 3.2808))

;; (: celsius->fahrenheit (-> Real Real))
(define (celsius->fahrenheit c) (+ 32 (* c 1.8)))

;; (: mm->inch (-> Real Real))
(define (mm->inch mm) (* mm 0.0393700787))

;; (: kg->lb (-> Real Real))
(define (kg->lb kg) (* kg 2.20462262))

;; The actual converter function, used by the formatters.  They are set up by
;; `setup-measurement-system' based on the value of `measurement-system'

(define m/s->speed m/s->km/h)
(define speed->m/s km/h->m/s)
(define speed-label "km/h")
(define m/s->pace m/s->sec/km)
(define pace->m/s sec/km->m/s)
(define pace-label "min/km")
(define m/s->swim-pace m/s->sec/100m)
(define swim-pace->m/s sec/100m->m/s)
(define swim-pace-label "min/100m")
(define celsius->temperature celsius->fahrenheit) ; !!!
(define temperature-label "℃")
(define m->distance m->km)
(define distance-label "km")
(define m->short-distance m->yd)        ; !!!
(define short-distance-label "yd")
(define m->vertical-distance m->ft)
(define vertical-distance-label "ft")
(define m->vertical-speed m->ft)
(define vertical-speed-label "m/h")
(define m->vertical-oscillation mm->inch) ; !!!
(define vertical-oscillation-label "in")
(define m->weight kg->lb)
(define weight-label "kg")
(define vertical-distance->m values)

;; Export converters as functions, we cannot export the defines above
;; directly, because any code that uses them will not pick up any changes.
(define (convert-m/s->speed val) (m/s->speed val))
(define (convert-speed->m/s val) (speed->m/s val))
(define (convert-m/s->pace val) (m/s->pace val))
(define (convert-pace->m/s val) (pace->m/s val))
(define (convert-swim-pace->m/s val) (swim-pace->m/s val))
(define (convert-m/s->swim-pace val) (m/s->swim-pace val))
(define (convert-vertical-distance->m val) (vertical-distance->m val))
(provide convert-m/s->speed
         convert-speed->m/s
         convert-m/s->pace
         convert-pace->m/s
         convert-swim-pace->m/s
         convert-m/s->swim-pace
         convert-vertical-distance->m)

;; (: setup-measurement-system (-> Symbol Void))
(define (setup-measurement-system mu)
  (if (eq? mu 'statute)
      (begin
        (set! m/s->speed m/s->mi/h)
        (set! speed->m/s mi/h->m/s)
        (set! speed-label "mi/h")
        (set! m/s->pace m/s->sec/mi)
        (set! pace->m/s sec/mi->m/s)
        (set! pace-label "min/mi")
        (set! m/s->swim-pace m/s->sec/100m)
        (set! swim-pace->m/s sec/100m->m/s)
        (set! swim-pace-label "min/100yd")
        (set! celsius->temperature celsius->fahrenheit)
        (set! temperature-label "°F")
        (set! m->distance m->mi)
        (set! distance-label "mi")
        (set! m->short-distance m->yd)
        (set! short-distance-label "yd")
        (set! m->vertical-distance m->ft)
        (set! vertical-distance-label "ft")
        (set! m->vertical-speed m->ft)
        (set! vertical-speed-label "ft/h")
        (set! m->vertical-oscillation mm->inch)
        (set! vertical-oscillation-label "in")
        (set! m->weight kg->lb)
        (set! weight-label "lb")
        (set! vertical-distance->m ft->m))

      (begin
        (set! m/s->speed m/s->km/h)
        (set! speed->m/s km/h->m/s)
        (set! speed-label "km/h")
        (set! m/s->pace m/s->sec/km)
        (set! pace->m/s sec/km->m/s)
        (set! pace-label "min/km")
        (set! m/s->swim-pace m/s->sec/100m)
        (set! swim-pace->m/s sec/100m->m/s)
        (set! swim-pace-label "min/100m")
        (set! celsius->temperature values)
        (set! temperature-label "°C")
        (set! m->distance m->km)
        (set! distance-label "km")
        (set! m->short-distance values)
        (set! short-distance-label "m")
        (set! m->vertical-distance values)
        (set! vertical-distance-label "m")
        (set! m->vertical-speed values)
        (set! vertical-speed-label "m/h")
        (set! m->vertical-oscillation values)
        (set! vertical-oscillation-label "mm")
        (set! m->weight values)
        (set! weight-label "kg")
        (set! vertical-distance->m values)
        )))

(define ms-tag 'activity-log:measurement-system)

;; (: ms-val (U 'metric 'statute))
(define ms-val
  (let ((v (get-pref ms-tag (lambda () 'metric))))
    (if (or (eq? v 'metric) (eq? v 'statute))
        v
        'metric)))

;; (: al-pref-measurement-system (-> (U 'metric 'statute)))
(define (al-pref-measurement-system)
  ms-val)
;; (: set-al-pref-measurement-system (-> (U 'metric 'statute) Any))
(define (set-al-pref-measurement-system val)
  (unless (eq? val ms-val)
    (put-pref ms-tag val)
    (set! ms-val val)
    (setup-measurement-system ms-val)
    (log-event 'measurement-system-changed ms-val)))
(provide al-pref-measurement-system set-al-pref-measurement-system)

(setup-measurement-system ms-val)



;;......................................................... weather data ....

(define wind-rose
  #("NNE" "NE" "NEE" "E" "ESE" "SE" "SSE" "S" "SSW" "SW" "WSW" "W" "WNW" "NW" "NNW" "N"))

(define (degrees->wind-rose deg)
  (let* ((nslices (vector-length wind-rose))
         (slice (/ 360.0 nslices))
         (adjusted-deg
          (modulo (exact-round (- deg (/ slice 2))) 360)))
    (vector-ref wind-rose (exact-truncate (/ adjusted-deg slice)))))

;; NOTE: SPEED is in meters/second
;; (: wind->string (-> Real Real String))
(define (wind->string speed direction)
  (if (> speed 0)
      (format "~a ~a ~a" (~r (m/s->speed speed) #:precision 1) speed-label (degrees->wind-rose direction))
      ""))

;; NOTE: DEGC is always in degrees celsius
;; (: temperature->string (->* (Real) (Boolean) String))
(define (temperature->string degc [unit-label #f])
  (if (> degc -1000)
      (string-append
       (~r (celsius->temperature degc) #:precision 1)
       (if unit-label (string-append " " temperature-label) ""))
      ""))

;; NOTE: humidity is between 0 and 100
;; (: humidity->string (->* (Real) (Boolean) String))
(define (humidity->string hum [unit-label #f])
  (if (and (> hum 0) (<= hum 100))
      (string-append
       (~r hum #:precision 0)
       (if unit-label " %" ""))
      ""))

;; (: pressure->string (->* (Real) (Boolean) String))
(define (pressure->string p [unit-label #f])
  (if (> p 0)
      (string-append
       (~r p #:precision 1)
       (if unit-label " hPa" ""))
      ""))


;;..................................................... speeds and paces ....

;; (: speed->string (->* (Real) (Boolean) String))
(define (speed->string speed/mps [unit-label #f])
  (let ((speed (m/s->speed speed/mps)))
    (string-append
     (~r speed #:precision '(= 1))
     (if unit-label (string-append " " speed-label) ""))))

;; (: pace->string (->* (Real) (Boolean) String))
(define (pace->string speed/mps [unit-label #f])
  ;; NOTE: not sure if we should just force a Positive-Real on the SPEED/MPS
  ;; and not return an empty string...
  (if (> speed/mps 0)
      (let* ((sec/km (exact-round (m/s->pace speed/mps)))
             (min (exact-truncate (/ sec/km 60.0)))
             (sec (exact-round (- sec/km (* min 60.0)))))
        (string-append
         (~r min #:precision 0 #:min-width 2 #:pad-string "0")
         ":"
         (~r sec #:precision 0 #:min-width 2 #:pad-string "0")
         (if unit-label (string-append " " pace-label) "")))
      ""))

;; (: swim-pace->string (->* (Real) (Boolean) String))
(define (swim-pace->string speed/mps [unit-label #f])
  (if (> speed/mps 0)
      (let* ((sec/100m (exact-round (m/s->swim-pace speed/mps)))
             (min (exact-truncate (/ sec/100m 60.0)))
             (sec (exact-truncate (- sec/100m (* min 60.0)))))
        (string-append
         (~r min #:precision 0 #:min-width 2 #:pad-string "0")
         ":"
         (~r sec #:precision 0 #:min-width 2 #:pad-string "0")
         (if unit-label (string-append " " swim-pace-label) "")))
      ""))


;;.............................................. distances and durations ....

;; (: distance->string (->* (Real) (Boolean) String))
(define (distance->string distance/m [unit-label #f])
  (string-append
   (~r (m->distance distance/m) #:precision 2)
   (if unit-label (string-append " " distance-label) "")))

;; (: short-distance->string (->* (Real) (Boolean) String))
(define (short-distance->string distance/m [unit-label #f])
  (string-append
   (~r (m->short-distance distance/m) #:precision 0)
   (if unit-label (string-append " " short-distance-label)  "")))

;; (: vertical-distance->string (->* (Real) (Boolean) String))
(define (vertical-distance->string distance/m [unit-label #f])
  (if (rational? distance/m)
      (string-append
       (~r (m->vertical-distance distance/m) #:precision '(= 1))
       (if unit-label (string-append " " vertical-distance-label)  ""))
      (~a distance/m)))

(define (vertical-speed->string speed/m [unit-label #f])
  (if (rational? speed/m)
      (string-append
       (~r (m->vertical-speed speed/m) #:precision 0)
       (if unit-label (string-append " " vertical-speed-label)  ""))
      (~a speed/m)))

;; (: duration->string (->* (Real) (Boolean) String))
(define (duration->string duration [high-precision? #f])
  (define-values (h m s ms)
    (let ([seconds (exact-round (* duration 10))])
      (let-values ([(h m+s) (quotient/remainder seconds 36000)])
        (let-values ([(m s+ms) (quotient/remainder m+s 600)])
          (let-values ([(s ms) (quotient/remainder s+ms 10)])
            (values h
                    m
                    (cond (high-precision? s)
                          ((>= ms 5) (add1 s))
                          (else s))
                    (if high-precision? ms 0)))))))
  (if high-precision?
      (if (> h 0)
          (string-append
           (~r h #:precision 0 #:min-width 2 #:pad-string "0")
           ":"
           (~r m #:precision 0 #:min-width 2 #:pad-string "0")
           ":"
           (~r s #:precision 0 #:min-width 2 #:pad-string "0")
           "."
           (~r ms #:precision 0))
          (string-append
           (~r m #:precision 0 #:min-width 2 #:pad-string "0")
           ":"
           (~r s #:precision 0 #:min-width 2 #:pad-string "0")
           "."
           (~r ms #:precision 0)))
      (if (> h 0)
          (string-append
           (~r h #:precision 0 #:min-width 2 #:pad-string "0")
           ":"
           (~r m #:precision 0 #:min-width 2 #:pad-string "0")
           ":"
           (~r s #:precision 0 #:min-width 2 #:pad-string "0"))
          (string-append
           (~r m #:precision 0 #:min-width 2 #:pad-string "0")
           ":"
           (~r s #:precision 0 #:min-width 2 #:pad-string "0")))))


;;................................................... cadence and stride ....

;; (: stride->string (->* (Real) (Boolean) String))
(define (stride->string stride [unit-label #f])
  (if (> stride 0)
      (string-append
       (~r (m->short-distance stride) #:precision 2)
       (if unit-label (string-append " " short-distance-label) ""))
      ""))

;; (: vosc->string (->* (Real) (Boolean) String))
(define (vosc->string vosc [unit-label #f])
  (if (> vosc 0)
      (string-append
       (~r (m->vertical-oscillation vosc) #:precision 1)
       (if unit-label (string-append " " vertical-oscillation-label) ""))
      ""))

;; (: vratio->string (->* (Real) (Boolean) String))
(define (vratio->string vratio [unit-label #f])
  (if (> vratio 0)
      (string-append
       (~r vratio #:precision 1)
       (if unit-label " %" ""))
      ""))

;; (: stance-time->string (->* (Real) (Boolean) String))
(define (stance-time->string stime [unit-label #f])
  (if (> stime 0)
      (string-append
       (~r stime #:precision 0)
       (if unit-label " ms" ""))
      ""))

;; (: stance-time-pct->string (->* (Real) (Boolean) String))
(define (stance-time-pct->string pct [unit-label #f])
  (if (> pct 0)
      (string-append
       (~r pct #:precision 1)
       (if unit-label " %" ""))
      ""))

;; (: stance->string (-> Real Real String))
(define (stance->string stime pct)
  (if (and (> pct 0) (> stime 0))
      (format "~a ms (~a %)" (~r stime #:precision 0) (~r pct #:precision 0))
      ""))

;; (: cadence->string (->* (Real (U Symbol Nonnegative-Integer)) (Boolean) String))
(define (cadence->string cadence sport [unit-label #f])
  (if (> cadence 0)
      (string-append
       (~r cadence #:precision 0)
       " "
       (case sport
         ((running 1) "SPM")
         ((biking 2) "RPM")
         ((swimming 5) "strokes/min")
         (else "SPM")))
      ""))


;;.............................................. heart rate and calories ....

;; (: calories->string (-> Real String))
(define (calories->string cal)
  (string-append
   (~r cal #:precision 0)
   " KCal"))

;; (: power->string (->* (Real) (Boolean) String))
(define (power->string p [unit-label #f])
  (if (> p 0)
      (string-append
       (~r p #:precision 0)
       (if unit-label " watts" ""))
      ""))

;; (: work->string (->* (Real) (Boolean) String))
(define (work->string w [unit-label #f])
  (if (> w 0)
      (format "~a~a" (~r (/ w 1000.0) #:precision 1) (if unit-label " kJ" ""))
      ""))

;; (: weight->string (->* (Real) (Boolean) String))
(define (weight->string w [unit-label #f])
  (if (> w 0)
      (string-append
       (~r (m->weight w) #:precision 1)
       (if unit-label (string-append " " weight-label) ""))
      ""))

;; (: pco->string (->* (Real) (Boolean) String))
(define (pco->string vosc [unit-label #f])
  (string-append
   (~r (m->vertical-oscillation vosc) #:precision 1)
   (if unit-label (string-append " " vertical-oscillation-label) "")))


;; (: power-phase->string (-> Real Real String))
(define (power-phase->string start end)
  (format "~a° - ~a°" (~r start #:precision 0) (~r end #:precision 0)))


;;................................................................ other ....

;; (: n->string (-> Real String))
(define (n->string val)
  (if (= val 0) "" (number->string (exact-round val))))

;; (: pct->string (-> Real String))
(define (pct->string val)
  (string-append (~r val #:precision 1) " %"))


;.............................................................. readers ....

;; Convert a string in the format "mm:ss" into a number of seconds
;; (: str->seconds (-> String (U 'empty #f Real)))
(define (str->seconds data)
  (let ((t (string-trim data)))
    (cond ((= (string-length t) 0) 'empty)
          ((regexp-match "^([0-9]+):([0-9]+)$" t) =>
           (lambda (m)
             (let* ((mstr (list-ref (cdr m) 0))
                    (sstr (list-ref (cdr m) 1))
                    (minutes (if (string? mstr) (string->number mstr) #f))
                    (seconds (if (string? sstr) (string->number sstr) #f)))
               (if (and minutes seconds (< (real-part minutes) 60) (< (real-part seconds) 60))
                   (exact->inexact (+ (* (real-part minutes) 60) (real-part seconds)))
                   #f))))
          (#t #f))))

;; Convert a pace value (mm:ss/km or mm:ss/mile) into meters per second
;; (: run-pace-string->mps (-> String (U #f Real)))
(define (run-pace-string->mps str)
  (let ((seconds (str->seconds str)))
    (if (and (real? seconds) (positive? seconds))
        (pace->m/s seconds)
        #f)))

;; Convert a swim pace value (mm:ss/100m or mm:ss/100yd) into meters per
;; second
;; (: swim-pace-string->mps (-> String (U #f Real)))
(define (swim-pace-string->mps str)
  (let ((seconds (str->seconds str)))
    (if (and (real? seconds) (positive? seconds))
        (swim-pace->m/s seconds)
        #f)))
