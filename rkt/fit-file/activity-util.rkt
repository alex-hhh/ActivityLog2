#lang racket/base
;; activity-util.rkt -- various utilities for inspecting activity structures
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015, 2020, 2021 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require racket/date
         racket/dict
         racket/math)

;; NOTE: this file is littered with provide calls throughout it.  They should
;; all be moved at the top...

(provide for-each-activity-trackpoint)
(provide map-activity-trackpoints)
(provide for-each-session-trackpoint)
(provide map-session-trackpoints)
(provide for-each-lap-trackpoint)
(provide map-lap-trackpoints)
(provide for-each-session-length)
(provide map-session-lengths)
(provide compute-summary-data)
(provide ->start-time ->seconds make-manual-activity)


;;................................................ map over trackpoinnts ....

(define (for-each-trackpoint fn prev-trackpoint track rest-lengths rest-laps rest-sessions)
  (cond ((pair? track)
         (fn prev-trackpoint (car track))
         (for-each-trackpoint fn (car track) (cdr track) rest-lengths rest-laps rest-sessions))

        ((pair? rest-lengths)
         (let ((new-track (assq 'track (car rest-lengths))))
           (if new-track
               (for-each-trackpoint fn prev-trackpoint (cdr new-track) (cdr rest-lengths) rest-laps rest-sessions)
               (for-each-trackpoint fn prev-trackpoint '() (cdr rest-lengths) rest-laps rest-sessions))))

        ((pair? rest-laps)
         (let ((new-lengths (assq 'lengths (car rest-laps))))
           (if new-lengths
               (for-each-trackpoint fn prev-trackpoint '() (cdr new-lengths) (cdr rest-laps) rest-sessions)
               (for-each-trackpoint fn prev-trackpoint '() '() (cdr rest-laps) rest-sessions))))

        ((pair? rest-sessions)
         (let ((new-lap (assq 'laps (car rest-sessions))))
           (if new-lap
               (for-each-trackpoint fn prev-trackpoint '() '() (cdr new-lap) (cdr rest-sessions))
               (for-each-trackpoint fn prev-trackpoint '() '() '() (cdr rest-sessions)))))

        (#t #t)))

;; apply FN on each (prev, current) trackpoint pair of records in ACTIVITY
(define (for-each-activity-trackpoint activity fn)
  (let ((sessions (assq 'sessions activity)))
    (when sessions
      (for-each-trackpoint fn #f '() '() '() (cdr sessions)))))

(define (map-activity-trackpoints activity fn)
  (let ((result '()))
    (for-each-activity-trackpoint
     activity
     (lambda (prev current)
       (set! result (cons (fn prev current) result))))
    (reverse result)))

(define (for-each-session-trackpoint session fn)
  (for-each-trackpoint fn #f '() '() '() (list session)))

(define (map-session-trackpoints session fn)
  (let ((result '()))
    (for-each-session-trackpoint
     session
     (lambda (prev current)
       (set! result (cons (fn prev current) result))))
    (reverse result)))

(define (for-each-lap-trackpoint lap fn)
  (for-each-trackpoint fn #f '() '() (list lap) '()))

(define (map-lap-trackpoints lap fn)
  (let ((result '()))
    (for-each-lap-trackpoint
     lap
     (lambda (prev current)
       (set! result (cons (fn prev current) result))))
    (reverse result)))


;;...................................................... for each length ....

(define (for-each-length fn lengths rest-laps)
  (cond ((pair? lengths)
         (fn (car lengths))
         (for-each-length fn (cdr lengths) rest-laps))
        ((pair? rest-laps)
         (for-each-length fn (dict-ref (car rest-laps) 'lengths #f) (cdr rest-laps)))
        (#t #t)))

(define (for-each-session-length session fn)
  (for-each-length fn '() (dict-ref session 'laps #f)))

(define (map-session-lengths session fn)
  (let ((result '()))
    (for-each-session-length
     session
     (lambda (l) (set! result (cons (fn l) result))))
    (reverse result)))


;;................................................. compute-summary-data ....

(define (compute-summary-data trackpoints rest-lengths rest-laps rest-sessions)
  (let ((start-time #f)
        (last-timestamp #f)
        (total-timer-time 0)
        (total-distance 0)

        (max-speed 0)

        (total-ascent 0)
        (total-descent 0)

        (total-cycles 0)                ; for computing avg cadence
        (max-cadence 0)

        (total-heart-rate-beats 0)      ; for computing avg heart rate
        (max-heart-rate 0)

        (total-work 0)                  ; for computing avg power
        (max-power 0))

    (for-each-trackpoint
     (lambda (prev current)
       (when current
         (let ((timestamp (dict-ref current 'timestamp #f))
               (distance (dict-ref current 'distance #f))
               (altitude (dict-ref current 'altitude #f))
               (speed (dict-ref current 'speed #f))
               (heart-rate (dict-ref current 'heart-rate #f))
               (cadence (dict-ref current 'cadence #f))
               (power (dict-ref current 'power #f)))

           (when timestamp
             (unless start-time (set! start-time timestamp))
             (set! last-timestamp timestamp))

           ;; NOTE: speed comes form foot-pod or bike/cadence sensor.  we
           ;; compute max-speed from the GPS data.

           (when heart-rate
             (set! max-heart-rate (max heart-rate max-heart-rate)))
           (when cadence
             (set! max-cadence (max cadence max-cadence)))
           (when power
             (set! max-power (max power max-power)))

           (when prev
             (let ((prev-timestamp (dict-ref prev 'timestamp #f))
                   (prev-distance (dict-ref prev 'distance #f))
                   (prev-altitude (dict-ref prev 'altitude #f))
                   (prev-speed (dict-ref prev 'speed #f))
                   (prev-heart-rate (dict-ref prev 'heart-rate #f))
                   (prev-cadence (dict-ref prev 'cadence #f))
                   (prev-power (dict-ref prev 'power #f)))

               ;; NOTE: the activity might not have a distance series.
               (define delta-distance
                 (and prev-distance distance
                      (- distance prev-distance)))

               (when prev-timestamp
                 (let ((delta-time (- timestamp prev-timestamp)))
                   (when (or (not delta-distance) (> delta-distance 0.1))
                     ;; This was not a stop, we can update the accumulated
                     ;; fields
                     (set! total-timer-time (+ total-timer-time delta-time))

                     (when delta-distance
                       (set! total-distance (+ total-distance delta-distance))
                       (let ((instant-speed (if (> delta-time 0) (/ delta-distance delta-time) 0)))
                         (set! max-speed (max max-speed instant-speed))))

                     (when (and altitude prev-altitude)
                       (let ((delta (- altitude prev-altitude)))
                         (if (> delta 0)
                             (set! total-ascent (+ total-ascent delta))
                             (set! total-descent (+ total-descent (- delta))))))

                     (when (and cadence prev-cadence)
                       (let ((avg (/ (+ cadence prev-cadence) 2.0)))
                         (set! total-cycles (+ total-cycles (* delta-time (/ avg 60.0))))))

                     (when (and heart-rate prev-heart-rate)
                       (let ((avg (/ (+ heart-rate prev-heart-rate) 2.0)))
                         (set! total-heart-rate-beats (+ total-heart-rate-beats (* delta-time (/ avg 60.0))))))

                     (when (and power prev-power)
                       (set! total-work (+ total-work (* (/ (+ power prev-power) 2) delta-time))))

                     )))))))
       #f)
     #f trackpoints rest-lengths rest-laps rest-sessions)

    (if start-time
        (filter
         ;; Filter out 0 values, as they have no actual data
         (lambda (x)
           (let ([v (cdr x)])
             (and (number? v) (> v 0))))
         (list
          (cons 'start-time start-time)
          (cons 'total-elapsed-time (- last-timestamp start-time))
          (cons 'total-timer-time total-timer-time)
          (cons 'total-distance total-distance)
          (cons 'total-cycles total-cycles)
          (cons 'avg-speed (if (> total-timer-time 0) (/ total-distance total-timer-time) 0))
          (cons 'max-speed max-speed)
          (cons 'total-ascent total-ascent)
          (cons 'total-descent total-descent)
          (cons 'max-heart-rate max-heart-rate)
          (cons 'avg-heart-rate (* (if (> total-timer-time 0) (/ total-heart-rate-beats total-timer-time) 0) 60.0))
          (cons 'max-cadence max-cadence)
          (cons 'avg-cadence (* (if (> total-timer-time 0) (/ total-cycles total-timer-time) 0) 60.0))
          (cons 'max-power max-power)
          (cons 'avg-power (if (> total-timer-time 0) (/ total-work total-timer-time) 0))))
        '())

    ))



;;................................................. make-manual-activity ....


(define (->start-time min hour)
  (let ((now (current-date)))
    (find-seconds
     0
     min
     hour
     (date-day now)
     (date-month now)
     (date-year now)
     #t)))

(define (->seconds h m s)
  (+ (* h 3600.0 ) (* m 60.0) s))

(define (make-manual-activity start-time name description sport total-time total-distance)
  (list (cons 'start-time start-time)
        (cons 'sessions
              (list
               (list
                (cons 'start-time start-time)
                (cons 'name name)
                (cons 'description description)
                (cons 'total-timer-time total-time)
                (cons 'total-elapsed-time total-time)
                (cons 'total-distance total-distance)
                (cons 'sport sport))))))



;;............................................... Session Data Accessors ....

(provide
 session-start-time
 session-time-zone
 session-time
 session-elapsed-time
 session-moving-time                    ; useful for swim activities only

 session-distance
 session-calories

 session-sport
 session-sub-sport

 session-avg-speed
 session-max-speed

 session-avg-hr
 session-max-hr
 session-aerobic-decoupling
 session-hrv

 session-avg-cadence
 session-avg-vertical-oscillation
 session-avg-stance-time
 session-avg-stance-time-percent
 session-training-effect
 session-training-stress-score
 session-intensity-factor
 session-rpe

 session-max-cadence
 session-total-cycles
 session-avg-stride
 session-avg-vratio

 session-total-ascent
 session-total-descent

 session-pool-length
 session-avg-swolf
 session-swim-stroke
 session-avg-strokes-per-length

 session-avg-power
 session-max-power
 session-normalized-power
 session-avg-torque-effectiveness
 session-avg-left-torque-effectiveness
 session-avg-right-torque-effectiveness
 session-avg-pedal-smoothness
 session-avg-left-pedal-smoothness
 session-avg-right-pedal-smoothness
 session-left-right-balance

 session-laps)

(define (session-start-time session)
  (dict-ref session 'start-time #f))

(define (session-time-zone session)
  (dict-ref session 'time-zone #f))

(define (session-time session)
  (dict-ref session 'total-timer-time #f))

(define (session-elapsed-time session)
  (dict-ref session 'total-elapsed-time #f))

;; Compute the moving time of a (swim) session.  This is done by counting the
;; total-timer-time of all the laps that had a total-distance greater than 0.
;; Garmin Swim uses laps with a distance of 0 to indicate the rest periods.
;;
;; NOTE: this only works meaningfullly for Swim sport types.  For other sports
;; it does not give the same result as Garmin Connect does.
(define (session-moving-time session)
  (let ((mtime 0.0))
    (for-each (lambda (lap)
		(let ((distance (dict-ref lap 'total-distance #f))
                      (time (dict-ref lap 'total-timer-time #f)))
                  (when (and time distance (> distance 0))
                    (set! mtime (+ time mtime)))))
              (dict-ref session 'laps #f))
    mtime))

(define (session-distance session)
  (dict-ref session 'total-distance #f))

(define (session-calories session)
  (dict-ref session 'total-calories #f))

(define (session-sport session)
  (dict-ref session 'sport #f))

(define (session-sub-sport session)
  (dict-ref session 'sub-sport #f))

(define (session-avg-speed session)
  (dict-ref session 'avg-speed #f))

(define (session-max-speed session)
  (dict-ref session 'max-speed #f))

(define (session-avg-hr session)
  (dict-ref session 'avg-heart-rate #f))

(define (session-max-hr session)
  (dict-ref session 'max-heart-rate #f))

(define (session-aerobic-decoupling session)
  (dict-ref session 'aerobic-decoupling #f))

(define (session-hrv session)
  (dict-ref session 'hrv #f))

(define (session-avg-cadence session)
  (dict-ref session 'avg-cadence #f))

(define (session-avg-vertical-oscillation session)
  (dict-ref session 'avg-vertical-oscillation #f))

(define (session-avg-stance-time session)
  (dict-ref session 'avg-stance-time #f))

(define (session-avg-stance-time-percent session)
  (dict-ref session 'avg-stance-time-percent #f))

(define (session-training-effect session)
  (dict-ref session 'total-training-effect #f))

(define (session-training-stress-score session)
  (dict-ref session 'training-stress-score #f))

(define (session-intensity-factor session)
  (dict-ref session 'intensity-factor #f))

(define (session-rpe session)
  (dict-ref session 'rpe-scale #f))

(define (session-max-cadence session)
  (dict-ref session 'max-cadence #f))

(define (session-total-cycles session)
  (dict-ref session 'total-cycles #f))

(define (session-avg-stride session)
  (let ((total-distance (dict-ref session 'total-distance #f))
	(total-cycles (dict-ref session 'total-cycles #f)))
    (if (and total-distance total-cycles (> total-cycles 0))
	(/ total-distance (* 2 total-cycles))
	#f)))

(define (session-avg-vratio session)
  (let ((stride (session-avg-stride session))
        (vosc (session-avg-vertical-oscillation session)))
    (if (and stride vosc (> stride 0)) (* 100.0 (/ vosc (* stride 1000))) #f)))

(define (session-total-ascent session)
  (or 
   (dict-ref session 'total-corrected-ascent #f)
   (dict-ref session 'total-ascent #f)))

(define (session-total-descent session)
  (or
   (dict-ref session 'total-corrected-descent #f)
   (dict-ref session 'total-descent #f)))

(define (session-pool-length session)
  (dict-ref session 'pool-length #f))

(define (session-avg-swolf session)
  (let ((avg-speed (session-avg-speed session))
	(avg-cadence (session-avg-strokes-per-length session))
	(pool-length (session-pool-length session)))
    (if (and avg-speed avg-cadence pool-length (> pool-length 0))
	(let ((avg-seconds-per-length
               (/ 1 (/ avg-speed pool-length))))
          (exact-round (+ avg-seconds-per-length avg-cadence)))
	#f)))

(define (session-swim-stroke session)
  (dict-ref session 'swim-stroke #f))

(define (session-laps session)
  (cdr (assq 'laps session)))

(define (session-avg-strokes-per-length session)
  (let ((total-cycles (session-total-cycles session))
        (num-lengths (foldl (lambda (a b)
                              ;; NOTE: don't add up the rest or drill lenghs
                              (let ((avg-cadence (lap-avg-cadence a)))
                                (+ (if (and avg-cadence (> avg-cadence 0))
                                       (length (lap-lengths a))
                                       0)
                                   b)))
                            0
                            (session-laps session))))
    (if (and total-cycles num-lengths (> num-lengths 0))
        (exact-truncate (/ total-cycles num-lengths))
        #f)))

(define (session-avg-power session)
  (dict-ref session 'avg-power #f))

(define (session-max-power session)
  (dict-ref session 'max-power #f))

(define (session-normalized-power session)
  (dict-ref session 'normalized-power #f))

(define (session-avg-left-torque-effectiveness session)
  (dict-ref session 'avg-left-torque-effectiveness #f))

(define (session-avg-right-torque-effectiveness session)
  (dict-ref session 'avg-right-torque-effectiveness #f))

(define (session-avg-torque-effectiveness session)
  (let ((left (dict-ref session 'avg-left-torque-effectiveness #f))
        (right (dict-ref session 'avg-right-torque-effectiveness #f)))
    (if (and left right)
        (/ (+ left right) 2.0)
        (or left right))))

(define (session-avg-left-pedal-smoothness session)
  (dict-ref session 'avg-left-pedal-smoothness #f))

(define (session-avg-right-pedal-smoothness session)
  (dict-ref session 'avg-right-pedal-smoothness #f))

(define (session-avg-pedal-smoothness session)
  (let ((left (dict-ref session 'avg-left-pedal-smoothness #f))
        (right (dict-ref session 'avg-right-pedal-smoothness #f)))
    (if (and left right)
        (/ (+ left right) 2.0)
        (or left right))))

(define (session-left-right-balance session)
  (dict-ref session 'left-right-balance #f))

(define (session-total-vertical-travel session)
  (let ((ncycles (session-total-cycles session))
        (vosc (session-avg-vertical-oscillation session)))
    (and ncycles vosc (/ (* 2 ncycles vosc) 1000.0))))
(provide session-total-vertical-travel)



;................................................... lap data accessors ....

(provide
 lap-start-time
 lap-time
 lap-elapsed-time

 lap-distance
 lap-calories

 lap-avg-speed
 lap-max-speed

 lap-avg-hr
 lap-max-hr
 lap-aerobic-decoupling

 lap-avg-cadence
 lap-max-cadence
 lap-total-cycles
 lap-avg-stride
 lap-avg-vratio
 lap-avg-vertical-oscillation
 lap-avg-stance-time
 lap-avg-stance-time-percent

 lap-avg-power
 lap-max-power
 lap-normalized-power
 lap-avg-left-torque-effectiveness
 lap-avg-right-torque-effectiveness
 lap-avg-left-pedal-smoothness
 lap-avg-right-pedal-smoothness
 lap-left-right-balance


 lap-total-ascent
 lap-total-descent

 lap-avg-swolf
 lap-best-swolf
 lap-avg-strokes
 lap-swim-stroke

 lap-lengths
 lap-num-lengths)

(define lap-time session-time)
(define lap-start-time session-start-time)
(define lap-elapsed-time session-elapsed-time)

(define lap-distance session-distance)
(define lap-calories session-calories)

(define lap-avg-speed session-avg-speed)
(define lap-max-speed session-max-speed)

(define lap-avg-hr session-avg-hr)
(define lap-max-hr session-max-hr)
(define lap-aerobic-decoupling session-aerobic-decoupling)

(define lap-avg-cadence session-avg-cadence)
(define lap-max-cadence session-max-cadence)
(define lap-total-cycles session-total-cycles)
(define lap-avg-stride session-avg-stride)
(define lap-avg-vratio session-avg-vratio)

(define lap-avg-vertical-oscillation session-avg-vertical-oscillation)
(define lap-avg-stance-time session-avg-stance-time)
(define lap-avg-stance-time-percent session-avg-stance-time-percent)

(define lap-avg-power session-avg-power)
(define lap-max-power session-max-power)
(define lap-normalized-power session-normalized-power)
(define lap-avg-left-torque-effectiveness session-avg-left-torque-effectiveness)
(define lap-avg-right-torque-effectiveness session-avg-right-torque-effectiveness)
(define lap-avg-left-pedal-smoothness session-avg-left-pedal-smoothness)
(define lap-avg-right-pedal-smoothness session-avg-right-pedal-smoothness)
(define lap-left-right-balance session-left-right-balance)

(define lap-total-ascent session-total-ascent)
(define lap-total-descent session-total-descent)

(define lap-swim-stroke session-swim-stroke)

(define (lap-avg-swolf lap)
  (let ((time (lap-time lap))
        (strokes (lap-total-cycles lap))
        (num-lengths (length (lap-lengths lap))))
    (if (and time strokes num-lengths (> num-lengths 0))
        (/ (+ time strokes) num-lengths)
        #f)))

(define (lap-best-swolf lap)
  (let ((candidates
         (filter values (map length-swolf (lap-lengths lap)))))
    (if (> (length candidates) 0)
        (apply min candidates)
        #f)))

(define (lap-avg-strokes lap)
  (let ((total-strokes (lap-total-cycles lap))
        (num-lengths (lap-num-lengths lap)))
    (if (and total-strokes num-lengths (> num-lengths 0))
        (/ total-strokes num-lengths)
        #f)))

(define (lap-lengths lap)
  (dict-ref lap 'lengths '()))

(define (lap-num-lengths lap)
  (length (lap-lengths lap)))


;................................................ length data accessors ....

(provide
 length-time
 length-start-time
 length-track
 length-distance
 length-avg-speed

 length-swim-stroke
 length-total-cycles
 length-avg-cadence
 length-swolf
 length-avg-hr
 length-max-hr)

(define length-time lap-time)
(define length-start-time lap-start-time)
(define length-swim-stroke lap-swim-stroke)
(define length-total-cycles lap-total-cycles)
(define length-avg-speed lap-avg-speed)
(define length-avg-cadence lap-avg-cadence)
(define length-avg-hr lap-avg-hr)
(define length-max-hr lap-max-hr)

(define (length-track length)
  (cond ((assq 'track length) => cdr)
        (#t #f)))

(define (length-distance length)
  (let ((track (length-track length)))
    (if (and track (pair? track))
        (dict-ref (car track) 'distance #f)
        #f)))

(define (length-swolf length)
  (let ((time (length-time length))
        (strokes (length-total-cycles length)))
    (if (and time strokes)
        (+ time strokes)
        0)))


;;............................................ session weather accessors ....

(provide
 session-temperature
 session-dew-point
 session-humidity
 session-wind-speed
 session-wind-gusts
 session-wind-direction
 session-barometric-pressure
 session-weather-source)
 
(define (session-temperature session)
  (let ((w (dict-ref session 'weather #f)))
    (if w (dict-ref w 'temperature #f) #f)))

(define (session-dew-point session)
  (let ((w (dict-ref session 'weather #f)))
    (if w (dict-ref w 'dew-point #f) #f)))
        
(define (session-humidity session)
  (let ((w (dict-ref session 'weather #f)))
    (if w (dict-ref w 'humidity #f) #f)))

(define (session-wind-speed session)
  (let ((w (dict-ref session 'weather #f)))
    (if w (dict-ref w 'wind-speed #f) #f)))

(define (session-wind-gusts session)
  (let ((w (dict-ref session 'weather #f)))
    (if w (dict-ref w 'wind-gusts #f) #f)))

(define (session-wind-direction session)
  (let ((w (dict-ref session 'weather #f)))
    (if w (dict-ref w 'wind-direction #f) #f)))

(define (session-barometric-pressure session)
  (let ((w (dict-ref session 'weather #f)))
    (if w (dict-ref w 'pressure #f) #f)))

(define (session-weather-source session)
  (let ((w (dict-ref session 'weather #f)))
    (if w (dict-ref w 'source #f) #f)))


;;........................................... cycling dynamics accessors ....

(provide
  session-avg-left-pco
  session-avg-right-pco
  session-avg-left-pp-start
  session-avg-left-pp-end
  session-avg-right-pp-start
  session-avg-right-pp-end
  session-avg-left-ppp-start
  session-avg-left-ppp-end
  session-avg-right-ppp-start
  session-avg-right-ppp-end)

(define (session-avg-left-pco session)
  (dict-ref session 'avg-left-pco #f))

(define (session-avg-right-pco session)
  (dict-ref session 'avg-right-pco #f))

(define (session-avg-left-pp-start session)
  (dict-ref session 'avg-left-pp-start #f))

(define (session-avg-left-pp-end session)
  (dict-ref session 'avg-left-pp-end #f))

(define (session-avg-right-pp-start session)
  (dict-ref session 'avg-right-pp-start #f))

(define (session-avg-right-pp-end session)
  (dict-ref session 'avg-right-pp-end #f))

(define (session-avg-left-ppp-start session)
  (dict-ref session 'avg-left-ppp-start #f))

(define (session-avg-left-ppp-end session)
  (dict-ref session 'avg-left-ppp-end #f))

(define (session-avg-right-ppp-start session)
  (dict-ref session 'avg-right-ppp-start #f))

(define (session-avg-right-ppp-end session)
  (dict-ref session 'avg-right-ppp-end #f))

(provide
  lap-avg-left-pco
  lap-avg-right-pco
  lap-avg-left-pp-start
  lap-avg-left-pp-end
  lap-avg-right-pp-start
  lap-avg-right-pp-end
  lap-avg-left-ppp-start
  lap-avg-left-ppp-end
  lap-avg-right-ppp-start
  lap-avg-right-ppp-end)

(define (lap-avg-left-pco lap)
  (dict-ref lap 'avg-left-pco #f))

(define (lap-avg-right-pco lap)
  (dict-ref lap 'avg-right-pco #f))

(define (lap-avg-left-pp-start lap)
  (dict-ref lap 'avg-left-pp-start #f))

(define (lap-avg-left-pp-end lap)
  (dict-ref lap 'avg-left-pp-end #f))

(define (lap-avg-right-pp-start lap)
  (dict-ref lap 'avg-right-pp-start #f))

(define (lap-avg-right-pp-end lap)
  (dict-ref lap 'avg-right-pp-end #f))

(define (lap-avg-left-ppp-start lap)
  (dict-ref lap 'avg-left-ppp-start #f))

(define (lap-avg-left-ppp-end lap)
  (dict-ref lap 'avg-left-ppp-end #f))

(define (lap-avg-right-ppp-start lap)
  (dict-ref lap 'avg-right-ppp-start #f))

(define (lap-avg-right-ppp-end lap)
  (dict-ref lap 'avg-right-ppp-end #f))

