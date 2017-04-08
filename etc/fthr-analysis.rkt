#lang racket
(require "al-interactive.rkt")
(require "../rkt/sport-charms.rkt")
(require "../rkt/weather.rkt")
(require math/statistics)

(define session-info-sql
  "select S.start_time,
       S.name,
       SW.temperature,
       SW.dew_point,
       SW.humidity,
       SW.wind_speed,
       SW.wind_gusts,
       SW.wind_direction
  from A_SESSION S left join SESSION_WEATHER SW on SW.session_id = S.id
 where S.id = ?")

(define (pp-session-info df)
  (define sid (send df get-property 'session-id))
  (define sport (send df get-property 'sport))
  (match-define
    (vector start-time name temperature dew-point humidity wind-speed wind-gusts wind-direction)
    (query-row (current-database) session-info-sql sid))
  (printf "Session:    ~a~%Start Time: ~a~%Sport:      ~a~%"
          name
          (date-time->string start-time)
          (get-sport-name (vector-ref sport 0) (vector-ref sport 1)))
  (printf "Weather:    ~a RH ~a; feels like ~a~%"
          (temperature->string temperature #t)
          (humidity->string humidity #t)
          (temperature->string (humindex temperature dew-point) #t))
  (printf "Wind:       ~a~a~%"
          (if (zero? wind-speed) "no wind" (wind->string wind-speed wind-direction))
          (if (zero? wind-gusts) ""
              (format "; gusts ~a" (wind->string wind-gusts wind-direction)))))

(define (pp-hr-zones fthr)
  (printf "Heart Rate Zones:~%")
  (printf "Z1 start: ~a~%" (exact-round (* 0.80 fthr)))
  (printf "Z2 start: ~a~%" (exact-round (* 0.85 fthr)))
  (printf "Z3 start: ~a~%" (exact-round (* 0.90 fthr)))
  (printf "Z4 start: ~a~%" (exact-round (* 0.95 fthr)))
  (printf "Z5 start: ~a~%" (exact-round (* 1.00 fthr)))
  (newline))

(define (pp-pace-zones thspd)
  (printf "Pace zones:~%")
  (printf "Z1  start: ~a~%" (pace->string (* 0.60000 thspd) #t)) ; some low value
  (printf "Z2  start: ~a~%" (pace->string (* 0.77519 thspd) #t)) ; 129% of TPACE
  (printf "Z3  start: ~a~%" (pace->string (* 0.88496 thspd) #t)) ; 113% of TPACE
  (printf "Z4  start: ~a~%" (pace->string (* 0.95238 thspd) #t)) ; 105% of TPACE
  (printf "Z5a start: ~a~%" (pace->string (* 1.00000 thspd) #t)) ; 100% of TPACE
  (printf "Z5b start: ~a~%" (pace->string (* 1.04167 thspd) #t)) ; 96% of TPACE
  (printf "Z5c start: ~a~%" (pace->string (* 1.11111 thspd) #t)) ; 90% of TPACE
  (newline))

;; https://www.trainingpeaks.com/blog/joe-friel-s-quick-guide-to-setting-zones/
(define (fthr/run sid)
  (define df (sid->df sid))
  (unless (send df contains? "hr" "timer" "spd")
    (error "series missing required data (hr, dst, spd)"))
  (pp-session-info df)
  ;; HR is the best 20 min of the session
  (define hr-best (df-best-avg df "hr" #:weight-column "timer" #:durations (list (* 20 60))))
  (when (null? hr-best)
    (error "session is too short"))
  (match-define (vector duration hr pos) (car hr-best))
  ;; SPD is the best 30 min of the session
  (define spd-aux (df-best-avg df "spd" #:weight-column "timer" #:durations (list (* 30 60))))
  (match-define (vector duration2 spd pos2) (car spd-aux))
  (printf "Best 20min: HR ~a; @~a~%"
          (heart-rate->string/bpm hr)
          (duration->string pos))
  (printf "Best 30min: Pace ~a; @~a~%"
          (pace->string spd #t)
          (duration->string pos2))
  (match-define (list start mid end)
    (send df get-index* "timer" pos2 (+ pos2 (* 0.5 duration2)) (+ pos2 duration2)))
  (define stats-first-half (df-statistics df "spd" #:start start #:end mid))
  (define stats-second-half (df-statistics df "spd" #:start mid #:end end))
  (let* ((spd1 (statistics-mean stats-first-half))
         (spd2 (statistics-mean stats-second-half))
         (split (* 100.0 (sub1 (/ spd1 spd2)))))
    (printf "Pace Split: ~a; ~a vs. ~a~%"
            (pct->string split)
            (pace->string spd1 #t) (pace->string spd2 #t)))
  (pp-hr-zones hr)
  (pp-pace-zones spd))

