#lang racket/base

;; dashboard-common.rkt -- common utilities for various dashboard dialogs
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2021 Alex Harsányi <AlexHarsanyi@gmail.com>
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
;;
;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Return information about the session SID from the database.  This is used
;; to retrieve things like the session headline and start time (plus weather
;; information) to be displayed in the FTHR dialog.  Returns #f is not session
;; is found or a hash table with the information about the session.

(require db/base
         pict
         racket/class
         racket/contract
         racket/draw
         racket/format
         racket/gui/base
         racket/match
         racket/math
         "../dbapp.rkt"
         "../fmt-util-ut.rkt"
         "../fmt-util.rkt"
         "../sport-charms.rkt"
         "../weather.rkt")

;; Fetch session information about a session id SID from the database DB and
;; return it as a hash.  Session information include the session name, start
;; time, sport and other summary level information about the session -- the
;; information is intended for displaying a header about the session.
(define (get-session-info sid [db (current-database)])
  (define row
    (query-maybe-row
     db "
select S.start_time,
       S.name,
       S.sport_id,
       S.sub_sport_id,
       SW.temperature,
       SW.dew_point,
       SW.humidity,
       SW.wind_speed,
       SW.wind_gusts,
       SW.wind_direction
  from A_SESSION S left join SESSION_WEATHER SW on SW.session_id = S.id
 where S.id = ?" sid))
  (if row
      (match-let ([(vector start-time headline sport sub-sport
                           temperature dew-point humidity
                           wind-speed wind-gusts wind-direction) row])
        (hash
         'start-time (if (sql-null? start-time) #f start-time)
         'headline (if (sql-null? headline) #f headline)
         'sport-id (if (sql-null? sport) #f sport)
         'sub-sport-id (if (sql-null? sub-sport) #f sub-sport)
         'temperature (if (sql-null? temperature) #f temperature)
         'dew-point (if (sql-null? dew-point) #f dew-point)
         'humidity (if (sql-null? humidity) #f humidity)
         'wind-speed (if (sql-null? wind-speed) #f wind-speed)
         'wind-gusts (if (sql-null? wind-gusts) #f wind-gusts)
         'wind-direction (if (sql-null? wind-direction) #f wind-direction)))
      #f))

;; Pretty print to the current output port, the session info object SINFO, as
;; retrieved by `get-session-info`.
(define (pp-session-info sinfo)
  (printf "Session:    ~a~%Start Time: ~a~%Sport:      ~a~%"
          (hash-ref sinfo 'headline)
          (let ([start-time (hash-ref sinfo 'start-time)])
            (if start-time
                (date-time->string start-time)
                "unknown"))
          (get-sport-name (hash-ref sinfo 'sport-id) (hash-ref sinfo 'sub-sport-id)))
  (let ([temperature (hash-ref sinfo 'temperature)]
        [humidity (hash-ref sinfo 'humidity)]
        [dew-point (hash-ref sinfo 'dew-point)])
    (cond ((and temperature humidity dew-point)
           (printf "Weather:    ~a RH ~a; feels like ~a~%"
                   (temperature->string temperature #t)
                   (humidity->string humidity #t)
                   (temperature->string (humindex temperature dew-point) #t)))
          ((and temperature humidity)
           (printf "Weather:   ~a RH ~a~%"
                   (temperature->string temperature #t)
                   (humidity->string humidity #t)))
          (temperature
           (printf "Weather:    ~a~%" (temperature->string temperature #t)))
          (#t
           (printf "Weather:    no weather data"))))
  (let ([wind-speed (hash-ref sinfo 'wind-speed)]
        [wind-gusts (hash-ref sinfo 'wind-gusts)]
        [wind-direction (hash-ref sinfo 'wind-direction)])
    (cond ((and wind-speed wind-gusts)
           (printf "Wind:       ~a~a~%"
                    (if (zero? wind-speed) "no wind" (wind->string wind-speed wind-direction))
                    (if (zero? wind-gusts) ""
                        (format "; gusts ~a" (wind->string wind-gusts wind-direction)))))
          (wind-speed
           (printf "Wind:       ~a~%"
                   (if (zero? wind-speed) "no wind" (wind->string wind-speed wind-direction)))))))

;; Format session information SINFO (as retrieved by `get-session-info`) into
;; a pict to be displayed in the GUI.
(define (pp-session-info/pict sinfo)
  (define b-item-color (make-object color% #x2f #x4f #x4f))
  (define b-label-color (make-object color% #x77 #x88 #x99))

  (define b-item-font (send the-font-list find-or-create-font 14 'default 'normal 'normal))
  (define b-label-font (send the-font-list find-or-create-font 12 'default 'italic 'light))
  (define b-mini-label-font (send the-font-list find-or-create-font 8 'default 'italic 'light))

  (define b-item-face (cons b-item-color b-item-font))
  (define b-label-face (cons b-label-color b-label-font))
  (define b-mini-label-face (cons b-label-color b-mini-label-font))

  (define b-title-font (send the-font-list find-or-create-font 16 'default 'normal 'normal))
  (define b-title-face (cons b-item-color b-title-font))

  (define temperature (hash-ref sinfo 'temperature))
  (define humidity (hash-ref sinfo 'humidity))
  (define dew-point (hash-ref sinfo 'dew-point))
  (define wind-speed (hash-ref sinfo 'wind-speed))
  (define wind-gusts (hash-ref sinfo 'wind-gusts))
  (define wind-direction (hash-ref sinfo 'wind-direction))
  (define sport-id (hash-ref sinfo 'sport-id))
  (define sub-sport-id (hash-ref sinfo 'sub-sport-id))
  (define headline
    (hbl-append
     5
     (text (~a (hash-ref sinfo 'headline)) b-title-face)
     (text (let ([sport (get-sport-name sport-id sub-sport-id)]
                 [start-time (hash-ref sinfo 'start-time)])
             (if start-time
                 (format "(~a on ~a)" sport (date-time->string start-time))
                 (format "(~a)" sport)))
           b-item-face)))
  (define weather
    (hbl-append
     5
     (text (cond ((and temperature humidity dew-point)
                  (format "~a; ~a RH; feels like ~a~%"
                          (temperature->string temperature #t)
                          (humidity->string humidity #t)
                          (temperature->string (humindex temperature dew-point) #t)))
                 ((and temperature humidity)
                  (format "~a RH ~a~%"
                          (temperature->string temperature #t)
                          (humidity->string humidity #t)))
                 (temperature
                  (format "~a~%" (temperature->string temperature #t)))
                 (#t
                  (format "no weather data")))
           b-item-face)
     (text (cond ((and wind-speed wind-gusts)
                  (format "; ~a~a~%"
                          (if (zero? wind-speed)
                              "no wind"
                              (string-append "wind " (wind->string wind-speed wind-direction)))
                          (if (zero? wind-gusts)
                              ""
                              (format "; gusts ~a" (wind->string wind-gusts wind-direction)))))
                 (wind-speed
                  (format "; ~a~%"
                          (if (zero? wind-speed)
                              "no wind"
                              (string-append "wind " (wind->string wind-speed wind-direction)))))
                 (#t ""))
           b-item-face)))
  (define icon (get-sport-bitmap-colorized sport-id sub-sport-id))
  (hc-append 5 (bitmap icon) (vl-append 5 headline weather)))



;;......................................................... pict-canvas% ....

;; A canvas% holding a pict.  This widget allows adding picts to GUI
;; applications.
(define pict-canvas%
  (class object%
    (init-field parent [alignment '(center center)]
                [horizontal-inset 5]
                [vertical-inset 5]
                [stretchable-width #t]
                [stretchable-height #t])
    (super-new)

    (define the-pict #f)

    (define (on-pict-canvas-paint canvas dc)
      (send dc clear)
      (send dc set-smoothing 'smoothed)
      (when the-pict
        (let-values ([(dc-width dc-height) (send dc get-size)])
          (match-define (list halign valign) alignment)
          (define ox
            (case halign
              ((center centre) (/ (- dc-width (pict-width the-pict)) 2))
              ((left) horizontal-inset)
              ((right) (- dc-width horizontal-inset (pict-width the-pict)))))
          (define oy
            (case valign
              ((center centre) (/ (- dc-height (pict-height the-pict)) 2))
              ((top) vertical-inset)
              ((bottom) (- dc-height vertical-inset (pict-height the-pict)))))
          (draw-pict the-pict dc ox oy))))

    (define the-canvas
      (new canvas% [parent parent]
           [stretchable-height stretchable-height]
           [stretchable-width stretchable-width]
           [paint-callback on-pict-canvas-paint]))

    (define/public (set-pict pict)
      (set! the-pict pict)
      (when pict
        (send* the-canvas
          (min-width
           (+ horizontal-inset horizontal-inset
              (exact-round (pict-width pict))))
          (min-height
           (+ vertical-inset vertical-inset
              (exact-round (pict-height pict))))))
      (send parent reflow-container)
      (send the-canvas refresh))

    ))


;;............................................................. provides ....

;; Note the session information returned by `get-session-info` and printed by
;; `pp-session-info` and `pp-session-info/pict` is a hash table containing
;; various key mappings with the session information. The contract simply
;; specifies `hash?` for this value, which is a bit vague...
(provide/contract
 (get-session-info (->* (exact-positive-integer?) (connection?) (or/c hash? #f)))
 (pp-session-info (-> hash? any/c))
 (pp-session-info/pict (-> hash? pict?)))

(provide pict-canvas%)
