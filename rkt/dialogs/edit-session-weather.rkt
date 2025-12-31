#lang racket/base
;; edit-session-weather.rkt -- edit weather data for a session
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015, 2019-2020, 2023, 2025 Alex Harsányi <AlexHarsanyi@gmail.com>
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
         gui-widget-mixins
         racket/class
         racket/format
         racket/gui/base
         "../dbutil.rkt"
         "../fmt-util-ut.rkt"
         "../fmt-util.rkt"
         "../utilities.rkt"
         "../weather.rkt"
         "../widgets/main.rkt")

(provide get-weather-editor)


;;........................................................ weather-edit% ....

(define min-text-field-width 120)
(define min-wind-speed 0)
(define max-wind-speed 100)              ; km/h

(define (validate-positive-rational v)
  (define n (string->number v))
  (and n (rational? n) (> n 0) n))

(define (validate-non-negative-rational v)
  (define n (string->number v))
  (and n (rational? n) (>= n 0) n))

(define ((validate-rational-between minimum maximum) v)
  (define n (string->number v))
  (and n (rational? n) (>= n minimum) (<= n maximum) n))

(define wind-rose
  (list "N" "NNE" "NE" "NEE" "E" "ESE" "SE" "SSE" "S"
        "SSW" "SW" "WSW" "W" "WNW" "NW" "NNW"))

(define weather-edit%
  (class edit-dialog-base%
    (init)
    (super-new [title "Session Weather"] [icon (weather-icon)])

    (define db #f)
    (define sid #f)

    ;; The start time of the activtity, used to determine the weather
    ;; obesrvation timestamp.
    (define start-time (current-seconds))

    (define wstation "")

    (define message-font
      (send the-font-list find-or-create-font 12 'default 'normal 'normal))

    (define gui-controls (make-hash))

    (define/private (value-of control-name)
      (define control (hash-ref gui-controls control-name))
      (cond ((is-a? control text-field%)
             (send control get-value/validated))
            ((is-a? control check-box%)
             (send control get-value))
            ((is-a? control slider%)
             (send control get-value))
            ((is-a? control choice%)
             (send control get-selection))
            (#t
             (error "value-of: unknown control type"))))

    (define/private (put-value control-name value)
      (define control (hash-ref gui-controls control-name))
      (cond ((is-a? control text-field%)
             (send control set-value value))
            ((is-a? control choice%)
             (send control set-selection value))
            ((is-a? control message%)
             (send control set-label value))
            ((is-a? control check-box%)
             (send control set-value value))
            ((is-a? control slider%)
             (send control set-value value))
            (#t
             (error "put-value: unknown control type"))))

    (let ((p (send this get-client-pane)))

      (let ((hp (make-horizontal-pane p #f)))
        (new message%
             [parent hp]
             [label "Activity: "]
             [stretchable-width #f])
        (hash-set!
         gui-controls
         'activity-headline
         (new message%
              [parent hp]
              [label "Untitled"]
              [font message-font]
              [stretchable-width #t])))
      (let ((hp (make-horizontal-pane p #f)))
        (new message%
             [parent hp]
             [label "Start time: "]
             [stretchable-width #f])
        (hash-set!
         gui-controls
         'activity-start-time
         (new message%
              [parent hp]
              [label "Untitled"]
              [font message-font]
              [stretchable-width #t])))

      (define contents-gb (make-group-box-panel p))

      (let ((hp (make-horizontal-pane contents-gb #f)))
        (hash-set!
         gui-controls
         'temperature
         (new (validate-mixin
               validate-non-negative-rational
               (lambda (v) (~r v #:precision 1))
               (decorate-mixin
                (decorate-with "℃" #:validate validate-non-negative-rational)
                (cue-mixin
                 "℃"
                 (tooltip-mixin text-field%))))
              [parent hp]
              [min-width min-text-field-width]
              [allow-empty? #t]
              [tooltip "Temperature"]
              [label "Temperature: "]
              [stretchable-width #f]))

        (hash-set!
         gui-controls
         'humidity
         (new (validate-mixin
               (validate-rational-between 0 100)
               (lambda (v) (~r v #:precision 3))
               (decorate-mixin
                (decorate-with "%" #:validate (validate-rational-between 0 100))
                (cue-mixin
                 "%"
                 (tooltip-mixin text-field%))))
              [parent hp]
              [allow-empty? #t]
              [min-width min-text-field-width]
              [tooltip "Relative Humidity"]
              [label "Humidity: "]
              [stretchable-width #f]))

        (hash-set!
         gui-controls
         'dew-point
         (new (validate-mixin
               validate-non-negative-rational
               (lambda (v) (~r v #:precision 1))
               (decorate-mixin
                (decorate-with "℃" #:validate validate-non-negative-rational)
                (cue-mixin
                 "℃"
                 (tooltip-mixin text-field%))))
              [parent hp]
              [allow-empty? #t]
              [min-width min-text-field-width]
              [tooltip "Temperature"]
              [label "Temperature: "]
              [stretchable-width #f])))

      (let ((hp (make-horizontal-pane contents-gb #f)))
        (hash-set!
         gui-controls
         'wind-speed
         (new (validate-mixin
               (validate-rational-between min-wind-speed max-wind-speed)
               (lambda (v) (~r v #:precision 2))
               (decorate-mixin
                (decorate-with "km/h" #:validate validate-non-negative-rational)
                (cue-mixin "km/h" text-field%)))
              [parent hp]
              [label "Wind Speed: "]
              [allow-empty? #t]
              [min-width min-text-field-width]
              [stretchable-width #f]))
        (hash-set!
         gui-controls
         'wind-gusts
         (new (validate-mixin
               (validate-rational-between min-wind-speed max-wind-speed)
               (lambda (v) (~r v #:precision 2))
               (decorate-mixin
                (decorate-with "km/h" #:validate validate-non-negative-rational)
                (cue-mixin "km/h" text-field%)))
              [parent hp]
              [label "Wind Gusts: "]
              [allow-empty? #t]
              [min-width min-text-field-width]
              [stretchable-width #f]))

        (hash-set!
         gui-controls
         'wind-direction
         (new choice%
              [parent hp]
              [label "Direction: "]
              [choices wind-rose])))

      (let ((hp (make-horizontal-pane contents-gb #f)))
        (hash-set!
       gui-controls
       'pressure
       (new (validate-mixin
             validate-positive-rational
             (lambda (v) (~r v #:precision 3))
             (decorate-mixin
              (decorate-with "hPa" #:validate validate-positive-rational)
              (cue-mixin
               "hPa"
               (tooltip-mixin text-field%))))
            [parent hp]
            [label ""]
            [allow-empty? #t]
            [min-width min-text-field-width]
            [tooltip "Barometric Pressure (sea level) in hPa"]
            [stretchable-width #f]))))

    (define (set-wind-direction wdir)
      (if wdir
          (let ((label (degrees->wind-rose wdir)))
            (let loop ((idx 0) (labels wind-rose))
              (unless (null? labels)
                (if (equal? label (car labels))
                    (put-value 'wind-direction idx)
                    (loop (+ idx 1) (cdr labels))))))
          (put-value 'wind-direction 0)))

    (define (get-wind-direction)
      ;; this is a hack :-)
      (* (/ 360 (length wind-rose)) (value-of 'wind-direction)))

    ;; Setup values in the weather fields, based on data in WOBS.  If
    ;; UNIT-LABEL is #t, the values will have unit labels (temperature, speed,
    ;; etc)
    (define (setup-weather-fields wobs)
      (when (wobs-ts wobs)
        (set! start-time (wobs-ts wobs)))
      (put-value
       'temperature
       (if (wobs-temp wobs)
           (temperature->string (wobs-temp wobs) #f)
           ""))
      (put-value
       'dew-point
       (if (wobs-dewp wobs)
           (temperature->string (wobs-dewp wobs) #f)
           ""))
      (put-value
       'humidity
       (if (wobs-hum wobs)
           (humidity->string (wobs-hum wobs) #f)
           ""))
      (put-value
       'wind-speed
       (if (wobs-wspd wobs)
           (speed->string (wobs-wspd wobs) #f)
           ""))
      (put-value
       'wind-gusts
       (if (wobs-wgust wobs)
           (speed->string (wobs-wgust wobs) #f)
           ""))
      (set-wind-direction (wobs-wdir wobs))
      (put-value
       'pressure
       (if (wobs-pressure wobs)
           (pressure->string (wobs-pressure wobs) #f)
           "")))

    (define (clear-weather-fields)
      (put-value 'temperature "")
      (put-value 'dew-point "")
      (put-value 'humidity "")
      (put-value 'wind-speed "")
      (put-value 'wind-gusts "")
      (set-wind-direction 0)
      (put-value 'pressure ""))

    (define (setup-activity-info db sport-charms sid)
      (let ((row (query-row db "
select name, sport_id, sub_sport_id, start_time from A_SESSION where id = ?"
                            sid)))
        (let ((headline (format "~a (~a)"
                                (sql-column-ref row 0 "Untitled")
                                (send sport-charms get-sport-name
                                      (sql-column-ref row 1 #f)
                                      (sql-column-ref row 2 #f)))))
          (put-value 'activity-headline headline))
        (set! start-time (sql-column-ref row 3 0))
        (put-value 'activity-start-time (date-time->string start-time))))

    ;; NOTE: multiple weather records can be present for a session, we select
    ;; and allow editing only of the first one.
    (define (setup-activity-weather db sid)
      (let ((rows (query-rows db "
select wstation,
       timestamp,
       temperature,
       dew_point,
       humidity,
       wind_speed,
       wind_gusts,
       wind_direction,
       pressure
from SESSION_WEATHER where session_id =?
order by timestamp" sid)))
        (unless (null? rows)
          (let* ([row (car rows)]
                 [wo (wobs (sql-column-ref row 1)
                           (sql-column-ref row 2)
                           (sql-column-ref row 3)
                           (sql-column-ref row 4)
                           (sql-column-ref row 5)
                           (sql-column-ref row 6)
                           (sql-column-ref row 7)
                           (sql-column-ref row 8))])
            (set! wstation (sql-column-ref row 0 ""))
            (setup-weather-fields wo)))))

    (define (save-weather-data database sid)
      (define (->sql v)
        (if (rational? v) v #f))
      (let ((wo (wobs start-time
                      (->sql (value-of 'temperature))
                      (->sql (value-of 'dew-point))
                      (->sql (value-of 'humidity))
                      (->sql (value-of 'wind-speed))
                      (->sql (value-of 'wind-gusts))
                      (get-wind-direction)
                      (->sql (value-of 'pressure)))))
        (update-session-weather database sid wstation wo)))

    (define/override (has-valid-data?)
      (define (valid? v)
        (or (rational? v) (equal? v 'empty)))
      (and (valid? (value-of 'temperature))
           (valid? (value-of 'dew-point))
           (valid? (value-of 'humidity))
           (valid? (value-of 'wind-speed))
           (valid? (value-of 'wind-gusts))
           (valid? (value-of 'pressure))))

    (define/public (begin-edit parent database sport-charms session-id)
      (clear-weather-fields)
      (set! db database)
      (set! sid session-id)

      (with-busy-cursor
        (lambda ()
          (setup-activity-info database sport-charms sid)
          ;; Retrieve previous weather data, if any, for this activity and
          ;; setup dialog mode accordingly.
          (setup-activity-weather database sid)))

      (let ((result (send this do-edit parent)))
        (when (and result (has-valid-data?))
          (save-weather-data database sid)
          (log-event 'weather-data-changed sid))
        (set! db #f)
        (set! sid #f)
        result))

    ))

(define the-weather-editor #f)

(define (get-weather-editor)
  (unless the-weather-editor
    (set! the-weather-editor (new weather-edit%)))
  the-weather-editor)
