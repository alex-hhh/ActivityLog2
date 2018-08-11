#lang racket/base
;; edit-session-weather.rkt -- edit weather data for a session
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
         racket/class
         racket/gui/base
         "../fmt-util.rkt"
         "../widgets/map-widget/map-util.rkt"
         "../sport-charms.rkt"
         "../dbutil.rkt"
         "../weather.rkt"
         "../widgets/main.rkt"
         "../utilities.rkt")

(provide get-weather-editor)


;;........................................................ weather-edit% ....

(define message-font
  (send the-font-list find-or-create-font 12 'default 'normal 'normal))

(define weather-edit%
  (class edit-dialog-base%
    (init)
    (super-new [title "Session Weather"] [icon (weather-icon)])

    ;; A message% object where we display any errors while fetching weather
    ;; data.
    (define info-message #f)

    ;; The start time of the activtity, used to determine the weather
    ;; obesrvation timestamp.
    (define start-time (current-seconds))

    ;; Widgets used to display to the user the activity for which we edit the
    ;; weather.
    (define activity-headline #f)
    (define activity-start-time #f)

    ;; Source of the weather data. Can be: 'nearby -- from one of the nearby
    ;; weather stations, 'weather-station -- from a specific weather station,
    ;; or 'manual -- manually entered.
    (define weather-source #f)

    ;; When (eq? weather-source 'weather-station), this contains the name of
    ;; the station.
    (define weather-station #f)

    ;; When (eq? weather-source 'nearby), this contains a list of (cons
    ;; wstation distance) objects
    (define nearby-stations '())

    ;; list of wobs objects (weather observations) for the current wstation
    (define observations '())

    ;; Widgets used to select the weather source, weather station and
    ;; observation timestamp.
    (define wsource-choice #f)
    (define wselection-pane #f)
    (define wstation-field #f)
    (define wstation-choice #f)
    (define wobs-choice #f)

    ;; Widgets used to select weather data values.  When (memq? weather-source
    ;; '(nearby weather-station), these are read-only and filled in from
    ;; weather observations (wobs objects)
    (define temperature-field #f)
    (define dew-point-field #f)
    (define humidity-field #f)
    (define wind-speed-field #f)
    (define wind-gusts-field #f)
    (define wind-direction-field #f)
    (define baromethric-pressure-field #f)

    (define wind-rose
      (list "N" "NNE" "NE" "NEE" "E" "ESE" "SE" "SSE" "S"
            "SSW" "SW" "WSW" "W" "WNW" "NW" "NNW"))

    (let ((p (send this get-client-pane)))
      (let ((hp (make-horizontal-pane p #f)))
        (send hp set-alignment 'center 'center)
        (set! info-message (new message% [parent hp] [label ""]
                                [font message-font] [auto-resize #t])))

      (let ((hp (make-horizontal-pane p #f)))
        (new message% [parent hp] [label "Activity: "]
             [stretchable-width #f])
        (set! activity-headline (new message% [parent hp] [label "Untitled"]
                                     [font message-font]
                                     [stretchable-width #t])))
      (let ((hp (make-horizontal-pane p #f)))
        (new message% [parent hp] [label "Start time: "]
             [stretchable-width #f])
        (set! activity-start-time (new message% [parent hp] [label "Untitled"]
                                       [font message-font]
                                       [stretchable-width #t])))

      (define sel-gb (make-group-box-panel p))

      (let ((hp (make-horizontal-pane sel-gb #f)))
        (set! wsource-choice
              (new choice% [parent hp]
                   [label "Source: "]
                   [choices '("Nearby" "Weather Station" "Manual")]
                   [callback (lambda (c e)
                               (let ((sel (send c get-selection)))
                                 (on-weather-source-changed
                                  (list-ref '(nearby weather-station manual) sel))))]))
        (set! wselection-pane (make-horizontal-pane hp #f))
        (set! wstation-field
              (new validating-input-field%
                   [parent wselection-pane] [label ""]
                   [style '(single deleted)]
                   [cue-text "pws:NAME or COUNTRY/NAME"]
                   [convert-fn values]
                   [valid-value-cb
                    (lambda (v)
                      (on-new-wstation (send wstation-field get-converted-value)))]
                   [validate-fn (lambda (v) #t)]))
        (set! wstation-choice
              (new choice% [parent wselection-pane]
                   [label ""]
                   [min-width 250]
                   [choices '("None")]
                   [callback (lambda (c e) (on-wstation-selected (send c get-selection)))]))
        (set! wobs-choice
              (new choice% [parent wselection-pane]
                   [label ""]
                   [choices '("None")]
                   [callback (lambda (c e) (on-observation-selected (send c get-selection)))])))

      (define contents-gb (make-group-box-panel p))

      (let ((hp (make-horizontal-pane contents-gb #f)))
        (set! temperature-field (new number-input-field%
                                     [parent hp]
                                     [cue-text "degrees Celsius"]
                                     [label "Temperature: "]))
        (set! humidity-field (new number-input-field%
                                  [parent hp]
                                  [cue-text "percentage"]
                                  [label "Humidity: "]))
        (set! dew-point-field (new number-input-field%
                                   [parent hp]
                                   [cue-text "degrees Celsius"]
                                   [label "Dew point: "])))

      (let ((hp (make-horizontal-pane contents-gb #f)))
        (set! wind-speed-field (new number-input-field%
                                     [parent hp]
                                     [cue-text "km/h"]
                                     [label "Wind speed: "]))
        (set! wind-gusts-field (new number-input-field%
                                    [parent hp]
                                    [cue-text "km/h"]
                                    [label "Gusts: "]))
        (set! wind-direction-field (new choice%
                                        [parent hp]
                                        [label "Direction: "]
                                        [choices wind-rose])))

      (let ((hp (make-horizontal-pane contents-gb #f)))
        (set! baromethric-pressure-field
              (new number-input-field%
                   [parent hp]
                   [stretchable-width #f]
                   [min-width 50]
                   [cue-text "hPa"]
                   [label "Baromethric Pressure: "])))

      )

    (define (on-weather-source-changed new-source)
      (unless (eq? new-source weather-source)
        (case new-source
          ((nearby)
           (send wselection-pane change-children
                 (lambda (old) (list wstation-choice wobs-choice)))
           (let ((index (send wobs-choice get-selection)))
             (when index
               (setup-weather-fields (list-ref observations index) #t)))
           (enable-manual-edit #f))

          ((weather-station)
           (send wselection-pane change-children
                 (lambda (old) (list wstation-field wobs-choice)))
           ;; Init the field based on the previous weather source
           (case weather-source
             ((nearby)
              (when (> (length nearby-stations) 0)
                (let* ((index (send wstation-choice get-selection))
                       (wstation (car (list-ref nearby-stations index))))
                  (send wstation-field set-value
                        (let ((type (wstation-type wstation))
                              (ident (wstation-ident wstation)))
                          (string-append (if (symbol? type) (symbol->string type) type)
                                         ":" ident))))))
             ((manual)
              (clear-weather-fields)
              (on-new-wstation (send wstation-field get-converted-value))))
           (enable-manual-edit #f))

          ((manual)
           (send wselection-pane change-children
                 (lambda (old) '()))
           (let ((index (send wobs-choice get-selection)))
             ;; Inherit the weather data from the last selected observation,
             ;; and use that as a starting point for the edit.
             (if index
                 (setup-weather-fields (list-ref observations index) #f)
                 (clear-weather-fields)))
           (enable-manual-edit #t)))
        (set! weather-source new-source)))

    ;; Called when the user types the NAME (in the format pws:IDENT or
    ;; icao:IDENT) of a weather station.  Note that we also act as a
    ;; validation function, as such, we return #f if we fail to setup the
    ;; weather station
    (define (on-new-wstation name)
      ;; We might be delay called when the source is not 'weather-station,
      ;; don't change anything in that case.
      (when (eq? weather-source 'weather-station)
        (with-busy-cursor
         (lambda ()
           (send wstation-field mark-valid #f)
           (send wobs-choice enable #f)
           (when (and name (not (string=? name "")))
             (with-handlers
               (((lambda (e) #t)
                 (lambda (e)
                   (send info-message set-label
                         (if (exn? e) (exn-message e) "Unknown error while fetching weather"))
                   (clear-weather-fields))))
               (let ((obs (get-observations-for-station name start-time)))
                 (send info-message set-label "")
                 (when obs
                   (setup-observations obs)
                   (send wobs-choice enable #t)
                   (send wstation-field mark-valid #t)))))))))

    (define (on-wstation-selected index)
      (with-busy-cursor
        (lambda ()
          (with-handlers
            (((lambda (e) #t)
              (lambda (e)
                (send info-message set-label
                      (if (exn? e) (exn-message e) "Unknown error while fetching weather"))
                (clear-weather-fields))))
            (let* ((wstation (car (list-ref nearby-stations index)))
                   (obs (get-observations-for-station wstation start-time)))
              (setup-observations obs)
              (send info-message set-label ""))))))

    (define (on-observation-selected index)
      (setup-weather-fields (list-ref observations index) #t))

    (define (set-wind-direction wdir)
      (if wdir
          (let ((label (degrees->wind-rose wdir)))
            (let loop ((idx 0) (labels wind-rose))
              (unless (null? labels)
                (if (equal? label (car labels))
                    (send wind-direction-field set-selection idx)
                    (loop (+ idx 1) (cdr labels))))))
          (send wind-direction-field set-selection 0)))

    (define (get-wind-direction)
      ;; this is a hack :-)
      (* (/ 360 (length wind-rose)) (send wind-direction-field get-selection)))

    ;; Fill WSTATION-CHOICE with weather stations that are close to the GPS
    ;; track of the session id, SID. DB is the activity log database.  The
    ;; list of weather stations is also stored in NEARBY-STATIONS.
    (define (setup-nearby-wstations db sid)
      (send wstation-choice clear)
      (set! nearby-stations '())

      (with-handlers
        (((lambda (e) #t)
          (lambda (e)
            (send info-message set-label
                  (if (exn? e) (exn-message e) "Unknown error while fetching weather"))
            (clear-weather-fields))))
        
        (set! nearby-stations (get-nearby-wstations-for-session db sid))
        (for ((w (in-list nearby-stations)))
          (send wstation-choice append
                (format "~a (~a, ~a)"
                        (wstation-name (car w))
                        (wstation-ident (car w))
                        (distance->string (cdr w) #t))))
        (unless (null? nearby-stations)
          (let ((obs (get-observations-for-station
                      (car (car nearby-stations)) start-time)))
            (when obs (setup-observations obs))))
        (send info-message set-label "")))

    ;; Fill WOBS-CHOICE with observation data, which is also stored in
    ;; OBSERVATIONS.
    (define (setup-observations new-observations)
      (send wobs-choice clear)
      (send wobs-choice enable #t)
      (set! observations (sort new-observations < #:key wobs-ts))
      (for ((o (in-list observations)))
        (send wobs-choice append
              (time-of-day->string (wobs-ts o))))
      (clear-weather-fields)
      ;; Find the best observation that matches START-TIME and select it.
      (let loop ((idx 0) (obs observations))
        (unless (null? obs)
          (if (> (wobs-ts (car obs)) start-time)
              (begin
                (send wobs-choice set-selection idx)
                (setup-weather-fields (car obs) #t))
              (loop (+ idx 1) (cdr obs))))))

    ;; Setup values in the weather fields, based on data in WOBS.  If
    ;; UNIT-LABEL is #t, the values will have unit labels (temperature, speed,
    ;; etc)
    (define (setup-weather-fields wobs unit-label)
      (send temperature-field set-value
            (if (wobs-temp wobs)
                (temperature->string (wobs-temp wobs) unit-label)
                ""))
      (send dew-point-field set-value
            (if (wobs-dewp wobs)
                (temperature->string (wobs-dewp wobs) unit-label)
                ""))
      (send humidity-field set-value
            (if (wobs-hum wobs)
                (humidity->string (wobs-hum wobs) unit-label)
                ""))
      (send wind-speed-field set-value
            (if (wobs-wspd wobs)
                (speed->string (wobs-wspd wobs) unit-label)
                ""))
      (send wind-gusts-field set-value
            (if (wobs-wgust wobs)
                (speed->string (wobs-wgust wobs) unit-label)
                ""))
      (set-wind-direction (wobs-wdir wobs))
      (send baromethric-pressure-field set-value
            (if (wobs-pressure wobs)
                (pressure->string (wobs-pressure wobs) unit-label)
                "")))

    (define (clear-weather-fields)
      (send temperature-field set-value "")
      (send dew-point-field set-value "")
      (send humidity-field set-value "")
      (send wind-speed-field set-value "")
      (send wind-gusts-field set-value "")
      (set-wind-direction 0)
      (send baromethric-pressure-field set-value ""))

    ;; Enable/Disable manual editing of weather fields.
    (define (enable-manual-edit enable?)
      (for ((f (in-list (list temperature-field dew-point-field humidity-field
                              wind-speed-field wind-gusts-field wind-direction-field
                              baromethric-pressure-field))))
        (send f enable enable?)))

    (define (setup-activity-info db sid)
      (let ((row (query-row db "
select name, sport_id, sub_sport_id, start_time from A_SESSION where id = ?"
                            sid)))
        (let ((headline (format "~a (~a)"
                                (sql-column-ref row 0 "Untitled")
                                (get-sport-name (sql-column-ref row 1 #f)
                                                (sql-column-ref row 2 #f)))))
          (send activity-headline set-label headline))
        (set! start-time (sql-column-ref row 3 0))
        (send activity-start-time set-label (date-time->string start-time))))

    ;; NOTE: WSTATION is a string, as stored in the SESSION_WEATHER table.  It
    ;; is used to look up a weather station, but there is a chance that it
    ;; might not be found.  Return #t if a weather station was selected, #f
    ;; otherwise
    (define (maybe-select-wstation wstation timestamp)
      (let loop ((idx 0) (ws nearby-stations))
        (if (null? ws)
            #f
            (if (equal? wstation (wstation-ident (car (car ws))))
                (begin
                  (send wstation-choice set-selection idx)
                  (on-wstation-selected idx)
                  (maybe-select-observation timestamp)
                  #t)
                (loop (+ idx 1) (cdr ws))))))

    (define (maybe-select-observation timestamp)
      (let ((ts (if (equal? timestamp 0) start-time timestamp)))
        (let loop ((idx 0) (obs observations))
          (unless (null? obs)
            (if (>= (wobs-ts (car obs)) ts)
                (begin
                  (send wobs-choice set-selection idx)
                  (on-observation-selected idx))
                (loop (+ idx 1) (cdr obs)))))))

    (define (setup-activity-weather db sid)
      (let ((row (query-maybe-row db "
select wstation, timestamp,
       ifnull(temperature, -1000),
       ifnull(dew_point, -1000),
       ifnull(humidity, -1),
       ifnull(wind_speed, -1),
       ifnull(wind_gusts, -1),
       ifnull(wind_direction, -1),
       ifnull(pressure, -1)
from SESSION_WEATHER where session_id =?" sid)))
        (when row
          (let ((wstation (sql-column-ref row 0 ""))
                (timestamp (sql-column-ref row 1 0)))
            (set! weather-source #f)
            (cond ((equal? wstation "")
                   (on-weather-source-changed 'manual)
                   (send wsource-choice set-selection 2)
                   (let ((wo (wobs (sql-column-ref row 1 0)
                                   (sql-column-ref row 2 0)
                                   (sql-column-ref row 3 0)
                                   (sql-column-ref row 4 0)
                                   (sql-column-ref row 5 0)
                                   (sql-column-ref row 6 0)
                                   (sql-column-ref row 7 0)
                                   (sql-column-ref row 8 0))))
                     (setup-weather-fields wo #f)))
                  ((maybe-select-wstation wstation timestamp)
                   (on-weather-source-changed 'nearby)
                   (send wsource-choice set-selection 0))
                  (#t
                   (on-weather-source-changed 'weather-station)
                   (send wsource-choice set-selection 1)
                   (send wstation-field set-value wstation)
                   (on-new-wstation wstation)
                   (maybe-select-observation timestamp)
                   ))))))

    (define (save-weather-data database sid)
      (cond ((eq? weather-source 'nearby)
             (let ((wstation (car (list-ref nearby-stations (send wstation-choice get-selection))))
                   (wobs (list-ref observations (send wobs-choice get-selection))))
               (update-session-weather database sid wstation wobs)))
            ((eq? weather-source 'weather-station)
             (let ((wstation (send wstation-field get-value))
                   (wobs (list-ref observations (send wobs-choice get-selection))))
               (update-session-weather database sid wstation wobs)))
            ((eq? weather-source 'manual)
             (let ((wo (wobs start-time
                             (send temperature-field get-converted-value)
                             (send dew-point-field get-converted-value)
                             (send humidity-field get-converted-value)
                             (send wind-speed-field get-converted-value)
                             (send wind-gusts-field get-converted-value)
                             (get-wind-direction)
                             (send baromethric-pressure-field get-converted-value))))
               (update-session-weather database sid "" wo)))))

    (define/override (has-valid-data?)
      (case weather-source
        ((nearby) (and (> (length nearby-stations) 0)
                       (> (length observations) 0)))
        ((weather-station)
         (> (length observations) 0))
        ((manual)
         (and (send temperature-field has-valid-value?)
              (send dew-point-field has-valid-value?)
              (send humidity-field has-valid-value?)
              (send wind-speed-field has-valid-value?)
              (send wind-gusts-field has-valid-value?)
              (send baromethric-pressure-field has-valid-value?)))
        (else #f)))

    (define/public (begin-edit parent database sid)
      ;; First clear any previous data
      (send wstation-field set-value "")
      (send wstation-choice clear)
      (send wobs-choice clear)
      (set! observations '())
      (set! weather-station #f)
      (set! nearby-stations '())
      (clear-weather-fields)

      (if (allow-weather-download)
          (if (wu-api-key)
              (send info-message set-label "")
              (send info-message set-label "No Wundergdound API key set"))
          (send info-message set-label "Weather data download disabled"))

      (with-busy-cursor
        (lambda ()
          ;; Setup nearby weather stations and set 'nearby as the default
          ;; setup.
          (setup-activity-info database sid)
          (setup-nearby-wstations database sid)
          (send wsource-choice set-selection 0)
          (on-weather-source-changed 'nearby)
          ;; Retrieve previous weather data, if any, for this activity and
          ;; setup dialog mode accordingly.
          (setup-activity-weather database sid)))
         
      (let ((result (send this do-edit parent)))
        (when (and result (has-valid-data?))
          (save-weather-data database sid)
          (log-event 'weather-data-changed sid))
        result))

    ))

(define the-weather-editor #f)

(define (get-weather-editor)
  (unless the-weather-editor
    (set! the-weather-editor (new weather-edit%)))
  the-weather-editor)
