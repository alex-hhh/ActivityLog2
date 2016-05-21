#lang racket/base
;; view-activities.rkt -- activity list panel
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

(require db
         racket/class
         racket/gui/base
         (rename-in srfi/48 (format format-48))
         racket/list
         racket/string
         "activity-edit.rkt"
         "al-prefs.rkt"
         "al-widgets.rkt"
         "database.rkt"
         "fmt-util.rkt"
         "heatmap.rkt"
         "icon-resources.rkt"
         "sport-charms.rkt"
         "weather.rkt"
         "widgets.rkt")

(provide view-activities%)

(define identity (lambda (x) x))

(define (make-col-name->col-id-hash db-headers)
  (let ((result (make-hash)))
    (let loop ((headers db-headers)
               (index 0))
      (unless (null? headers)
        (let ((name (cdr (assq 'name (car headers)))))
          (hash-set! result name index))
        (loop (cdr headers) (+ 1 index))))
    result))

(define (db-row-ref row column-name headers [if-null? #f])
  (let ((index (hash-ref headers column-name
                         (lambda () (raise (format "column not found: ~a" column-name))))))
    (let ((v (vector-ref row index)))
      (if (sql-null? v) if-null? v))))

(define (get-activity-list-query sport date-range distance duration labels equipment)
  (format "select * from V_ACTIVITY_LIST VAL
 where ~a and ~a
   and ~a and ~a
   and ~a and ~a
   and ~a and ~a and ~a
  order by VAL.start_time desc"
          (if (and distance (car distance))
              (format "VAL.distance >= ~a" (* 1000.0 (car distance)))
              "1 = 1")
          (if (and distance (cdr distance))
              (format "VAL.distance <= ~a" (* 1000.0 (cdr distance)))
              "1 = 1")
          (if (and duration (car duration))
              (format "VAL.duration >= ~a" (* 3600.0 (car duration)))
              "1 = 1")
          (if (and duration (cdr duration))
              (format "VAL.duration <= ~a" (* 3600.0 (cdr duration)))
              "1 = 1")
          (if (and date-range (car date-range))
              (format "VAL.start_time >= ~a" (car date-range))
              "1 = 1")
          (if (and date-range (cdr date-range))
              (format "VAL.start_time <= ~a" (cdr date-range))
              "1 = 1")
          (if sport
              (cond ((cdr sport)
                     (format "VAL.sub_sport = ~a" (cdr sport)))
                    ((car sport)
                     (format "VAL.sport = ~a" (car sport)))
                    (#t
                     "1 = 1"))
              "1 = 1")
          (if (pair? labels)
              (let ((labels-str (map (lambda (l) (format "~a" l)) labels)))
                (format "VAL.session_id in (select SL.session_id from SESSION_LABEL SL where SL.label_id in (~a))"
                        (string-join labels-str ", ")))
              "1 = 1")

          (if (pair? equipment)
              (let ((equipment-str (map (lambda (l) (format "~a" l)) equipment)))
                (format "VAL.session_id in (select EU.session_id from EQUIPMENT_USE EU where EU.equipment_id in (~a))"
                        (string-join equipment-str ", ")))
              "1 = 1")

          ))

(define (get-activity-list db sport date-range distance duration labels equipment)
  (query
   db
   (get-activity-list-query sport date-range distance duration labels equipment)))

(define (get-activity-list-1 db session-id)
  (query-row
   db
   "select * from V_ACTIVITY_LIST VAL where VAL.session_id = ?" session-id))

(define (make-activity-summary-label rows headers)
  (let ((nitems (length rows))
        (total-distance 0)
        (total-duration 0))
    (for-each (lambda (row)
                (let ((duration (db-row-ref row "duration" headers 0))
                      (distance (db-row-ref row "distance" headers 0)))
                  (set! total-duration (+ total-duration duration))
                  (set! total-distance (+ total-distance distance))))
              rows)
    (format "~a activities, total duration: ~a, total distance: ~a"
            nitems
            (duration->string total-duration)
            (distance->string total-distance #t))))

(define view-activities%
  (class* object% (activity-operations<%>)
    (init parent)
    (init-field database [select-activity-callback #f])
    (super-new)

    (define tag 'activity-log:view-activities-visual-layout)

    (define pane (new (class vertical-panel%
                        (init)(super-new)
                        (define/public (interactive-export-sql-query)
                          (on-interactive-export-sql-query))
                        (define/public (interactive-generate-heatmap)
                          (on-interactive-generate-heatmap)))
                      [parent parent]
                      [alignment '(left center)]))

    ;; Hash table mapping a column name to a row index in the result set
    (define headers (make-hash))

    ;; This is the data, as retrieved by the SQL query, a list of vectors,
    ;; each vector representing a row in the result set.
    (define data '())

    (define sport-filter #f)
    (define date-range-filter #f)
    (define distance-filter #f)
    (define duration-filter #f)
    (define labels-filter '())
    (define equipment-filter '())
    (define text-filter #f)
    (define label-input-field #f)
    (define equipment-input-field #f)
    (define date-range-field #f)

    (let ((sel-pane (new horizontal-pane% [parent pane]
                         [border 0]
                         [stretchable-height #f]
                         [stretchable-width #t]
                         [alignment '(left center)])))

      (new message% [parent sel-pane] [label stopwatch-icon])

      (let ((q (new vertical-pane% [spacing 5] [parent sel-pane] [alignment '(left center)])))

        (new validating-input-field%
             [label "Search:"]
             [validate-fn (lambda (v) #t)]
             [convert-fn identity]
             [valid-value-cb (lambda (v) (set! text-filter v) (on-text-filter-changed))]
             [parent q])

        (let ((q (new horizontal-pane% [parent q] [spacing 5] [alignment '(left center)])))

          (let ((p (new vertical-pane% [parent q] [alignment '(left top)] [stretchable-width #f])))
            (new sport-selector% [parent p]
                 [callback (lambda (s)
                             (set! sport-filter s)
                             (on-filter-changed))])
            (let ((drs (new date-range-selector% [parent p]
                            [initial-selection 'last-30-days]
                            [callback (lambda (s)
                                        (set! date-range-filter s)
                                        (on-filter-changed))])))
              (send drs set-seasons (db-get-seasons database))
              ;; Setup the date-range-filter to the selector's initial value
              (set! date-range-filter (send drs get-selection))
              (set! date-range-field drs)))

          (let ((p (new vertical-pane% [parent q] [alignment '(left top)] [stretchable-width #f])))
            (new number-range-selector% [parent p] [label "Distance (km)"]
                 [callback (lambda (d) (set! distance-filter d) (on-filter-changed))])
            (new number-range-selector% [parent p] [label "Time (hours)"]
                 [callback (lambda (d) (set! duration-filter d) (on-filter-changed))]))

          (let ((p (new vertical-pane% [parent q] [alignment '(center top)] [stretchable-width #t])))
            (set! label-input-field
                  (new label-input-field%
                       [parent p]
                       [callback (lambda (o)
                                   (set! labels-filter (send o get-contents-as-tag-ids))
                                   (on-filter-changed))])))
          
          (let ((p (new vertical-pane% [parent q] [alignment '(center top)] [stretchable-width #t])))
            (set! equipment-input-field
                  (new equipment-input-field%
                       [parent p]
                       [use-retired-equipment? #t]
                       [callback (lambda (o)
                                   (set! equipment-filter (send o get-contents-as-tag-ids))
                                   (on-filter-changed))]))))
        ))

    (define lb
      (new (class qresults-list% (init) (super-new)
             (define/override (on-double-click row-index row-data)
               (when select-activity-callback
                 (select-activity-callback
                  (db-row-ref row-data "session_id" headers "")))))
           [parent pane]
           [tag 'activity-log:activity-list]
           [right-click-menu
            (send (new activity-operations-menu% [target this]) get-popup-menu)]))

    ;; Restore visual layout
    (let ((vdata (al-get-pref tag (lambda () #f))))
      (when (and vdata (= (length vdata) 1))
        (send date-range-field restore-from (first vdata))))

    (define *activity-list-display-columns*
      (list

       (let ((fn (lambda (row) (db-row-ref row "headline" headers ""))))
         (column-info "Activity Name" fn fn))

       (let ((fn (lambda (row)
                   (let ((sport (db-row-ref row "sport" headers 0))
                         (sub-sport (db-row-ref row "sub_sport" headers 0)))
                     (get-sport-name sport sub-sport)))))
         (column-info "Sport" fn fn))

       (let ((fn (lambda (row) (db-row-ref row "start_time" headers 0))))
         (column-info "Start Time" (lambda (row) (date-time->string (fn row))) fn))

       (let ((fn (lambda (row) (db-row-ref row "duration" headers 0))))
         (column-info "Duration"
                      (lambda (row) 
                        (let ((v (fn row)))
                          (if (> v 0) (duration->string v) "")))
                      fn))

       (let ((fn (lambda (row) (db-row-ref row "distance" headers 0))))
         (column-info "Distance"
                      (lambda (row)
                        (let ((sport (db-row-ref row "sport" headers 0))
                              (distance (fn row)))
                          (if (> distance 100) ; meters
                              (if (= sport 5)  ; swimming 
                                  (short-distance->string distance #t)
                                  (distance->string distance #t))
                              "")))
                      fn))

       (let ((fn (lambda (row) (db-row-ref row "speed" headers 0))))
         (column-info "Speed"
                      (lambda (row)
                        (let ((sport (db-row-ref row "sport" headers 0))
                              (speed (fn row)))
                          (cond ((< speed 0.00002) "") ; approx 0.1 km/h
                                ((= sport 1) (pace->string speed #t))
                                ((= sport 5) (swim-pace->string speed #t))
                                (#t (speed->string speed #t)))))
                      fn))

       (let ((fn (lambda (row) (db-row-ref row "max_speed" headers 0))))
         (column-info "Max Speed"
                      (lambda (row)
                        (let ((sport (db-row-ref row "sport" headers 0))
                              (speed (fn row)))
                          (cond ((< speed 0.00002) "") ; approx 0.1 km/h
                                ((= sport 1) (pace->string speed #t))
                                ((= sport 5) (swim-pace->string speed #t))
                                (#t (speed->string speed #t)))))
                      fn))

       (let ((fn (lambda (row) (db-row-ref row "ascent" headers 0))))
         (column-info "Ascent"
                      (lambda (row)
                        (let ((v (fn row)))
                          (if (> v 0) (vertical-distance->string v #t) "")))
                      fn))

       (let ((fn (lambda (row) (db-row-ref row "descent" headers 0))))
         (column-info "Descent"
                      (lambda (row)
                        (let ((v (fn row)))
                          (if (> v 0) (vertical-distance->string v #t) "")))
                      fn))

       (let ((fn (lambda (row) (db-row-ref row "hr" headers 0))))
         (column-info "HR" (lambda (row) (n->string (fn row))) fn))
       
       (let ((fn (lambda (row) (db-row-ref row "max_hr" headers 0))))
         (column-info "Max HR" (lambda (row) (n->string (fn row))) fn))

       (let ((fn (lambda (row) (db-row-ref row "adecl" headers #f))))
         (column-info "A Decl"
                      (lambda (row)
                        (let ((val (fn row)))
                          (if val (pct->string val) "")))
                      fn))

       (let ((fn (lambda (row) (db-row-ref row "cadence" headers 0))))
         (column-info "Cadence" (lambda (row) (n->string (fn row))) fn))

       (let ((fn (lambda (row) (db-row-ref row "max_cadence" headers 0))))
         (column-info "Max Cadence" (lambda (row) (n->string (fn row))) fn))

       (let ((fn (lambda (row) (db-row-ref row "stride" headers 0))))
         (column-info "Stride"
                      (lambda (row)
                        (let ((val (fn row)))
                          (if (= val 0) "" (stride->string val #t))))
                      fn))

       (let ((fn (lambda (row) (db-row-ref row "vosc" headers 0))))
         (column-info "VOSC"
                      (lambda (row) (vosc->string (fn row) #t))
                      fn))

       (let ((fn (lambda (row)
                   (let ((st (db-row-ref row "stride" headers #f))
                         (vosc (db-row-ref row "vosc" headers #f)))
                     (if (and st vosc (> st 0) (> vosc 0))
                         (* 100.0 (/ vosc (* st 1000)))
                         0)))))
         (column-info "VRATIO"
                      (lambda (row)
                        (let ((vratio (fn row)))
                          (if (> vratio 0) (pct->string (fn row)) "")))
                      fn))

       (let ((fn1 (lambda (row) (db-row-ref row "gct" headers 0)))
             (fn2 (lambda (row) (db-row-ref row "gct_pct" headers 0))))
         (column-info "GCT"
                      (lambda (row) (stance->string (fn1 row) (fn2 row)))
                      fn1))

       (let ((fn (lambda (row) (db-row-ref row "power" headers 0))))
         (column-info "Power"
                      (lambda (row) (let ((v (fn row))) (power->string v #t)))
                      fn))

       (let ((fn (lambda (row) (db-row-ref row "max_power" headers 0))))
         (column-info "Max Power"
                      (lambda (row) (let ((v (fn row))) (power->string v #t)))
                      fn))

       (let ((fn (lambda (row) (db-row-ref row "np" headers 0))))
         (column-info "Normalized Power"
                      (lambda (row) (let ((v (fn row))) (power->string v #t)))
                      fn))

       (let ((fn (lambda (row) (db-row-ref row "lrbal" headers 0))))
         (column-info "L-R Bal"
                      (lambda (row)
                        (let ((v (fn row)))
                          (if (> v 0) (format-48 "~1,1F%" v) "")))
                      fn))

       (let ((fn (lambda (row) (db-row-ref row "ltorqeff" headers 0))))
         (column-info "Left TEff"
                      (lambda (row)
                        (let ((v (fn row)))
                          (if (> v 0) (format-48 "~1,1F%" v) "")))
                      fn))

       (let ((fn (lambda (row) (db-row-ref row "rtorqeff" headers 0))))
         (column-info "Right TEff"
                      (lambda (row)
                        (let ((v (fn row)))
                          (if (> v 0) (format-48 "~1,1F%" v) "")))
                      fn))

       (let ((fn (lambda (row) (db-row-ref row "lpdlsmth" headers 0))))
         (column-info "Left PSmth"
                      (lambda (row)
                        (let ((v (fn row)))
                          (if (> v 0) (format-48 "~1,1F%" v) "")))
                      fn))
       
       (let ((fn (lambda (row) (db-row-ref row "rpdlsmth" headers 0))))
         (column-info "Right PSmth"
                      (lambda (row)
                        (let ((v (fn row)))
                          (if (> v 0) (format-48 "~1,1F%" v) "")))
                      fn))

       (let ((fn (lambda (row) (db-row-ref row "lpco" headers #f))))
         (column-info "Left PCO"
                      (lambda (row)
                        (let ((v (fn row)))
                          (if v (pco->string v #t) "")))
                      ;; NOTE: sorting on #f is not nice :-)
                      (lambda (row) (or (fn row) -1000))))

       (let ((fn (lambda (row) (db-row-ref row "rpco" headers #f))))
         (column-info "Right PCO"
                      (lambda (row)
                        (let ((v (fn row)))
                          (if v (pco->string v #t) "")))
                      ;; NOTE: sorting on #f is not nice :-)
                      (lambda (row) (or (fn row) -1000))))

       (let ((fn1 (lambda (row) (db-row-ref row "lppstart" headers #f)))
             (fn2 (lambda (row) (db-row-ref row "lppend" headers #f))))
         (column-info "Left PP"
                      (lambda (row)
                        (let ((start (fn1 row))
                              (end (fn2 row)))
                          (if (and start end) (power-phase->string start end) "")))
                      ;; NOTE: sorting on #f is not nice :-)
                      (lambda (row) (or (fn1 row) -1000))))

       (let ((fn1 (lambda (row) (db-row-ref row "rppstart" headers #f)))
             (fn2 (lambda (row) (db-row-ref row "rppend" headers #f))))
         (column-info "Right PP"
                      (lambda (row)
                        (let ((start (fn1 row))
                              (end (fn2 row)))
                          (if (and start end) (power-phase->string start end) "")))
                      ;; NOTE: sorting on #f is not nice :-)
                      (lambda (row) (or (fn1 row) -1000))))

       (let ((fn1 (lambda (row) (db-row-ref row "lpppstart" headers #f)))
             (fn2 (lambda (row) (db-row-ref row "lpppend" headers #f))))
         (column-info "Left Peak PP"
                      (lambda (row)
                        (let ((start (fn1 row))
                              (end (fn2 row)))
                          (if (and start end) (power-phase->string start end) "")))
                      ;; NOTE: sorting on #f is not nice :-)
                      (lambda (row) (or (fn1 row) -1000))))

       (let ((fn1 (lambda (row) (db-row-ref row "rpppstart" headers #f)))
             (fn2 (lambda (row) (db-row-ref row "rpppend" headers #f))))
         (column-info "Right Peak PP"
                      (lambda (row)
                        (let ((start (fn1 row))
                              (end (fn2 row)))
                          (if (and start end) (power-phase->string start end) "")))
                      ;; NOTE: sorting on #f is not nice :-)
                      (lambda (row) (or (fn1 row) -1000))))

       (let ((fn (lambda (row) (db-row-ref row "calories" headers 0))))
         (column-info "Calories" (lambda (row) (n->string (fn row))) fn))

       (let ((fn (lambda (row) (db-row-ref row "te" headers 0))))
         (column-info "Training Effect"
                      (lambda (row) 
                        (let ((v (fn row)))
                          (if (> v 0) (format-48 "~1,1F" v) "")))
                      fn))
       
       (let ((fn (lambda (row) (db-row-ref row "rpe" headers 0))))
         (column-info "RPE"
                      (lambda (row) 
                        (let ((v (fn row)))
                          (if (> v 0) (format-48 "~1F" v) "")))
                      fn))

       (let ((fn (lambda (row) (db-row-ref row "tss" headers 0))))
         (column-info "TSS"
                      (lambda (row) 
                        (let ((v (fn row)))
                          (if (> v 0) (format-48 "~1,1F" v) "")))
                      fn))
       (let ((fn (lambda (row) (db-row-ref row "ifact" headers 0))))
         (column-info "Intensity Factor"
                      (lambda (row) 
                        (let ((v (fn row)))
                          (if (> v 0) (format-48 "~1,1F" v) "")))
                      fn))

       (let ((fn (lambda (row) (db-row-ref row "body_weight" headers 0))))
         (column-info "Body Weight"
                      (lambda (row) (weight->string (fn row) #t))
                      fn))

       (let ((fn (lambda (row) (db-row-ref row "temperature" headers -1000))))
         (column-info "Temperature"
                      (lambda (row) (temperature->string (fn row) #t))
                      fn))

       (let ((fn (lambda (row) (db-row-ref row "dew_point" headers -1000))))
         (column-info "Dew Point"
                      (lambda (row) (temperature->string (fn row) #t))
                      fn))

       (let ((fn (lambda (row) (db-row-ref row "humidity" headers 0))))
         (column-info "Humidity"
                      (lambda (row) (humidity->string (fn row) #t))
                      fn))
       
       (let ((fn1 (lambda (row) (db-row-ref row "wind_speed" headers 0)))
             (fn2 (lambda (row) (db-row-ref row "wind_direction" headers 0))))
         (column-info "Wind"
                      (lambda (row) (wind->string (fn1 row) (fn2 row)))
                      fn1))
       
       (let ((fn (lambda (row) 
                   (let ((temp (db-row-ref row "temperature" headers #f))
                         (dewp (db-row-ref row "dew_point" headers #f)))
                     (if (and temp dewp) (humindex temp dewp) -1000)))))
         (column-info "Humindex"
                      (lambda (row) (temperature->string (fn row) #t))
                      fn))

       (let ((fn (lambda (row) (db-row-ref row "activity_guid" headers ""))))
         (column-info "Activity-Guid" fn fn))

       (let ((fn (lambda (row) (db-row-ref row "session_id" headers 0))))
         (column-info "Session-Id"
                      (lambda (row) (format "~a" (fn row)))
                      fn))

       ))

    (send lb setup-column-defs *activity-list-display-columns*)

    (define (get-text-filtered-data data text)
      (if (and text (not (equal? text "")))
          (let ((regexps (map (lambda (s) (regexp-quote s #f)) (string-split text))))
            (let ((ndata '()))
              (for ((item (in-list data)))
                (let ((name (db-row-ref item "headline" headers "")))
                  (when (and (string? name)
                             (andmap (lambda (r) (regexp-match r name)) regexps))
                    (set! ndata (cons item ndata)))))
              (reverse ndata)))
          data))

    (define (on-filter-changed)
      (let ((result (get-activity-list
                     database sport-filter date-range-filter
                     distance-filter duration-filter labels-filter equipment-filter)))
        (set! data (rows-result-rows result))
        (set! headers (make-col-name->col-id-hash (rows-result-headers result))))
      (on-text-filter-changed))

    (define (on-text-filter-changed)
      (let ((rows (get-text-filtered-data data text-filter)))
        (send lb set-data rows)
        (send (send pane get-top-level-window) 
              set-status-text 
              (make-activity-summary-label rows headers))))

    (define dirty? #t)

    (define/public (activated)
      (if dirty?
          (refresh)
          ;; Set the status text when activated
          (let ((rows (get-text-filtered-data data text-filter)))
            (send (send pane get-top-level-window) 
                  set-status-text 
                  (make-activity-summary-label rows headers))))
      (set! dirty? #f))

    (define/public (refresh)
      (send label-input-field setup-for-session database #f)
      (send equipment-input-field setup-for-session database #f)
      (send date-range-field set-seasons (db-get-seasons database))
      ;; Refresh the date range (in case the selection is something like "last
      ;; 30 days" and the date has changed.
      (set! date-range-filter (send date-range-field get-selection))
      (on-filter-changed))

    (define/public (save-visual-layout)
      (let ((data (list (send date-range-field get-restore-data))))
        (al-put-pref tag data))
      (send lb save-visual-layout))

    (define/public (on-interactive-export-sql-query)
      (let ((query (get-activity-list-query
                    sport-filter date-range-filter distance-filter 
                    duration-filter labels-filter equipment-filter)))
        (send (get-sql-export-dialog) 
              show-dialog (send pane get-top-level-window) query)))

    (define/public (on-interactive-generate-heatmap)
      (let ((session-ids (for/list ((d (get-text-filtered-data data text-filter)))
                           (db-row-ref d "session_id" headers 0))))
        (interactive-generate-heatmap database session-ids)))

    
    ;;................................. the activity-operations<%> interface ....

    (define selected-row-index #f)

    (define/public (get-top-level-window)
      (send pane get-top-level-window))
    (define/public (get-database)
      database)
    (define/public (get-selected-sid)
      (when selected-row-index
        (let ((data (send lb get-data-for-row selected-row-index)))
          (if data (db-row-ref data "session_id" headers "") #f))))
    (define/public (get-selected-guid)
      (when selected-row-index
        (let ((data (send lb get-data-for-row selected-row-index)))
          (if data
              (let ((guid (db-row-ref data "activity_guid" headers "")))
                (if (or (sql-null? guid) (equal? guid ""))
                    #f
                    guid))
              #f))))
    (define/public (after-update sid)
      (when selected-row-index
        (let ((new-data (get-activity-list-1 database sid)))
          (send lb update-row selected-row-index new-data))))
    (define/public (after-new sid)
      (let ((new-data (get-activity-list-1 database sid)))
        (send lb add-row new-data)))
    (define/public (can-delete? sid)
      (number? selected-row-index))
    (define/public (after-delete sid)
      (when selected-row-index
        (send lb delete-row selected-row-index)))
    (define/public (before-popup)
      (set! selected-row-index (send lb get-selected-row-index)))
    (define/public (after-popdown)
      (set! selected-row-index #f))


    ))
