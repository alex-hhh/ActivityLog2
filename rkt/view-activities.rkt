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

(require db/base
         racket/class
         racket/gui/base
         (rename-in srfi/48 (format format-48))
         racket/list
         racket/string
         racket/format
         "dialogs/activity-edit.rkt"
         "utilities.rkt"
         "al-widgets.rkt"
         "database.rkt"
         "fmt-util.rkt"
         "heatmap.rkt"
         "sport-charms.rkt"
         "weather.rkt"
         "widgets/main.rkt")

(provide view-activities%)

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
              (let ((labels-str (string-join (map ~a (sort labels <)) ", ")))
                ;; NOTE: We want to select sessions that have ALL the labels
                ;; listed in LABELS.  To do this, we use a group by than
                ;; concatenate the equipment values in a string and compare
                ;; against another string.  This works only if the equipment
                ;; IDs are sorted before being passed to group_concat().  This
                ;; seems to be the case, but I believe it is a side effect of
                ;; the SQLite query planner, not an explicit SQL statement, so
                ;; it may break in the future.
                (format "VAL.session_id in (
select X.session_id
  from (select session_id,
               group_concat(label_id, ', ') as test
          from SESSION_LABEL
         where label_id in (~a)
         group by session_id) as X
 where X.test = '~a')" labels-str labels-str))
              "1 = 1")

          (if (pair? equipment)
              (let ((equipment-str (string-join (map ~a (sort equipment <)) ", ")))
                ;; NOTE: We want to select sessions that have ALL the
                ;; equipment listed in EQUIPMENT.  To do this, we use a group
                ;; by than concatenate the equipment values in a string and
                ;; compare against another string.  This works only if the
                ;; equipment IDs are sorted before being passed to
                ;; group_concat().  This seems to be the case, but I believe
                ;; it is a side effect of the SQLite query planner, not an
                ;; explicit SQL statement, so it may break in the future.
                (format "VAL.session_id in (
select X.session_id
  from (select session_id,
               group_concat(equipment_id, ', ') as test
          from EQUIPMENT_USE
         where equipment_id in (~a)
         group by session_id) as X
 where X.test = '~a')" equipment-str equipment-str))
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

    (define text-search-input #f)
    (define distance-input #f)
    (define duration-input #f)
    (define sport-selector #f)
    (define date-range-selector #f)
    (define label-input #f)
    (define equipment-input #f)

    (define change-notification-source (make-log-event-source))

    (let ((sel-pane (new horizontal-pane% [parent pane]
                         [border 0]
                         [stretchable-height #f]
                         [stretchable-width #t]
                         [alignment '(left center)])))
      (make-spacer sel-pane)
      (new message% [parent sel-pane] [label (stopwatch-icon)])

      (let ((q (new vertical-pane% [spacing 5] [parent sel-pane] [alignment '(left center)])))

        (set! text-search-input
              (new validating-input-field%
                   [label "Search:"]
                   [validate-fn (lambda (v) #t)]
                   [convert-fn values]
                   [valid-value-cb (lambda (v) (set! text-filter v) (on-text-filter-changed))]
                   [parent q]))

        (let ((q (new horizontal-pane% [parent q] [spacing 5] [alignment '(left center)])))

          (let ((p (new vertical-pane% [parent q] [alignment '(left top)] [stretchable-width #f])))
            (set! sport-selector
                  (new sport-selector% [parent p]
                       [callback (lambda (s)
                                   (set! sport-filter s)
                                   (on-filter-changed))]))
            (let ((drs (new date-range-selector% [parent p]
                            [initial-selection 'last-30-days]
                            [callback (lambda (s)
                                        (set! date-range-filter s)
                                        (on-filter-changed))])))
              (send drs set-seasons (db-get-seasons database))
              ;; Setup the date-range-filter to the selector's initial value
              (set! date-range-filter (send drs get-selection))
              (set! date-range-selector drs)))

          (let ((p (new vertical-pane% [parent q] [alignment '(left top)] [stretchable-width #f])))
            (set! distance-input
                  (new number-range-selector% [parent p] [label "Distance (km)"]
                       [callback (lambda (d) (set! distance-filter d) (on-filter-changed))]))
            (set! duration-input
                  (new number-range-selector% [parent p] [label "Time (hours)"]
                       [callback (lambda (d) (set! duration-filter d) (on-filter-changed))])))

          (let ((p (new vertical-pane% [parent q] [alignment '(center top)] [stretchable-width #t])))
            (set! label-input
                  (new label-input-field%
                       [parent p]
                       [callback (lambda (o)
                                   (set! labels-filter (send o get-contents-as-tag-ids))
                                   (on-filter-changed))])))
          
          (let ((p (new vertical-pane% [parent q] [alignment '(center top)] [stretchable-width #t])))
            (set! equipment-input
                  (new equipment-input-field%
                       [parent p]
                       [callback (lambda (o)
                                   (set! equipment-filter (send o get-contents-as-tag-ids))
                                   (on-filter-changed))])))))
      
      (make-spacer sel-pane))

    (define lb
      (new (class qresults-list% (init) (super-new)
             (define/override (on-double-click row-index row-data)
               (when select-activity-callback
                 (select-activity-callback
                  (db-row-ref row-data "session_id" headers "")))))
           [parent pane]
           [pref-tag 'activity-log:activity-list]
           [right-click-menu
            (send (new activity-operations-menu% [target this]) get-popup-menu)]))

    (send lb set-default-export-file-name "activities.csv")

    ;; This is called when the view is first activated.  This is done to make
    ;; sure all items in this class are defined as callbacks will start to be
    ;; when we set filters.
    (define (restore-visual-layout)

      (send label-input setup-for-session database #f)
      (send equipment-input setup-for-session database #f)
      (send date-range-selector set-seasons (db-get-seasons database))

      (let ((data (get-pref tag (lambda () #f))))
        (when (hash? data)
          (let ((text (hash-ref data 'text-search "")))
            (send text-search-input set-value text))
          (let ((labels (hash-ref data 'labels '())))
            ;; NOTE: set the contents even if they are empty, as this sets the
            ;; available tags, allowing new ones to be added
            (send label-input set-contents-from-tag-ids labels))
          (let ((equipment (hash-ref data 'equipment '())))
            ;; NOTE: set the contents even if they are empty, as this sets the
            ;; available tags, allowing new ones to be added
            (send equipment-input set-contents-from-tag-ids equipment))
          (let ((dr (hash-ref data 'date-range #f)))
            (when dr
              (send date-range-selector restore-from dr)))
          (let ((sp (hash-ref data 'sport #f)))
            (when sp
              (send sport-selector set-selected-sport (car sp) (cdr sp))))
          (let ((dr (hash-ref data 'duration #f)))
            (when (and dr (cons? dr))
              (send duration-input set-number-range (car dr) (cdr dr))))
          (let ((ds (hash-ref data 'distance #f)))
            (when (and ds (cons? ds))
              (send distance-input set-number-range (car ds) (cdr ds))))))
      
      (set! sport-filter (send sport-selector get-selection))
      (set! date-range-filter (send date-range-selector get-selection))
      (set! distance-filter (send distance-input get-number-range))
      (set! duration-filter (send duration-input get-number-range))
      (set! labels-filter (send label-input get-contents-as-tag-ids))
      (set! equipment-filter (send equipment-input get-contents-as-tag-ids))
      (set! text-filter (send text-search-input get-value)))

    (define *activity-list-display-columns*
      (list

       (let ((fn (lambda (row) (db-row-ref row "headline" headers ""))))
         (qcolumn "Activity Name" fn fn))

       (let ((fn (lambda (row)
                   (let ((sport (db-row-ref row "sport" headers 0))
                         (sub-sport (db-row-ref row "sub_sport" headers 0)))
                     (get-sport-name sport sub-sport)))))
         (qcolumn "Sport" fn fn))

       (let ((fn (lambda (row) (db-row-ref row "start_time" headers 0))))
         (qcolumn "Start Time" (lambda (row) (date-time->string (fn row))) fn))

       (let ((fn (lambda (row) (db-row-ref row "duration" headers 0))))
         (qcolumn "Duration"
                  (lambda (row) 
                    (let ((v (fn row)))
                      (if (> v 0) (duration->string v) "")))
                  fn))

       (let ((fn (lambda (row) (db-row-ref row "distance" headers 0))))
         (qcolumn "Distance"
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
         (qcolumn "Speed"
                  (lambda (row)
                    (let ((sport (db-row-ref row "sport" headers 0))
                          (speed (fn row)))
                      (cond ((< speed 0.00002) "") ; approx 0.1 km/h
                            ((= sport 1) (pace->string speed #t))
                            ((= sport 5) (swim-pace->string speed #t))
                            (#t (speed->string speed #t)))))
                  fn))

       (let ((fn (lambda (row) (db-row-ref row "max_speed" headers 0))))
         (qcolumn "Max Speed"
                  (lambda (row)
                    (let ((sport (db-row-ref row "sport" headers 0))
                          (speed (fn row)))
                      (cond ((< speed 0.00002) "") ; approx 0.1 km/h
                            ((= sport 1) (pace->string speed #t))
                            ((= sport 5) (swim-pace->string speed #t))
                            (#t (speed->string speed #t)))))
                  fn))

       (let ((fn (lambda (row) (db-row-ref row "ascent" headers 0))))
         (qcolumn "Ascent"
                  (lambda (row)
                    (let ((v (fn row)))
                      (if (> v 0) (vertical-distance->string v #t) "")))
                  fn))

       (let ((fn (lambda (row) (db-row-ref row "descent" headers 0))))
         (qcolumn "Descent"
                  (lambda (row)
                    (let ((v (fn row)))
                      (if (> v 0) (vertical-distance->string v #t) "")))
                  fn))

       (let ((fn (lambda (row) (db-row-ref row "hr" headers 0))))
         (qcolumn "HR" (lambda (row) (n->string (fn row))) fn))
       
       (let ((fn (lambda (row) (db-row-ref row "max_hr" headers 0))))
         (qcolumn "Max HR" (lambda (row) (n->string (fn row))) fn))

       (let ((fn (lambda (row) (db-row-ref row "hrv" headers 0))))
         (qcolumn "HRV" (lambda (row) (n->string (fn row))) fn))

       (let ((fn (lambda (row) (db-row-ref row "adecl" headers #f))))
         (qcolumn "A Decl"
                  (lambda (row)
                    (let ((val (fn row)))
                      (if val (pct->string val) "")))
                  fn))

       (let ((fn (lambda (row) (db-row-ref row "cadence" headers 0))))
         (qcolumn "Cadence" (lambda (row) (n->string (fn row))) fn))

       (let ((fn (lambda (row) (db-row-ref row "max_cadence" headers 0))))
         (qcolumn "Max Cadence" (lambda (row) (n->string (fn row))) fn))

       (let ((fn (lambda (row) (db-row-ref row "stride" headers 0))))
         (qcolumn "Stride"
                  (lambda (row)
                    (let ((val (fn row)))
                      (if (= val 0) "" (stride->string val #t))))
                  fn))

       (let ((fn (lambda (row) (db-row-ref row "vosc" headers 0))))
         (qcolumn "VOSC"
                  (lambda (row) (vosc->string (fn row) #t))
                  fn))

       (let ((fn (lambda (row)
                   (let ((st (db-row-ref row "stride" headers #f))
                         (vosc (db-row-ref row "vosc" headers #f)))
                     (if (and st vosc (> st 0) (> vosc 0))
                         (* 100.0 (/ vosc (* st 1000)))
                         0)))))
         (qcolumn "VRATIO"
                  (lambda (row)
                    (let ((vratio (fn row)))
                      (if (> vratio 0) (pct->string (fn row)) "")))
                  fn))

       (let ((fn1 (lambda (row) (db-row-ref row "gct" headers 0)))
             (fn2 (lambda (row) (db-row-ref row "gct_pct" headers 0))))
         (qcolumn "GCT"
                  (lambda (row) (stance->string (fn1 row) (fn2 row)))
                  fn1))

       (let ((fn (lambda (row) (db-row-ref row "power" headers 0))))
         (qcolumn "Power"
                  (lambda (row) (let ((v (fn row))) (power->string v #t)))
                  fn))

       (let ((fn (lambda (row) (db-row-ref row "max_power" headers 0))))
         (qcolumn "Max Power"
                  (lambda (row) (let ((v (fn row))) (power->string v #t)))
                  fn))

       (let ((fn (lambda (row) (db-row-ref row "np" headers 0))))
         (qcolumn "Weighted Power"
                  (lambda (row) (let ((v (fn row))) (power->string v #t)))
                  fn))

       (let ((fn (lambda (row) (db-row-ref row "lrbal" headers 0))))
         (qcolumn "L-R Bal"
                  (lambda (row)
                    (let ((v (fn row)))
                      (if (> v 0) (format-48 "~1,1F%" v) "")))
                  fn))

       (let ((fn (lambda (row) (db-row-ref row "ltorqeff" headers 0))))
         (qcolumn "Left TEff"
                  (lambda (row)
                    (let ((v (fn row)))
                      (if (> v 0) (format-48 "~1,1F%" v) "")))
                  fn))

       (let ((fn (lambda (row) (db-row-ref row "rtorqeff" headers 0))))
         (qcolumn "Right TEff"
                  (lambda (row)
                    (let ((v (fn row)))
                      (if (> v 0) (format-48 "~1,1F%" v) "")))
                  fn))

       (let ((fn (lambda (row) (db-row-ref row "lpdlsmth" headers 0))))
         (qcolumn "Left PSmth"
                  (lambda (row)
                    (let ((v (fn row)))
                      (if (> v 0) (format-48 "~1,1F%" v) "")))
                  fn))
       
       (let ((fn (lambda (row) (db-row-ref row "rpdlsmth" headers 0))))
         (qcolumn "Right PSmth"
                  (lambda (row)
                    (let ((v (fn row)))
                      (if (> v 0) (format-48 "~1,1F%" v) "")))
                  fn))

       (let ((fn (lambda (row) (db-row-ref row "lpco" headers #f))))
         (qcolumn "Left PCO"
                  (lambda (row)
                    (let ((v (fn row)))
                      (if v (pco->string v #t) "")))
                  ;; NOTE: sorting on #f is not nice :-)
                  (lambda (row) (or (fn row) -1000))))

       (let ((fn (lambda (row) (db-row-ref row "rpco" headers #f))))
         (qcolumn "Right PCO"
                  (lambda (row)
                    (let ((v (fn row)))
                      (if v (pco->string v #t) "")))
                  ;; NOTE: sorting on #f is not nice :-)
                  (lambda (row) (or (fn row) -1000))))

       (let ((fn1 (lambda (row) (db-row-ref row "lppstart" headers #f)))
             (fn2 (lambda (row) (db-row-ref row "lppend" headers #f))))
         (qcolumn "Left PP"
                  (lambda (row)
                    (let ((start (fn1 row))
                          (end (fn2 row)))
                      (if (and start end) (power-phase->string start end) "")))
                  ;; NOTE: sorting on #f is not nice :-)
                  (lambda (row) (or (fn1 row) -1000))))

       (let ((fn1 (lambda (row) (db-row-ref row "rppstart" headers #f)))
             (fn2 (lambda (row) (db-row-ref row "rppend" headers #f))))
         (qcolumn "Right PP"
                  (lambda (row)
                    (let ((start (fn1 row))
                          (end (fn2 row)))
                      (if (and start end) (power-phase->string start end) "")))
                  ;; NOTE: sorting on #f is not nice :-)
                  (lambda (row) (or (fn1 row) -1000))))

       (let ((fn1 (lambda (row) (db-row-ref row "lpppstart" headers #f)))
             (fn2 (lambda (row) (db-row-ref row "lpppend" headers #f))))
         (qcolumn "Left Peak PP"
                  (lambda (row)
                    (let ((start (fn1 row))
                          (end (fn2 row)))
                      (if (and start end) (power-phase->string start end) "")))
                  ;; NOTE: sorting on #f is not nice :-)
                  (lambda (row) (or (fn1 row) -1000))))

       (let ((fn1 (lambda (row) (db-row-ref row "rpppstart" headers #f)))
             (fn2 (lambda (row) (db-row-ref row "rpppend" headers #f))))
         (qcolumn "Right Peak PP"
                  (lambda (row)
                    (let ((start (fn1 row))
                          (end (fn2 row)))
                      (if (and start end) (power-phase->string start end) "")))
                  ;; NOTE: sorting on #f is not nice :-)
                  (lambda (row) (or (fn1 row) -1000))))

       (let ((fn (lambda (row) (db-row-ref row "calories" headers 0))))
         (qcolumn "Calories" (lambda (row) (n->string (fn row))) fn))

       (let ((fn (lambda (row) (db-row-ref row "te" headers 0))))
         (qcolumn "Training Effect"
                  (lambda (row) 
                    (let ((v (fn row)))
                      (if (> v 0) (format-48 "~1,1F" v) "")))
                  fn))
       
       (let ((fn (lambda (row) (db-row-ref row "rpe" headers 0))))
         (qcolumn "RPE"
                  (lambda (row) 
                    (let ((v (fn row)))
                      (if (> v 0) (format-48 "~1F" v) "")))
                  fn))

       (let ((fn (lambda (row) (db-row-ref row "tss" headers 0))))
         (qcolumn "Effort"
                  (lambda (row) 
                    (let ((v (fn row)))
                      (if (> v 0) (format-48 "~1,1F" v) "")))
                  fn))
       (let ((fn (lambda (row) (db-row-ref row "ifact" headers 0))))
         (qcolumn "Intensity"
                  (lambda (row) 
                    (let ((v (fn row)))
                      (if (> v 0) (format-48 "~1,1F" v) "")))
                  fn))

       (let ((fn (lambda (row) (db-row-ref row "body_weight" headers 0))))
         (qcolumn "Body Weight"
                  (lambda (row) (weight->string (fn row) #t))
                  fn))

       (let ((fn (lambda (row) (db-row-ref row "temperature" headers -1000))))
         (qcolumn "Temperature"
                  (lambda (row) (temperature->string (fn row) #t))
                  fn))

       (let ((fn (lambda (row) (db-row-ref row "dew_point" headers -1000))))
         (qcolumn "Dew Point"
                  (lambda (row) (temperature->string (fn row) #t))
                  fn))

       (let ((fn (lambda (row) (db-row-ref row "humidity" headers 0))))
         (qcolumn "Humidity"
                  (lambda (row) (humidity->string (fn row) #t))
                  fn))
       
       (let ((fn1 (lambda (row) (db-row-ref row "wind_speed" headers 0)))
             (fn2 (lambda (row) (db-row-ref row "wind_direction" headers 0))))
         (qcolumn "Wind"
                  (lambda (row) (wind->string (fn1 row) (fn2 row)))
                  fn1))
       
       (let ((fn (lambda (row) 
                   (let ((temp (db-row-ref row "temperature" headers #f))
                         (dewp (db-row-ref row "dew_point" headers #f)))
                     (if (and temp dewp) (humindex temp dewp) -1000)))))
         (qcolumn "Humindex"
                  (lambda (row) (temperature->string (fn row) #t))
                  fn))

       (let ((fn (lambda (row) (db-row-ref row "activity_guid" headers ""))))
         (qcolumn "Activity-Guid" fn fn))

       (let ((fn (lambda (row) (db-row-ref row "session_id" headers 0))))
         (qcolumn "Session-Id"
                  (lambda (row) (format "~a" (fn row)))
                  fn))

       ))

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

    (define inhibit-updates? #f)

    (define (with-inhibit-updates thunk)
      (set! inhibit-updates? #f)
      (with-handlers
        (((lambda (e) #t)
          (lambda (e) (set! inhibit-updates? #f) (raise e))))
        (thunk)))

    (define (on-filter-changed)
      (unless inhibit-updates?
        (let ((result (get-activity-list
                       database sport-filter date-range-filter
                       distance-filter duration-filter labels-filter equipment-filter)))
          (set! data (rows-result-rows result))
          (set! headers (make-col-name->col-id-hash (rows-result-headers result))))
        (on-text-filter-changed)))

    (define (on-text-filter-changed)
      (let ((rows (get-text-filtered-data data text-filter)))
        (send lb set-data rows)
        (send (send pane get-top-level-window) 
              set-status-text 
              (make-activity-summary-label rows headers))
        (when (null? rows)
          (let ((nsessions (query-value database "select count(*) from A_SESSION")))
            (unless (or (sql-null? nsessions) (zero? nsessions))
              (notify-user 'info "current filter does not select any activities (there are ~a activities in the database)"
                           nsessions))))))

    (define first-time? #t)

    (define (row-index-for-sid sid)
      (for/or ([pos (in-range (send lb get-row-count))])
        (let ((data (send lb get-data-for-row pos)))
          (if (and data (= sid (db-row-ref data "session_id" headers 0)))
              pos #f))))

    (define (maybe-delete sid)
      (set! data (remove sid data
                         (lambda (sid item)
                           (= sid (db-row-ref item "session_id" headers 0)))))
      (let ((index (row-index-for-sid sid)))
        (when index
          (send lb delete-row index))))

    (define (maybe-update sid)
      (define row-data #f)
      (when (member sid data (lambda (sid item)
                               (= sid (db-row-ref item "session_id" headers 0))))
        (set! row-data (get-activity-list-1 database sid))
        (set! data
              (for/list ((item data))
                (if (= sid (db-row-ref item "session_id" headers 0))
                    row-data
                    item)))
        (let ((index (row-index-for-sid sid)))
          (send lb update-row index row-data #f))))
    
    (define/public (activated)
      ;; Get the full list of events, but we will discard them if the view is
      ;; activated the first time and has to do a full refresh anyway
      (define events (collect-events change-notification-source))
      (if first-time?
          (begin
            (restore-visual-layout)
            (send lb setup-column-defs *activity-list-display-columns*)
            (refresh)
            (set! first-time? #f))
          (begin
            ;; Process changes that happened while we were inactive
            (for ((sid (hash-ref events 'session-deleted '())))
              (maybe-delete sid))
            (for ((sid (remove-duplicates
                        (append*
                         (hash-ref events 'session-updated '())
                         (hash-ref events 'weather-data-changed '())
                         ;; Athlete metrics updates also list sessions which
                         ;; might be affected by their change as part of their
                         ;; data.
                         (for/list ((aid
                                     (append
                                      (hash-ref events 'athlete-metrics-created '())
                                      (hash-ref events 'athlete-metrics-updated '())
                                      (hash-ref events 'athlete-metrics-deleted '()))))
                           (if (null? aid) '() (cdr aid)))))))
              (maybe-update sid))
            (let ((new-sids (hash-ref events 'session-created #f)))
              (when new-sids
                ;; Check if any of the new sessions are present in the
                ;; dataset, if they are, update the view.
                (let* ((result (get-activity-list
                                database sport-filter date-range-filter
                                distance-filter duration-filter
                                labels-filter equipment-filter))
                       (ndata (rows-result-rows result))
                       (nheaders (make-col-name->col-id-hash
                                  (rows-result-headers result))))
                  (when (for/first
                            ((item ndata)
                             #:when (member
                                     (db-row-ref item "session_id" nheaders 0)
                                     new-sids))
                          #t)
                    (set! data ndata)
                    (set! headers nheaders)
                    (on-text-filter-changed)))))))
      
      ;; Set the status text when activated
      (let ((rows (get-text-filtered-data data text-filter)))
        (send (send pane get-top-level-window) 
              set-status-text 
              (make-activity-summary-label rows headers))))
    
    (define/public (refresh)
      (with-inhibit-updates
        (lambda ()
          ;; NOTE: calling 'setup-for-session' on the tag-input-field% objects
          ;; causes all tags to be erased and, via the callback, our own tag
          ;; lists too.  The code below calls 'setup-for-session' while
          ;; preserving any tags that are already set.
          (let ((c (send label-input get-contents-as-tag-ids)))
            (send label-input setup-for-session database #f)
            (send label-input set-contents-from-tag-ids c)
            (set! labels-filter c))
          (let ((c (send equipment-input get-contents-as-tag-ids)))
            (send equipment-input setup-for-session database #f)
            (send equipment-input set-contents-from-tag-ids c)
            (set! equipment-filter c))
          (send date-range-selector set-seasons (db-get-seasons database))
          ;; Refresh the date range (in case the selection is something like
          ;; "last 30 days" and the date has changed.
          (set! date-range-filter (send date-range-selector get-selection))
          (collect-events change-notification-source) ; discard the events
          (on-filter-changed))))

    (define/public (save-visual-layout)
      (let ((data (hash
                   'sport (send sport-selector get-selection)
                   'date-range (send date-range-selector get-restore-data)
                   'distance (send distance-input get-number-range)
                   'duration (send duration-input get-number-range)
                   'labels (send label-input get-contents-as-tag-ids)
                   'equipment (send equipment-input get-contents-as-tag-ids)
                   'text-search (send text-search-input get-value))))
        (put-pref tag data))
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
    (define/public (get-selected-sport)
      (when selected-row-index
        (let ((data (send lb get-data-for-row selected-row-index)))
          (if data
              (cons (db-row-ref data "sport" headers 0)
                    (db-row-ref data "sub_sport" headers 0))
              #f))))
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
      (activated))
    (define/public (after-new sid)
      (activated))
    (define/public (can-delete? sid)
      (number? selected-row-index))
    (define/public (after-delete sid)
      (activated))
    (define/public (before-popup)
      (set! selected-row-index (send lb get-selected-row-index)))
    (define/public (after-popdown)
      (set! selected-row-index #f))


    ))
