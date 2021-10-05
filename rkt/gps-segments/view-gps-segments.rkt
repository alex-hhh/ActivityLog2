#lang racket/base

;; SPDX-License-Identifier: GPL-3.0-or-later
;; view-gps-segments.rkt -- the GPS segments view on the side panel
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

(require data-frame
         data-frame/gpx
         db/base
         (only-in framework
                  panel:vertical-dragable%
                  panel:horizontal-dragable%)
         racket/class
         racket/dict
         racket/format
         racket/gui/base
         racket/match
         (only-in "../../rkt/utilities.rkt"
                  get-pref
                  put-pref
                  log-event
                  collect-events
                  make-log-event-source)
         "../dbutil.rkt"
         "../fmt-util-ut.rkt"
         "../fmt-util.rkt"
         "../models/elevation-correction.rkt"
         "../session-df/session-df.rkt"
         "../sport-charms.rkt"
         "../widgets/icon-resources.rkt"
         "../widgets/qresults-list.rkt"
         "../widgets/widget-utilities.rkt"
         "gps-segments-dialogs.rkt"
         "gps-segments.rkt"
         "map-and-elevation-widget.rkt")

(provide view-gps-segments%
         gps-segment-operations<%>
         gps-segment-operations-menu%)

;; Fetch all GPS segment information from the database DB.  The query columns
;; have to match the column definitions in *gps-segment-display-columns*.
;; This is used to fetch data for the segments list view.
(define (get-all-segments db)
  (query-rows
   db
   "select GS.id, GS.name, GS.segment_length, GS.segment_height, GS.segment_grade, GS.total_ascent, GS.total_descent, GS.max_grade, GS.min_elevation, GS.max_elevation from GPS_SEGMENT GS"))

;; Fetch information about the GPS segment identified by SEGMENT-ID from the
;; database DB.  The returned rows have to match the `get-all-segments` query,
;; as this function is used to update one row in the segments list view.
(define (get-one-segment db segment-id)
  (query-row
   db
   "select GS.id, GS.name, GS.segment_length, GS.segment_height, GS.segment_grade, GS.total_ascent, GS.total_descent, GS.max_grade, GS.min_elevation, GS.max_elevation from GPS_SEGMENT GS where GS.id = ?" segment-id))

;; Convenience function to fetch the segment name from the database.
(define (get-segment-headline db segment-id)
  (query-maybe-value
   db "select ifnull(GS.name, 'Segment ID ' || GS.id) from GPS_SEGMENT GS where GS.id = ?" segment-id))

;; Convenience function to construct a `qcolumn` entry for the segments list
;; view.
(define (make-qcolumn name column-index default-value formatter
                      #:default-visible? (default-visible? #t))
  (let ([fn (lambda (row) (sql-column-ref row column-index default-value))])
    (qcolumn name
             (lambda (row)
               (let ([val (fn row)])
                 (if val (formatter val) "")))
             fn
             #:default-visible? default-visible?)))

;; These are the column definitions for the segments list view.
(define *gps-segment-display-columns*
  (let ([d->s (lambda (v) (distance->string v #t))]
        [vd->s (lambda (v) (vertical-distance->string v #t))]
        [pct->s (lambda (v) (string-append (~r v #:precision 1) " %"))])
    (list
     (make-qcolumn "Name" 1 #f ~a)
     (make-qcolumn "Length" 2 #f d->s)
     (make-qcolumn "Height" 3 #f vd->s)
     (make-qcolumn "Grade (%)" 4 #f pct->s)
     (make-qcolumn "Ascent" 5 #f vd->s)
     (make-qcolumn "Descent" 6 #f vd->s)
     (make-qcolumn "Max Grade" 7 #f pct->s)
     (make-qcolumn "Min Elevation" 8 #f vd->s #:default-visible? #f)
     (make-qcolumn "Max Elevation" 9 #f vd->s #:default-visible? #f))))

;; Return all match information for the segment SEGMENT-ID from the database.
;; qresults-list% columns for the match view are constructed using named
;; lookups, so the column order does not matter.
(define (get-matches-for-segment db segment-id)
  (query
   db
   "select * from V_GPS_SEGMENT_MATCH_LIST where segment_id = ?" segment-id))

;; Index all the columns from an SQL query, returns a hash mapping the column
;; name to the position in each row of the result set.  RESULT is a query
;; result as returned by `query`
(define (column-name->index-hash result)
  (for/hash ([column (rows-result-headers result)]
             [index (in-naturals)])
    (values (dict-ref column 'name) index)))


;;............................................ gps-segment-operations<%> ....

;; Interface containing the basic methods used to implement the operations
;; listed in the "GPS Segments..." menu.  They are implemented by
;; view-gps-segments%, but toplevel.rkt contains a forwarder with this
;; interface as well.
(define gps-segment-operations<%>
  (interface ()
    get-top-level-window
    get-database
    before-popup
    after-popdown
    switch-to-view
    inspect-session

    get-selected-segment
    get-selected-match

    after-new
    after-delete
    after-update-summary
    after-update-data
    after-update-matches))


;;................................................... view-gps-segments% ....

;; This is the GPS segments view, implementing the GPS Segments tab in the
;; application.
(define view-gps-segments%
  (class* object% (gps-segment-operations<%>)
    (init-field parent database select-activity-callback)
    (super-new)

    (define pref-tag 'al2-view-gps-segments)
    (define gui-prefs (get-pref pref-tag (lambda () (hash))))
    (define the-toplevel (new vertical-pane% [parent parent]))

    (define change-notification-source (make-log-event-source))

    (let ((sel-pane (new horizontal-pane% [parent the-toplevel]
                         [spacing 20]
                         [border 0]
                         [stretchable-height #f]
                         [stretchable-width #t]
                         [alignment '(left center)])))
      (make-spacer sel-pane)
      (new message% [parent sel-pane] [label (waypoints-icon)]))

    (define hcontents
      (new panel:horizontal-dragable%
           [parent the-toplevel]))

    (define left-panel
      (new panel:vertical-dragable%
           [parent hcontents]))

    (define segments-panel (make-vertical-pane left-panel))
    (let ((p (make-horizontal-pane segments-panel #f)))
      (make-spacer p)
      (new message%
           [parent p]
           [stretchable-height #f]
           [stretchable-width #f]
           [font (send the-font-list find-or-create-font 16 'default 'normal 'normal)]
           [label "GPS Segments"]))

    (define segment-lv
      (new (class qresults-list%
             (init)(super-new)
             (define/override (on-select row row-data)
               (on-segment-selected row-data)))
           [parent segments-panel]
           [pref-tag 'al2-view-gps-segments:segment-list]))

    (define matches-panel (make-vertical-pane left-panel))
    (let ((p (make-horizontal-pane matches-panel #f)))
      (make-spacer p)
      (new message%
           [parent p]
           [stretchable-height #f]
           [stretchable-width #f]
           [font (send the-font-list find-or-create-font 16 'default 'normal 'normal)]
           [label "Matches for Segment"]))

    (define match-lv
      (new (class qresults-list%
             (init)(super-new)
             (define/override (on-double-click row row-data)
               (and select-activity-callback
                    (select-activity-callback (column-ref-by-name row-data "session_id" #f))))
             (define/override (on-select row row-data)
               (on-match-selected row-data)))
           [parent matches-panel]
           [pref-tag 'al2-view-gps-segments:match-list]))

    (define the-map-and-elevation
      (new map-and-elevation-widget%
           [parent hcontents]
           [pref-tag 'al2-view-gps-segments:map-and-elevation]))

    (send hcontents set-percentages
          (hash-ref gui-prefs 'panel-map-split (lambda () '(1/3 2/3))))

    (send left-panel set-percentages
          (hash-ref gui-prefs 'left-panel-split (lambda () '(1/2 1/2))))

    ;; Headers for the match list view, constructed by column-name->index-hash
    (define headers (hash))

    ;; Reference a column by name from ROW.  This uses the `headers` variable
    ;; above to map the column name to a position in ROW.  This is used for
    ;; the match list view.
    (define/private (column-ref-by-name row column-name [if-null? #f])
      (define index (hash-ref headers column-name
                              (lambda ()
                                (raise (format "column not found: ~a" column-name)))))
      (let ((v (vector-ref row index)))
        (if (sql-null? v) if-null? v)))

    ;; Make a `qcolumn` entry for the match list view.
    (define/private (make-mcolumn name column-name default-value formatter
                                  #:default-visible? (default-visible? #t))
      (let ([fn (lambda (row) (column-ref-by-name row column-name default-value))])
        (qcolumn name
                 (lambda (row)
                   (let ([val (fn row)])
                     (if (equal? val default-value)
                         ""
                         (formatter val))))
                 fn
                 #:default-visible? default-visible?)))

    ;; Make a `qcolumn` entry for the match list view for formatting local
    ;; times, which take the timezone into account.
    (define/private (make-mcolumn/time name column-name #:default-visible? (default-visible? #t))
      (qcolumn name
               (lambda (row)
                 (let ([start-time (column-ref-by-name row column-name 0)]
                       [time-zone (column-ref-by-name row "time_zone" #f)])
                   (date-time->string start-time #:include-seconds? #t #:time-zone time-zone)))
               (lambda (row)
                 (column-ref-by-name row column-name 0))
               #:default-visible? default-visible?))

    ;; Column definitions for the match list view.
    (define match-lv-columns
      (list (make-mcolumn "Session Name" "headline" "" ~a #:default-visible? #t)
            (make-mcolumn/time "Start Time (session)" "session_start_time" #:default-visible? #f)
            (make-mcolumn/time "Start Time (segment)" "segment_start_time" #:default-visible? #t)
            (make-mcolumn/time "End Time (segment)" "segment_end_time" #:default-visible? #f)
            (make-mcolumn "Match Cost" "match_cost" #f n->string #:default-visible? #f)
            (let ((fn (lambda (row)
                        (let ((sport (column-ref-by-name row "sport" 0))
                              (sub-sport (column-ref-by-name row "sub_sport" 0)))
                          (get-sport-name sport sub-sport)))))
              (qcolumn "Sport" fn fn #:default-visible? #t))
            (make-mcolumn "Duration" "duration" 0 duration->string #:default-visible? #t)
            (make-mcolumn "Distance" "distance" 0 distance->string #:default-visible? #f)
            (let ((fn (lambda (row) (column-ref-by-name row "speed" 0))))
              (qcolumn "Speed"
                       (lambda (row)
                         (let ((sport (column-ref-by-name row "sport" 0))
                               (speed (fn row)))
                           (cond ((< speed 0.00002) "") ; approx 0.1 km/h
                                 ((= sport 1) (pace->string speed #t))
                                 ((= sport 5) (swim-pace->string speed #t))
                                 (#t (speed->string speed #t)))))
                       fn
                  #:default-visible? #t))
            (let ((fn (lambda (row) (column-ref-by-name row "max_speed" 0))))
              (qcolumn "Max Speed"
                  (lambda (row)
                    (let ((sport (column-ref-by-name row "sport" 0))
                          (speed (fn row)))
                      (cond ((< speed 0.00002) "") ; approx 0.1 km/h
                            ((= sport 1) (pace->string speed #t))
                            ((= sport 5) (swim-pace->string speed #t))
                            (#t (speed->string speed #t)))))
                  fn
                  #:default-visible? #f))
            (make-mcolumn "HR" "hr" 0 n->string #:default-visible? #t)
            (make-mcolumn "Max HR" "max_hr" 0 n->string #:default-visible? #f)
            (make-mcolumn "Cadence" "cadence" 0 n->string #:default-visible? #f)
            (make-mcolumn "Max Cadence" "max_cadence" 0 n->string #:default-visible? #f)
            (make-mcolumn "Stride" "stride" 0 stride->string #:default-visible? #f)
            (make-mcolumn "Ascent" "ascent" #f vertical-distance->string #:default-visible? #t)
            (make-mcolumn "Descent" "descent" #f vertical-distance->string #:default-visible? #f)
            (make-mcolumn "VOSC" "vosc" #f vosc->string #:default-visible? #f)
            (let ((fn1 (lambda (row) (column-ref-by-name row "gct" 0)))
                  (fn2 (lambda (row) (column-ref-by-name row "gct_pct" 0))))
              (qcolumn "GCT"
                       (lambda (row) (stance->string (fn1 row) (fn2 row)))
                       fn1
                       #:default-visible? #f))
            (make-mcolumn "Power" "power" #f power->string #:default-visible? #t)
            (make-mcolumn "Max Power" "max_power" #f power->string #:default-visible? #f)
            (make-mcolumn "Iso Power" "np" #f power->string #:default-visible? #t)

            (let ((fn (lambda (row) (column-ref-by-name row "lrbal" 0))))
              (qcolumn "L-R Bal"
                  (lambda (row)
                    (let ((v (fn row)))
                      (if (> v 0) (~r v #:precision 1) "")))
                  fn
                  #:default-visible? #f))

            (let ((fn (lambda (row) (column-ref-by-name row "ltorqeff" 0))))
              (qcolumn "Left TEff"
                       (lambda (row)
                         (let ((v (fn row)))
                           (if (> v 0) (~r v #:precision 1) "")))
                       fn
                       #:default-visible? #f))

            (let ((fn (lambda (row) (column-ref-by-name row "rtorqeff" 0))))
              (qcolumn "Right TEff"
                       (lambda (row)
                         (let ((v (fn row)))
                           (if (> v 0) (~r v #:precision 1) "")))
                       fn
                       #:default-visible? #f))

            (let ((fn (lambda (row) (column-ref-by-name row "lpdlsmth" 0))))
              (qcolumn "Left PSmth"
                       (lambda (row)
                         (let ((v (fn row)))
                           (if (> v 0) (~r v #:precision 1) "")))
                       fn
                       #:default-visible? #f))

            (let ((fn (lambda (row) (column-ref-by-name row "rpdlsmth" 0))))
              (qcolumn "Right PSmth"
                       (lambda (row)
                         (let ((v (fn row)))
                           (if (> v 0) (~r v #:precision 1) "")))
                       fn
                       #:default-visible? #f))

            (let ((fn (lambda (row) (column-ref-by-name row "lpco" #f))))
              (qcolumn "Left PCO"
                       (lambda (row)
                         (let ((v (fn row)))
                           (if v (pco->string v #t) "")))
                       ;; NOTE: sorting on #f is not nice :-)
                       (lambda (row) (or (fn row) -1000))
                       #:default-visible? #f))

            (let ((fn (lambda (row) (column-ref-by-name row "rpco" #f))))
              (qcolumn "Right PCO"
                       (lambda (row)
                         (let ((v (fn row)))
                           (if v (pco->string v #t) "")))
                       ;; NOTE: sorting on #f is not nice :-)
                       (lambda (row) (or (fn row) -1000))
                       #:default-visible? #f))

            (let ((fn1 (lambda (row) (column-ref-by-name row "lppstart" #f)))
                  (fn2 (lambda (row) (column-ref-by-name row "lppend" #f))))
              (qcolumn "Left PP"
                       (lambda (row)
                         (let ((start (fn1 row))
                               (end (fn2 row)))
                           (if (and start end) (power-phase->string start end) "")))
                       ;; NOTE: sorting on #f is not nice :-)
                       (lambda (row) (or (fn1 row) -1000))
                       #:default-visible? #f))

            (let ((fn1 (lambda (row) (column-ref-by-name row "rppstart" #f)))
                  (fn2 (lambda (row) (column-ref-by-name row "rppend" #f))))
              (qcolumn "Right PP"
                       (lambda (row)
                         (let ((start (fn1 row))
                               (end (fn2 row)))
                           (if (and start end) (power-phase->string start end) "")))
                       ;; NOTE: sorting on #f is not nice :-)
                       (lambda (row) (or (fn1 row) -1000))
                       #:default-visible? #f))

            (let ((fn1 (lambda (row) (column-ref-by-name row "lpppstart" #f)))
                  (fn2 (lambda (row) (column-ref-by-name row "lpppend" #f))))
              (qcolumn "Left Peak PP"
                       (lambda (row)
                         (let ((start (fn1 row))
                               (end (fn2 row)))
                           (if (and start end) (power-phase->string start end) "")))
                       ;; NOTE: sorting on #f is not nice :-)
                       (lambda (row) (or (fn1 row) -1000))
                       #:default-visible? #f))

            (let ((fn1 (lambda (row) (column-ref-by-name row "rpppstart" #f)))
                  (fn2 (lambda (row) (column-ref-by-name row "rpppend" #f))))
              (qcolumn "Right Peak PP"
                       (lambda (row)
                         (let ((start (fn1 row))
                               (end (fn2 row)))
                           (if (and start end) (power-phase->string start end) "")))
                       ;; NOTE: sorting on #f is not nice :-)
                       (lambda (row) (or (fn1 row) -1000))
                       #:default-visible? #f))

            (make-mcolumn "A Decl" "adecl" #f pct->string #:default-visible? #t)
            (make-mcolumn "Avg Tempe" "avg_temperature" #f pct->string #:default-visible? #f)
            (make-mcolumn "Max Tempe" "max_temperature" #f pct->string #:default-visible? #f)))

    ;; Called when a new segment is created -- updates the segment list view
    ;; and selects the segment.
    (define/private (on-segment-created segment-id)
      (define row (get-one-segment database segment-id))
      (send segment-lv add-row row)
      (on-segment-selected row))

    ;; Called when a segment is deleted -- removes the segment from the list
    ;; view.
    (define/private (on-segment-deleted segment-id)
      (define index (row-index-for-gsid segment-id))
      (define selected-index (send segment-lv get-selected-row-index))
      (when index
        (when (equal? index selected-index)
          (on-segment-selected #f))
        (send segment-lv delete-row index)))

    ;; Called when a segment is updated (e.g. its name changes) -- updates the
    ;; list view with the new data.
    (define/public (on-segment-updated segment-id data-updated?)
      (define index (row-index-for-gsid segment-id))
      (define selected-index (send segment-lv get-selected-row-index))
      (when index
        (define row (get-one-segment database segment-id))
        (send segment-lv update-row index row #f)
        (when (and data-updated? (equal? index selected-index))
          (on-segment-selected row))))

    (define first-time? #t)

    (define/public (activated)
      ;; Get the full list of events, but we will discard them if the view is
      ;; activated the first time and has to do a full refresh anyway
      (define events (collect-events change-notification-source))

      (if first-time?
          (begin
            (send segment-lv setup-column-defs *gps-segment-display-columns*)
            (send match-lv setup-column-defs match-lv-columns)
            (refresh)
            (let ([segment-id (hash-ref gui-prefs 'selected-segment-id (lambda () #f))]
                  [match-id (hash-ref gui-prefs 'selected-match-id (lambda () #f))])
              (when segment-id
                (define index (row-index-for-gsid segment-id))
                (when index
                  (send segment-lv select-row index)))
              (queue-callback
               (lambda ()
                 (when match-id
                   (define index (row-index-for-mid match-id))
                   (when index
                     (send match-lv select-row index)
                     (on-match-selected (send match-lv get-data-for-row index)))))
               #f))
            (set! first-time? #f))
          (let ()
            ;; process changes that happened while we were inactive
            (for ([gsid (hash-ref events 'gps-segment-created '())])
              (on-segment-created gsid))
            (for ([gsid (hash-ref events 'gps-segment-deleted '())])
              (on-segment-deleted gsid))
            (define updated-segments (hash-ref events 'gps-segment-updated-data '()))
            (for ([gsid (in-list updated-segments)])
              (on-segment-updated gsid #t))
            (for ([gsid (hash-ref events 'gps-segment-updated '())]
                  #:unless (member gsid updated-segments))
              (on-segment-updated gsid #f))

            ;; If the current selected segment has new matches, refresh it.
            (let* ([index (send segment-lv get-selected-row-index)]
                   [data (and index (send segment-lv get-data-for-row index))]
                   [segment-id (and data (vector-ref data 0))])
              (when (member segment-id (hash-ref events 'gps-segment-updated-matches '()))
                (on-segment-updated segment-id #f))))))

    (define/private (on-segment-selected row-data)
      (if row-data
          (let ([gsid (vector-ref row-data 0)])
            (define segment (gps-segment-df database gsid))
            (send the-map-and-elevation set-segment segment)
            (define matches (get-matches-for-segment database gsid))
            (set! headers (column-name->index-hash matches))
            (send match-lv set-data (rows-result-rows matches)))
          (begin
            (send match-lv clear)
            (send the-map-and-elevation set-segment #f))))

    (define/private (on-match-selected row-data)
      (define gsid (column-ref-by-name row-data "session_id" #f))
      (define start (column-ref-by-name row-data "segment_start_time" #f))
      (define end (column-ref-by-name row-data "segment_end_time" #f))
      ;; NOTE: these should really be present and we should error out if they are not
      (when (and gsid start end)
        (send the-map-and-elevation set-session-match (session-df database gsid) start end)))

    (define/public (refresh)
      (define selected-gsid
        (let ([index (send segment-lv get-selected-row-index)])
          (and index
               (let ([data (send segment-lv get-data-for-row index)])
                 (vector-ref data 0)))))
      (define selected-mid
        (let ([index (send match-lv get-selected-row-index)])
          (and index
               (let ([data (send match-lv get-data-for-row index)])
                 (column-ref-by-name data "match_id" #f)))))
      (send segment-lv clear)
      (send match-lv clear)
      (send segment-lv set-data (get-all-segments database))
      (when selected-gsid
        (define new-gsid-index (row-index-for-gsid selected-gsid))
        (when new-gsid-index
          (send segment-lv select-row new-gsid-index)
          (queue-callback
           (lambda ()
             (when selected-mid
               (define new-mid-index (row-index-for-mid selected-mid))
               (when new-mid-index
                 (send match-lv select-row new-mid-index)
                 (on-match-selected (send match-lv get-data-for-row new-mid-index)))))
           #f))))

    (define/public (save-visual-layout)
      (define prefs
        (hash
         'panel-map-split (send hcontents get-percentages)
         'left-panel-split (send left-panel get-percentages)
         'selected-segment-id
         (let* ([index (send segment-lv get-selected-row-index)]
                [data (and index (send segment-lv get-data-for-row index))])
           (and data (vector-ref data 0)))
         'selected-match-id
         (let* ([index (send match-lv get-selected-row-index)]
                [data (and index (send match-lv get-data-for-row index))])
           (and data (column-ref-by-name data "match_id" #f)))))
      (put-pref pref-tag prefs)
      (send segment-lv save-visual-layout)
      (send match-lv save-visual-layout)
      (send the-map-and-elevation save-visual-layout))

    (define/private (row-index-for-gsid gsid)
      (for/or ([pos (in-range (send segment-lv get-row-count))])
        (let ([data (send segment-lv get-data-for-row pos)])
          (and data (equal? gsid (vector-ref data 0)) pos))))

    (define/private (row-index-for-mid mid)
      (for/or ([pos (in-range (send match-lv get-row-count))])
        (let ([data (send match-lv get-data-for-row pos)])
          (and data (equal? mid (column-ref-by-name data "match_id" #f)) pos))))


    ;;............................................ gps-segment-operations<%> ....

    (define selected-segment-row-index #f)
    (define selected-match-row-index #f)

    (define/public (get-top-level-window)
      (send the-toplevel get-top-level-window))

    (define/public (get-database)
      database)

    (define/public (before-popup)
      (set! selected-segment-row-index (send segment-lv get-selected-row-index))
      (set! selected-match-row-index (send match-lv get-selected-row-index)))

    (define/public (after-popdown)
      (set! selected-segment-row-index #f)
      (set! selected-match-row-index #f))

    (define/public (switch-to-view)
      ;; this method is actually implemented in the forwarder in toplevel.rkt,
      ;; and the call never reaches us.
      #f)

    (define/public (inspect-session sid)
      ;; this method is actually implemented in the forwarder in toplevel.rkt,
      ;; and the call never reaches us.
      (void))

    (define/public (get-selected-segment)
      (and selected-segment-row-index
           (let ([data (send segment-lv get-data-for-row selected-segment-row-index)])
             (vector-ref data 0))))

    (define/public (get-selected-match)
      (and selected-match-row-index
           (let ([data (send match-lv get-data-for-row selected-match-row-index)])
             (list
              (column-ref-by-name data "match_id" #f)
              (column-ref-by-name data "session_id" #f)
              (column-ref-by-name data "segment_id" #f)
              (column-ref-by-name data "segment_start_time" #f)
              (column-ref-by-name data "segment_end_time" #f)))))

    (define/public (after-new segment-id)
      (activated))

    (define/public (after-delete segment-id)
      (activated))

    (define/public (after-update-summary segment-id)
      (activated))

    (define/public (after-update-data segment-id)
      (activated))

    (define/public (after-update-matches segment-id)
      (activated))

    ))



;;......................................... gps-segment-operations-menu% ....

;; This class implements the operations on segments, available in the "GPS
;; Segments" menu.
(define gps-segment-operations-menu%
  (class object%
    (init-field target)
    (init [menu-bar #f])

    (unless (is-a? target gps-segment-operations<%>)
      (error "Target must implement the gps-segment-operations<%> interface"))

    (super-new)

    (define (on-demand m)
      (send target before-popup)
      (let ([have-gsid? (number? (send target get-selected-segment))]
            [have-mid? (list? (send target get-selected-match))])
        (send delete-menu enable have-gsid?)
        (send create-by-reversing-menu enable have-gsid?)
        (send rescan-for-matches-menu enable have-gsid?)
        (send export-segment-menu enable have-gsid?)
        (send rename-segment-menu enable have-gsid?)
        (send fixup-elevation-menu enable have-gsid?)
        (send visit-session-menu enable have-mid?)
        (send export-match-menu enable have-mid?)))

    (define (on-popdown m e)
      (send target after-popdown))

    (define (on-switch-to-view m e)
      (send target switch-to-view))

    (define (on-create-from-gpx m e)
      (let ((db (send target get-database))
            (toplevel (send target get-top-level-window)))
        (define file-name (get-file "Read GPX file" toplevel #f #f "gpx" '()
                                    '(("GPX Files" "*.gpx") ("Any" "*.*"))))
        (when file-name
          (define segment (gps-segment-from-gpx file-name))
          (define dlg (get-create-segment-dialog))
          (define gsid (send dlg show-dialog toplevel segment db))
          (when gsid
            (log-event 'gps-segment-created gsid)
            (send target after-new gsid)))))

    (define (on-create-by-reversing m e)
      (let ((db (send target get-database))
            (toplevel (send target get-top-level-window))
            (segment-id (send target get-selected-segment)))
        (define original (gps-segment-df db segment-id))
        (define reversed (gps-segment-by-reversing original))
        (define dlg (get-create-segment-dialog))
        (define gsid (send dlg show-dialog toplevel reversed db))
        (when gsid
          (log-event 'gps-segment-created gsid)
          (send target after-new gsid))))

    (define (on-rename-segment m e)
      (let ((db (send target get-database))
            (toplevel (send target get-top-level-window))
            (segment-id (send target get-selected-segment)))
        (define segment (fetch-gps-segment db segment-id))
        (when (send (get-rename-segment-dialog) show-dialog toplevel segment)
          (define name (df-get-property segment 'name #f))
          (query-exec db "update GPS_SEGMENT set name = ? where id = ?" name segment-id)
          (log-event 'gps-segment-updated segment-id)
          (send target after-update-summary segment-id))))

    (define (on-delete-segment m e)
      (let ((db (send target get-database))
            (toplevel (send target get-top-level-window))
            (segment-id (send target get-selected-segment)))
        (let ((mresult (message-box/custom
                        "Confirm delete"
                        (format "Really delete segment \"~a\"?~%This cannot be undone."
                                (get-segment-headline db segment-id))
                          #f "Delete" "Cancel" toplevel '(caution default=3))))
          (when (equal? mresult 2)
            (delete-gps-segment db segment-id)
            (log-event 'gps-segment-deleted segment-id)
            (send target after-delete segment-id)))))

    (define (on-fixup-elevation m e)
      (let ((db (send target get-database))
            (toplevel (send target get-top-level-window))
            (segment-id (send target get-selected-segment)))
        (interactive-fixup-elevation-for-segment db segment-id toplevel)
        (log-event 'gps-segment-updated segment-id)
        (log-event 'gps-session-updated-data segment-id)
        (send target after-update-data segment-id)))

    (define (on-rescan-for-matches m e)
      (let ((db (send target get-database))
            (toplevel (send target get-top-level-window))
            (segment-id (send target get-selected-segment)))
        (define segment (gps-segment-df db segment-id))
        (when (send (get-rescan-for-matches-dialog) show-dialog toplevel segment segment-id db)
          ;; NOTE: should we flood the event system with a
          ;; 'gps-segment-match-created and 'gps-segment-match-deleted
          ;; messages?
          (log-event 'gps-segment-updated-matches segment-id)
          (send target after-update-matches segment-id))))

    (define (on-export-segment m e)
      (let* ((db (send target get-database))
             (toplevel (send target get-top-level-window))
             (segment-id (send target get-selected-segment))
             (fname (format "gps-segment-~a.gpx" segment-id))
             (path (put-file "Select file to export to" toplevel #f fname #f '()
                             '(("GPX Files" "*.gpx") ("Any" "*.*")))))
        (when path
          (let ([segment (fetch-gps-segment db segment-id)])
            (df-write/gpx segment path)))))

    (define (on-visit-session m e)
      (match-define (list match-id session-id segment-id begin end)
        (send target get-selected-match))
      (send target inspect-session session-id))

    (define (on-export-match m e)
      (define toplevel (send target get-top-level-window))
      (match-define (list match-id session-id segment-id begin end)
        (send target get-selected-match))
      (define fname (format "match-~a-~a-~a.csv" session-id segment-id match-id))
      (define path (put-file "Select file to export to" toplevel #f fname #f '()
                             '(("CSV Files" "*.csv") ("Any" "*.*"))))
      (when path
        (define db (send target get-database))
        (define df (session-df db session-id))
        (match-define (list start stop) (df-index-of* df "timestamp" begin end))
        (df-write/csv df path #:start start #:stop stop)))

    (define the-menu
      (if menu-bar
          (new menu% [parent menu-bar] [label "&GPS Segment"]
               [demand-callback on-demand])
          (new popup-menu% [title "GPS Segments"]
               [demand-callback on-demand]
               [popdown-callback on-popdown])))

    (define/private (make-menu-item label callback [shortcut #f])
      (new menu-item%
           [parent the-menu]
           [label label]
           [callback callback]
           [shortcut shortcut]))

    (define switch-to-view-menu
      (make-menu-item "Switch to GPS Segments View" on-switch-to-view))
    (new separator-menu-item% [parent the-menu])
    (define create-from-gpx-menu
      (make-menu-item "Create segment from GPX ..." on-create-from-gpx))
    (define create-by-reversing-menu
      (make-menu-item "Create segment by reversing selected ..." on-create-by-reversing))
    (new separator-menu-item% [parent the-menu])
    (define export-segment-menu
      (make-menu-item "Export segment (GPX) ..." on-export-segment))
    (define export-match-menu
      (make-menu-item "Export match data (CSV) ..." on-export-match))
    (new separator-menu-item% [parent the-menu])
    (define rename-segment-menu
      (make-menu-item "Rename segment ..." on-rename-segment))
    (define rescan-for-matches-menu
      (make-menu-item "Rescan for segment matches ..." on-rescan-for-matches))
    (define fixup-elevation-menu
      (make-menu-item "Fixup elevation ..." on-fixup-elevation))
    (new separator-menu-item% [parent the-menu])
    (define delete-menu
      (make-menu-item "Delete segment..." on-delete-segment))
    (new separator-menu-item% [parent the-menu])
    (define visit-session-menu
      (make-menu-item "Visit session..." on-visit-session))

    (define/public (get-popup-menu) the-menu)

    ))
