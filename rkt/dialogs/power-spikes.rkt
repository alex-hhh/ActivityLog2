#lang racket/base
;; power-spikes.rkt -- clear power spikes and re-calculate power related
;; metrics for an activity
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2021, 2023, 2025 Alex Harsányi <AlexHarsanyi@gmail.com>
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
         db/base
         math/statistics
         plot
         plot-container
         plot-container/hover-util
         racket/class
         racket/contract
         racket/format
         racket/gui/base
         racket/list
         racket/match
         racket/math
         "../fmt-util.rkt"
         "../models/coggan.rkt"
         "../models/time-in-zone.rkt"
         "../session-df/series-metadata.rkt"
         "../session-df/session-df.rkt"
         "../sport-charms.rkt"
         "../utilities.rkt"
         "../widgets/box-and-whiskers.rkt"
         "../widgets/main.rkt"
         "dashboard-common.rkt")

;; Return a hash table, mapping timestamp to trackpoint ID for every
;; trackpoint in the session SID.
(define (get-timestamp-mapping db sid)
  (define trackpoint-id-sql
    "select T.id as id,
            T.timestamp as timestamp
       from A_TRACKPOINT T, A_LENGTH L, A_LAP P
      where T.length_id = L.id
        and L.lap_id = P.id
        and P.session_id = ?")

  (for/hash (([id timestamp] (in-query db trackpoint-id-sql sid)))
    (values timestamp id)))

;; Return a hash table, mapping timestamp to lap section summary ID for each
;; lap in the session SID.
;;
;; NOTE: WE DON'T RETURN LAP IDS!!!!!
(define (get-lap-mapping db sid)
  (define lap-ssid-sql
    "select P.summary_id as id,
            P.start_time as timestamp
       from A_LAP P
      where P.session_id = ?")

  (for/hash (([id timestamp] (in-query db lap-ssid-sql sid)))
    (values timestamp id)))

;; Clear out the power and cycling dynamics values for all track points which
;; are above the CUTOFF power inside the dataframe DF.  This code operates
;; directly on the database, and after running it the data-frame will contain
;; outdated data, a 'session-updated-data event is logged, so, when the data
;; frame is cleared from the cache and `session-df` reads it back from the
;; database.
(define (clear-power-spikes db df cutoff)
  (define clear-power-data-sql
    "update A_TRACKPOINT
        set power = null,
            accumulated_power = null,
            left_right_balance = null,
            left_torque_effectiveness = null,
            right_torque_effectiveness = null,
            left_pedal_smoothness = null,
            right_pedal_smoothness = null,
            left_pco = null,
            right_pco = null,
            left_pp_start = null,
            left_pp_end = null,
            right_pp_start = null,
            right_pp_end = null,
            left_ppp_start = null,
            left_ppp_end = null,
            right_ppp_start = null,
            right_ppp_end = null
      where id = ?")
  (define sid (df-get-property df 'session-id))
  (define mapping (get-timestamp-mapping db sid))
  (call-with-transaction
   db
   (lambda ()
     (for (([timestamp power] (in-data-frame df "timestamp" "pwr")))
       (when (and power (> power cutoff))
         (query-exec db clear-power-data-sql (hash-ref mapping timestamp))))))
  (log-event 'session-updated-data sid))

;; Store a value in the SECTION_SUMMARY table for the SSID row.  FIELD-NAME is
;; updated to VALUE.
(define (put-section-summary-value db ssid field-name value)
  (query-exec
   db
   (format "update SECTION_SUMMARY set ~a = ? where id = ?" field-name)
   value ssid))

;; Store/Update the Coggan metrics CGMETRICS for session SID.
(define (put-session-cg-metrics sid cgmetrics #:database db)
  (match-define (cg ftp np if tss) cgmetrics)
  (call-with-transaction
   db
   (lambda ()
     (define ssid (query-value db "select summary_id from A_SESSION where id = ?" sid))
     (query-exec
      db
      "update A_SESSION set intensity_factor = ?, training_stress_score = ? where id = ?"
      if tss sid)
     (put-section-summary-value db ssid "normalized_power" np))))

;; Update the section summary SSID based on the data-frame averages from START
;; to STOP.  Updates average and maximum power plus all the average cycling
;; dynamics values.
(define (put-section-summary-stats db ssid df #:start (start 0) #:stop (stop (df-row-count df)))

  (when (df-contains? df "pwr")
    (let ((stats (df-statistics df "pwr" #:start start #:stop stop)))
      (put-section-summary-value db ssid "avg_power" (statistics-mean stats))
      (put-section-summary-value db ssid "max_power" (statistics-max stats))))

  (define (put-avg series dbcol)
    (when (df-contains? df series)
      (let ((stats (df-statistics df series #:start start #:stop stop)))
        (put-section-summary-value db ssid dbcol (statistics-mean stats)))))

  (put-avg "lrbal" "left_right_balance")
  (put-avg "lteff" "avg_left_torque_effectiveness")
  (put-avg "rteff" "avg_right_torque_effectiveness")
  (put-avg "lpsmth" "avg_left_pedal_smoothness")
  (put-avg "rpsmth" "avg_right_pedal_smoothness")
  (put-avg "lpco" "avg_left_pco")
  (put-avg "rpco" "avg_right_pco")
  (put-avg "lpps" "avg_left_pp_start")
  (put-avg "lppe" "avg_left_pp_end")
  (put-avg "rpps" "avg_right_pp_start")
  (put-avg "rppe"  "avg_right_pp_end")

  (put-avg "lppps" "avg_left_ppp_start")
  (put-avg "lpppe" "avg_left_ppp_end")
  (put-avg "rppps" "avg_right_ppp_start")
  (put-avg "rpppe"  "avg_right_ppp_end"))

;; Fix power spikes: power data values above CUTOFF are cleared out and Coggan
;; metrics + averages are recalculated.  The fix is done directly to database
;; and the data-frame corresponding to this session will be invalid after
;; calling this function and would have to be re-read using `session-df`.
(define (fix-power-spikes df cutoff #:database db #:ftp ftp #:sport-charms sport-charms)
  (call-with-transaction
   db
   (lambda ()
     (define sid (df-get-property df 'session-id))
     (define ssid (query-value db "select summary_id from A_SESSION where id = ?" sid))
     (clear-power-spikes db df cutoff)
     (define ndf (session-df db sid))      ; read it back again
     (define scgm (cg-metrics ndf #:ftp ftp))
     (put-section-summary-stats db ssid ndf)
     (put-session-cg-metrics sid scgm #:database db)
     (define lmapping (get-lap-mapping db sid))
     (define laps (df-get-property df 'laps))
     (for ([start (in-vector laps)]
           [end (in-sequences (in-vector laps 1) (in-value #f))])
       (match-define (list sindex eindex)
         (if end
             (df-index-of* df "timestamp" start end)
             (list
              (df-index-of df "timestamp" start)
              (df-row-count df))))
       (define lcgm (cg-metrics ndf #:ftp ftp #:start sindex #:stop eindex))
       (define ssid (hash-ref lmapping start))
       (put-section-summary-value db ssid "normalized_power" (cg-np lcgm))
       (put-section-summary-stats db ssid ndf #:start sindex #:stop eindex)
       (query-exec db "delete from BAVG_CACHE where session_id = ?" sid)
       (query-exec db "delete from HIST_CACHE where session_id = ?" sid)
       (query-exec db "delete from SCATTER_CACHE where session_id = ?" sid)
       (update-some-session-metrics sid db sport-charms)))))

;; Maintain a plot snip showing the power data (actually any specified
;; SERIES), marking the outliers and showing a box and whiskers plot for the
;; series data.  The plot also has a "hover" information for the series.
;; Parameters for the plot can be dynamically adjusted (see `set-iqr-scale`)
;; and the cutoff outlier value can be retrieved from the plot (see
;; `get-cutoff`).
(define outlier-plot%
  (class object%
    (init-field df series [iqr-scale 4.0] [width 100] [height 100])
    (super-new)

    (define md (find-series-metadata series))
    (define samples
      (df-select df series #:filter (lambda (sample) (and sample (> sample 0)))))
    (define plot-data (df-select* df "elapsed" series #:filter valid-only))

    (define max-x (df-ref df (sub1 (df-row-count df)) "elapsed"))
    (define box-plot-gap (* max-x 0.08))
    (define box-plot-room (* max-x 0.05))
    (define box-plot-x (- (+ box-plot-room (* 0.5 box-plot-gap))))
    (define min-x (- (+ box-plot-room box-plot-room box-plot-gap)))
    (define min-y 0)
    (define max-y (* 1.1 (for/fold ([m 0])
                                   ([p (in-vector samples)])
                           (max m p))))

    (define snip
      (parameterize ([plot-y-label (send md axis-label)]
                     [plot-x-label #f]
                     [plot-x-ticks (time-ticks #:formats '("~H:~M"))])
        (plot-snip
         (list
          (lines plot-data #:color (send md plot-color) #:width 1.5))
         #:x-min min-x #:y-min min-y #:y-max max-y
         #:width width #:height height)))

    (define bnw #f)    ; box-and-whiskers data produced by `samples->bnw-data`
    (define outliers #())               ; list of outlier points based on BNW
    (define bnw-renderer #f)
    (define bnw-renderer-highlighted #f)
    (define outlier-renderer #f)

    ;; Update the Inter-Quantile scale for the plot.  This is a factor used to
    ;; multiply the inter quantile range (the difference between the 25% and
    ;; 75% quantiles), and is used to determine the highest and lowest
    ;; reasonable values for the data.  Data outside that is considered to be
    ;; an outlier.
    (define/public (set-iqr-scale new-iqr-scale)
      (set! iqr-scale new-iqr-scale)
      (set! bnw (samples->bnw-data samples #:iqr-scale iqr-scale))
      (define upper-limit (bnw-data-uppwer-whisker bnw))
      (set! outliers
            (df-select*
             df "elapsed" series
             #:filter (lambda (v)
                        (match-define (vector e p) v)
                        (and e p (> p upper-limit)))))
      (set! bnw-renderer
            (box-and-whiskers
             bnw
             #:x box-plot-x
             #:gap box-plot-gap
             #:invert? #f

             #:box-color "Light Sky Blue"
             #:box-line-color "Steel Blue"
             #:box-line-width 1.5

             #:whiskers-color "Slate Gray"
             #:whiskers-width 2.0
             #:whiskers-style 'short-dash

             #:median-color "Dark Red"
             #:median-width 3.0

             #:outlier-color "Salmon"
             #:outlier-sym 'fullcircle4
             ;; #:outlier-size (* 2.0 (point-size))
             #:outlier-line-width 1.5
             ))
      (set! bnw-renderer-highlighted
            (box-and-whiskers
             bnw
             #:x box-plot-x
             #:gap box-plot-gap
             #:invert? #f

             #:box-color "Sky Blue"
             #:box-line-color "Royal Blue"
             #:box-line-width 2.25

             #:whiskers-color "Dark Slate Gray"
             #:whiskers-width 3.0
             #:whiskers-style 'short-dash

             #:median-color "Red"
             #:median-width 4.5

             #:outlier-color "Orange"
             #:outlier-sym 'fullcircle4
             #:outlier-size (* 1.5 (point-size))
             #:outlier-line-width 2.25
             ))
      (set! outlier-renderer
            (points outliers
                    #:color "coral"
                    #:sym 'circle8
                    #:size 5
                    #:line-width 3))

      (when snip
        (send snip set-overlay-renderers
              (flatten (list bnw-renderer outlier-renderer)))))

    ;; Find the closest outlier to the position X, Y.  If outliers are
    ;; present, a closest outlier will always be found.  Return the outlier
    ;; point as well as the distance to it -- the distance is used to
    ;; determine if the point is in fact too far from the X, Y coordinate.
    (define/private (closest-outlier x y)
      (for/fold ([point #f]
                 [distance #f])
                ([outlier (in-vector outliers)])
        (match-define (vector e p) outlier)
        (define d (let ([dx (/ (- e x) max-x)]
                        [dy (/ (- p y) max-y)])
                    (sqrt (+ (* dx dx) (* dy dy)))))
        (if (or (not distance) (< d distance))
            (values outlier d)
            (values point distance))))

    ;; Create a hover label for the plot to be shown at X,Y for the point
    ;; POINT.  If OUTLIER? is #t, the label will reflect the fact that this is
    ;; an outlier point.
    (define/private (make-hover-label x y point #:outlier? [outlier? #f])
      (match-define (vector duration value) point)
      (if outlier?
          (hover-label
           x y
           (make-hover-badge
            `(("Time" ,(duration->string duration))
              (,(send md name) ,(~a (exact-round value)))
              ("Outlier point"))))
          (hover-label
           x y
           (make-hover-badge
            `(("Time" ,(duration->string duration))
              (,(send md name) ,(~a (exact-round value))))))))

    ;; Hover callback fort the plot snip. Displays additional information on
    ;; the plot depending on where the mouse is:
    ;;
    ;; * on the plot itself, it displays the current value of the series
    ;;
    ;; * close to an outlier point will highlight the outlier and display
    ;;   information about the outlier.
    ;;
    ;; * on the box-and-whiskers plot it displays information about the
    ;;   quantiles.
    ;;
    ;; NOTE: this cannot be a method (e.g. define/private as it is used as a
    ;; callback
    (define (hover-callback snip event x y)
      (define renderers (list bnw-renderer outlier-renderer))
      (cond ((not (good-hover? snip x y event)) (void))
            ((> x 0)
             (let-values ([(outlier distance) (closest-outlier x y)])
               (if (and outlier (< distance 1e-2))
                   ;; Mouse is over (close to) an outlier point, highlight it
                   (set! renderers
                         (append
                          renderers
                          (list
                           (points (list outlier)
                                   #:color "indianred"
                                   #:sym 'fullcircle8
                                   #:size 6
                                   #:line-width 5)
                           (make-hover-label x y outlier #:outlier? #t))))
                   ;; Mouse is just somewhere over the plot, so we highlight
                   ;; the current value
                   (let ([index (df-index-of df "elapsed" x)])
                     (when (and index (< index (df-row-count df)))
                       (define point (df-ref* df index "elapsed" series))
                       (when (vector-ref point 1)
                         (set! renderers
                               (append
                                renderers
                                (list
                                 (hover-vrule (vector-ref point 0))
                                 (make-hover-label x y point))))))))))
            ((and bnw
                  (> x (- box-plot-x (* box-plot-gap 1/2)))
                  (< x (+ box-plot-x (* box-plot-gap 1/2))))
             ;; Mouse is over the box-and-whiskers section
             (match-define (bnw-data q1 median q3 lower-whisker uppwer-whisker o) bnw)
             (set! renderers
                   (append
                    (remove bnw-renderer renderers)
                    (list
                     bnw-renderer-highlighted
                     (hrule q1 #:width 1.5 #:style 'long-dash #:color "Slate Gray")
                     (hrule median #:width 1.5 #:style 'long-dash #:color "Slate Gray")
                     (hrule q3 #:width 1.5 #:style 'long-dash #:color "Slate Gray")
                     (hrule uppwer-whisker #:width 1.5 #:style 'long-dash #:color "Slate Gray")
                     (hover-label
                      x y
                      (make-hover-badge
                       `(("Q1 (25%)" ,(~a (exact-round q1)))
                         ("Median (50%)" ,(~a (exact-round median)))
                         ("Q3 (75%)" ,(~a (exact-round q3)))
                         ("Upper Cutoff" ,(~a (exact-round uppwer-whisker)))
                         ("Outlier Count" ,(~a (length o)))))))))))
      (send snip set-overlay-renderers (flatten renderers)))

    (set-iqr-scale iqr-scale)
    (send snip set-mouse-event-callback hover-callback)

    (define/public (get-snip) snip)
    (define/public (get-cutoff) (bnw-data-uppwer-whisker bnw))
    (define/public (get-outlier-count) (length (bnw-data-outliers bnw)))

    ))

;; Dialog to clear power spikes for a session -- will display a plot
;; representing the power data series and outlier points based on an
;; Inter-Quantile range approach.  the Inter-Quantile range is adjustable,
;; thus the outlier points are adjustable as well.  Once the user is happy
;; with them, they can clear the selected outlier points.
(define power-spikes-dashboard%
  (class object%
    (init-field
     parent
     dbc
     sport-charms
     session-id
     [min-width 1000]
     [min-height 625])
    (super-new)

    (define df #f)

    (define/private (make-toplevel-dialog parent)
      (new
       (class dialog% (init) (super-new)
         (define/augment (on-close) (on-close-dashboard)))
       [label "Clear Power Spikes"]
       [min-width min-width]
       [min-height min-height]
       [parent (if parent (send parent get-top-level-window) #f)]))

    (define message-font
      (send the-font-list find-or-create-font 12 'default 'normal 'normal))

    (define toplevel-window (make-toplevel-dialog parent))

    (define plot #f)

    (define dashboard-contents
      (new vertical-panel%
           [parent toplevel-window]
           [spacing 5]
           [border 5]
           [alignment '(left top)]))

    (define headline
      (new pict-canvas%
           [parent dashboard-contents]
           [alignment '(left center)]
           [stretchable-height #f]))

    (define plot-container
      (new plot-container%
           [parent dashboard-contents]))

    (define controls-group-box
      (new group-box-panel%
           [parent dashboard-contents]
           [label "Outlier Filtering"]
           [alignment '(center center)]
           [stretchable-height #f]
           [border 20]
           [spacing 20]))

    (define controls-pane
      (new horizontal-pane%
           [parent controls-group-box]
           [spacing 10]))

    (define iqr-scale-message
      (new message%
           [label "IQR Scale:"]
           [parent controls-pane]))

    (define iqr-scale-value
      (new message%
           [label "      "]
           [parent controls-pane]
           [font message-font]))

    (define iqr-scale-slider
      (new slider%
           [label ""]
           [parent controls-pane]
           [min-value 15]
           [max-value 100]
           [init-value 40]
           [style '(horizontal plain)]
           [callback (lambda (c e) (on-iqr-scale c e))]))

    (define cutoff-message
      (new message%
           [label "Cutoff Value:"]
           [parent controls-pane]))

    (define cutoff-value
      (new message%
           [label "      "]
           [parent controls-pane]
           [font message-font]))

    (define outlier-count-message
      (new message%
           [label "Outlier Count:"]
           [parent controls-pane]))

    (define outlier-count
      (new message%
           [label "      "]
           [parent controls-pane]
           [font message-font]))

    (define ftp-message
      (new message%
           [label "FTP Setting:"]
           [parent controls-pane]))

    (define ftp-input
      (new number-input-field% [parent controls-pane]
           [label ""]
           [min-width 100] [stretchable-width #f]
           [font message-font]
           [allow-empty? #f]
           [cue-text "watts"]))

    (define buttons-box
      (new horizontal-pane%
           [parent dashboard-contents]
           [alignment '(right center)]
           [stretchable-height #f]
           [border 20]
           [spacing 20]))

    (define do-fixups-button
      (new button%
           [parent buttons-box]
           [label "Remove Marked Outlier Points"]
           [callback (lambda (b e) (on-remove-outliers))]))

    (define close-button
      (new button%
           [parent buttons-box]
           [label "Close"]
           [callback (lambda (b e)
                       (when toplevel-window
                         (send toplevel-window show #f)))]))

    (define/private (on-remove-outliers)
      (let/ec return
        (unless plot     ; we should not be called when there is no power data
         (return (void)))
        (define ftp (send ftp-input get-converted-value))
        (define cutoff (send plot get-cutoff))
        (unless ftp
          (message-box
           "Invalid FTP Setting"
           "Need a valid FTP value"
           toplevel-window
           '(ok stop)
           #:dialog-mixin al2-message-box-mixin)
          (return #f))
        (define proceed?
          (message-box
           "Clear Power Spikes?"
           (format "Clear Power Spikes above ~a watts?" cutoff)
           toplevel-window
           '(yes-no)
           #:dialog-mixin al2-message-box-mixin))
        (when (equal? proceed? 'yes)
          (send do-fixups-button enable #f)
          (send close-button enable #f)
          (send plot-container clear-all)
          (send plot-container set-background-message "Clearing power spikes...")
          (thread/dbglog
           (lambda ()
             (fix-power-spikes df cutoff #:database dbc #:ftp ftp #:sport-charms sport-charms)
             (define sid (df-get-property df 'session-id))
             (send sport-charms put-athlete-ftp ftp)
             ;; Save the IQR scale as a preference
             (let ([iqr-scale (/ (send iqr-scale-slider get-value) 10.0)])
               (put-pref 'power-spikes-iqr-scale iqr-scale))
             (load-data)
             (queue-callback
              (lambda ()
                (send plot-container set-background-message "")
                (send close-button enable #t))))))))

    (define/private (on-iqr-scale control event)
      (define v (/ (send control get-value) 10.0))
      (when plot
        (send plot set-iqr-scale v)
        (send iqr-scale-value set-label (~r v #:precision 2))
        (when plot
          (send cutoff-value set-label (~a (exact-round (send plot get-cutoff))))
          (send outlier-count set-label (~a (exact-round (send plot get-outlier-count)))))))

    (define (on-close-dashboard)
      (void))

    (define/private (load-data)
      (set! df (session-df dbc session-id))
      (define sinfo (get-session-info session-id dbc))
      (send dashboard-contents begin-container-sequence)
      (when sinfo
        (send headline set-pict (and sinfo (pp-session-info/pict sinfo))))
      (define iqr-scale (get-pref 'power-spikes-iqr-scale (lambda () 4.0)))
      (define-values (w h) (send plot-container cell-dimensions 1))
      (if (df-contains? df "pwr")
          (set! plot (new outlier-plot%
                          [df df]
                          [series "pwr"]
                          [iqr-scale iqr-scale]
                          [width w]
                          [height h]))
          (set! plot #f))
      (send iqr-scale-slider set-value (exact-round (* iqr-scale 10.0)))
      (send iqr-scale-value set-label (~r iqr-scale #:precision 2))
      (send cutoff-value set-label (~a (exact-round (if plot (send plot get-cutoff) 0))))
      (send outlier-count set-label (~a (exact-round (if plot (send plot get-outlier-count) 0))))
      (let ((ftp (send sport-charms get-athlete-ftp)))
        (when ftp
          (send ftp-input set-value (n->string ftp))))
      (if plot
          (send plot-container set-snip (send plot get-snip))
          (begin
            (send plot-container clear-all)
            (send plot-container set-background-message "No Power Data")
            (send do-fixups-button enable #f)))
      (send dashboard-contents end-container-sequence))

    (define/public (show-dashboard)
      (thread/dbglog (lambda () (load-data)))
      (send close-button enable #t)
      (send plot-container set-background-message "")
      (send do-fixups-button enable #t)
      (send toplevel-window show #t) ; will block until finish-dialog is called
      )

    ))

(define (show-power-spikes-dashboard toplevel database sport-charms sid)
  (define dashboard
    (new power-spikes-dashboard%
         [parent toplevel]
         [dbc database]
         [sport-charms sport-charms]
         [session-id sid]))
  (send dashboard show-dashboard))

(provide/contract
 (show-power-spikes-dashboard (-> (or/c #f (is-a?/c top-level-window<%>))
                                  connection?
                                  (is-a?/c sport-charms%)
                                  exact-positive-integer? any/c)))
