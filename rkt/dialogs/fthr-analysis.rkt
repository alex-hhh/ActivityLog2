#lang racket/base
;; fthr-analysis.rkt -- FTHR analysis dashboard
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2020, 2021, 2022, 2023, 2026 Alex Harsányi <AlexHarsanyi@gmail.com>
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
         db
         map-widget
         math/statistics
         pict
         plot
         plot-container
         plot-container/hover-util
         plot/utils
         racket/contract
         racket/gui
         racket/port
         "../color-theme.rkt"
         "../fmt-util.rkt"
         "../models/fthr.rkt"
         "../models/sport-zone.rkt"
         "../models/time-in-zone-gui.rkt"
         "../models/time-in-zone.rkt"
         "../session-df/session-df.rkt"
         "../sport-charms.rkt"
         "../utilities.rkt"
         "../widgets/grid-pane.rkt"
         "../widgets/widget-utilities.rkt"
         "dashboard-common.rkt")

(define (make-mmax-df df series)
  ;; Hack: if we are asked for the "pace" series, calculate the best avg for
  ;; "spd" and convert it.  This gives a better "best average" value.  See #17
  (define actual-series
    (if (equal? series "pace") "spd" series))
  (define durations (for/list ([x (in-range 1 (df-row-count df))]) x))
  (define data (df-mean-max df actual-series #:durations durations))
  (define nitems (length data))
  (define duration-data (make-vector nitems #f))
  (define value-data (make-vector nitems #f))
  (define position-data (make-vector nitems #f))
  (for ([(x index) (in-indexed (in-list data))])
    (match-define (vector duration value position) x)
    (vector-set! duration-data index duration)
    (vector-set! value-data index (if (equal? series "pace")
                                      (convert-m/s->pace value)
                                      value))
    (vector-set! position-data index position))
  (define mmax-df (make-data-frame))
  (df-add-series! mmax-df (make-series "duration" #:data duration-data))
  (df-add-series! mmax-df (make-series series #:data value-data))
  (df-add-series! mmax-df (make-series "position" #:data position-data))
  (df-set-sorted! mmax-df "duration" <)
  mmax-df)

(define (make-map-snip df . segments)
  (define map (new map-snip%))

  (define main-track-color (make-object color% 82 137 199))     ; No. 12
  (define selected-track-color (make-object color% 170 51 119))

  (define main-track-pen
    (send the-pen-list find-or-create-pen main-track-color 3 'solid))
  (define selected-track-pen
    (send the-pen-list find-or-create-pen selected-track-color 6 'solid))

  ;; Setup the color and Z-order for the main track on the map
  (send map set-group-pen 'main-track main-track-pen)
  (send map set-group-zorder 'main-track 0.2)
  ;; Setup the color and Z-order for the selected track segment on the map
  ;; (this will be added to the map later when the user hovers over a plot)
  (send map set-group-pen 'selected-track selected-track-pen)
  (send map set-group-zorder 'selected-track 0.1)
  ;; We also center the map on the current location, which will be specified
  ;; later when the user hovers on a plot
  (send map track-current-location #t)

  ;; Add the main track to the map
  (send map add-track (df-select* df "lat" "lon" #:filter valid-only) 'main-track)

  ;; For each of the segments, mark the start and end location on the map
  (for ([(segment index) (in-indexed (in-list segments))] #:when segment)
    (define start-location (df-ref* df (hash-ref segment 'start-index) "lat" "lon"))
    (define end-location (df-ref* df (hash-ref segment 'end-index) "lat" "lon"))
    (define name (hash-ref segment 'name #f))
    (define color (hash-ref segment 'color (lambda () (make-color 0 0 0))))
    (send map add-marker start-location
          (format "~a Start" name)
          (if (odd? index) 1 -1)
          color)
    (send map add-marker end-location
          (format "~a End" name)
          (if (odd? index) -1  1)
          color))

  ;; Finally, center the map and resize it to fit the track
  (send map center-map 'main-track)
  (send map resize-to-fit)

  map)

(define (make-guest-transform base-min base-max guest-min guest-max)
  (define base-range (- base-max base-min))
  (define guest-range (- guest-max guest-min))
  (invertible-function
   (lambda (v)                          ; Transform from base to guest
     (let ([p (/ (- v base-min) base-range)])
       (+ guest-min (* p guest-range))))
   (lambda (v)                          ; Transform from guest to base
     (let ([p (/ (- v guest-min) guest-range)])
       (+ base-min (* p base-range))))))

(define (guest->base guest-data transform)
  (let ([tr (invertible-function-g transform)])
    (for/vector ([item guest-data])
      (match-define (vector x y) item)
      (vector x (tr y)))))

(define (get-low+high df series [room 0.25])
  (let* ([stats (df-statistics df series #:weight-series "elapsed")]
         [low (statistics-min stats)]
         [high (statistics-max stats)]
         [adjust (* (statistics-range stats) room)])
    ;; Make the Y range of the plot slightly larger than the min/max values
    (values (- low adjust) (+ high adjust))))

(define (make-best-rectangle best transform color)
  (define start-position (hash-ref best 'start-position))
  (define end-position (hash-ref best 'end-position))
  (define min-value (hash-ref best 'min-value))
  (define max-value (hash-ref best 'max-value))
  (rectangles
   (list (vector (ivl start-position end-position)
                 (ivl (transform min-value) (transform max-value))))
   #:color color #:alpha 0.2 #:line-width 0 #:line-style 'transparent))

(define (make-combined-plot df segment1 segment2)
  (define s1 (hash-ref segment1 'series))
  (define s2 (hash-ref segment2 'series))
  (define label1 (hash-ref segment1 'plot-label))
  (define label2 (hash-ref segment2 'plot-label))
  (define color1 (hash-ref segment1 'color))
  (define color2 (hash-ref segment2 'color))
  (define ticks1 (hash-ref segment1 'plot-ticks))
  (define ticks2 (hash-ref segment2 'plot-ticks))

  (let-values ([(s1-min s1-max) (get-low+high df s1)]
               [(s2-min s2-max) (get-low+high df s2)])
    (let* ([transform (make-guest-transform s1-min s1-max s2-min s2-max)]
           [data1 (df-select* df "elapsed" s1 #:filter valid-only)]
           [data2 (guest->base (df-select* df "elapsed" s2 #:filter valid-only) transform)])

      (parameterize ([plot-x-ticks (time-ticks #:formats '("~H:~M:~f"))]
                     [plot-y-ticks ticks1]
                     [plot-x-label "Elapsed Time"]
                     [plot-y-label label1]
                     [plot-y-far-label label2]
                     [plot-y-far-ticks (ticks-scale ticks2 transform)])
        (plot-snip (list (tick-grid)
                         (lines data2 #:color color2 #:width 2)
                         (lines data1 #:color color1 #:width 2)
                         (make-best-rectangle segment1 identity color1)
                         (make-best-rectangle segment2 (invertible-function-g transform) color2))
                   #:x-min 0 #:y-min s1-min #:y-max s1-max)))))

(define (make-single-plot df segment)
  (define series (hash-ref segment 'series))
  (define label (hash-ref segment 'plot-label))
  (define color (hash-ref segment 'color))
  (define ticks (hash-ref segment 'plot-ticks))

  (let-values ([(s-min s-max) (get-low+high df series)])
    (let* ([data (df-select* df "elapsed" series #:filter valid-only)])
      (parameterize ([plot-x-ticks (time-ticks #:formats '("~H:~M:~f"))]
                     [plot-y-ticks ticks]
                     [plot-x-label "Elapsed Time"]
                     [plot-y-label label])
        (plot-snip (list (tick-grid)
                         (lines data #:color color #:width 2)
                         (make-best-rectangle segment identity color))
                   #:x-min 0 #:y-min s-min #:y-max s-max)))))

(define (make-mmax-plot mmax-df segment)
  (define series(hash-ref segment 'series))
  (define label (hash-ref segment 'plot-label))
  (define color (hash-ref segment 'color))
  (define ticks (hash-ref segment 'plot-ticks))
  (define data (df-select* mmax-df "duration" series #:filter valid-only))
  (parameterize ([plot-x-ticks (time-ticks #:formats '("~H:~M:~f"))]
                 [plot-y-ticks ticks]
                 ;; [plot-x-transform log-transform]
                 [plot-x-label "Duration"]
                 [plot-y-label label])
    (plot-snip (list (tick-grid) (lines data #:color color #:width 2)) #:x-min 1)))

(define ((make-mmax-marker-renderer df series label fmtfn) x y)
  (define index (df-index-of df "duration" x))
  (and index (< index (df-row-count df))
       (match-let ([(vector elapsed value) (df-ref* df index "duration" series)])
         (list
          (hover-vrule elapsed)
          (hover-label x y
                       (format "Duration: ~a" (duration->string elapsed))
                       (format "~a: ~a" label (fmtfn value)))))))

(define (make-mmax-plot-callback df mmax-df plot mmax-plot map-snip series label fmtfn)
  (define mmax-renderer (make-mmax-marker-renderer mmax-df series label fmtfn))

  (lambda (snip event x y)
    (define mmax-renderers #f)
    (define renderers #f)
    (define track #f)

    (when (good-hover? snip x y event)
      (let ([index (df-index-of mmax-df "duration" x)])
        (set! mmax-renderers (mmax-renderer x y))
        (define e (df-ref mmax-df index "position"))
        (set! renderers (list (hover-vrange e (+ e x) (make-object color% 0 0 50 0.1))))

        (when map-snip
          (match-define (list b1 e1) (df-index-of* df "elapsed" e (+ e x)))
          (set! track (df-select* df "lat" "lon" #:start b1 #:stop e1 #:filter valid-only)))))

    (when map-snip
      (send map-snip begin-edit-sequence)
      (send map-snip delete-group 'selected-track)
      (when track
        (send map-snip current-location #f)
        (send map-snip add-track track 'selected-track))
      (send map-snip end-edit-sequence))

    (send mmax-plot set-overlay-renderers mmax-renderers)
    (send plot set-overlay-renderers renderers)))

(define (make-current-position-marker df segment1 segment2)

  (define-values (series1 name1 format-value1)
    (if segment1
        (values (hash-ref segment1 'series)
                (hash-ref segment1 'name)
                (hash-ref segment1 'format-value))
        (values #f #f #f)))

  (define-values (series2 name2 format-value2)
    (if segment2
        (values (hash-ref segment2 'series)
                (hash-ref segment2 'name)
                (hash-ref segment2 'format-value))
        (values #f #f #f)))

  (lambda (x y)
    (define (make-marker index)
      (define-values (elapsed v1 v2)
        (cond ((and series1 series2)
               (match-define (vector elapsed v1 v2)
                 (df-ref* df index "elapsed" series1 series2))
               (values elapsed v1 v2))
              (series1
               (match-define (vector elapsed v1)
                 (df-ref* df index "elapsed" series1))
               (values elapsed v1 #f))
              (series2
               (match-define (vector elapsed v2)
                 (df-ref* df index "elapsed" series2))
               (values elapsed #f v2))
              (#t (values #f #f))))
      (list
       (hover-vrule elapsed)
       (hover-label x y
                    (format "Time: ~a" (duration->string elapsed))
                    (format "~a: ~a" name1 (if v1 (format-value1 v1) v1))
                    (format "~a: ~a" name2 (if v2 (format-value2 v2) v2)))))

    (let ([index (df-index-of df "elapsed" x)])
      (and index (< index (df-row-count df))
           (make-marker index)))))


;; Hold all information about the FTHR analysis.  Note that the session ID is
;; available as the 'session-id property on the DATA-FRAME slot.
(struct fthr (data-frame
              session-info
              primary-segment secondary-segment
              primary-zones secondary-zones))

;; Load the session SID from the database and find the best primary and
;; secondary segments and create sport zones for them.  The entire thing is
;; returned as a FTHR structure (see above).
;;
;; Note that FTHR analisys is only supported for running and cycling
;; activities for now. and an error will be signaled if the activity is not
;; one of those.
(define (load-fthr-data db sid)
  (define df (session-df db sid))
  (define sinfo (get-session-info sid db))
  (define sport (df-get-property df 'sport #f))
  (define-values
    (primary secondary)
    (cond ((is-runnig? sport)
           (values
            (best-pace-segment df)
            (best-heart-rate-segment df)))
          ((is-cycling? sport)
           (values
            (best-power-segment df)
            (best-heart-rate-segment df)))
          (#t
           (printf "Unsupported activity type ~a~%" sport)
           (values #f #f))))
  (define valid-from
    (hash-ref sinfo 'start-time (lambda () (current-seconds))))
  (define pz (and primary (make-sport-zones primary valid-from)))
  (define sz (and secondary (make-sport-zones secondary valid-from)))
  (fthr df sinfo primary secondary pz sz))

(define (setup-plots canvas fthr-data)
  (queue-callback
   (lambda ()
     (send canvas set-background-message "Loading data...")))

  (match-define
    (fthr df session-info primary-segment secondary-segment pz sz) fthr-data)

  ;; We calculate the best for speed, but switch to pace for displaying -- see
  ;; issue #17 for why.

  (define map-snip
    (and (df-contains? df "lat" "lon")
         (apply make-map-snip df (filter values (list primary-segment secondary-segment)))))

  (define time-plot
    (cond ((and primary-segment secondary-segment)
           (make-combined-plot df primary-segment secondary-segment))
          (primary-segment
           (make-single-plot df primary-segment))
          (secondary-segment
           (make-single-plot df secondary-segment))
          (#t #f)))

  (define primary-mmax-plot
    (if primary-segment
        (let* ((series (hash-ref primary-segment 'series))
               (name (hash-ref primary-segment 'name))
               (format-value (hash-ref primary-segment 'format-value))
               (mmax-df (make-mmax-df df series))
               (mmax-plot (make-mmax-plot mmax-df primary-segment))
               (hover-callback
                (make-mmax-plot-callback
                 df mmax-df time-plot mmax-plot map-snip
                 series name format-value)))
          (send mmax-plot set-mouse-event-callback hover-callback)
          mmax-plot)
        #f))

  (define secondary-mmax-plot
    (if secondary-segment
        (let* ((series (hash-ref secondary-segment 'series))
               (name (hash-ref secondary-segment 'name))
               (format-value (hash-ref secondary-segment 'format-value))
               (mmax-df (make-mmax-df df series))
               (mmax-plot (make-mmax-plot mmax-df secondary-segment))
               (hover-callback
                (make-mmax-plot-callback
                 df mmax-df time-plot mmax-plot map-snip
                 series name format-value)))
          (send mmax-plot set-mouse-event-callback hover-callback)
          mmax-plot)
        #f))

  (define position-marker
    (make-current-position-marker df primary-segment secondary-segment))

  (define (hover-callback snip event x y)
    (define renderers #f)
    (define current-location #f)

    ;; The callback might be invoked when the mouse is outside the plot area.
    ;; `good-hover? determines if we should display hover information
    (when (good-hover? snip x y event)
      (set! renderers (position-marker x y))
      (when map-snip
        (let ([index (df-index-of df "elapsed" x)])
          (when (and index (< index (df-row-count df)))
            (set! current-location (df-ref* df index "lat" "lon"))))))

    (when map-snip
      (send map-snip current-location current-location))
    (send snip set-overlay-renderers renderers))

  (when time-plot
    (send time-plot set-mouse-event-callback hover-callback))

  (send canvas set-snips/layout
        (apply
         hgroup
         (filter values
                 (list map-snip
                       (apply vgroup
                              (filter values
                                      (list
                                       time-plot
                                       primary-mmax-plot
                                       secondary-mmax-plot))))))))

;; Return a text representation of FTHR-DATA, suitable for saving to a file.
(define (fthr->text fthr-data sport-charms)
  (with-output-to-string
    (lambda ()
      (match-define (fthr df sinfo primary secondary pz sz) fthr-data)
      (pp-session-info sinfo sport-charms)
      (when primary
        (newline)
        (pp-segment primary)
        (newline)
        (pp-sport-zones pz #:show-validity-range? #f)
        (newline))
      (when secondary
        (newline)
        (pp-segment secondary)
        (newline)
        (pp-sport-zones sz #:show-validity-range? #f)
        (newline)))))

;; Save the FTHR-DATA to a nicely formatted PDF document written to
;; OUTPUT-FILE
(define (fthr->pdf fthr-data sport-charms output-file)
  (match-define (fthr df sinfo primary secondary pz sz) fthr-data)
  (define header (pp-session-info/pict sinfo sport-charms))
  (define zones1
    (and primary
         (vc-append
          30
          (pp-segment/pict primary)
          (pp-sport-zones/pict pz #:show-validity-range? #f))))
  (define zones2
    (and secondary
         (vc-append
          30
          (pp-segment/pict secondary)
          (pp-sport-zones/pict sz #:show-validity-range? #f))))
  (define zones
    (cond ((and zones1 zones2)
           (ht-append 30 zones1 zones2))
          (zones1 zones1)
          (zones2 zones2)))
  (define full-page (vc-append 20 header zones))

  (call-with-output-file
    output-file
    (lambda (out)
      (define dc (new pdf-dc%
                      [interactive #f]
                      [use-paper-bbox #t]
                      [output out]))
      (send dc start-doc "FTHR Analysis")
      (send dc start-page)
      (let-values (((w h) (send dc get-size)))
        (draw-pict full-page dc
                   (exact-round (/ (- w (pict-width full-page)) 2))
                   (exact-round (/ (- h (pict-height full-page)) 2))))
      (send dc end-page)
      (send dc end-doc))
    #:exists 'replace))

;; Return true if sport zones for METRIC have already been set from the
;; SESSION-ID (i.e. FTHR analisys has already been done on this session and
;; sport zones have been set from it).
(define (have-sport-zones-from-session? database session-id metric)
  (query-maybe-row
   database
   "select SZS.zone_id
    from SPORT_ZONE_SOURCE SZS, SPORT_ZONE SZ
    where SZS.zone_id = SZ.id
    and SZ.zone_metric_id = ?
    and SZS.session_id = ?"
   (metric->id metric)
   session-id))

;; This is the FTHR analysis window
(define fthr-dashboard%
  (class object%
    (init-field [min-width 1000] [min-height 625])
    (super-new)

    (define fthr-data #f)               ; a FTHR struct
    (define database #f)
    (define sport-charms #f)
    (define sport-zones #f)

    (define (make-toplevel-dialog parent)
      (new
       (class dialog% (init) (super-new)
         (define/augment (on-close) (on-close-dashboard)))
       [label "FTHR Analysis Dashboard"]
       [min-width min-width]
       [min-height min-height]
       [parent (if parent (send parent get-top-level-window) #f)]))

    (define toplevel-window (make-toplevel-dialog #f))

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

    (define detail-panel
      (new tab-panel%
           [stretchable-height #t]
           [choices '("Sport Zones" "Data Plots")]
           [callback (lambda (p c) (switch-tabs (send p get-selection)))]
           [parent dashboard-contents]))

    (define analysis-panel
      (new horizontal-panel% [parent detail-panel]))

    (define fthr-panel
      (new grid-pane% [parent analysis-panel] [columns 2]))

    (define plot-panel
      (new plot-container%
           [parent detail-panel]
           [columns 1]
           [spacing 5]
           [style '(deleted)]))
    (send plot-panel set-background-message "No Data Available")

    (define primary-best (new pict-canvas% [parent fthr-panel]))
    (define secondary-best (new pict-canvas% [parent fthr-panel]))
    (define primary-zones (new pict-canvas% [parent fthr-panel]))
    (define secondary-zones (new pict-canvas% [parent fthr-panel]))


    (define control-panel
      (new vertical-panel% [parent analysis-panel]))

    (define export-group-box
      (new group-box-panel%
           [parent control-panel] [label "Export"]
           [alignment '(center center)]
           [border 20]
           [spacing 10]))

    (define ctc-button
      (new button%
           [parent export-group-box]
           [label "Copy to Clipboard..."]
           [callback (lambda (b e) (on-copy-to-clipboard))]))

    (define pdf-button
      (new button%
           [parent export-group-box]
           [label "Save to PDF..."]
           [callback (lambda (b e) (on-save-to-pdf))]))

    (define txt-button
      (new button%
           [parent export-group-box]
           [label "Save to TXT..."]
           [callback (lambda (b e) (on-save-to-txt))]))

    (define close-button
      (new button%
           [parent export-group-box]
           [label "Close"]
           [callback (lambda (b e) (send toplevel-window show #f))]))

    (define primary-group-box
      (new group-box-panel%
           [parent control-panel]
           [label "Primary Zones"]
           [border 10]
           [spacing 10]))

    (define primary-description
      (let ([c (new editor-canvas%
                    [parent primary-group-box]
                    [style '(no-hscroll)])]
            [t (new text%)])
        (send c set-editor t)
        (send t set-tabs '(8) 8 #f)
        (send t auto-wrap #t)
        t))

    (define set-primary-button
      (new button%
           [parent primary-group-box]
           [label "Set These Zones"]
           [stretchable-width #t]
           [callback (lambda (b e) (on-set-primary-zones))]))

    (define secondary-group-box
      (new group-box-panel%
           [parent control-panel]
           [label "Secondary Zones"]
           [border 10]
           [spacing 10]))

    (define secondary-description
      (let ([c (new editor-canvas%
                    [parent secondary-group-box]
                    [style '(no-hscroll)])]
            [t (new text%)])
        (send c set-editor t)
        (send t set-tabs '(8) 8 #f)
        (send t auto-wrap #t)
        t))

    (define set-secondary-button
      (new button%
           [parent secondary-group-box]
           [label "Set These Zones"]
           [stretchable-width #t]
           [callback (lambda (b e) (on-set-secondary-zones))]))

    ;; Make the export buttons the same width
    (let ([button-width
           (for/fold ([width 0])
                     ([b (in-list (list ctc-button pdf-button txt-button))])
             (define-values (w h) (send b get-size))
             (max w width))])
      (for ([b (in-list (list ctc-button pdf-button txt-button))])
        (send b min-width button-width)))

    ;; Copy the contents of the FTHR analysis tand sport zones o clipboard as
    ;; text.
    (define (on-copy-to-clipboard)
      (when fthr-data
        (define text (fthr->text fthr-data sport-charms))
        (send the-clipboard set-clipboard-string text (current-seconds))
        (message-box
         "Copied to Clipboard"
         "Analysis was copied to the clipboard"
         toplevel-window
         '(ok)
         #:dialog-mixin al2-message-box-mixin)))

    ;; Save the FTHR analisys and sport zones to a PDF document.  The user is
    ;; prompted for the output file name.
    (define (on-save-to-pdf)
      (when fthr-data
        (define output-file (put-file "Save Analysis To"
                                      toplevel-window
                                      #f
                                      "fthr-analysis"
                                      "pdf"
                                      '()
                                      '(("PDF Files" "*.pdf") ("Any" "*.*"))))
        (when output-file
          (fthr->pdf fthr-data sport-charms output-file))))

    ;; Save the FTHR analisys and sport zones to a Text file.  The user is
    ;; prompted for the output file name.
    (define (on-save-to-txt)
      (when fthr-data
        (define output-file (put-file "Save Analysis To"
                                      toplevel-window
                                      #f
                                      "fthr-analysis"
                                      "txt"
                                      '()
                                      '(("Txt Files" "*.txt") ("Any" "*.*"))))
        (when output-file
          (define text (fthr->text sport-charms fthr-data))
          (call-with-output-file
            output-file
            (lambda (out) (write-string text out))
            #:exists 'replace))))

    ;; Set the primary zones based on the FTHR analysis
    (define (on-set-primary-zones)
      (match-define (fthr df sinfo primary secondary pz sz) fthr-data)
      (call-with-transaction
       database
       (lambda ()
         (define session-id (df-get-property df 'session-id))
         (define zone-id (send sport-zones put-sport-zones pz))
         (query-exec
          database
          "insert into SPORT_ZONE_SOURCE(zone_id, session_id) values(?, ?)"
          zone-id
          session-id)
         (define outdated (get-tiz-outdated-sessions database))
         (update-tiz-for-sessions/interactive outdated database sport-charms toplevel-window)))
      (put-description primary-description "Sport zones based on this analysis have been set. You can edit sport zones from the Athlete / Edit Sport Zones menu")
      (send set-primary-button enable #f))

    ;; Set the secondary zones based on the FTHR analysis
    (define (on-set-secondary-zones)
      (match-define (fthr df sinfo primary secondary pz sz) fthr-data)
      (call-with-transaction
       database
       (lambda ()
         (define session-id (df-get-property df 'session-id))
         (define zone-id (send sport-zones put-sport-zones sz))
         (query-exec
          database
          "insert into SPORT_ZONE_SOURCE(zone_id, session_id) values(?, ?)"
          zone-id
          session-id)
         (define outdated (get-tiz-outdated-sessions database))
         (update-tiz-for-sessions/interactive outdated database sport-charms toplevel-window)))
      (put-description secondary-description "Sport zones based on this analysis have been set. You can edit sport zones from the Athlete / Edit Sport Zones menu")
      (send set-secondary-button enable #f))

    (define (put-description editor contents)
      (send editor lock #f)
      (send editor begin-edit-sequence)
      (send editor select-all)
      (send editor clear)
      (send editor insert (make-object string-snip% contents))
      (send editor set-modified #f)
      (send editor end-edit-sequence)
      (send editor lock #t))

    (define (switch-tabs index)
      (send detail-panel change-children
            (lambda (old)
              (list (if (= index 0) analysis-panel plot-panel)))))

    (define (setup-analysis-display session-id
                                    segment
                                    zones
                                    best-pict-canvas
                                    zones-canvas
                                    group-box
                                    description
                                    button)
      (if segment
          (let ()
            (send best-pict-canvas set-pict (pp-segment/pict segment))
            (send zones-canvas set-pict (pp-sport-zones/pict zones #:show-validity-range? #t))
            (define name (hash-ref segment 'name #f))
            (send group-box set-label (format "~a Zones" name))
            (send button set-label (format "Set These ~a Zones" name))
            (define actual-zones (send sport-zones sport-zones-for-sport
                                  (hash-ref segment 'sport)
                                  #f ; NOTE: we don't support zones for sub-sports for now
                                  (hash-ref segment 'zone-metric)))
            (cond ((and actual-zones
                        (> (sz-valid-from actual-zones) (sz-valid-from zones)))
                   (put-description description "Cannot set sport zones, because newer set of sport zones are installed. You can edit sport zones from the Athlete / Edit Sport Zones menu")
                   (send button enable #f))
                  ((have-sport-zones-from-session? database session-id (hash-ref segment 'zone-metric))
                   (put-description description "Sport zones based on this analysis have already been set.  You can edit sport zones from the Athlete / Edit Sport Zones menu")
                   (send button enable #f))
                  (#t
                   (put-description description "Set these sport zones.")
                   (send button enable #t)))
            (send group-box reflow-container))
          (begin
            (put-description description "")
            (send button enable #f)
            (send group-box set-label "No Zones")
            (send button set-label "No Primary Zones")
            (send best-pict-canvas set-pict #f)
            (send zones-canvas set-pict #f))))

    (define/private (load-data db sc szs session-id)
      (set! database db)
      (set! sport-charms sc)
      (set! sport-zones szs)
      (set! fthr-data (load-fthr-data db session-id))
      (match-define (fthr df sinfo primary secondary pz sz) fthr-data)
      (when sinfo
        (send headline set-pict (and sinfo (pp-session-info/pict sinfo sport-charms))))
      (if (or primary secondary)
          (begin
            (send detail-panel enable #t)
            (queue-task "fthr-analysis/setup-plots"
                        (lambda () (setup-plots plot-panel fthr-data)))
            (setup-analysis-display session-id
                                    primary
                                    pz
                                    primary-best
                                    primary-zones
                                    primary-group-box
                                    primary-description
                                    set-primary-button)
            (setup-analysis-display session-id
                                    secondary
                                    sz
                                    secondary-best
                                    secondary-zones
                                    secondary-group-box
                                    secondary-description
                                    set-secondary-button))
          ;; else, try to provide a meaningful error message
          (let ([sport (df-get-property df 'sport #f)])
            (send detail-panel enable #f)
            (switch-tabs 1)
            (define message
              (cond ((is-runnig? sport)
                     (if (df-contains/any? df "hr" "pace")
                         "Session is too short"
                         "Session needs Heart Rate or Pace data"))
                    ((is-cycling? sport)
                     (if (df-contains/any? df "power" "hr")
                         "Session is too short"
                         "Session needs Power or Heart Rate data"))
                    (#t
                     "Session has unsupported sport type")))
            (send plot-panel set-background-message message))))

    (define/private (on-close-dashboard)
      (set! database #f)
      (set! sport-charms #f)
      (set! sport-zones #f)
      (set! fthr-data #f))

    ;; Show the dialog.  PARENT is the parent window for the dialog.  This
    ;; method does not return until either "Save" or "Cancel" are pressed, or
    ;; the dialog is closed.  Returns #t if save was pressed, #f otherwise.
    ;;
    ;; A derived class might want to provide a "show-dialog" method that wraps
    ;; this one, and sets up the dialog contents for editing and actually
    ;; saves the result when the dialog is closed.
    (define/public (show-dashboard parent db sport-charms sport-zones sid)
      (let ((old-toplevel toplevel-window))
        (let ((toplevel (if parent (make-toplevel-dialog parent) toplevel-window)))
          (send dashboard-contents reparent toplevel)
          (set! toplevel-window toplevel))
        (thread/dbglog (lambda () (load-data db sport-charms sport-zones sid)))
        (send toplevel-window show #t) ; will block until finish-dialog is called
        (set! toplevel-window old-toplevel)
        (void)))

    ))

(define (show-fthr-analisys-dashboard toplevel database sport-charms sport-zones sid)
  (define dashboard (new fthr-dashboard%))
  (send dashboard show-dashboard toplevel database sport-charms sport-zones sid))

(provide/contract
 (show-fthr-analisys-dashboard (-> (or/c #f (is-a?/c top-level-window<%>))
                                   connection?
                                   (is-a?/c sport-charms%)
                                   (is-a?/c sport-zones%)
                                   exact-positive-integer? any/c)))
