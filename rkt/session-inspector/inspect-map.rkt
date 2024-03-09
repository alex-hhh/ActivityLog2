#lang racket/base
;; inspect-map.rkt -- map view for a session
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015, 2018, 2019, 2020, 2021, 2023, 2024 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require data-frame
         map-widget
         map-widget/utils
         racket/class
         racket/contract
         racket/dict
         racket/gui/base
         racket/match
         racket/format
         racket/math
         pict
         plot
         "../al-widgets.rkt"
         "../widgets/dragable-split-panel.rkt"
         "../fit-file/activity-util.rkt"
         "../session-df/native-series.rkt"
         "../utilities.rkt"
         "../fmt-util.rkt"
         "../fmt-util-ut.rkt"
         "inspect-graphs.rkt"
         "../session-df/traffic.rkt")

(provide map-panel%)

;; TODO: copied from native-series.rkt, really need to normalize these
;; overlay colors, fonts and faces
(define pd-background (make-object color% #xff #xf8 #xdc 0.95))
(define pd-item-color (make-object color% #x2f #x4f #x4f))
(define pd-label-color (make-object color% #x77 #x88 #x99))
(define pd-title-font (send the-font-list find-or-create-font 12 'default 'normal 'normal))
(define pd-item-font (send the-font-list find-or-create-font 12 'default 'normal 'normal))
(define pd-label-font (send the-font-list find-or-create-font 10 'default 'normal 'normal))
(define pd-title-face (cons pd-item-color pd-title-font))
(define pd-item-face (cons pd-item-color pd-item-font))
(define pd-label-face (cons pd-label-color pd-label-font))

(define (get-index df timestamp)
  (if timestamp
      (df-index-of df "timestamp" timestamp)
      (df-row-count df)))

(define (extract-track* df start stop)
  (df-select* df "lat" "lon" #:filter valid-only #:start start #:stop stop))

;; Create map layers for the data frame DF using DRAWING-PEN and ZORDER as the
;; pen and zorder respectively.  A separate layer is created for each lap, the
;; layer name being the lap index.  This way the map view itself can highlight
;; / unhighlight laps by just adjusting the pen/zorder of the layer as well as
;; removing / adding layers.
;;
;; Special care is taken to create split tracks around teleport points so the
;; teleport does not show up as an unnatural line on the map (for example ski
;; lift runs on skiing activities), see the implementation.
(define (make-map-layers df drawing-pen zorder)

  ;; NOTE: TELEPORTS are the timestamps where the recording was stopped, the
  ;; user traveled a significant distance, than re-started the recording.
  ;; They are used, for example, when recording ski activities and the user is
  ;; taking the ski lift. For other activities, this is usually empty.
  ;;
  ;; TODO: TELEPORTS is a list, while LAPS is a vector -- this was a bad
  ;; decision, should be fixed by making LAPS a list as well...

  (define laps
    (df-get-property df 'laps (lambda () (vector 0 (df-row-count df)))))
  (define lap-count (vector-length laps))

  (let layer-loop ([layers null]
                   [teleports (df-get-property df 'teleport-points null)]
                   [lap-number 0])
    (if (< lap-number lap-count)
        (let ([lap-start (vector-ref laps lap-number)]
              [lap-end (if (< (add1 lap-number) lap-count)
                           (vector-ref laps (add1 lap-number))
                           #f)])
          (define start-index (get-index df lap-start))
          (define end-index (get-index df lap-end))
          (if (null? teleports)
              (let ([l (line-layer lap-number (extract-track* df start-index end-index) #:pen drawing-pen #:zorder zorder)])
                (layer-loop (cons l layers) teleports (add1 lap-number)))
              (let ([t (car teleports)])
                (cond ((< t lap-start)
                       (layer-loop layers (cdr teleports) lap-number))
                      ((>= t lap-end)
                       ;; No teleports in this lap
                       (let ([l (line-layer lap-number (extract-track* df start-index end-index) #:pen drawing-pen #:zorder zorder)])
                         (layer-loop (cons l layers) teleports (add1 lap-number))))
                      (else
                       (define-values (tracks remaining-teleports)
                         (let teleport-loop ([teleports teleports]
                                             [tracks null]
                                             [start-index start-index])
                           (if (null? teleports)
                               (let ([track (extract-track* df start-index end-index)])
                                 (values (cons track tracks) teleports))
                               (let ([t (car teleports)])
                                 (if (< t lap-end)
                                     (let* ([teleport-index (get-index df t)]
                                            [track (extract-track* df start-index (sub1 teleport-index))])
                                       (teleport-loop
                                        (cdr teleports)
                                        (cons track tracks)
                                        (add1 teleport-index)))
                                     (values tracks teleports))))))
                       (let ([l (lines-layer lap-number (reverse tracks) #:pen drawing-pen #:zorder zorder)])
                         (layer-loop (cons l layers) remaining-teleports (add1 lap-number))))))))
        (reverse layers))))

;; Create a layer (named 'traffic) to show traffic data from the data frame,
;; or return #f if there is no traffic data in the data frame.  The traffic
;; layer is a points layer with a hover callback showing traffic information
;; about each overtaking.
;;
;; Traffic data is recorded by a Garmin Varia radar using the MyBikeTraffic IQ
;; field, see https://www.mybiketraffic.com/about/ for more details.
(define (make-traffic-layer df)
  (define traffic (maybe-get-traffic-data! df))
  (if traffic
      (let ([hover-picts (make-vector (length traffic) #f)])

        (define (hover-callback pindex)
          (unless (vector-ref hover-picts pindex)
            (define tentry (list-ref traffic pindex))

            (define data-table
              (let* ([vnum (hash-ref tentry 'total-vehicles #f)]
                     [tod (hash-ref tentry 'timestamp #f)]
                     [vspeed (hash-ref tentry 'vehicle-speed #f)]
                     [rspeed (hash-ref tentry 'rider-speed #f)]
                     [ospeed (hash-ref tentry 'overtaking-speed)]
                     [odist (hash-ref tentry 'overtaking-distance #f)]
                     [oduration (hash-ref tentry 'overtaking-duration #f)]
                     [entries
                      (list
                       (text "Vehicle #" pd-label-face)
                       (text (if (rational? vnum) (~a (exact-truncate vnum)) "N/A") pd-item-face)
                       (text "Time Of Day" pd-label-face)
                       (text (if (rational? tod) (time-of-day->string tod) "N/A") pd-item-face)
                       (text "Vehicle Speed" pd-label-face)
                       (text (if (rational? vspeed) (speed->string vspeed #t) "N/A") pd-item-face)
                       (text "Rider Speed" pd-label-face)
                       (text (if (rational? rspeed) (speed->string rspeed #t) "N/A") pd-item-face)
                       (text "Overtaking Speed" pd-label-face)
                       (text (if (rational? ospeed) (speed->string ospeed #t) "N/A") pd-item-face)
                       (text "Overtaking Distance" pd-label-face)
                       (text (if (rational? odist) (short-distance->string odist #t) "N/A") pd-item-face)
                       (text "Overtaking Duration" pd-label-face)
                       (text (if (rational? oduration) (string-append (~a (exact-truncate oduration) " seconds")) "N/A") pd-item-face))])
                (table 2 entries
                       (list lc-superimpose rc-superimpose lc-superimpose)
                       cc-superimpose 15 3)))

            (define speed-plot
              (match-let ([(vector start stop) (hash-ref tentry'df-index)])
                (if (and (rational? start)
                         (rational? stop)
                         (< start stop)
                         (< stop (df-row-count df)))
                    (let ([speed-data (df-select* df "elapsed" "mbrt_absolute_speed"
                                                  #:start start
                                                  #:stop stop
                                                  #:filter valid-only)])
                      (define-values (y-min y-max)
                        (for/fold ([y-min +inf.0]
                                   [y-max -inf.0]
                                   #:result (values (max 0 (* 10 (exact-floor (/ (- y-min 1) 10))))
                                                    (* 10 (exact-ceiling (/ (+ y-max 1) 10)))))
                                  ([d (in-vector speed-data)])
                          (define y (vector-ref d 1))
                          (values (min y-min y) (max y-max y))))
                      (parameterize ([plot-x-tick-labels? #f]
                                     ;;[plot-y-tick-labels? #f]
                                     [plot-x-label #f]
                                     [plot-y-label #f]
                                     [plot-line-width 0.3]
                                     [plot-background-alpha 0.0]
                                     [plot-font-size 7])
                        (plot-pict (list (tick-grid)
                                         (lines speed-data #:width 2 #:color '(238 51 119)))
                                   #:y-min y-min
                                   #:y-max y-max
                                   #:width (* 0.8 (pict-width data-table))
                                   #:height 70)))
                    #f)))

            (define data-pict
              (if speed-plot (vc-append 5 data-table speed-plot) data-table))

            (define final-pict
              (cc-superimpose
               (filled-rounded-rectangle
                (+ (pict-width data-pict) 20) (+ (pict-height data-pict) 20) -0.01
                #:draw-border? #t
                #:border-width 0.5
                #:color pd-background)
               data-pict))
            (vector-set! hover-picts pindex final-pict))

          (vector-ref hover-picts pindex))

        (points-layer
         'traffic
         (for/list ([t (in-list traffic)])
           (hash-ref t 'location))
         #:hover-callback hover-callback))

      ;; No traffic layer can be added
      #f))


;; Add a map-point data series to the data frame DF.  map-points represent a
;; location in normalized coordinates (0..1).  These are used to interpolate a
;; position by `lookup-position`
(define/contract (add-map-point-series! df)
  (-> data-frame? any/c)
  (df-add-derived!
   df
   "map-point"
   '("lat" "lon")
   (lambda (val)
     (and val
          (match-let (((list lat lon) val))
            (and (real? lat)
                 (real? lon)
                 (lat-lon->npoint lat lon)))))))

;; Lookup a GPS position in the data frame DF at distance DST.  The data frame
;; is assumed to have "lat", "lon", a "dst" and a "map-point" (see
;; `add-map-points`) data series.  If DST does not fall on an exact item in
;; the data series, a location is interpolated between two adjacent GPS
;; positions.  If DST is outside the range if the dst series, the first or
;; last position is returned.
(define/contract (lookup-position df dst)
  (-> data-frame? real? (or/c (vector/c real? real?) #f))

  (unless (df-contains? df "map-point")
    (add-map-point-series! df))

  (define index (df-index-of df "distance" dst))

  (cond ((<= index 0)
         (define location (df-ref* df 0 "lat" "lon"))
         (match-define (vector lat lon) location)
         (if (and lat lon) location #f))
        ((>= index (df-row-count df))
         (define location (df-ref* df (sub1 (df-row-count df)) "lat" "lon"))
         (match-define (vector lat lon) location)
         (if (and lat lon) location #f))
        (#t
         (let* ((pdst (df-ref df (sub1 index) "distance"))
                (adst (df-ref df index "distance"))
                (prev-pos (df-ref df (sub1 index) "map-point"))
                (next-pos (df-ref df index "map-point"))
                (factor (/ (- dst pdst) (- adst pdst))))
           ;; positions might not be found in the data frame -- this can
           ;; happen if the track has gaps in it, for example if
           ;; running/riding through a tunnel.
           (if (and prev-pos next-pos)
               (let ((pos (npoint
                           (+ (* factor (npoint-x next-pos))
                              (* (- 1 factor) (npoint-x prev-pos)))
                           (+ (* factor (npoint-y next-pos))
                              (* (- 1 factor) (npoint-y prev-pos))))))
                 (let-values (((lat lon) (npoint->lat-lon pos)))
                   (vector lat lon)))
               #f)))))

(define map-panel%
  (class object%
    (init parent)
    (init-field
     [get-preference get-pref]
     [put-preference put-pref])
    (super-new)

    ;; Tag used to save preferences for this panel
    (define the-pref-tag 'activity-log:map-panel)

    ;; Drawing resources

    (define header-font
      (send the-font-list find-or-create-font 18 'default 'normal 'normal))
    (define warning-font
      (send the-font-list find-or-create-font 12 'default 'normal 'normal))

    (define main-track-pen
      (send the-pen-list find-or-create-pen
            (make-color 226 34 62) 3 'solid 'round 'round))
    (define selected-track-pen
      (send the-pen-list find-or-create-pen
            (make-color 24 60 165) 7 'solid 'round 'round))

    (define the-session #f)

    ;; Data frame associated with the session
    (define data-frame #f)

    ;; The map layers representing tracks in this session, one lines-layer for
    ;; each lap...
    (define track-map-layers null)

    ;; The name of the file used by 'on-interactive-export-image'. This is
    ;; remembered between subsequent exports, but reset when the session
    ;; changes
    (define export-file-name #f)

    ;; Stores a copy of the map-panel split (get-percentages) -- When there is
    ;; no elevation data, we delete the elevation plot from the map panel,
    ;; however, we keep the panel split ratio here, so we can restore it if a
    ;; session with elevation data is inspected again.
    (define saved-map-panel-split '())

    ;; When #t, the selected lap is made to fit the view
    (define zoom-to-lap? #f)

    ;; When #t, only the selected lap is shown on the map.  This is useful if
    ;; the track overlaps onto itself several times.
    (define show-selected-lap-only? #f)

    ;; When #t, and if traffic data is available, show it on the map.
    (define show-traffic-layer? #t)

    (define selected-lap #f)
    (define selected-lap-data #f)

    (define panel (new (class horizontal-dragable-split-panel%
                         (init)
                         (super-new)
                         (define/public (interactive-export-image)
                           (on-interactive-export-image)))
                       [parent parent]
                       [border 0]
                       [alignment '(center top)]))

    (define interval-view-panel
      (new vertical-pane%
           [parent panel]
           [border 0]
           [spacing 1]
           [min-width 220]
           [stretchable-width #t]
           [alignment '(left top)]))
    
    (define interval-coice
      (let ((p (new horizontal-pane%
                    [parent interval-view-panel]
                    [spacing 10]
                    [stretchable-height #f]
                    [alignment '(left center)])))
        (new message%
             [parent p]
             [label "Laps"]
             [font header-font])
        (new interval-choice%
             [parent p]
             [tag 'interval-choice-map]
             [label ""])))

    (define interval-view
      (new mini-interval-view%
           [parent interval-view-panel]
           [tag 'activity-log:map-mini-lap-view]
           [callback (lambda (n lap selected?)
                       (if selected?
                           (highlight-lap lap)
                           (unhighlight-lap)))]))
    (send interval-coice set-interval-view interval-view)

    (define map-panel (new vertical-dragable-split-panel%
                           [parent panel]
                           [border 0]
                           [alignment '(left top)]))

    (define zoom-slider #f)
    (define info-message #f)
    (define the-map #f)
    (define traffic-layer #f)
    (define track-location-checkbox #f)
    (define show-traffic-checkbox #f)

    (let ([p0 (new vertical-pane% [parent map-panel] [border 0] [spacing 1])])
      (let ((p (new horizontal-pane%
                    [parent p0]
                    [spacing 10]
                    [stretchable-height #f]
                    [alignment '(left center)])))
        (new message% [parent p] [label "Map"] [font header-font])
        (new check-box% [parent p] [label "Zoom to Lap"]
             [value zoom-to-lap?]
             [callback (lambda (b e) (zoom-to-lap (send b get-value)))])
        (new check-box% [parent p] [label "Show Only Selected Lap"]
             [value show-selected-lap-only?]
             [callback (lambda (b e) (show-selected-lap-only (send b get-value)))])
        (set! track-location-checkbox
              (new check-box%
                   [parent p]
                   [label "Track Location"]
                   [callback (lambda (c e)
                               (on-track-location (send c get-value)))]))
        (set! show-traffic-checkbox
              (new check-box%
                   [parent p]
                   [label "Show Traffic"]
                   [callback (lambda (c e)
                               (on-show-traffic (send c get-value)))]))
        (set! zoom-slider
              (new slider% [parent p] [label "Zoom Level "]
                   [min-value (min-zoom-level)]
                   [max-value (max-zoom-level)]
                   [stretchable-width #f]
                   [min-width 200]
                   [style '(horizontal plain)]
                   [callback (lambda (b e) (set-zoom-level (send b get-value)))]))
        (new button% [parent p] [label "Fit to Window"]
             [callback (lambda (b e) (resize-to-fit))])
        (let ((p0 (new horizontal-pane%
                       [parent p]
                       [alignment '(right center)])))
          (set! info-message (new message% [parent p0] [label ""]
                                  [font warning-font]
                                  [stretchable-width #f] [auto-resize #t]))
          ;; Add a spacer here
          (new message% [parent p0] [label ""] [stretchable-width #f] [min-width 10])))

      (set! the-map
            (new (class map-widget% (init) (super-new)
                   (define/override (on-zoom-level-change zl)
                     (send zoom-slider set-value zl)))
                 [parent p0])))

    (define the-cll (current-location-layer 'current-location))
    (send the-map add-layer the-cll)

    (define elevation-graph-pane
      (new horizontal-panel% [parent map-panel] [stretchable-height #t]))

    (define the-elevation-graph #f)

    (define grade+alt-graph
      (new alt+shaded-grade-graph%
           [parent elevation-graph-pane]
           [min-height 150]
           [style '(deleted)]
           [hover-callback (lambda (x) (on-hover x))]))

    (define grade+calt-graph
      (new calt+shaded-grade-graph%
           [parent elevation-graph-pane]
           [min-height 150]
           [style '(deleted)]
           [hover-callback (lambda (x) (on-hover x))]))

    (send* grade+alt-graph
      (begin-edit-sequence)
      (zoom-to-lap zoom-to-lap?)
      (set-filter-amount 0) ; no elevation filtering
      (set-x-axis axis-distance)
      (end-edit-sequence))

    (send* grade+calt-graph
      (begin-edit-sequence)
      (zoom-to-lap zoom-to-lap?)
      (set-filter-amount 0) ; no elevation filtering
      (set-x-axis axis-distance)
      (end-edit-sequence))

    (define/private (on-hover x)
      (when the-elevation-graph
        (send the-elevation-graph draw-marker-at x))
      (define location (and x (lookup-position data-frame x)))
      (send the-cll current-location location))

    (define/private (zoom-to-lap flag)
      (set! zoom-to-lap? flag)
      (when the-elevation-graph
        (send the-elevation-graph zoom-to-lap flag))
      (when zoom-to-lap?
        (send the-map resize-to-fit selected-lap)))

    (define/private (show-selected-lap-only flag)
      (unless (eq? flag show-selected-lap-only?)
        (set! show-selected-lap-only? flag)
        (when selected-lap-data
          (highlight-lap selected-lap-data))))

    (define/private (set-zoom-level v)
      (send the-map zoom-level v))

    (define/private (resize-to-fit)
      (send the-map resize-to-fit))

    (define/private (remove-all-layers)
      (send the-map remove-layer 'markers)
      (send the-map remove-layer 'custom)
      (send the-map remove-layer 'traffic)
      (for ([l (in-list track-map-layers)])
        (send the-map remove-layer (send l get-name))))

    (define/private (remove-track-layers)
      (for ([l (in-list track-map-layers)])
        (send the-map remove-layer (send l get-name))))

    (define/private (highlight-lap lap)
      (send the-map begin-edit-sequence)
      (remove-track-layers)

      (let ((lap-num (dict-ref lap 'lap-num #f))
            (custom-lap? (dict-ref lap 'custom-lap #f))
            (start (lap-start-time lap))
            (elapsed (lap-elapsed-time lap)))

        ;; Highlight corresponding lap on the elevation graph
        (when the-elevation-graph
          (send the-elevation-graph highlight-interval start (+ start elapsed)))

        (set! selected-lap (if custom-lap? 'custom (- lap-num 1)))

        (if custom-lap?
            ;; Extract the track data for the current lap and add it to the map.
            (begin
              (unless show-selected-lap-only?
                (for ([l (in-list track-map-layers)])
                  (send* l
                    (set-pen main-track-pen)
                    (set-zorder 0.8))
                  (send the-map add-layer l)))
              (match-let (((list start-idx end-idx)
                           (df-index-of* data-frame "timestamp" start (+ start elapsed))))
                (let ((track (extract-track* data-frame start-idx (add1 end-idx))))
                  (send the-map add-layer (line-layer 'custom track #:pen selected-track-pen #:zorder 0.1)))))
            (for ([l (in-list track-map-layers)])
              (cond ((equal? (send l get-name) selected-lap)
                     (send* l
                       (set-pen selected-track-pen)
                       (set-zorder 0.75))
                     (send the-map add-layer l))
                    ((not show-selected-lap-only?)
                     (send* l
                       (set-pen main-track-pen)
                       (set-zorder 0.8))
                     (send the-map add-layer l)))))

        ;; Zoom canvas to current lap (if needed)
        (when zoom-to-lap?
          (send the-map resize-to-fit selected-lap))

        (set! selected-lap-data lap)
        (send the-map end-edit-sequence)))

    (define/private (unhighlight-lap)
      (when the-elevation-graph
        (send the-elevation-graph highlight-interval #f #f))
      (send the-map begin-edit-sequence)
      (remove-track-layers)
      (for ([l (in-list track-map-layers)])
        (send* l
          (set-pen main-track-pen)
          (set-zorder 0.8))
        (send the-map add-layer l))
      (send the-map end-edit-sequence)
      (set! selected-lap-data #f))

    (define/private (on-track-location flag)
      (send the-cll track-current-location flag))

    (define/private (on-show-traffic flag)
      (set! show-traffic-layer? flag)
      (when traffic-layer
        (if show-traffic-layer?
            (send the-map add-layer traffic-layer)
            (send the-map remove-layer 'traffic))))

    (let ([pref (get-preference the-pref-tag #f)])
      ;; Restore the panel splits for the interval and map panels, or set
      ;; default ones.
      (let-values
          ([(ips mps)
            (if (hash? pref)
                (values
                 (hash-ref pref 'interval-panel-split '(1/5 4/5))
                 (hash-ref pref 'map-panel-split '(4/5 1/5)))
                (values
                 '(1/5 4/5)
                 '(4/5 1/5)))])
        (send panel set-percentages ips)
        (send map-panel set-percentages mps))
      (let ([flag (if (hash? pref)
                      (hash-ref pref 'track-location #t)
                      #t)])
        (send track-location-checkbox set-value flag)
        (on-track-location flag))
      (let ([flag (if (hash? pref)
                      (hash-ref pref 'show-traffic #t)
                      #t)])
        (send show-traffic-checkbox set-value flag)
        (on-show-traffic flag)))

    (define/public (save-visual-layout)
      (send interval-coice save-visual-layout)
      (send interval-view save-visual-layout)
      (define ips (send panel get-percentages))
      (define mps
        (let ([mps (send map-panel get-percentages)])
          (if (= (length mps) 2)
              mps
              saved-map-panel-split)))
      (put-preference
       the-pref-tag
       (hash
        'interval-panel-split ips
        'map-panel-split mps
        'track-location (send the-cll track-current-location)
        'show-traffic show-traffic-layer?)))

    ;; Return a suitable file name for use by 'on-interactive-export-image'.
    ;; If 'export-file-name' is set, we use that, otherwise we compose a file
    ;; name from the session id.
    (define (get-default-export-file-name)
      (or export-file-name
          (let ((sid (df-get-property data-frame 'session-id)))
            (if sid (format "map-~a.png" sid) "map.png"))))

    (define/public (on-interactive-export-image)
      (let ((file (put-file "Select file to export to" #f #f
                            (get-default-export-file-name) "png" '()
                            '(("PNG Files" "*.png") ("Any" "*.*")))))
        (when file
          (set! export-file-name file)
          (send the-map export-image-to-file file))))

    (define/private (maybe-setup-traffic)
      (set! traffic-layer (make-traffic-layer data-frame))
      (send show-traffic-checkbox enable (if traffic-layer #t #f))
      (when (and traffic-layer show-traffic-layer?)
        (send the-map add-layer traffic-layer)))

    (define/public (set-session session df)

      (set! the-session session)
      (set! data-frame df)
      (set! export-file-name #f)

      (set! the-elevation-graph
            (cond ((df-contains? df "calt") grade+calt-graph)
                  ((df-contains? df "alt") grade+alt-graph)
                  (#t #f)))

      (when the-elevation-graph
        (send* the-elevation-graph
          (begin-edit-sequence)
          (highlight-interval #f #f)
          (zoom-to-lap zoom-to-lap?)
          (set-data-frame df)
          (end-edit-sequence)))

      (send elevation-graph-pane
            change-children
            (lambda (old)
              (define canvas
                (and the-elevation-graph
                     (send the-elevation-graph get-graph-canvas)))
              (if canvas (list canvas) '())))

      ;; When the elevation plot is present (two items in the panel), save the
      ;; split ratios
      (let ([split (send map-panel get-percentages)])
        (when (= (length split) 2)
          (set! saved-map-panel-split split)))

      ;; Put or delete the elevation panel (depending on whether we have an
      ;; elevation plot or not.
      (send map-panel
            change-children
            (lambda (old)
              ;; NOTE: (car old) is p0, see widget creation above, which holds
              ;; the map
              (if the-elevation-graph
                  (list (car old) elevation-graph-pane)
                  (list (car old)))))

      ;; Must reflow container in map-panel, so it "knows" its number of
      ;; children and splits and the next step works correctly.
      (send map-panel reflow-container)

      ;; If the map panel contains an elevation plot, restore the percentages
      ;; now.
      (when (= (length (send map-panel get-percentages)) 2)
        (send map-panel set-percentages saved-map-panel-split))

      (send info-message set-label
            (if (allow-tile-download) "" "Map tile download disabled"))

      (send interval-coice set-session session df)

      (send the-map begin-edit-sequence)
      (remove-all-layers)
      (set! track-map-layers (make-map-layers df main-track-pen 0.8))
      (for ([l (in-list track-map-layers)])
        (send the-map add-layer l))
      ;; Add flags for the first and last valid GPS points on the route.  We
      ;; search the first valid point from both ends, rather than just
      ;; assuming that the first and last points are valid.
      (let ([first-position
             (for/first ([(lat lon) (in-data-frame data-frame "lat" "lon")]
                         #:when (and (rational? lat) (rational? lon)))
               (vector lat lon))]
            [last-position
             (for/first ([(lat lon) (in-data-frame data-frame "lat" "lon"
                                                   #:start (sub1 (df-row-count data-frame))
                                                   #:stop -1)]
                         #:when (and (rational? lat) (rational? lon)))
               (vector lat lon))])
        (when (and first-position last-position)
          (send the-map add-layer
                (markers-layer
                 'markers
                 (list
                  (list first-position "Start" 1 (make-color 0 135 36))
                  (list last-position "End" -1 (make-color 150 33 33)))))))

      (maybe-setup-traffic)
      (send the-map end-edit-sequence)

      ;; NOTE: the `resize-to-fit` event must be queued with a low priority --
      ;; this will produce a flicker on the map, but otherwise, the map widget
      ;; will not receive the resize event and will not be able to use the
      ;; correct window dimensions and the resize-to-fit will produce
      ;; incorrect results...
      ;;
      ;; The problem is that the map-widget% must receive a on-size callback
      ;; (also delivered as an event), so resize-to-fit will not have the
      ;; correct results when we change the size of the map via the
      ;; `change-children` calls above.
      (queue-callback
       (lambda ()
         (send the-map resize-to-fit)
         (send zoom-slider set-value (send the-map zoom-level)))
       #f))

    ))
