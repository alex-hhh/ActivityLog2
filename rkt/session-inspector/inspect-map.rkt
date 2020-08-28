#lang racket/base
;; inspect-map.rkt -- map view for a session
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015, 2018, 2019, 2020 Alex Harsányi <AlexHarsanyi@gmail.com>
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
         math/statistics
         plot
         racket/class
         racket/contract
         racket/dict
         racket/gui/base
         racket/match
         "../al-widgets.rkt"
         "../fit-file/activity-util.rkt"
         "../session-df/native-series.rkt"
         "../session-df/session-df.rkt"
         "inspect-graphs.rkt")

(provide map-panel%)

(define *header-font*
  (send the-font-list find-or-create-font 18 'default 'normal 'normal))
(define *warning-font*
  (send the-font-list find-or-create-font 12 'default 'normal 'normal))

(define (get-index df timestamp)
  (if timestamp
      (df-index-of df "timestamp" timestamp)
      (df-row-count df)))

(define (extract-track* df start stop)
  (df-select* df "lat" "lon" #:filter valid-only #:start start #:stop stop))

;; Add a map-point data series to the data frame DF.  map-points represent a
;; location in normalized coordinates (0..1).  These are used to interpolate a
;; position by `lookup-position`
(define/contract (add-map-points df)
  (-> data-frame? any/c)
  (df-add-derived
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
    (add-map-points df))

  (define index (df-index-of df "distance" dst))

  (cond ((<= index 0)
         (df-ref* df 0 "lat" "lon"))
        ((>= index (df-row-count df))
         (df-ref* df (sub1 (df-row-count df)) "lat" "lon"))
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
    (super-new)

    (define the-pref-tag 'activity-log:map-panel)
    (define the-session #f)

    ;; When #t, the selected lap is made to fit the view
    (define zoom-to-lap? #f)

    ;; When #t, only the selected lap is shown on the map.  This is useful if
    ;; the track overlaps onto itself several times.
    (define show-selected-lap-only? #f)

    (define panel (new (class horizontal-pane%
                         (init)
                         (super-new)
                         (define/public (interactive-export-image)
                           (on-interactive-export-image)))
                       [parent parent]
                       [border 0]
                       [spacing 1]
                       [alignment '(center top)]))

    (define interval-view-panel (new vertical-pane%
                                     [parent panel]
                                     [border 0]
                                     [spacing 1]
                                     [min-width 220]
                                     [stretchable-width #f]
                                     [alignment '(left top)]))
    (define interval-coice #f)
    (let ((p (new horizontal-pane%
                  [parent interval-view-panel]
                  [spacing 10]
                  [stretchable-height #f]
                  [alignment '(left center)])))
      (new message% [parent p] [label "Laps"] [font *header-font*])
      (set! interval-coice (new interval-choice% [tag 'interval-choice-map] [parent p] [label ""])))

    (define interval-view
      (new mini-interval-view%
           [parent interval-view-panel]
           [tag 'activity-log:map-mini-lap-view]
           [callback (lambda (n lap) (highlight-lap lap))]))
    (send interval-coice set-interval-view interval-view)

    (define map-panel (new vertical-pane%
                           [parent panel]
                           [border 0]
                           [spacing 1]
                           [alignment '(left top)]))

    (define zoom-slider #f)
    (define info-message #f)

    (let ((p (new horizontal-pane%
                  [parent map-panel]
                  [spacing 10]
                  [stretchable-height #f]
                  [alignment '(left center)])))
      (new message% [parent p] [label "Map"] [font *header-font*])
      (new check-box% [parent p] [label "Zoom to Lap"]
           [value zoom-to-lap?]
           [callback (lambda (b e) (zoom-to-lap (send b get-value)))])
      (new check-box% [parent p] [label "Show Only Selected Lap"]
           [value show-selected-lap-only?]
           [callback (lambda (b e) (show-selected-lap-only (send b get-value)))])
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
                                [font *warning-font*]
                                [stretchable-width #f] [auto-resize #t]))
        ;; Add a spacer here
        (new message% [parent p0] [label ""] [stretchable-width #f] [min-width 10])))

    (define map-view
      (new (class map-widget% (init) (super-new)
             (define/override (on-zoom-level-change zl)
               (send zoom-slider set-value zl)))
           [parent map-panel]))

    (send map-view track-current-location #t)

    (define elevation-graph-pane
      (new horizontal-pane% [parent map-panel] [stretchable-height #f]))

    (define the-elevation-graph #f)

    (define grade+alt-graph
      (new grade+alt-graph%
           [parent elevation-graph-pane]
           [min-height 150]
           [style '(deleted)]
           [hover-callback (lambda (x) (on-hover x))]))

    (define grade+calt-graph
      (new grade+calt-graph%
           [parent elevation-graph-pane]
           [min-height 150]
           [style '(deleted)]
           [hover-callback (lambda (x) (on-hover x))]))

    (send grade+alt-graph begin-edit-sequence)
    (send grade+alt-graph zoom-to-lap zoom-to-lap?)
    (send grade+alt-graph set-filter-amount 0) ; no elevation filtering
    (send grade+alt-graph set-x-axis axis-distance)
    (send grade+alt-graph end-edit-sequence)

    (send grade+calt-graph begin-edit-sequence)
    (send grade+calt-graph zoom-to-lap zoom-to-lap?)
    (send grade+calt-graph set-filter-amount 0) ; no elevation filtering
    (send grade+calt-graph set-x-axis axis-distance)
    (send grade+calt-graph end-edit-sequence)

    (define (on-hover x)
      (when the-elevation-graph
        (send the-elevation-graph draw-marker-at x))
      (define location (and x (lookup-position data-frame x)))
      (send map-view current-location location))

    (define selected-lap #f)
    (define selected-lap-data #f)

    (define (zoom-to-lap flag)
      (set! zoom-to-lap? flag)
      (when the-elevation-graph
        (send the-elevation-graph zoom-to-lap flag))
      (when zoom-to-lap?
        (send map-view resize-to-fit selected-lap)))

    (define (show-selected-lap-only flag)
      (unless (eq? flag show-selected-lap-only?)
        (set! show-selected-lap-only? flag)
        (when selected-lap-data
          (highlight-lap selected-lap-data))))

    (define (set-zoom-level v)
      (send map-view zoom-level v))

    (define (resize-to-fit)
      (send map-view resize-to-fit))

    (define (highlight-lap lap)
      (send map-view begin-edit-sequence)
      ;; Remove custom any lap and set all other tracks to default pen and
      ;; z-order (effectively un-highlights any highlighted lap).
      (send map-view delete-group 'custom)
      (send map-view set-group-pen #f
            (send the-pen-list find-or-create-pen
                  (make-object color% 226 34 62)
                  3
                  (if show-selected-lap-only? 'transparent 'solid)
                  'round 'round))
      (send map-view set-group-zorder #f 0.5)

      (let ((lap-num (dict-ref lap 'lap-num #f))
            (custom-lap? (dict-ref lap 'custom-lap #f))
            (start (lap-start-time lap))
            (elapsed (lap-elapsed-time lap)))

        ;; Highlight corresponding lap on the elevation graph
        (when the-elevation-graph
          (send the-elevation-graph highlight-interval start (+ start elapsed)))

        (set! selected-lap (if custom-lap? 'custom (- lap-num 1)))

        (when custom-lap?
          ;; Extract the track data for the current lap and add it to the map.
          (match-let (((list start-idx end-idx)
                       (df-index-of* data-frame "timestamp" start (+ start elapsed))))
            (let ((track (extract-track* data-frame start-idx (add1 end-idx))))
              (send map-view add-track track selected-lap)))))

      ;; Highlight current lap by setting a thicker pen of a different color
      ;; and putting the track on top.
      (send map-view set-group-pen selected-lap
            (send the-pen-list find-or-create-pen
                  (make-object color% 24 60 165)
                  7
                  'solid 'round 'round))
      (send map-view set-group-zorder selected-lap 0.1)

      ;; Zoom canvas to current lap (if needed)
      (when zoom-to-lap?
        (send map-view resize-to-fit selected-lap))

      (set! selected-lap-data lap)
      (send map-view end-edit-sequence))

    (define/public (save-visual-layout)
      (send interval-coice save-visual-layout)
      (send interval-view save-visual-layout))

    (define data-frame #f)
    ;; The name of the file used by 'on-interactive-export-image'. This is
    ;; remembered between subsequent exports, but reset when the session
    ;; changes
    (define export-file-name #f)

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
          (send map-view export-image-to-file file))))

    (define/public (set-session session df)
      (set! the-session session)
      (set! data-frame df)
      (set! export-file-name #f)
      (set! the-elevation-graph
        (cond ((df-contains? df "calt") grade+calt-graph)
              ((df-contains? df "alt") grade+alt-graph)
              (#t #f)))
      (when the-elevation-graph
        (send the-elevation-graph begin-edit-sequence)
        (send the-elevation-graph highlight-interval #f #f)
        (send the-elevation-graph zoom-to-lap zoom-to-lap?)
        (send the-elevation-graph set-data-frame df)
        (send the-elevation-graph end-edit-sequence))
      (send elevation-graph-pane
            change-children
            (lambda (old)
              (define canvas
                (and the-elevation-graph
                     (send the-elevation-graph get-graph-canvas)))
              (if canvas (list canvas) '())))

      (send info-message set-label
            (if (allow-tile-download) "" "Map tile download disabled"))

      (set! selected-lap #f)
      (send interval-coice set-session session df)

      ;; Add the data tracks

      ;; teleports are the timestamps where the recording was stopped, than
      ;; the user traveled a significant distance and re-started the
      ;; recording.  They are used intensively when recording ski runs, for
      ;; other activities, this should hopefully be empty.
      (define teleports (df-get-property data-frame 'teleport-points))

      (define laps (df-get-property data-frame 'laps))
      (define start (vector-ref laps 0))
      (define start-idx 0)

      (send map-view begin-edit-sequence)
      (send map-view clear)

      (for ([(lap group) (in-indexed (in-sequences (in-vector laps 1) (in-value #f)))])
        (for ([t teleports] #:when (and (< start t) lap (< t lap)))
          (let* ([end-idx (get-index data-frame t)]
                 [track (extract-track* data-frame start-idx end-idx)])
            (send map-view add-track track group)
            ;; Skip the teleport point
            (set! start-idx (add1 end-idx))))
        (let* ([end-idx (get-index data-frame lap)]
               [track (extract-track* data-frame start-idx end-idx)])
          (send map-view add-track track group)
          (set! start lap)
          ;; Make sure lap tracks are joined, but do not go backwards (this
          ;; accounts for empty laps)
          (set! start-idx (max start-idx (sub1 end-idx)))))

      (send map-view set-group-pen #f
            (send the-pen-list find-or-create-pen
                  (make-object color% 226 34 62)
                  3 'solid 'round 'round))
      (let ((nitems (df-row-count data-frame)))
        ;; Add flags for the first and last valid GPS points on the route.  We
        ;; search the first valid point from both ends, rather than just
        ;; assuming that the first and last points are valid.
        (for*/first ((index (in-range 0 nitems))
                     (position (in-value (df-ref* data-frame index "lat" "lon")))
                     #:when (and (vector-ref position 0) (vector-ref position 1)))
          (send map-view add-marker position "Start" 1 (make-color 0 135 36)))
        (for*/first ((index (in-range (sub1 nitems) 0 -1))
                     (position (in-value (df-ref* data-frame index "lat" "lon")))
                     #:when (and (vector-ref position 0) (vector-ref position 1)))
          (send map-view add-marker position "End" -1 (make-color 150 33 33))))
      (send map-view resize-to-fit)
      (send map-view end-edit-sequence))

    ))
