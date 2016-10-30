#lang racket/base
;; inspect-map.rkt -- map view for a session
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

(require racket/class
         racket/gui/base
         racket/list
         "activity-util.rkt"
         "al-widgets.rkt"
         "inspect-graphs.rkt"
         "map-widget.rkt"
         "map-util.rkt"
         "data-frame.rkt"
         "series-meta.rkt"
         "utilities.rkt")

(provide map-panel%)

(define *header-font*
  (send the-font-list find-or-create-font 18 'default 'normal 'normal))

(define (delta-time-and-distance df timestamp)
  (let ((index (send df get-index "timestamp" timestamp))
        (timer-s (send df select "timestamp"))
        (lat-s (send df select "lat"))
        (lon-s (send df select "lon")))
    (let ((t1 (vector-ref timer-s index))
          (t2 (vector-ref timer-s (+ index 1)))
          (lat1 (vector-ref lat-s index))
          (lat2 (vector-ref lat-s (+ index 1)))
          (lon1 (vector-ref lon-s index))
          (lon2 (vector-ref lon-s (+ index 1))))
      (values
       (- t2 t1)
       (map-distance/degrees lat1 lon1 lat2 lon2)))))

(define (is-teleport? df timestamp)
  (let-values (([dt dd] (delta-time-and-distance df timestamp)))
    (> dd 20)))

(define (get-index df timestamp)
  (if timestamp
      (send df get-index "timestamp" timestamp)
      (send df get-row-count)))

(define (extract-track* df start end)
  (send df select* "lat" "lon" #:filter valid-only #:start start #:end end))

(define map-panel%
  (class object%
    (init parent)
    (super-new)

    (define the-pref-tag 'activity-log:map-panel)
    (define the-session #f)

    ;; When #t, the selected lap is made to fit the view
    (define zoom-to-lap? #f)

    (define panel (new (class horizontal-pane%
                         (init)
                         (super-new)
                         (define/public (interactive-export-image)
                           (on-interactive-export-image)))
                       [parent parent]
                       [border 0]
                       [spacing 1]
                       [alignment '(center top)]))

    (define mini-lap-view
      (let ((p (new vertical-pane%
                    [parent panel]
                    [border 0]
                    [spacing 1]
                    [min-width 220]
                    [stretchable-width #f]
                    [alignment '(left top)])))
        (new message% [parent p] [label "Laps"] [font *header-font*])
        (new mini-lap-view%
             [parent p]
             [tag 'activity-log:map-mini-lap-view]
             [callback (lambda (n lap) (highlight-lap n lap))])))

    (define map-panel (new vertical-pane%
                           [parent panel]
                           [border 0]
                           [spacing 1]
                           [alignment '(left top)]))

    (define zoom-slider #f)

    (let ((p (new horizontal-pane%
                  [parent map-panel]
                  [spacing 10]
                  [stretchable-height #f]
                  [alignment '(left center)])))
      (new message% [parent p] [label "Map"] [font *header-font*])
      (new check-box% [parent p] [label "Zoom to Lap"]
           [value zoom-to-lap?]
           [callback (lambda (b e) (zoom-to-lap (send b get-value)))])
      (set! zoom-slider
            (new slider% [parent p] [label "Zoom Level "]
                 [min-value (get-min-zoom-level)]
                 [max-value (get-max-zoom-level)]
                 [stretchable-width #f]
                 [min-width 200]
                 [style '(horizontal plain)]
                 [callback (lambda (b e) (set-zoom-level (send b get-value)))]))
      (new button% [parent p] [label "Fit to Window"]
           [callback (lambda (b e) (resize-to-fit))])
      )

    (define map-view
      (new (class map-widget% (init) (super-new)
             (define/override (on-zoom-level-change zl)
               (send zoom-slider set-value zl)))
           [parent map-panel]))

    (define elevation-graph
      (let ((p (new horizontal-pane% [parent map-panel] [stretchable-height #f])))
        (new elevation-graph% [parent p] [min-height 150])))

    (send elevation-graph zoom-to-lap zoom-to-lap?)
    (send elevation-graph set-filter-amount 1)

    (define selected-lap #f)

    (define (zoom-to-lap flag)
      (set! zoom-to-lap? flag)
      (send elevation-graph zoom-to-lap flag)
      (when zoom-to-lap?
        (send map-view resize-to-fit selected-lap)))

    (define (set-zoom-level v)
      (send map-view set-zoom-level v))

    (define (resize-to-fit)
      (send map-view resize-to-fit))

    (define (highlight-lap n lap)
      (let ((lap-num (assq1 'lap-num lap)))
        (when lap-num
          (define selected-lap (- lap-num 1))
          (send map-view set-track-group-pen #f
                (send the-pen-list find-or-create-pen
                      (make-object color% 226 34 62)
                      3 'solid 'round 'round))
          (send map-view set-track-group-zorder #f 0.5)
          (send map-view set-track-group-pen selected-lap
                (send the-pen-list find-or-create-pen
                      (make-object color% 24 60 165)
                      7
                      'solid 'round 'round))
          (send map-view set-track-group-zorder selected-lap 0.1) ; on top
          (send elevation-graph highlight-lap selected-lap)
          (when zoom-to-lap?
            (send map-view resize-to-fit selected-lap)))))

    (define/public (save-visual-layout)
      (send mini-lap-view save-visual-layout))

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
          (let ((sid (send data-frame get-property 'session-id)))
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
      (send mini-lap-view set-session session)
      (send elevation-graph set-data-frame df)
      (send elevation-graph set-x-axis axis-distance)
      (send map-view clear-items)

      ;; Add the data tracks

      ;; teleports are the timestamps where the recording was stopped, than
      ;; the user traveled a significant distance and re-started the
      ;; recording.  They are used intensively when recording ski runs, for
      ;; other activities, this should hopefully be empty.
      (define teleports
        (filter (lambda (p) (is-teleport? data-frame p))
                (send data-frame get-property 'stop-points)))
      (define laps (send data-frame get-property 'laps))
      (define start (vector-ref laps 0))
      (define start-idx 0)

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
          ;; Make sure lap tracks are joined
          (set! start-idx (sub1 end-idx))))

      (send map-view set-track-group-pen #f
            (send the-pen-list find-or-create-pen
                  (make-object color% 226 34 62)
                  3 'solid 'round 'round))
      (send map-view add-marker
            (send data-frame ref* 0 "lat" "lon")
            "Start"
            1
            (make-color 0 135 36))
      (send map-view add-marker
            (send data-frame ref* (- (send data-frame get-row-count) 1) "lat" "lon")
            "End"
            -1
            (make-color 150 33 33))
      (send map-view resize-to-fit)
      (set! selected-lap #f))

    ))
