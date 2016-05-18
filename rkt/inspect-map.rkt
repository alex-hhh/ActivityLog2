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
         "plot-axis-def.rkt"
         "data-frame.rkt"
         "utilities.rkt")

(provide map-panel%)

(define *header-font*
  (send the-font-list find-or-create-font 18 'default 'normal 'normal))

(define (extract-track data-frame)
  (send data-frame select* "lat" "lon" #:filter valid-only))

(define (extract-track-for-lap data-frame lap-num)
  (let* ((laps (send data-frame get-property 'laps))
         (start (vector-ref laps lap-num))
         (end (if (>= (+ 1 lap-num) (vector-length laps))
                  #f
                  (vector-ref laps (+ 1 lap-num))))
         (idx (send data-frame get-index* "timestamp" start end)))
    (send data-frame select* "lat" "lon" #:filter valid-only
          #:start (first idx)
          #:end (second idx))))

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

    (define (zoom-to-lap flag)
      (set! zoom-to-lap? flag)
      (send elevation-graph zoom-to-lap flag)
      (when zoom-to-lap?
        (send map-view resize-to-fit #t)))

    (define (set-zoom-level v)
      (send map-view set-zoom-level v))

    (define (resize-to-fit)
      (send map-view resize-to-fit))

    (define (highlight-lap n lap)
      (let ((lap-num (assq1 'lap-num lap)))
        (when lap-num
          (send map-view set-selected-section
                (extract-track-for-lap data-frame (- lap-num 1)))
          (send elevation-graph highlight-lap (- lap-num 1))))
      (when zoom-to-lap?
        (send map-view resize-to-fit #t)))

    (define/public (save-visual-layout)
      (send mini-lap-view save-visual-layout))

    (define/public (on-interactive-export-image)
      (let ((file (put-file "Select file to export to" #f #f #f "png" '()
                            '(("PNG Files" "*.png") ("Any" "*.*")))))
        (when file
          (send map-view export-image-to-file file))))

    (define generation -1)
    (define data-frame #f)

    (define/public (set-session session df)
      (set! generation (+ 1 generation))
      (set! the-session session)
      (set! data-frame df)
      (send mini-lap-view set-session session)
      (send elevation-graph set-data-frame df)
      (send elevation-graph set-x-axis axis-distance)
      (send elevation-graph show-grid #t)
      (send map-view set-track (extract-track data-frame))
      
      ;; label nearby weather stations (TODO: should be optional)
      ;; (for ((ws (in-list (get-nearby-wstations (assq1 'database-id session)))))
      ;;   (send map-view add-label
      ;;         (wstation-ident ws) (wstation-lat ws) (wstation-lon ws))))
      )

    (define/public (get-generation) generation)
    ))
