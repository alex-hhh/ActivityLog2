#lang racket/base

;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; map-and-elevation-widget.rkt -- implement a widget that shows the map and
;; an elevation plot for a data frame
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2021, 2022, 2023 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require racket/math
         racket/gui/base
         data-frame
         math/statistics
         racket/list
         racket/class
         racket/match
         racket/format
         plot
         plot/utils
         plot-container
         plot-container/hover-util
         gui-widget-mixins
         map-widget
         colormaps
         (only-in framework panel:vertical-dragable%)
         (only-in "../../rkt/utilities.rkt" get-pref put-pref)
         "../fmt-util.rkt")

(define *color-map* 'cb-rdbu-11)        ; 'cb-rdylgn-11 is also a nice one

(define (make-grade-color-indexer color-count invert?)
  (define offset (exact-floor (/ color-count 2)))
  (lambda (grade)
    (define index0
      (* (if invert? -1 1)
         (sgn grade)
         (exact-floor
          (let ([absolute-grade (abs grade)])
            (if (< absolute-grade 1.0) 0 (add1 (log absolute-grade 2)))))))
    (define index1
      (if (odd? color-count)
          (+ offset index0)
          (if (> grade 0)
              (+ offset index0)
              (+ offset -1 index0))))
    (inexact->exact (min (sub1 color-count) (max 0 index1)))))

(define (make-grade-color-renderers df)
  (let ([renderers '()]
        [current-span '()]
        [current-color #f])
    (for ([(dst alt color) (in-data-frame df "dst" "alt" "grade-color")]
          #:when (and dst alt color))
      ;; always add the current point!
      (cond ((not current-color)
             (set! current-span (cons (vector dst alt) current-span))
             (set! current-color color))
            ((not color)
             (set! current-span (cons (vector dst alt) current-span)))
            ((equal? color current-color)
             (set! current-span (cons (vector dst alt) current-span)))
            (#t
             (when (and current-color
                        (not (null? current-span))
                        (not (null? (cdr current-span))))
               (let* ([data (reverse current-span)]
                   [start (vector-ref (first data) 0)]
                   [end (vector-ref (last data) 0)]
                   [r (lines-interval
                       data
                       (list (vector start 0) (vector end 0))
                       #:line1-color "darkgreen"
                       #:line1-width 2
                       #:line2-style 'transparent
                       #:alpha 1.0
                       #:color current-color)])
              (set! renderers (cons r renderers))))
             (set! current-span (cons (vector dst alt)
                                      (if (null? current-span) '() (list (car current-span)))))
             (set! current-color color))))
    (when (and current-color
                        (not (null? current-span))
                        (not (null? (cdr current-span))))
      (let* ([data (reverse current-span)]
             [start (vector-ref (first data) 0)]
             [end (vector-ref (last data) 0)]
             [r (lines-interval
                 data
                 (list (vector start 0) (vector end 0))
                 #:line1-color "darkgreen"
                 #:line1-width 2
                 #:line2-style 'transparent
                 #:alpha 1.0
                 #:color current-color)])
        (set! renderers (cons r renderers))))
    renderers))

(define (prepare-for-display df #:color-map color-map)
  (when (df-contains? df "grade")
    (define cm (df-get-property df 'color-map #f))
    (unless (and cm (df-contains? df "grade-color"))
      (unless cm
        (set! cm color-map)
        (df-put-property! df 'color-map cm))
      (define grade->color-index (make-grade-color-indexer (color-map-size cm) #t))
      (df-add-derived!
       df
       "grade-color"
       '("grade")
       (lambda (v)
         (define grade (car v))
         (and grade (grade->color-index grade)))))))

(define (make-plot-callback df map)
  (lambda (snip event dst _alt)
    (let/ec return
      (unless (good-hover? snip dst _alt event)
        (send snip set-overlay-renderers #f)
        (send map current-location #f)
        (return (void)))

      (define location (df-lookup df "dst" '("lat" "lon") dst))
      (if (and (vector-ref location 0) (vector-ref location 1))
          (send map current-location location)
          (send map current-location #f))

      (match-define (vector alt grade) (df-lookup df "dst" '("alt" "grade") dst))

      (define renderers
        (if (and alt grade)
            (let ([main-badge (make-hover-badge
                               `(("Distance" ,(distance->string dst #t))
                                 ("Altitude" ,(vertical-distance->string alt #t))
                                 ("Grade" ,(string-append (~r grade #:precision 1) "%"))))])
              (list (vrule dst #:style 'long-dash)
                    (point-pict (vector dst _alt) main-badge #:anchor 'auto)))
            '()))

      (send snip set-overlay-renderers renderers))))

;; Widget to show a map and elevation plot for a GPS segment and also allow
;; displaying a segment match from a session.
(define map-and-elevation-widget%
  (class object%
    (init-field parent pref-tag)
    (init [show-session-controls? #t])
    (super-new)

    (define segment-df #f)    ; data frame for the segment on the plot
    ;; A data frame representing the session from which we show a segment
    ;; match
    (define session-df #f)
    ;; Start and end timestamps for which we show a segment match
    (define-values (match-start match-end) (values #f #f))
    (define gui-prefs (get-pref pref-tag (lambda () (hash))))

    (define map-and-plot-panel
      (new panel:vertical-dragable%
           [parent parent]))

    (define map-pane
      (new vertical-pane%
           [parent map-and-plot-panel]
           [border 0]
           [spacing 0]))

    (define map-controls-pane
      (new horizontal-pane%
           [parent map-pane]
           [border 5]
           [spacing 20]
           [stretchable-height #f]))

    (define track-location-check-box
      (new (tooltip-mixin check-box%)
           [parent map-controls-pane]
           [label "Track Location"]
           [tooltip "Keep selected map location in view"]
           [value (hash-ref gui-prefs 'track-location? (lambda () #f))]
           [callback (lambda (c e) (on-track-location (send c get-value)))]))

    (define show-map-layer-check-box
      (new (tooltip-mixin check-box%)
           [parent map-controls-pane]
           [label "Map Layer"]
           [tooltip "Show/Hide the map itself"]
           [value (hash-ref gui-prefs 'show-map-layer? (lambda () #t))]
           [callback (lambda (c e) (on-show-map-layer (send c get-value)))]))

    (define show-session-track-check-box
      (new (tooltip-mixin check-box%)
           [parent map-controls-pane]
           [label "Entire Session"]
           [tooltip "Show/Hide the track of the matches session"]
           [value (hash-ref gui-prefs 'show-session-track? (lambda () #t))]
           [callback (lambda (c e) (on-show-session-track (send c get-value)))]))

    (define zoom-slider
      (new (tooltip-mixin slider%)
           [parent map-controls-pane]
           [label "Zoom"]
           [tooltip "Select the map zoom level"]
           [style '(horizontal plain)]
           [min-value (min-zoom-level)]
           [max-value (max-zoom-level)]
           [callback (lambda (z e) (on-zoom-level-change (send z get-value)))]))

    (define fit-to-window-button
      (new (tooltip-mixin button%)
           [parent map-controls-pane]
           [label "Fit to Window"]
           [tooltip "Center and zoom map so entire track is visible"]
           [callback (lambda (b e) (on-fit-to-window))]))

    (define the-map
      (new (class map-widget% (init) (super-new)
             (define/override (on-zoom-level-change zl)
               (send zoom-slider set-value zl)))
           [parent map-pane]))

    (define the-plot-container
      (new plot-container%
           [parent map-and-plot-panel]
           [background-message "No Elevation Data"]))

    (unless show-session-controls?
      (send map-controls-pane change-children
            (lambda (old)
              (list track-location-check-box
                    show-map-layer-check-box
                    zoom-slider
                    fit-to-window-button))))

    (send the-map set-group-pen 'segment
          (send the-pen-list find-or-create-pen
                (make-object color% 238 51 119)
                6
                'solid))
    (send the-map set-group-zorder 'segment 0.5)

    (send the-map set-group-pen 'match
          (send the-pen-list find-or-create-pen
                (make-object color% 204 51 17)
                4
                'solid))
    (send the-map set-group-zorder 'match 0.25)

    (send the-map set-group-pen 'session
          (send the-pen-list find-or-create-pen
                (make-object color% 0 119 187)
                3
                'solid))
    (send the-map set-group-zorder 'session 0.75)

    (send the-map track-current-location (send track-location-check-box get-value))

    ;; Needs to be done after the panel has all the children added
    (send map-and-plot-panel set-percentages
          (hash-ref gui-prefs 'map-plot-split (lambda () '(2/3 1/3))))

    (send zoom-slider set-value (send the-map zoom-level))

    (define/private (construct-the-plot)
      (define-values (w h) (send the-plot-container cell-dimensions 1))
      (define colormap (df-get-property segment-df 'color-map *color-map*))
      (define-values (plot-min plot-max)
        (let* ([stats (df-statistics segment-df "alt")]
               [room (* (statistics-range stats) 0.1)])
          (values (- (statistics-min stats) room)
                  (+ (statistics-max stats) room))))
      (define the-plot
        (parameterize ([plot-decorations? #f]
                       [plot-x-label #f #;"Distance (km)"]
                       [plot-y-label #f #;"Elevation (meters)"]
                       [plot-pen-color-map colormap]
                       [plot-brush-color-map colormap])
          (plot-snip (make-grade-color-renderers segment-df) #:width w #:height h
                     #:y-min plot-min #:y-max plot-max)))
      (send the-plot set-mouse-event-callback (make-plot-callback segment-df the-map))
      (send the-plot set-overlay-renderers #f)
      (send the-plot-container set-snips the-plot))

    ;; Called when the user checks/unchecks the track-location-check-box
    ;; widget, sends the information to the map widget.
    (define (on-track-location track?)
      (send the-map track-current-location track?))

    ;; Called then the user checks/unchecks the show-map-layer-check-box
    ;; widget, sends the information to the map widget
    (define (on-show-map-layer show?)
      (send the-map show-map-layer show?))

    ;; Called when the user checks/unchecks the show-session-track-check-box
    ;; widget, shows/hides the session (if one is present)
    (define (on-show-session-track show?)
      (if show?
          (when (and session-df match-start match-end)
            (send the-map begin-edit-sequence)
            (send the-map delete-group 'session)
            (match-define (list begin end)
              (df-index-of* session-df "timestamp" match-start match-end))
            ;; NOTE: route simplification in the map widget means that the
            ;; full track and the segment might not align.  Just show the path
            ;; outside the matched segment for the full track, to work around
            ;; this limitation.
            (define full-track-before
              (df-select* session-df "lat" "lon" #:filter valid-only #:stop (add1 begin)))
            (send the-map add-track full-track-before 'session)
            (define full-track-after
              (df-select* session-df "lat" "lon" #:filter valid-only #:start (sub1 end)))
            (send the-map add-track full-track-after 'session)
            (send the-map end-edit-sequence))
          (send the-map delete-group 'session)))

    ;; Called when the user changes the zoom level using the zoom-slider.
    ;; Sends the new zoom level to the map widget.
    (define (on-zoom-level-change zl)
      (send the-map zoom-level zl))

    ;; Called when the user presses the fit-to-window-button.  Calls
    ;; `resize-to-fit` on the map widget
    (define (on-fit-to-window)
      (send the-map resize-to-fit 'segment)
      (send the-map center-map 'segment))

    ;; Called by the toplevel application when the application is closed.  The
    ;; method saves the currently selected options, so it can reopen as it was
    ;; last opened.
    (define/public (save-visual-layout)
      (define prefs
        (hash
         'map-plot-split (send map-and-plot-panel get-percentages)
         'track-location? (send track-location-check-box get-value)
         'show-map-layer? (send show-map-layer-check-box get-value)
         'show-session-track? (send show-session-track-check-box get-value)))
      (put-pref pref-tag prefs))

    ;; Set the current segment to be displayed to DF -- the segment will be
    ;; shown on the map and the elevation plot.  Any segment match will be
    ;; deleted.
    (define/public (set-segment df)
      (set! segment-df df)
      (if segment-df
          (begin
            (prepare-for-display df #:color-map *color-map*)
            (if (df-contains? df "alt" "grade" "grade-color")
                (construct-the-plot)
                (send the-plot-container clear-all))
            (send the-map begin-edit-sequence)
            (send the-map clear)
            (send the-map add-track (df-select* df "lat" "lon" #:filter valid-only) 'segment)
            (send the-map resize-to-fit)
            (send the-map center-map)
            (send the-map end-edit-sequence))
          (begin
            (send the-map clear)
            (send the-plot-container clear-all)))
      (set-session-match #f #f #f))

    ;; set a matched session (DF) with the matched segment between the START
    ;; and END row indexes.  The matched section will be displayed on the map
    (define/public (set-session-match df start end)
      (set! session-df df)
      (set! match-start start)
      (set! match-end end)
      (when (and session-df match-start match-end)
        (send the-map begin-edit-sequence)
        (send the-map delete-group 'session)
        (send the-map delete-group 'match)
        (match-define (list begin end)
          (df-index-of* session-df "timestamp" match-start match-end))
        (when (send show-session-track-check-box get-value)
          ;; NOTE: route simplification in the map widget means that the
          ;; full track and the segment might not align.  Just show the path
          ;; outside the matched segment for the full track, to fix this.
          (define full-track-before
            (df-select* session-df "lat" "lon" #:filter valid-only #:stop (add1 begin)))
          (send the-map add-track full-track-before 'session)
          (define full-track-after
            (df-select* session-df "lat" "lon" #:filter valid-only #:start (sub1 end)))
          (send the-map add-track full-track-after 'session))
        (define match-track
          (df-select* session-df "lat" "lon" #:filter valid-only
                      #:start begin #:stop end))
        (send the-map add-track match-track 'match)
        (send the-map end-edit-sequence)))

    ))

(provide map-and-elevation-widget%)
