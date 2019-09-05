#lang racket/base

;; map-impl.rkt -- common implementation for the map widget, shared between
;; canvas, snip and maybe pasteboard views
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2019 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require
 racket/math
 racket/class
 racket/sequence
 racket/stream
 racket/draw
 racket/list
 racket/gui/base
 math/flonum
 racket/match
 racket/contract
 "../../utilities.rkt"          ; for get-pref
 "map-util.rkt"
 "map-tiles.rkt")

(provide/contract
 (max-zoom-level (-> positive-integer?))
 (min-zoom-level (-> positive-integer?)))

(provide map-impl%)

(define tile-size 256)                  ; size of the map tiles, in pixels

;; The pen and brush to draw the "current location" marker on the map
(define current-location-pen
  (send the-pen-list find-or-create-pen (make-color 68 114 196) 5 'solid))
(define current-location-brush
  (send the-brush-list find-or-create-brush (make-color 68 114 196 0.5) 'solid))

;; Default pen and Z-order for the tracks added to the map (when the user does
;; not specify one explicitly.
(define default-pen
  (send the-pen-list find-or-create-pen (make-object color% 226 34 62) 3 'solid 'round 'round))
(define default-zorder 0.5)

;; Pen to draw debug aids on the map.
(define debug-pen
  (send the-pen-list find-or-create-pen (make-object color% 86 13 24) 2 'solid))

;; Bitmap to draw when we don't receive a tile
(define empty-bmp (make-bitmap tile-size tile-size #f))

;; Cursors for the mouse is over the map widget -- the hand-cursor is used
;; when the map is dragged around.
(define hand-cursor (make-object cursor% 'hand))
(define arrow-cursor (make-object cursor% 'arrow))

;; The map implementation considers tracks which are lists of vectors of
;; points, each point being a vector with the first two values being the
;; latitude and longitude.  The point vectors can have more elements, so
;; tracks can contain other data (which is not used by the map)
(define (point-lat p) (vector-ref p 0))
(define (point-long p) (vector-ref p 1))

;; Return a track that has fewer points than TRACK but should display OK at
;; ZOOM-LEVEL.  We drop points from TRACK such that there is a minimum
;; distance between two points.  When simplifying the track, we are carefull
;; not to drop points if the bearing of the track changes, as this will result
;; in a distorted track.
(define (simplify-track track zoom-level)

  ;; Minimum distance between points in meters (each segment will be approx 30
  ;; pixels).  Points closer together than this value will be dropped, unless
  ;; bearing changes by more than MAX-BDEV
  (define min-dist (* 30 (zoom-level->mpp zoom-level)))

  ;; Maximum bearning deviation we allow when dropping points.  Since the
  ;; track does not usually go in a straight line, the direction (bearing) of
  ;; the track will change if we skip a point.  MAX-BDEV is the maximum
  ;; direction change we allow, a point will not be dropped if the bearing
  ;; change would be greater than this value
  (define max-bdev (* 3 (/ pi 180)))

  (define (bear p1 p2)
    (map-bearing/degrees
     (point-lat p1) (point-long p1) (point-lat p2) (point-long p2)))

  (define (dist p1 p2)
    (map-distance/degrees
     (point-lat p1) (point-long p1) (point-lat p2) (point-long p2)))

  (if (< (sequence-length track) 3)
      (sequence->list track)            ; cannot simplify a short track
      (let* ((strack (sequence->stream track))
             (ntrack (list (stream-first strack)))) ; we always keep the first point
        (let loop ((start (stream-first strack))
                   (follow (stream-first (stream-rest strack)))
                   (rest (stream-rest (stream-rest strack)))
                   (bearing (bear (stream-first strack)
                                  (stream-first (stream-rest strack)))))
          (if (stream-empty? rest)
              (set! ntrack (cons follow ntrack)) ; last point
              (let* ((candidate (stream-first rest))
                     (ndist (dist start candidate))
                     (nbearing (bear start candidate)))
                (if (or (>  ndist min-dist)
                        (> (abs (- nbearing bearing)) max-bdev))
                    (begin
                      (set! ntrack (cons follow ntrack))
                      (loop follow candidate (stream-rest rest) (bear follow candidate)))
                    (begin
                      (loop start candidate (stream-rest rest) bearing))))))
        (reverse ntrack))))

;; Construct a dc-path% that draws the TRACK at ZOOM-LEVEL
(define (track->dc-path track zoom-level)
  (define max-coord (* tile-size (expt 2 zoom-level)))

  (define (p->pixel p)
    (let ((p (lat-lon->map-point (point-lat p) (point-long p))))
      (values (* max-coord (map-point-x p))
              (* max-coord (map-point-y p)))))

  (let ((path (new dc-path%)))
    (unless (null? track)
      (let-values (([start-x start-y] (p->pixel (car track))))
        (send path move-to start-x start-y))
      (for ((p (in-list (cdr track))))
        (let-values (([px py] (p->pixel p)))
          (send path line-to px py))))
    path))

;; Draw the BOUNDING-BOX onto the device DC at the specified ZOOM-LEVEL.  This
;; is intended for debugging purposes -- an outline of the bounding box is
;; drawn plus a circle in the middle of it.  The pen and brush are not
;; changed, so they can be set in before calling this method to the desired
;; values.
;;
;; NOTE: this function assumes that the map origin has been set up correctly
;; see `with-origin`
;;
(define (draw-bounding-box dc bounding-box zoom-level)

  (define (get-center zoom-level)
    (let ((max-coord (* tile-size (expt 2 zoom-level)))
          (center/ndcs (bbox-center/ndcs bounding-box)))
      (values (* (map-point-x center/ndcs) max-coord)
              (* (map-point-y center/ndcs) max-coord))))

  (let-values (([cx cy] (get-center zoom-level)))
    (send dc draw-ellipse (+ cx -10) (+ cy -10) 20 20))
  (match-define (map-bbox max-lat max-lon min-lat min-lon) bounding-box)
  (let ((max-coord (* tile-size (expt 2 zoom-level)))
        (map1 (lat-lon->map-point max-lat min-lon))
        (map2 (lat-lon->map-point min-lat max-lon)))
    (let ((x1 (* (map-point-x map1) max-coord))
          (y1 (* (map-point-y map1) max-coord))
          (x2 (* (map-point-x map2) max-coord))
          (y2 (* (map-point-y map2) max-coord)))
      (send dc draw-rectangle x1 y1 (- x2 x1) (- y2 y1)))))

;; Draw a label (marker) at POS (a `map-point`), at ZOOM-LEVEL.  Label is the
;; text to display, direction is 1 for the text to be displayed to the right,
;; and -1 for the text to be displayed on the left of the marker, while color
;; is the color of the label.
;;
;; NOTE: this function assumes that the map origin has been set up correctly
;; see `with-origin`
;;
(define (draw-label dc pos zoom-level label direction color)

  (send dc set-pen
        (send the-pen-list find-or-create-pen color 2 'solid))
  (send dc set-font
        (send the-font-list find-or-create-font 10 'default 'normal 'bold))
  (send dc set-brush
        (send the-brush-list find-or-create-brush
              (make-color
               (send color red)
               (send color green)
               (send color blue)
               0.7)
              'solid))
  (send dc set-text-foreground "white")

  ;; NOTE: we assume that the dc origin has been corectly set up!
  (let-values (([ox oy] (send dc get-origin)))
    (let* ((max-coord (* tile-size (expt 2 zoom-level)))
           (x (* (map-point-x pos) max-coord))
           (y (* (map-point-y pos) max-coord)))
    (let-values (([w h b e] (send dc get-text-extent label)))
      (let ((arrow-length 30)
            (text-spacing 2))
        (let ((label-baseline-x (+ x (* direction arrow-length)))
              (label-baseline-y (+ y (- arrow-length)))
              (label-length (+ w text-spacing text-spacing))
              (label-height (+ h text-spacing text-spacing)))
        (send dc draw-line x y label-baseline-x label-baseline-y)
        (send dc draw-line
              label-baseline-x label-baseline-y
              (+ label-baseline-x (* direction label-length))
              label-baseline-y)
        (send dc set-pen
              (send the-pen-list find-or-create-pen "black" 1 'transparent))
        (let ((rectangle-y (- label-baseline-y label-height))
              (rectangle-x (if (> direction 0)
                               label-baseline-x
                               (- label-baseline-x label-length))))
          (send dc draw-rectangle
                rectangle-x rectangle-y
                label-length label-height)
          (send dc draw-text label
                (+ rectangle-x text-spacing)
                (+ rectangle-y text-spacing)))))))))

;; Set the draw context on the device context DC such that the origin is at
;; ORIGIN-X and ORIGIN-Y than execute THUNK, the original origin is restored
;; at the end.
(define (with-draw-context dc thunk)
  (let ([old-smoothing (send dc get-smoothing)]
        [old-pen (send dc get-pen)]
        [old-brush (send dc get-brush)]
        [old-font (send dc get-font)]
        [old-text-fg (send dc get-text-foreground)]
        [old-text-bg (send dc get-text-background)])
    (dynamic-wind
      (lambda ()
        (send dc set-smoothing 'smoothed))
      thunk
      (lambda ()
        (send dc set-smoothing old-smoothing)
        (send dc set-pen old-pen)
        (send dc set-brush old-brush)
        (send dc set-font old-font)
        (send dc set-text-foreground old-text-fg)
        (send dc set-text-background old-text-bg)))))

(define (with-origin dc origin-x origin-y thunk)
  (let-values (([ox oy] (send dc get-origin)))
    (dynamic-wind
      (lambda ()
        (send dc set-origin (- origin-x) (- origin-y)))
      thunk
      (lambda ()
        (send dc set-origin ox oy)))))

;; Set a clipping rect at X,Y,WIDTH and HEIGHT onto the device context DC,
;; than execute THUNK.  The original clipping rect is restored at the end.
(define (with-clipping-rect dc x y width height thunk)
  (let ([old-clipping-region (send dc get-clipping-region)])
    (dynamic-wind
      (lambda () (send dc set-clipping-rect x y width height))
      thunk
      (lambda () (send dc set-clipping-region old-clipping-region)))))

;; Represent a GPS track that can be drawn on a dc<%> at different zoom
;; levels.  A track also has a group which is an integer, the map-impl% will
;; treat all tracks within a group identically with respect to draw order and
;; pen color.
(define track%
  (class object%
    (init-field track group)
    (super-new)

    (define bbox #f)
    (define debug?
      (get-pref 'activity-log:draw-track-bounding-box (lambda () #f)))
    (define paths-by-zoom-level (make-hash))

    (define/private (get-dc-path zoom-level)
      (let ((dc-path (hash-ref paths-by-zoom-level zoom-level #f)))
        (unless dc-path
          ;; no dc-path at this zoom level, create one now
          (let ((strack (simplify-track track zoom-level)))
            (set! dc-path (track->dc-path strack zoom-level))
            (hash-set! paths-by-zoom-level zoom-level dc-path)))
        dc-path))

    ;; NOTE: this function assumes that the map origin has been set up correctly
    ;; see `with-origin`
    (define/public (draw dc zoom-level pen brush)
      (let ((path (get-dc-path zoom-level)))
        (send dc set-pen pen)
        (send dc set-brush brush)
        (send dc draw-path path 0 0))
      (when debug?
        (send dc set-pen debug-pen)
        (send dc set-brush brush)
        (draw-bounding-box dc bbox zoom-level)))

    (define/public (get-bounding-box)
      (unless bbox
        (set! bbox (track-bbox track)))
      bbox)

    (define/public (get-group) group)

    ))

;; Represents a labeled marker drawn on the map at POS (a GPS coordinate) with
;; TEXT and COLOR.  Direction is 1 if the text is drawn on the right and -1 if
;; it is drawn on the left.
(define marker%
  (class object%
    (init-field pos text direction color)
    (super-new)

    (define point (lat-lon->map-point (point-lat pos) (point-long pos)))

    (define/public (draw dc zoom-level)
      (draw-label dc point zoom-level text direction color))

    (define/public (get-position) pos)

    ))

;; Legend color, pen and font -- these are used to draw the map legend.
(define legend-color (make-object color% 86 13 24))
(define legend-pen
  (send the-pen-list find-or-create-pen legend-color 2 'solid))
(define legend-font
  (send the-font-list find-or-create-font 8 'default 'normal 'normal))

;; Map distance markers for each zoom level, for metric distances
(define legend-distance-metric
  (list
   (list 1 8000000 "8000 km")
   (list 2 4000000 "4000 km")
   (list 3 2000000 "2000 km")
   (list 4 1000000 "1000 km")
   (list 5 500000 "500 km")
   (list 6 250000 "250 km")
   (list 7 100000 "100 km")
   (list 8  50000 "50 km")
   (list 9  25000 "25 km")
   (list 10 15000 "15 km")
   (list 11 10000 "10 km")
   (list 12  5000 "5 km")
   (list 13  2000 "2 km")
   (list 14  1000 "1 km")
   (list 15   500 "500 m")
   (list 16   200 "200 m")
   (list 17   100 "100 m")
   (list 18    50 "50 m")))

;; Map distance markers for each zoom level, for statute distances
(define legend-distance-statute
  (list
   (list 1 12874752.0 "8000 mi")
   (list 2 6437376.0 "4000 mi")
   (list 3 3218688.0 "2000 mi")
   (list 4 1609344.0 "1000 mi")
   (list 5 804672.0 "500 mi")
   (list 6 402336.0 "250 mi")
   (list 7 160934.4 "100 mi")
   (list 8  80467.20 "50 mi")
   (list 9  32186.88 "20 mi")
   (list 10 16093.44 "10 mi")
   (list 11  8046.72 "5 mi")
   (list 12  3218.68 "2 mi")
   (list 13  1609.344 "1 mi")
   (list 14   804.67 "0.5 mi")
   (list 15   457.20 "500 yd")
   (list 16   182.88 "200 yd")
   (list 17    91.44 "100 yd")
   (list 18    45.72 "50 yd")))

;; Draw the map legend on the device context DC, assuming the map is drawn
;; starting at DX, DY and has a WIDTH and HEIGHT.  The legend is drawn for the
;; specific ZOOM-LEVEL.
;;
;; Note that this method does not assume that the map is drawn onto the entire
;; device context, but only in the rectangle defined by DX, DY, WIDTH and
;; HEIGHT.
(define (draw-map-legend dc dx dy width height zoom-level)
  (define-values (metric-distance metric-label)
    (let ((entry (assq zoom-level legend-distance-metric)))
      (if entry
          (values (second entry) (third entry))
          (values 1000 "1 km"))))
  (define-values (statute-distance statute-label)
    (let ((entry (assq zoom-level legend-distance-statute)))
      (if entry
          (values (second entry) (third entry))
          (values 1609.344 "1 mi"))))
  (define mlabel
    (let ((backlog-size (get-download-backlog)))
      (if (> backlog-size 0)
          (format "~a  (ZL ~a; BL ~a)" metric-label zoom-level backlog-size)
          (format "~a  (ZL ~a)" metric-label zoom-level))))
  (define slabel statute-label)
  (define mdist (/ metric-distance (zoom-level->mpp zoom-level)))
  (define sdist (/ statute-distance (zoom-level->mpp zoom-level)))
  (define-values (ox oy) (values 10 10))
  (define-values (cw ch) (values width height))
  (send dc set-brush
        (send the-brush-list find-or-create-brush
              (make-color 255 255 255 0.7) 'solid))
  (send dc set-pen
        (send the-pen-list find-or-create-pen "white" 1 'transparent))
  (send dc set-font legend-font)
  (send dc set-text-foreground legend-color)

  (define Y (- (+ ch dy) oy 10))
  (define X (+ ox dx))

  (let-values (((w h x y) (send dc get-text-extent (tile-copyright-string) legend-font #t)))
    (send dc draw-rectangle (- (+ cw dx) ox 5 w) (- (+ ch dy) oy y 5 h) (+ w 5 5) (+ h 5 5))
    (send dc draw-text (tile-copyright-string) (- (+ cw dx) ox w) (- (+ ch dy -10) h))

    ;; use the height of the copyright string to determine the height of the
    ;; legend rectangle.
    (send dc draw-rectangle (- X 5) (- Y 20)
          (+ (max mdist sdist) 5 5) (+ (* 2 (+ h 3)) 5)))

  (send dc set-pen legend-pen)

  (send dc draw-line X Y (+ X (max mdist sdist)) Y)
  (send dc draw-line X Y X (- Y 10))
  (send dc draw-line X Y X (+ Y 10))
  (send dc draw-line (+ X mdist) Y (+ X mdist) (- Y 10))
  (send dc draw-line (+ X sdist) Y (+ X sdist) (+ Y 10))

  (let-values (([w h b e] (send dc get-text-extent mlabel)))
    (let ((tx (+ X 3))
          (ty (- Y 3 h)))
      (send dc draw-text mlabel tx ty)))

  (let-values (([w h b e] (send dc get-text-extent slabel)))
    (let ((tx (+ X 3))
          (ty (+ Y 3)))
      (send dc draw-text slabel tx ty))))


;; Maximum zoom level we allow for the map widget.
(define max-zl (get-pref 'activity-log:max-map-zoom-level (lambda () 16)))
(define min-zl 1)

(define (max-zoom-level) max-zl)
(define (min-zoom-level) min-zl)

;; Return a zoom-level such that the contents of BBOX will fit in a draw area
;; of WIDTH/HEIGHT pixels
(define (select-zoom-level bbox width height)
  (unless bbox (error "select-zoom-level" bbox))
  (let-values (([w h] (bbox-size bbox)))
    ;; If the BBOX is too small, just return the max zoom level we have (the
    ;; calculation below might return +/-inf otherwise
    (if (and (< w 5.0) (< h 5.0))
        (max-zoom-level)
        ;; mpp -- meters per pixel
        (let ((mpp-width (/ w width))
              (mpp-height (/ h height)))
          (mpp->zoom-level (max mpp-width mpp-height))))))

(define map-widget%/c
  (class/c
   (resize (-> positive? positive? any/c))
   (adjust-cursor (-> (is-a?/c dc<%>)
                      real? real?
                      real? real?
                      (is-a?/c mouse-event%)
                      (or/c (is-a?/c cursor%) #f)))
   (on-event (-> (is-a?/c dc<%>)
                 real? real? real? real? (is-a?/c mouse-event%)
                 boolean?))
   (on-char (-> (is-a?/c dc<%>)
                 real? real? real? real? (is-a?/c key-event%)
                 boolean?))
   (draw (-> (is-a?/c dc<%>) real? real? any/c))
   (get-size (-> (values positive? positive?)))
   ))

;; Map implementation -- serves as the implementation class for canvas% and
;; snip% based maps.  It implements the map drawing as well as event handling
;; code, but relies on the "outer" classes to call the right methods.
(define map-impl%
  (class object%
    (init [zoom 1])
    (init-field [width 300] [height 200]
                [request-refresh (lambda () (void))]
                [position #f]
                [track #f]
                [on-zoom-level-change (lambda (zl) (void))])
    (super-new)

    (define debug? (get-pref 'activity-log:draw-map-bounding-box (lambda () #f)))

    ;; When #f, the tiles are not drawn, only the tracks.
    (define show-map-layer? #t)

    ;;; data to display on the map
    (define tracks '())
    (define group-pens (make-hash))
    (define group-zorder (make-hash))

    (define markers '())
    ;; A (vector lat lon) where we draw a marker representing the "current
    ;; location"
    (define the-current-location #f)
    ;; When #t, the map is panned so that the current-location is in the
    ;; center of the view, this panning is animated
    (define should-track-current-location #f)
    ;; The X, Y coordinates of the current location (in canvas coordinates).
    ;; Updated by `on-current-location-updated`
    (define last-current-location-x #f)
    (define last-current-location-y #f)

    (define the-zoom-level zoom)
    (define max-tile-num (expt 2 the-zoom-level))
    (define max-coord (* tile-size max-tile-num))

    (define/private (valid-tile-num? n) (and (>= n 0) (< n max-tile-num)))
    (define/private (valid-coord? n) (and (>= n 0) (< n max-coord)))

    (define origin-x 0)
    (define origin-y 0)

    ;; These are needed to be able to implement `copy-from`, unfortunately, I
    ;; know no other way to get access to these members, but hopefully the
    ;; `internal-` prefix will be a clue that people should not call these
    ;; methods.  Even if they call them, they will get access to data they
    ;; cannot use to corrupt the state of the object.  Also note that there is
    ;; no need to copy `tracks` and `markers`, as lists are immutable in
    ;; Racket.
    (define/public (internal-get-tracks) tracks)
    (define/public (internal-get-markers) markers)
    (define/public (internal-get-group-pens) (hash-copy group-pens))
    (define/public (internal-get-group-zorder) (hash-copy group-zorder))
    (define/public (internal-get-origin) (values origin-x origin-y))

    ;; Copy the state of OTHER into this object instance.  This is used as a
    ;; helper method for map-snip% which needs to have a COPY method.
    (define/public (copy-from other)
      ;; Need to copy tracks, track group pens and track zorders
      (set! the-zoom-level (send other zoom-level))
      (set! the-current-location (send other current-location))
      (set! should-track-current-location (send other track-current-location))
      (define-values (ox oy) (send other internal-get-origin))
      (set! origin-x ox)
      (set! origin-y oy)
      (set! tracks (send other internal-get-tracks))
      (set! group-pens (send other internal-get-group-pens))
      (set! group-zorder (send other internal-get-group-zorder))
      (set! markers (send other internal-get-markers))
      (limit-origin width height)
      (request-refresh))

    ;; Adjust the map origin such that we don't have to draw past the map
    ;; edges at the current zoom level
    (define/private (limit-origin w h)
      (when (> (+ origin-x w) max-coord)
        (set! origin-x (- max-coord w)))
      (when (< origin-x 0)
        (set! origin-x 0))
      (when (> (+ origin-y h) max-coord)
        (set! origin-y (- max-coord h)))
      (when (< origin-y 0)
        (set! origin-y 0)))

    ;; Resize the map to W, H dimensions
    (define/public (resize w h)
      (set! origin-x (- (+ origin-x (/ width 2)) (/ w 2)))
      (set! origin-y (- (+ origin-y (/ height 2)) (/ h 2)))
      (limit-origin w h)
      (set! width w)
      (set! height h)
      (request-refresh))

    (define last-mouse-x #f)
    (define last-mouse-y #f)

    ;; Determine which cursor to use for the specified mouse EVENT.  This is a
    ;; helper method for the map-snip% class.
    (define/public (adjust-cursor dc x y editorx editory event)
      (cond ((or (send event dragging?)
                 (send event button-down? 'left))
             hand-cursor)
            ((send event button-up? 'left) arrow-cursor)
            (#t #f)))

    ;; Handle a mouse event.  Return #t if the event was handled, #f
    ;; otherwise.
    (define/public (on-event dc x y editorx editory event)
      (cond ((send event button-down? 'left)
             (set! last-mouse-x (send event get-x))
             (set! last-mouse-y (send event get-y))
             ;; Return as "Not handled', let others maybe handle it
             #f)
            ((send event button-up? 'left)
             (set! last-mouse-x #f)
             (set! last-mouse-y #f)
             ;; Return as "Not handled', let others maybe handle it
             #f)
            ((send event dragging?)
             (let ((mouse-x (send event get-x))
                   (mouse-y (send event get-y)))
               (when (and last-mouse-x last-mouse-y)
                 (set! origin-x (- origin-x (- mouse-x last-mouse-x)))
                 (set! origin-y (- origin-y (- mouse-y last-mouse-y)))
                 (limit-origin width height)
                 (request-refresh))
               (set! last-mouse-x mouse-x)
               (set! last-mouse-y mouse-y))
             ;; Event was handled
             #t)
            (#t
             ;; Not handled
             #f)))

    ;; Handle a keyboard event.  Return #t if the event was handled, #f
    ;; otherwise.  Note that the wheel scroll with the mouse is received as
    ;; keyboard events.  Also, in an `editor-canvas%`, these events are
    ;; handled by the canvas for scrolling unless you call:
    ;;
    ;; (send canvas  wheel-step #f)
    ;;
    ;; We use wheel scroll to implement the zoom/unzoom functionality
    (define/public (on-char dc x y editorx editory event)
      ;; Implement map zoom-in and out using the mouse wheel.  Mouse wheel
      ;; zoom is handled by the key event
      (case (send event get-key-code)
        [(wheel-up up add)
         (when (< the-zoom-level (max-zoom-level))
           (zoom-level (add1 the-zoom-level)))
         #t]
        [(wheel-down down subtract)
         (when (> the-zoom-level (min-zoom-level))
           (zoom-level (sub1 the-zoom-level)))
         #t]
        [(#\c) (center-map)]
        [(#\f) (resize-to-fit)]
        (else #f)))

    ;; Clear the DC to a white color in the rectangle X,Y,WIDTH,HEIGHT -- this
    ;; is used when show-map-layer? is #f
    (define/private (clear-dc dc x y width height)
      (let ((old-brush (send dc get-brush))
            (old-pen (send dc get-pen)))
        (send dc set-pen
              (send the-pen-list find-or-create-pen "black" 0 'transparent))
        (send dc set-brush
              (send the-brush-list find-or-create-brush "white" 'solid))
        (send dc draw-rectangle 0 0 width height)
        (send dc set-pen old-pen)
        (send dc set-brush old-brush)))

    ;; Draw the map on the device context DC at position X, Y.  The width and
    ;; height of the map is stored in this object.  Note that the code must
    ;; not assume that the entire device context is covered by the map.
    (define/public (draw dc x y)
      (with-draw-context dc
        (lambda ()
          (with-clipping-rect dc x y width height
            (lambda ()
              (if show-map-layer?
                  (draw-map-tiles dc x y)
                  (clear-dc dc x y width height))
              (with-origin dc (- origin-x x) (- origin-y y)
                (lambda ()
                  (define sorted-groups
                    (sort
                     (remove-duplicates (for/list ([t tracks]) (send t get-group)))
                     >
                     #:key (lambda (group)
                             (hash-ref group-zorder group default-zorder))))
                  (define brush
                    (send the-brush-list find-or-create-brush "black" 'transparent))
                  (for ([group sorted-groups])
                    (define pen (hash-ref group-pens group default-pen))
                    (for ([track (in-list tracks)] #:when (equal? (send track get-group) group))
                      (send track draw dc the-zoom-level pen brush)))

                  (for ([marker markers])
                    (send marker draw dc the-zoom-level))
                  ;; Draw the current location marker, as set by
                  ;; `on-current-location-updated'
                  (when (and last-current-location-x last-current-location-y)
                    (send dc set-pen current-location-pen)
                    (send dc set-brush current-location-brush)
                    (send dc draw-ellipse
                          (- last-current-location-x 12)
                          (- last-current-location-y 12)
                          24 24))

                  (when debug?
                    (define bbox (get-bounding-box))
                    (when bbox
                      (send dc set-pen debug-pen)
                      (send dc set-brush brush)
                      (draw-bounding-box dc bbox the-zoom-level)))))

              (draw-map-legend dc x y width height the-zoom-level))))))

    ;; Return the dimensions of the map
    (define/public (get-size) (values width height))

    ;; Timer to schedule a re-paint of the canvas when we have some missing
    ;; tiles -- hopefully the tiles will arrive by the time we get to re-paint
    (define redraw-timer
      (new timer% [notify-callback (lambda () (request-refresh))]))

    ;; Timer to schedule a map drag event to pan the current location in view
    (define auto-drag-map-timer
      (new timer% [notify-callback (lambda () (on-current-location-updated))]))

    ;; Draw the map tiles on the device context DC at DX, DY.  Note that this
    ;; function does not assume that the map is drawn on the entire device
    ;; context.
    (define/private (draw-map-tiles dc dx dy)
      (send redraw-timer stop)

      ;; Use smoothing on high DPI displays, but not on low DPI ones (each
      ;; look better in the corresponding mode).
      (define old-smoothing (send dc get-smoothing))
      (if (> (get-display-backing-scale) 1.0)
          (send dc set-smoothing 'smoothed)
          (send dc set-smoothing 'unsmoothed))

      (let* ((request-redraw? #f)
             ;; Coordinates of the tile at canvas origin (need not be a valid
             ;; tile)
             (tile0-x (exact-floor (/ origin-x tile-size)))
             (tile0-y (exact-floor (/ origin-y tile-size)))

             ;; offset inside the tile where the canvas origin lives.
             (xofs (- origin-x (* tile0-x tile-size)))
             (yofs (- origin-y (* tile0-y tile-size)))

             ;; Number of tiles on the width and height
             (tw (add1 (exact-ceiling (/ width tile-size))))
             (th (add1 (exact-ceiling (/ height tile-size)))))

        ;; Tell the bitmap cache how many tiles to keep in the cache
        (set-cache-threshold (* 5 tw th))

        (for* ((x (in-range 0 tw))
               (y (in-range 0 th)))
          (let ((tile-x (+ tile0-x x))
                (tile-y (+ tile0-y y)))
            (when (and (valid-tile-num? tile-x) (valid-tile-num? tile-y))
              (let ((bmp (or (get-tile-bitmap (map-tile the-zoom-level tile-x tile-y))
                             (begin (set! request-redraw? #t) empty-bmp))))
                (send dc draw-bitmap bmp
                      (+ dx (- (* x tile-size) xofs))
                      (+ dy (- (* y tile-size) yofs)))))))

        (when (or request-redraw? (> (get-download-backlog) 0))
          (send redraw-timer start 100))

        (send dc set-smoothing old-smoothing)))

    ;; Set and get the current zoom level
    (public zoom-level)
    (define zoom-level
      (case-lambda
        [() the-zoom-level]
        [(zl)
         ;; Ensure the zoom level is in the valid range
         (when (> zl (max-zoom-level)) (set! zl (max-zoom-level)))
         (when (< zl (min-zoom-level)) (set! zl (min-zoom-level)))
         ;; Don't do anything unless the zoom level actually changes
         (unless (eq? zl the-zoom-level)
           (let ((scale (expt 2 (- zl the-zoom-level))))
             (set! the-zoom-level zl)
             (set! max-tile-num (expt 2 the-zoom-level))
             (set! max-coord (* tile-size max-tile-num))
             ;; update the origin at the new zoom level (note that we scale
             ;; around the center of the view)
             (set! origin-x (- (* scale (+ origin-x (/ width 2))) (/ width 2)))
             (set! origin-y (- (* scale (+ origin-y (/ height 2))) (/ height 2))))
           (limit-origin width height)
           (set! last-current-location-x #f)
           (set! last-current-location-y #f)
           (on-current-location-updated)
           (request-refresh)
           (on-zoom-level-change the-zoom-level))]))

    (public show-map-layer)
    (define show-map-layer
      (case-lambda
        [() show-map-layer?]
        [(flag)
         (unless (equal? show-map-layer? flag)
           (set! show-map-layer? flag)
           (request-refresh))]))

    ;; Clear the map of all tracks and markers.
    (define/public (clear)
      (set! tracks '())
      (set! markers '())
      (request-refresh))

    ;; Add a GPS track to the map.  TRACK is a sequence of (Vector LAT LON)
    ;; and GROUP is a group identifier for the track.  Tracks are grouped
    ;; together using the same GROUP for the purposes of drawning and z-order.
    ;; Grouping is useful if there are several disjoint tracks in a logical
    ;; section (e.g. when the user stops recording, moves some distance and
    ;; starts recording again).
    (define/public (add-track track group)
      (define gtrack (new track% [track track] [group group]))
      (set! tracks (cons gtrack tracks))
      (request-refresh))

    ;; Add a label on the map at a specified position a (Vector LAT LON)
    (define/public (add-marker pos text direction color)
      (define gmarker (new marker% [pos pos] [text text]
                           [direction direction] [color color]))
      (set! markers (cons gmarker markers))
      (request-refresh))

    ;; Called when the current location has been updated, handles redisplay of
    ;; the current location as well as auto-dragging the map, if this is
    ;; enabled.
    (define/private (on-current-location-updated)

      (send auto-drag-map-timer stop)

      ;; The current location has been cleared, refresh the map
      (when (and (not the-current-location)
                 (or last-current-location-x last-current-location-y))
        (set! last-current-location-x #f)
        (set! last-current-location-x #f)
        (request-refresh))

      (when the-current-location
        (let* ((point (lat-lon->map-point
                       (point-lat the-current-location) (point-long the-current-location)))
               (px (* max-coord (map-point-x point)))
               (py (* max-coord (map-point-y point)))
               (cx (+ origin-x (/ width 2)))
               (cy (+ origin-y (/ height 2)))
               (dx (- cx px))
               (dy (- cy py))
               (need-refresh? #f))
          ;; We only need a refresh if the current location moved at least
          ;; one pixel on the screen.
          (set! need-refresh?
                (or (not last-current-location-x)
                    (not last-current-location-y)
                    (>= (abs (- last-current-location-x px)) 1.0)
                    (>= (abs (- last-current-location-y py)) 1.0)))
          (when need-refresh?
            (request-refresh)
            ;; Only update this if we need to refresh -- otherwise we can
            ;; creep out in small increments and never notice it!
            (set! last-current-location-x px)
            (set! last-current-location-y py))

          (when should-track-current-location
            (define auto-drag-map
              (cond
                ((or (> (abs dx) (/ width 8)) (> (abs dy) (/ height 8))) #t)
                ((and (< (abs dx) 1) (< (abs dy) 1)) #f)
                (#t #f)))
            (when auto-drag-map
              (set! origin-x (exact-round (- origin-x (* dx 0.1))))
              (set! origin-y (exact-round (- origin-y (* dy 0.1))))
              (limit-origin width height)
              (send auto-drag-map-timer start 100)
              (request-refresh))))))

    ;; Get and set the current location of the map
    (public current-location)
    (define current-location
      (case-lambda
        (() the-current-location)
        ((pos)
         (set! the-current-location pos)
         (on-current-location-updated))))

    ;; Get and set whether to track the current location -- if set, the map
    ;; will always be panned so that the current location is in the center of
    ;; the map.
    (public track-current-location)
    (define track-current-location
      (case-lambda
        (() should-track-current-location)
        ((flag)
         (set! should-track-current-location flag)
         (on-current-location-updated))))

    ;; Set the pen used to draw the specified track GROUP.  If GROUP is #f,
    ;; all the tracks will use this pen.
    (define/public (set-group-pen group pen)
      (if group
          (hash-set! group-pens group pen)
          (begin
            ; silly way in which inspect-map uses the widget.
            (set! group-pens (make-hash))
            (set! default-pen pen)))
      (request-refresh))

    ;; Set the Z-ORDER used to draw a track group.  If GROUP is #f, al the
    ;; tracks will use this Z-ORDER and the tracks are drawn in the order they
    ;; were added.  Tracks are drawn back to front, biggest Z-ORDER first.
    ;; This way, tracks with smaller Z-ORDER will be "on top".
    (define/public (set-group-zorder group zorder)
      (if group
          (hash-set! group-zorder group zorder)
          (begin
            ; silly way in which inspect-map uses the widget.
            (set! group-zorder (make-hash))
            (set! default-zorder zorder)))
      (request-refresh))

    ;; Delete all tracks in GROUP, or delete all tracks if GROUP is #f.
    (define/public (delete-group group)
      (define ntracks
        (for/list ([track tracks]
                   #:unless (or (not group) (equal? group (send track get-group))))
          track))
      (set! tracks ntracks)
      (request-refresh))

    ;; Return the bounding box for all tracks in GROUP, or if GROUP is #f for
    ;; all tracks on the map.
    (define/private (get-bounding-box [group #f])
      (define bb
        (for/fold ([outer #f])
                  ([track tracks]
                   #:when (or (not group) (equal? group (send track get-group))))
          (let ((bb (send track get-bounding-box)))
            (if outer (bbox-merge outer bb) bb))))
      (if group
          bb
          ;; If no group is specified, include the markers as well
          (for/fold ([bb bb])
                    ([marker markers])
            (if bb
                (bbox-extend bb (send marker get-position))
                (let ((pos (send marker get-position)))
                  (map-bbox (point-lat pos) (point-long pos)
                            (point-lat pos) (point-long pos)))))))

    ;; Return the center position for all tracks in GROUP, or the center
    ;; position for all tracks when GROUP is #f
    (define/private (get-center [group #f])
      (let ([bbox (get-bounding-box group)])
        (if bbox
            (let ([cp/ndcs (bbox-center/ndcs bbox)])
              (values (* (map-point-x cp/ndcs) max-coord)
                      (* (map-point-y cp/ndcs) max-coord)))
            ;; For no particular reason, the center of the map, when no
            ;; bounding box is available is the middle of Swan River, Perth,
            ;; Western Australia
            (let ([p (lat-lon->map-point -31.974762 115.839303)])
              (values (* (map-point-x p) max-coord)
                      (* (map-point-y p) max-coord))))))

    ;; Move the map so that the tracks are centered in the middle.
    (define/public (center-map [group #f])
      (let-values (([cx cy] (get-center group)))
        (set! origin-x (- cx (/ width 2)))
        (set! origin-y (- cy (/ height 2))))
      (limit-origin width height)
      (request-refresh))

    ;; Resize (set the zoom level) and center the map so that all tracks in
    ;; GROUP are visible.  If GROUP is #f, resize and center the map such that
    ;; all tracks are visible.
    (define/public (resize-to-fit [group #f])
      (let ((bbox (get-bounding-box group)))
        (when bbox
          (zoom-level (select-zoom-level bbox width height))))
      (center-map group)
      (request-refresh))

    ;; move the map such that POSITION is in the center
    (define/public (move-to position)
      (match-define (vector lat lon _ ...) position)
      (let* ([p (lat-lon->map-point lat lon)]
             [cx (* (map-point-x p) max-coord)]
             [cy (* (map-point-y p) max-coord)])
        (set! origin-x (- cx (/ width 2)))
        (set! origin-y (- cy (/ height 2))))
      (limit-origin width height)
      (request-refresh))

    ;; Write an image of the current map to FILE-NAME
    (define/public (export-image-to-file file-name)
      (let ((bmp (make-bitmap width height)))
        (draw (new bitmap-dc% [bitmap bmp]) 0 0)
        (send bmp save-file file-name 'png)))

    (when track
      (add-track track 0))
    (if position (move-to position) (center-map))

    ))
