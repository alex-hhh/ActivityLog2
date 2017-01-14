#lang racket/base
;; map-widget.rkt -- a map widget which allows displaying a primary and
;; secondary GPS track plus open-street map tiles for the map itself.  Map
;; tiles retrieved from the net are cached locally in a persistent store.
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

(require db
         math/base
         racket/class
         racket/draw
         racket/flonum
         racket/gui/base
         racket/list
         racket/sequence
         racket/stream
         "al-log.rkt"
         "al-prefs.rkt"
         "fmt-util.rkt"
         "map-util.rkt"
         "map-tiles.rkt")

(provide map-widget%)
(provide get-max-zoom-level get-min-zoom-level)

(define tile-size 256)                  ; size of the map tiles, in pixels
(define earth-radius (->fl 6371000))    ; meters

;; convert a zoom level to a "meters per pixel" value
(define (zoom-level->mpp zoom-level)
  (let ((n (expt 2 (+ 8 zoom-level))))
    (/ (* 2 pi earth-radius) n)))

;; convert a "meters per pixel" value to an approximate zoom level
(define (mpp->zoom-level mpp)
  (let ((n (/ (* 2 pi earth-radius) mpp)))
    (- (/ (log n) (log 2)) 8)))

;; Maximum zoom level we allow for the map widget.
(define max-zoom-level
  (al-get-pref 'activity-log:max-map-zoom-level (lambda () 16)))
(define min-zoom-level 1)

(define (get-max-zoom-level) max-zoom-level)
(define (get-min-zoom-level) min-zoom-level)


;;........................................................... gps-track% ....

;; Return a zoom-level such that the contents of BBOX will fit in a canvas of
;; CANVAS-WIDTH, CANVAS-HEIGHT pixels
(define (select-zoom-level bbox canvas-width canvas-height)
  (unless bbox (error "select-zoom-level" bbox))
  (let-values (([w h] (bbox-size bbox)))
    ;; If the BBOX is too small, just return the max zoom level we have (the
    ;; calculation below might return +/-inf otherwise
    (if (and (< w 5.0) (< h 5.0))
        (get-max-zoom-level)
        ;; mpp -- meters per pixel
        (let ((mpp-width (/ w canvas-width))
              (mpp-height (/ h canvas-height)))
          (- (exact-floor (min (mpp->zoom-level mpp-width)
                               (mpp->zoom-level mpp-height)))
             1)))))

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

(define (draw-bounding-box dc bounding-box zoom-level)

  (define (get-center zoom-level)
    (let ((max-coord (* tile-size (expt 2 zoom-level)))
          (center/ndcs (bbox-center/ndcs bounding-box)))
      (values (* (map-point-x center/ndcs) max-coord)
              (* (map-point-y center/ndcs) max-coord))))

  (let-values (([cx cy] (get-center zoom-level)))
    (send dc draw-ellipse (+ cx -10) (+ cy -10) 20 20))
  (let ((max-coord (* tile-size (expt 2 zoom-level)))
        (map1 (lat-lon->map-point (car (first bounding-box)) (cdr (first bounding-box))))
        (map2 (lat-lon->map-point (car (second bounding-box)) (cdr (second bounding-box)))))
    (let ((x1 (* (map-point-x map1) max-coord))
          (y1 (* (map-point-y map1) max-coord))
          (x2 (* (map-point-x map2) max-coord))
          (y2 (* (map-point-y map2) max-coord)))
      (send dc draw-rectangle x1 y1 (- x2 x1) (- y2 y1)))))

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

(define (with-draw-context dc origin-x origin-y thunk)
  (let-values (([ox oy] (send dc get-origin)))
    (send dc set-origin (- origin-x) (- origin-y))
    (send dc set-smoothing 'smoothed)
    (thunk)
    (send dc set-origin ox oy)))

;; Represent a GPS track that can be drawn on a dc<%> at different zoom
;; levels.
(define gps-track%
  (class object%
    (init-field track group)
    (super-new)

    (define zorder 0.5)
    (define bbox (track-bbox track))
    (define pen
      (send the-pen-list find-or-create-pen
            (make-object color% 226 34 62)
            3 'solid 'round 'round))
    (define brush
      (send the-brush-list find-or-create-brush "black" 'transparent))
    (define debug?
      (al-get-pref 'activity-log:draw-track-bounding-box (lambda () #f)))
    (define debug-pen
      (send the-pen-list find-or-create-pen (make-object color% 86 13 24) 2 'solid))
    (define paths-by-zoom-level (make-hash))

    (define (get-dc-path zoom-level)
      (let ((dc-path (hash-ref paths-by-zoom-level zoom-level #f)))
        (unless dc-path
          ;; no dc-path at this zoom level, create one now
          (let ((strack (simplify-track track zoom-level)))
            (set! dc-path (track->dc-path strack zoom-level))
            (hash-set! paths-by-zoom-level zoom-level dc-path)))
        dc-path))

    (define/public (draw dc zoom-level)
      (let ((path (get-dc-path zoom-level)))
        (send dc set-pen pen)
        (send dc set-brush brush)
        (send dc draw-path path 0 0))
      (when debug?
        (send dc set-pen debug-pen)
        (send dc set-brush brush)
        (draw-bounding-box dc bbox zoom-level)))

    (define/public (set-pen p) (set! pen p))
    (define/public (set-zorder z) (set! zorder z))
    (define/public (get-zorder) zorder)
    (define/public (get-bbox) bbox)
    (define/public (get-group) group)

    ))

(define gps-marker%
  (class object%
    (init-field pos text direction color)
    (super-new)

    (define/public (draw dc zoom-level)
      (define point (lat-lon->map-point (point-lat pos) (point-long pos)))
      (draw-label dc point zoom-level text direction color))
    
    ))


;.......................................................... map-widget% ....

(define legend-color (make-object color% 86 13 24))
(define legend-pen
  (send the-pen-list find-or-create-pen legend-color 2 'solid))
(define legend-font
  (send the-font-list find-or-create-font 8 'default 'normal 'normal))
;; (define legend-copyright "Copyright(c) OpenStreetMap contributors")
(define legend-copyright "Maps © Thunderforest, Data © OpenStreetMap contributors")

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

(define (legend-distance)
  (if (eq? (al-pref-measurement-system) 'metric)
      legend-distance-metric
      legend-distance-statute))

(define (draw-map-legend canvas dc zoom-level)
  (let* ((entry (assq zoom-level (legend-distance)))
         (distance (if entry (second entry) 1000))
         (label (if entry (third entry) "1 km")))
    (let ((km-dist (/ distance (zoom-level->mpp zoom-level)))
          (label (format "~a  (ZL ~a; BL ~a)" label zoom-level (get-download-backlog)))
          (ox 10)
          (oy 10))
      (let-values (([cw ch] (send canvas get-size)))
        (send dc set-brush
              (send the-brush-list find-or-create-brush
                    (make-color 255 255 255 0.7) 'solid))
        (send dc set-pen
              (send the-pen-list find-or-create-pen "white" 1 'transparent))
        (send dc set-font legend-font)
        (send dc set-text-foreground legend-color)

        (let-values (((w h x y) (send dc get-text-extent legend-copyright legend-font #t)))
          (send dc draw-rectangle (- cw ox 5 w) (- ch oy 5 h) (+ w 5 5) (+ h 5 5))
          (send dc draw-text legend-copyright (- cw ox w) (- ch oy h)))

        (send dc draw-rectangle (- ox 5) (- ch oy 20) (+ km-dist 5 5) 25)
        (send dc set-pen legend-pen)

        (send dc draw-line ox (- ch oy) (+ ox km-dist) (- ch oy))
        (send dc draw-line ox (- ch oy) ox (- ch oy 3))
        (send dc draw-line (+ ox km-dist) (- ch oy) (+ ox km-dist) (- ch oy 3))

        (let-values (([w h b e] (send dc get-text-extent label)))
          (let ((tx (+ ox (/ (- km-dist w) 2)))
                (ty (- ch oy 3 h)))
            (send dc draw-text label tx ty)))))))

(define debug-track-colors
  (vector (make-object color% #xad #xd8 #xe6) ; light blue
          (make-object color% #x00 #xbf #xff) ; deep sky blue
          (make-object color% #x22 #x8b #x22) ; forrest green
          (make-object color% #xff #x7f #x50) ; coral
          (make-object color% #xcd #x5c #x5c) ; indian red
          (make-object color% #xdc #x14 #x3c) ; crimson
          (make-object color% #x8b #x00 #x00) ; dark red
          (make-object color% #x99 #x32 #xcc) ; dark orchid
          (make-object color% #x00 #x00 #x8b) ; dark blue
          (make-object color% #xff #x8c #x00) ; dark orange
          (make-object color% #xda #xa5 #x20) ; golden rod
          ))
(define debug-track-color-index -1)
(define (get-next-pen)
  (set! debug-track-color-index (+ 1 debug-track-color-index))
  (set! debug-track-color-index (remainder debug-track-color-index (vector-length debug-track-colors)))
  (define color (vector-ref debug-track-colors debug-track-color-index))
  (send the-pen-list find-or-create-pen color 3 'solid 'round 'round))

(define map-widget%
  (class object%
    (init parent) (super-new)

    ;;; data to display on the map
    (define tracks '())
    (define markers '())
    (define zoom-level 1)
    (define max-tile-num (expt 2 zoom-level))
    (define max-coord (* tile-size max-tile-num))

    (define (valid-tile-num? n) (and (>= n 0) (< n max-tile-num)))
    (define (valid-coord? n) (and (>= n 0) (< n max-coord)))

    (define origin-x 0)
    (define origin-y 0)

    (define (limit-origin canvas)
      ;; Adjust the map origin such that we don't have to draw past the map
      ;; edges at the current zoom level
      (let-values (([w h] (send canvas get-size)))
        (when (> (+ origin-x w) max-coord)
          (set! origin-x (- max-coord w)))
        (when (< origin-x 0)
          (set! origin-x 0))
        (when (> (+ origin-y h) max-coord)
          (set! origin-y (- max-coord h)))
        (when (< origin-y 0)
          (set! origin-y 0))))

    (define (on-canvas-resize canvas old-width old-height)
      ;; Adjust the map origin as is the resize happened around the center of
      ;; the canvas.
      (let-values (((w h) (send canvas get-size)))
        (set! origin-x (- (+ origin-x (/ old-width 2)) (/ w 2)))
        (set! origin-y (- (+ origin-y (/ old-height 2)) (/ h 2)))
        (limit-origin canvas)
        (send canvas refresh)))

    (define last-mouse-x #f)
    (define last-mouse-y #f)

    (define (on-mouse-event canvas event)
      ;; Implement map panning by dragging the mouse
      (cond ((send event button-down? 'left)
             (set! last-mouse-x (send event get-x))
             (set! last-mouse-y (send event get-y)))
            ((send event button-up? 'left)
             (set! last-mouse-x #f)
             (set! last-mouse-y #f))
            ((send event dragging?)
             (let ((mouse-x (send event get-x))
                   (mouse-y (send event get-y)))
               (when (and last-mouse-x last-mouse-y)
                 (set! origin-x (- origin-x (- mouse-x last-mouse-x)))
                 (set! origin-y (- origin-y (- mouse-y last-mouse-y)))
                 (limit-origin canvas)
                 (send canvas refresh))
               (set! last-mouse-x mouse-x)
               (set! last-mouse-y mouse-y)))))

    (define (on-key-event canvas event)
      ;; Implement map zoom-in and out using the mouse wheel.  Mouse wheel
      ;; zoom is handled by the key event
      (let ((key-code (send event get-key-code)))
        (cond ((and (eq? key-code 'wheel-up) (< zoom-level 18))
               (set-zoom-level (+ zoom-level 1)))
              ((and (eq? key-code 'wheel-down) (> zoom-level 3))
               (set-zoom-level (- zoom-level 1))))))

    (define (on-canvas-paint canvas dc)
      (draw-map-tiles canvas dc)
      (with-draw-context
        dc origin-x origin-y
        (lambda ()
          (for ([track (sort tracks > #:key (lambda (t) (send t get-zorder)))])
            (send track draw dc zoom-level))
          (for ([marker markers])
            (send marker draw dc zoom-level))))
      (draw-map-legend canvas dc zoom-level))

    ;; Bitmap to draw when we don't receive a tile
    (define empty-bmp (make-bitmap tile-size tile-size #f))

    ;; Timer to schedule a re-paint of the canvas when we have some missing
    ;; tiles -- hopefully the tiles will arrive by the time we get to re-paint
    (define redraw-timer
      (new timer% [notify-callback (lambda () (send canvas refresh))]))

    (define (draw-map-tiles canvas dc)
      ;; Use smoothing on high DPI displays, but not on low DPI ones (each
      ;; look better in the corresponding mode).
      (if (> (get-display-backing-scale) 1.0)
          (send dc set-smoothing 'smoothed)
          (send dc set-smoothing 'unsmoothed))
      (send redraw-timer stop)
      (let* ((request-redraw? #f)

             ;; Coordinates of the tile at canvas origin (need not be a valid
             ;; tile)
             (tile0-x (exact-floor (/ origin-x tile-size)))
             (tile0-y (exact-floor (/ origin-y tile-size)))

             ;; offset inside the tile where the canvas origin lives.
             (xofs (- origin-x (* tile0-x tile-size)))
             (yofs (- origin-y (* tile0-y tile-size))))

        (let-values (((w h) (send canvas get-size)))
          (for* ((x (in-range 0 (+ 1 (exact-ceiling (/ w tile-size)))))
                 (y (in-range 0 (+ 1 (exact-ceiling (/ h tile-size))))))
                (let ((tile-x (+ tile0-x x))
                      (tile-y (+ tile0-y y)))
                  (when (and (valid-tile-num? tile-x) (valid-tile-num? tile-y))
                    (let ((bmp (or (get-tile-bitmap (map-tile zoom-level tile-x tile-y))
                                   (begin (set! request-redraw? #t) empty-bmp))))
                      (send dc draw-bitmap bmp
                            (- (* x tile-size) xofs)
                            (- (* y tile-size) yofs)))))))

        (when (or request-redraw? (> (get-download-backlog) 0))
          (send redraw-timer start 500))))

    (define canvas
      (new (class canvas% (init) (super-new)
             ;; Save the canvas width and height here, on-size will only get
             ;; the new size from the canvas.  Note that the first resize
             ;; event is ignored and the on-canvas-resize event will not fire.
             (define canvas-width #f)
             (define canvas-height #f)

             (define/override (on-size w h)
               (when (and canvas-width canvas-height)
                 (on-canvas-resize this canvas-width canvas-height))
               ;; NOTE: w and h are window dimensions, not client area
               ;; dimensions, save the canvas dimensions
               (let-values (([w h] (send this get-size)))
                 (set! canvas-width w)
                 (set! canvas-height h)))
             (define/override (on-event event)
               (on-mouse-event this event))
             (define/override (on-char event)
               (on-key-event this event)))
           [parent parent] [paint-callback on-canvas-paint]))

    (define/public (set-zoom-level zl)
      ;; Ensure the zoom level is in the valid range
      (when (> zl max-zoom-level) (set! zl max-zoom-level))
      (when (< zl min-zoom-level) (set! zl min-zoom-level))

      ;; Don't do anything unless the zoom level actually changes
      (unless (eq? zl zoom-level)

        (let ((scale (expt 2 (- zl zoom-level))))
          (set! zoom-level zl)
          (set! max-tile-num (expt 2 zoom-level))
          (set! max-coord (* tile-size max-tile-num))
          ;; update the origin at the new zoom level (note that we scale
          ;; around the center of the view)
          (let-values (((w h) (send canvas get-size)))
            (set! origin-x (- (* scale (+ origin-x (/ w 2))) (/ w 2)))
            (set! origin-y (- (* scale (+ origin-y (/ h 2))) (/ h 2)))))
        (limit-origin canvas)
        (send canvas refresh)
        (on-zoom-level-change zoom-level)))

    (define/public (clear-items)
      (send canvas suspend-flush)
      (set! tracks '())
      (set! markers '())
      (set! zoom-level 1)
      (send canvas resume-flush)
      (send canvas refresh))

    ;; Add a GPS track to the map.  TRACK is a sequence of (Vector LAT LON)
    ;; and GROUP is a group identifier for the track.  Tracks are grouped
    ;; toghether using the same GROUP for the purposes of drawning and
    ;; z-order.  Grouping is useful if there are several disjoint tracks in a
    ;; logical section (e.g. when the user stops recording, moves some
    ;; distance and starts recording again).
    (define/public (add-track track group)
      (send canvas suspend-flush)
      (define gtrack (new gps-track% [track track] [group group]))
      (set! tracks (cons gtrack tracks))
      (send canvas resume-flush)
      (send canvas refresh))

    ;; Add a label on the map at a specified position a (Vector LAT LON)
    (define/public (add-marker pos text direction color)
      (send canvas suspend-flush)
      (define gmarker (new gps-marker% [pos pos] [text text]
                           [direction direction] [color color]))
      (set! markers (cons gmarker markers))
      (send canvas resume-flush)
      (send canvas refresh))

    ;; Set the pen used to draw a certain track group.  If GROUP is #f, all
    ;; the tracks will use this pen.
    (define/public (set-track-group-pen group pen)
      (for ([track tracks]
            #:when (or (not group)
                       (equal? group (send track get-group))))
        (send track set-pen pen))
      (send canvas refresh))

    ;; Set the Z-ORDER used to draw a track group.  If GROUP is #f, al the
    ;; tracks will use this Z-ORDER and the tracks are drawn in the order they
    ;; were added.  Tracks are drawn back to front, biggest Z-ORDER first.
    ;; This way, tracks with smaller Z-ORDER will be "on top".
    (define/public (set-track-group-zorder group zorder)
      (for ([track tracks]
            #:when (or (not group)
                       (equal? group (send track get-group))))
        (send track set-zorder zorder))
      (send canvas refresh))

    ;; Delete all tracks in GROUP, or delete all tracks if GROUP is #f.
    (define/public (delete-track-group group)
      (define ntracks
        (for/list ([track tracks]
                   #:unless (or (not group) (equal? group (send track get-group))))
          track))
      (set! tracks ntracks)
      (send canvas refresh))
          
    (define/public (get-bbox [group #f])
      (for/fold ([bbox #f])
                ([track tracks]
                 #:when (or (not group)
                            (equal? group (send track get-group))))
        (let ((bb1 (send track get-bbox)))
          (if bbox (bbox-merge bbox bb1) bb1))))

    (define/public (get-center [group #f])
      (let ([bbox (get-bbox group)])
        (if bbox
            (let* ([cp/ndcs (bbox-center/ndcs bbox)]
                   [max-coord (* tile-size (expt 2 zoom-level))])
              (values (* (map-point-x cp/ndcs) max-coord)
                      (* (map-point-y cp/ndcs) max-coord)))
            (values 0 0))))

    (define/public (center-map [group #f])
      (let-values (([cx cy] (get-center group))
                   ([cwidth cheight] (send canvas get-size)))
        (let ((actual-center-x (+ origin-x (/ cwidth 2)))
              (actual-center-y (+ origin-y (/ cheight 2))))
          (set! origin-x (+ origin-x (- cx actual-center-x)))
          (set! origin-y (+ origin-y (- cy actual-center-y)))))
      (limit-origin canvas)
      (send canvas refresh))

    (define/public (resize-to-fit [group #f])
      (let-values (((cwidth cheight) (send canvas get-size)))
        (set-zoom-level (select-zoom-level (get-bbox group) cwidth cheight))
        (center-map group)
        (send canvas refresh)))

    ;; Can be overriden to be notified of zoom level changes
    (define/public (on-zoom-level-change zl)
      #f)
    
    (define/public (export-image-to-file file-name)
      (let-values (([cwidth cheight] (send canvas get-size)))
        (let ((bmp (send canvas make-bitmap cwidth cheight)))
          (on-canvas-paint canvas (new bitmap-dc% [bitmap bmp]))
          (send bmp save-file file-name 'png))))

    ))
