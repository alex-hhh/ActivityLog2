#lang racket/base
;; map-widget.rkt -- a map widget which allows displaying a primary and
;; secondary GPS track plus open-street map tiles for the map itself.  Map
;; tiles retrieved from the net are cached locally in a persistent store.
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015, 2019 Alex Harsányi <AlexHarsanyi@gmail.com>
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
         racket/contract
         racket/gui/base
         "../../utilities.rkt"
         "map-impl.rkt"
         "map-tiles.rkt"
         "map-util.rkt")

(define map-widget%/c
  (class/c
   (init [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) (is-a?/c panel%) (is-a?/c pane%))]
         [position (or/c #f (vector/c real? real?))])
   (zoom-level (case->m (-> exact-nonnegative-integer? any/c)
                        (-> exact-nonnegative-integer?)))
   (show-map-layer (case->m (-> boolean? any/c)
                            (-> boolean?)))
   (clear (->m any/c))
   (add-track (->m sequence? (or/c #f symbol? integer?) any/c))
   (add-marker (->m (vector/c real? real?) string? (or/c -1 1) (is-a?/c color%) any/c))
   (current-location (->m (or/c (vector/c real? real?) #f) any/c))
   (track-current-location (->m boolean? any/c))
   (set-group-pen (->m (or/c #f symbol? integer?) (is-a?/c pen%) any/c))
   (set-group-zorder (->m (or/c #f symbol? integer?) positive? any/c))
   (delete-group (->m (or/c #f symbol? integer?) any/c))
   (center-map (->*m () ((or/c #f symbol?)) any/c))
   (move-to (->m (vector/c real? real?) any/c))
   (resize-to-fit (->*m () ((or/c #f symbol?)) any/c))
   (export-image-to-file (->m path-string? any/c))))

(provide
 (contract-out [map-widget% map-widget%/c]))

(provide min-zoom-level max-zoom-level)

;; A map widget that can be embedded in a Racket GUI application along with
;; other widgets.  This implementation uses a canvas% to draw the map, but the
;; map drawing is implemented in map-impl%
(define map-widget%
  (class object%
    (init parent [position #f] [zoom 12]) (super-new)

    (define first-paint? #t)

    (define (on-canvas-paint canvas dc)
      (when first-paint?
        ;; If this is our first paint, we can determine the size of the canvas
        ;; (before the first paint invocation, the size will be 0.
        (let-values ([(w h) (send canvas get-size)])
          (send map-impl resize w h))
        (set! first-paint? #f))
      (send map-impl draw dc 0 0))

    ;; This is the canvas on which we paint the map.  We intercept the mouse
    ;; and keyboard events to pass them on to the map-impl% object.
    (define canvas
      (new (class canvas% (init) (super-new)
             (define/override (on-size w h)
               (send map-impl resize w h))
             (define/override (on-event event)
               (define dc (send this get-dc))
               (send map-impl on-event dc 0 0 0 0 event))
             (define/override (on-char event)
               (define dc (send this get-dc))
               (send map-impl on-char dc 0 0 0 0 event)))
           [parent parent]
           [paint-callback on-canvas-paint]
           [style '(no-autoclear)]))

    ;; The map implementation, handles drawing and event handling
    (define map-impl
      (new map-impl%
           [width 100] [height 100]
           [zoom zoom]
           [position position]
           [request-refresh (lambda () (send canvas refresh))]
           [on-zoom-level-change (lambda (zl) (on-zoom-level-change zl))]))

    ;; The methods below are provided as forwarding calls into the map-impl%
    ;; itself...

    (public zoom-level)
    (define zoom-level
      (case-lambda
        [() (send map-impl zoom-level)]
        [(zl) (send map-impl zoom-level zl)]))

    (public show-map-layer)
    (define show-map-layer
      (case-lambda
        [() (send map-impl show-map-layer)]
        [(flag) (send map-impl show-map-layer flag)]))

    (define/public (clear)
      (send map-impl clear))

    (define/public (add-track track group)
      (send map-impl add-track track group))

    (define/public (add-marker pos text direction color)
      (send map-impl add-marker pos text direction color))

    (define/public (current-location pos)
      (send map-impl current-location pos))

    (define/public (track-current-location flag)
      (send map-impl track-current-location flag))

    (define/public (set-group-pen group pen)
      (send map-impl set-group-pen group pen))

    (define/public (set-group-zorder group zorder)
      (send map-impl set-group-zorder group zorder))

    (define/public (delete-group group)
      (send map-impl delete-group group))

    (define/public (center-map [group #f])
      (send map-impl center-map group))

    (define/public (move-to position)
      (send map-impl move-to position))

    (define/public (resize-to-fit [group #f])
      (send map-impl resize-to-fit group))

    (define/public (export-image-to-file file-name)
      (send map-impl export-image-to-file file-name))

    ;; Can be overriden to be notified of zoom level changes
    (define/public (on-zoom-level-change zl)
      (void))
    ))
