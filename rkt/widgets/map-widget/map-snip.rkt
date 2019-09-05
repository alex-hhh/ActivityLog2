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
         "map-impl.rkt")

(define map-snip%/c
  (class/c
   (init [position (or/c #f (vector/c flonum? flonum?))]
         [track sequence?]
         [zoom exact-nonnegative-integer?])

   (zoom-level (case->m (-> exact-nonnegative-integer? any/c)
                        (-> exact-nonnegative-integer?)))
   (show-map-layer (case->m (-> boolean? any/c)
                            (-> boolean?)))
   (clear (->m any/c))
   (add-track (->m sequence? (or/c #f symbol? integer?) any/c))
   (add-marker (->m (vector/c flonum? flonum?) string? (or/c -1 1) (is-a?/c color%) any/c))
   (current-location (->m (or/c (vector/c flonum? flonum?) #f) any/c))
   (track-current-location (->m boolean? any/c))
   (set-group-pen (->m (or/c #f symbol? integer?) (is-a?/c pen%) any/c))
   (set-group-zorder (->m (or/c #f symbol? integer?) positive? any/c))
   (delete-group (->m (or/c #f symbol? integer?) any/c))
   (center-map (->*m () ((or/c #f symbol?)) any/c))
   (move-to (->m (vector/c flonum? flonum?) any/c))
   (resize-to-fit (->*m () ((or/c #f symbol?)) any/c))
   (export-image-to-file (->m path-string? any/c))))

(provide
 (contract-out [map-snip% map-snip%/c]))

(provide min-zoom-level max-zoom-level)

(define map-snip-class
  (make-object
   (class snip-class%
     (super-new)
     (send this set-classname "map-snip"))))
(send (get-the-snip-class-list) add map-snip-class)

;; A map widget that can be embedded in an editor-pasteboard% as a snip.  This
;; implementation uses a snip% to draw the map, but the map drawing is
;; implemented in map-impl%, so this class acts as an adapter.
(define map-snip%
  (class snip%
    (init [zoom 12] [position #f] [track #f])
    (init-field [width 600] [height 200])
    (super-new)

    (send this set-snipclass map-snip-class)
    (let ((flags (send this get-flags)))
      (unless (member 'handles-events flags)
        (set! flags (cons 'handles-events flags)))
      (unless (member 'handles-all-mouse-events flags)
        (set! flags (cons 'handles-all-mouse-events flags)))
      (send this set-flags flags))

    (define map-impl
      (new map-impl%
           [width width]
           [height height]
           [position position]
           [track track]
           [zoom zoom]
           [request-refresh
            (lambda ()
              (let ([admin (send this get-admin)])
                (when admin
                  (send admin needs-update this 0 0 width height))))]
           [on-zoom-level-change (lambda (zl) (on-zoom-level-change zl))]))

    ;; These methods are required by the snip% interface for this class to
    ;; function inside an editor canvas.  The derived methods are standard
    ;; snip-methods.

    (define/public (copy-map-impl-from other-map-impl)
      (send map-impl copy-from other-map-impl))

    ;; NOTE: the copy method is not strictly needed, but DrRacket will copy
    ;; all snips, so, to allow having a map widget in the DrRacket REPL we
    ;; must provide a copy method.
    (define/override (copy)
      (let ([snip (new this% [width width] [height height])])
        (send snip copy-map-impl-from map-impl)
        snip))
    
    (define/override (resize w h)
      (send map-impl resize w h)
      (set! width w)
      (set! height h)
      (send (send this get-admin) resized this #t))

    (define/private (get-editor)
      (let ((admin (send this get-admin)))
        (when admin
          (send admin get-editor))))

    (define/override (adjust-cursor dc x y editorx editory event)
      (send map-impl adjust-cursor dc x y editorx editory event))

    (define/override (on-event dc x y editorx editory event)
      (define handled? (send map-impl on-event dc x y editorx editory event))
      (unless handled?
        (let ((editor (get-editor)))
          (and editor (send editor on-default-event event))))
      (let ((editor (get-editor)))
        ;; WARNING: dogy!!!
        (and editor (send editor set-caret-owner this))))

    (define/override (on-char dc x y editorx editory event)
      (define handled? (send map-impl on-char dc x y editorx editory event))
      (unless handled?
        (let ((editor (get-editor)))
          (and editor (send editor on-default-char event)))))

    (define/override (draw dc x y . _)
      (send map-impl draw dc x y))

    (define/override (get-extent dc x y [w #f] [h #f] [descent #f] [space #f] [lspace #f] [rspace #f])
      (define-values (width height) (send map-impl get-size))
      (when w (set-box! w width))
      (when h (set-box! h height))
      (when descent (set-box! descent 0.0))
      (when space (set-box! space 0.0))
      (when lspace (set-box! lspace 0.0))
      (when rspace (set-box! rspace 0.0)))

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

    (define/public (resize-to-fit [group #f])
      (send map-impl resize-to-fit group))

    (define/public (move-to position)
      (send map-impl move-to position))

    (define/public (export-image-to-file file-name)
      (send map-impl export-image-to-file file-name))

    (define/public (on-zoom-level-change zl)
      (void))

    ))
