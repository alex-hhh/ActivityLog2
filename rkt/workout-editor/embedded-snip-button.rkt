#lang racket/base
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2018 Alex Harsanyi <AlexHarsanyi@gmail.com>
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

(require racket/class racket/draw)

(provide embedded-snip-button%)

;; Resources for drawing the button -- these are hardcoded for now.

(define foreground (make-object color% #x2f #x4f #x4f)) ; dark slate gray
(define foreground2 (make-object color% #xff #xfa #xcd)) ; lemon chiffon
(define pressed-color (make-object color% #xee #xe8 #xaa)) ; pale goldenrod
(define color (make-object color% #xda #xa5 #x20)) ; golden rod
(define pen (send the-pen-list find-or-create-pen foreground 2.0 'solid 'butt))
(define pen2 (send the-pen-list find-or-create-pen foreground2 2.0 'solid 'butt))
(define brush (send the-brush-list find-or-create-brush color 'solid))
(define pressed-brush (send the-brush-list find-or-create-brush pressed-color 'solid))
(define transparent-pen (send the-pen-list find-or-create-pen foreground 1 'transparent))

;; A button that can be placed inside a snip% object.
;;
;; Note that for this to work, the snip has to receive mouse events and has to
;; forward them to the ON-EVENT method in this class.  The snip% is also
;; responsible for calling the DRAW method as appropriate.
(define embedded-snip-button%
  (class object%
    (init-field parent-snip             ; snip% that owns this button
                [callback #f]           ; callback to be invoked on click
                [x 0]                   ; x position (snip relative)
                [y 0]                   ; y position (snip relative)
                [size 30]               ; the button size (a square)
                ;; When #t, the snip background is dark and the button will
                ;; use a light color for drawing the glyph
                [dark-background? #f]
                ;; Glyph displayed on the button.  Can be a bitmap% or a
                ;; symbol, one of 'cross. 'plus, 'minus or 'menu
                [glyph 'cross])
    (super-new)

    (define hover? #f)                  ; is the mouse hovering over the button?
    (define pushed? #f)                 ; is the button being pushed?

    (define (request-update)
      (let ((admin (send parent-snip get-admin)))
        (when admin
          (send admin needs-update parent-snip x y size size))))

    ;; Set the position of the button to NEW-X, NEW-Y (relative to the parent
    ;; snip top-left corner)
    (define/public (set-position new-x new-y)
      (set! x new-x)
      (set! y new-y))

    ;; Draw the button.  SNIP-X and SNIP-Y are the snip coordinates inside the
    ;; DC%.  This method needs to be invoked by the parent snip's own DRAW
    ;; method.
    (define/public (draw dc snip-x snip-y)
      (when (or hover? pushed?)
        (send dc set-pen transparent-pen)
        (send dc set-brush (if pushed? pressed-brush brush))
        (send dc draw-rectangle (+ snip-x x) (+ snip-y y) size size))

      (if (or hover? pushed? (not dark-background?))
          (send dc set-pen pen)
          (send dc set-pen pen2))

      (if (is-a? glyph bitmap%)
          (let ((x1 (/ (- size (send glyph get-width)) 2))
                (y1 (/ (- size (send glyph get-height)) 2)))
            (send dc draw-bitmap glyph (+ snip-x x x1) (+ snip-y y y1)))
          (case glyph
            ((cross)
             (let ((x1 (* 1/3 size))
                   (y1 (* 1/3 size))
                   (x2 (- size (* 1/3 size)))
                   (y2 (- size (* 1/3 size))))
               (send dc draw-line
                     (+ snip-x x x1) (+ snip-y y y1)
                     (+ snip-x x x2) (+ snip-y y y2))
               (send dc draw-line
                     (+ snip-x x x1) (+ snip-y y y2)
                     (+ snip-x x x2) (+ snip-y y y1))))
            ((plus)
             (let ((x1 (* 1/3 size))
                   (y1 (* 1/3 size))
                   (x2 (- size (* 1/3 size)))
                   (y2 (- size (* 1/3 size)))
                   (mid (/ size 2)))
               (send dc draw-line
                     (+ snip-x x x1) (+ snip-y y mid)
                     (+ snip-x x x2) (+ snip-y y mid))
               (send dc draw-line
                     (+ snip-x x mid) (+ snip-y y y2)
                     (+ snip-x x mid) (+ snip-y y y1))))
            ((minus)
             (let ((x1 (* 1/3 size))
                   (y1 (* 1/3 size))
                   (x2 (- size (* 1/3 size)))
                   (y2 (- size (* 1/3 size)))
                   (mid (/ size 2)))
               (send dc draw-line
                     (+ snip-x x x1) (+ snip-y y mid)
                     (+ snip-x x x2) (+ snip-y y mid))))
            ((menu)
             (let ((x1 (* 1/3 size))
                   (y1 (* 1/3 size))
                   (x2 (- size (* 1/3 size)))
                   (y2 (- size (* 1/3 size)))
                   (mid (/ size 2)))
               (send dc draw-line
                     (+ snip-x x x1) (+ snip-y y y1)
                     (+ snip-x x x2) (+ snip-y y y1))
               (send dc draw-line
                     (+ snip-x x x1) (+ snip-y y mid)
                     (+ snip-x x x2) (+ snip-y y mid))
               (send dc draw-line
                     (+ snip-x x x1) (+ snip-y y y2)
                     (+ snip-x x x2) (+ snip-y y y2))))
            )))

    ;; Handle a mouse event received by the SNIP.  Arguments are the same as
    ;; the snip% ON-EVENT method.  This method needs to be invoked by the
    ;; parent snip's own DRAW method.
    ;;
    ;; Returns #t if the event has been clicked and the callback was invoked,
    ;; #f otherwise
    (define/public (on-event dc snip-x snip-y editorx editory event)
      (let ((sx (- (send event get-x) snip-x))
            (sy (- (send event get-y) snip-y)))
        (define inside-button?
          (and (<= x sx (+ x size)) (<= y sy (+ y size))))
        (define pushed-button?
          (and inside-button? (eq? (send event get-event-type) 'left-down)))

        (define clicked?
          (and (eq? (send event get-event-type) 'left-up)
               callback inside-button? pushed?))

        (when clicked?
          (callback))

        ;; Request redraw for the parent snip if the state of the button has
        ;; changed.
        (unless (and (eq? hover? inside-button?)
                     (eq? pushed? pushed-button?))
          (set! hover? inside-button?)
          (set! pushed? pushed-button?)
          (request-update))

        clicked?))
    ))
