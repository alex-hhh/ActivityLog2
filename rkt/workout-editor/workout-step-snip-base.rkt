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

(require racket/gui/base
         racket/class
         pict
         "embedded-snip-button.rkt")

(provide define-snip-class
         workout-step-snip-base%
         button-size)

;; Helper function for defining snip% classes -- saves a few lines of code.
(define (define-snip-class name)
  (define object
    (make-object
     (class snip-class%
       (super-new)
       (send this set-classname name))))
  (send (get-the-snip-class-list) add object)
  object)

;; Space between buttons on the snip and between the right most button and the
;; snip right edge.
(define button-spacing 10)
;; Size of our buttons (they are square)
(define button-size 30)

;; Base class for our workout steps.  Encapsulates most of the snip% work,
;; leaving only the drawing for the derived classes.
(define workout-step-snip-base%
  (class snip%
    (init snip-class)
    (init-field pict               ; the pict to be shown as the snip contents
                [dark-background? #f]
                [show-close-button? #t])
    (super-new)

    (send this set-snipclass snip-class)
    (send this set-count 1)

    (let ((flags (send this get-flags)))
      (unless (memq 'handles-events flags)
        (set! flags (cons 'handles-events flags)))
      (unless (memq 'handles-all-mouse-events flags)
        (set! flags (cons 'handles-all-mouse-events flags)))
      (unless (memq 'handles-between-events flags)
        (set! flags (cons 'handles-between-events flags)))
      (send this set-flags flags))

    ;; nesting level inside repeats, snips are drawn indented if they are
    ;; inside repeats.  This value only affects the visuals of the snip.
    (define nlevel 0)

    (define hover-on-snip? #f)          ; is the mouse hovering our snip?
    (define width (pict-width pict))
    (define height (pict-height pict))

    ;; A list of buttons for this snip.  They are placed on the right of the snip.
    (define buttons '())

    ;; Add a BUTTON to this snip.  The position of the button will be updated
    ;; to be placed next to any other buttons already present.
    (define/public (add-additional-button button)
      (define by (* 0.5 (- height button-size)))
      (define bx (- width
                    button-spacing button-size
                    (* (length buttons) (+ button-spacing button-size))))
      (set! buttons (cons button buttons))
      (send button set-position bx by))
    
    (when show-close-button?
      ;; A close button (shown only when show-close-button? is #t) allows
      ;; deleting this snip from the pasteboard.
      (let ((close-button (new embedded-snip-button%
                               [parent-snip this]
                               [callback (lambda () (request-delete))]
                               [size button-size]
                               [dark-background? dark-background?])))
        (add-additional-button close-button)))

    ;; Construct a new pict to be displayed as the contents of this snip.
    ;; This needs to be overridden in derived classes.
    (define/public (make-pict nesting-level)
      (error "make-pict not implemented"))

    (define/override (get-extent dc x y w h descent space lspace rspace)
      (when w (set-box! w width))
      (when h (set-box! h height))
      (when descent (set-box! descent 0.0))
      (when space (set-box! space 0.0))
      (when lspace (set-box! lspace 0.0))
      (when rspace (set-box! rspace 0.0)))

    (define/override (draw dc x y . other)
      (draw-pict pict dc x y)
      ;; only draw the buttons if the mouse is hovering over the snip.
      (when hover-on-snip?
        (for ([b (in-list buttons)])
          (send b draw dc x y))))

    (define (get-editor)
      (let ((admin (send this get-admin)))
        (when admin
          (send admin get-editor))))

    (define (handle-event dc x y editorx editory event)
      (define inside-snip?
        (let ((sx (- (send event get-x) x))
              (sy (- (send event get-y) y)))
          (and (<= 0 sx width) (<= 0 sy height))))
      (define result
        (and inside-snip?
             (for/or ([b (in-list buttons)])
               (send b on-event dc x y editorx editory event))))
      (unless (eq? inside-snip? hover-on-snip?)
        (set! hover-on-snip? inside-snip?)
        (request-update))
      result)

    (define/override (on-event dc x y editorx editory event)
      (define result (handle-event dc x y editorx editory event))
      (unless result
        ;; Since we didn't handle the event, we send it to the pasteboard%,
        ;; maybe it knows what to do with it.
        (let ((editor (get-editor)))
          (and editor (send editor on-default-event event)))))

    (define/override (on-char dc x y editorx editory event)
      ;; Since the snip is set up to receive events, the pasteboard will not
      ;; see them.  Since we don't handle char events, they are forwarded to
      ;; the pasteboard%
      (let ((editor (get-editor)))
        (and editor (send editor on-default-char event))))

    (define/override (on-goodbye-event dc x y editorx editory event)
      (handle-event dc x y editorx editory event))

    (define/public (nesting-level)
      nlevel)

    (define/public (set-nesting-level level)
      (unless (eqv? nlevel level)
        (set! nlevel (max 0 level))
        (refresh-pict)))

    (define/public (get-width) width)
    (define/public (get-height) height)

    (define/public (refresh-pict)
      (set! pict (make-pict nlevel))
      (set! width (pict-width pict))
      (set! height (pict-height pict))
      (request-update))

    (define/public (request-update)
      (let ((admin (send this get-admin)))
        (and admin (send admin needs-update this 0 0 width height))))

    (define (request-delete)
      (let ((editor (get-editor)))
        (when editor
          ;; Queue an event -- the editor might be locked!
          (queue-callback
           (lambda () (send editor delete this))))))

    ))
