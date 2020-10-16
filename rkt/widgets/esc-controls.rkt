#lang racket/base
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2018, 2019, 2020 Alex Harsányi <AlexHarsanyi@gmail.com>
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
         racket/draw)

;; This file provides some GUI controls which can be embedded inside snip%
;; objects to allow building interactive snips.  The interface is still a work
;; in progress and the snip% object will need to pass events to these controls
;; via `on-event` and `on-char` methods as well as drawing them by calling
;; their `draw` method.

(provide esc-button%
         esc-slider%
         esc-label%
         esc-gauge%
         esc-checkbox%)

(define control-font (send the-font-list find-or-create-font 9 'default 'normal 'bold))

;; Base class for all the controls we provide.  Handles the positioning and
;; size, and the basic event handling.
(define esc-control%
  (class object%
    (init-field parent-snip
                [x 0]
                [y 0]
                [width #f]
                [height #f]
                [enabled? #t])
    (super-new)

    ;; Request our owner snip to update, since this control has changed
    ;; something about the way it looks.
    (define/public (request-update)
      (let ([admin (send parent-snip get-admin)])
        (when admin
          ;; NOTE: technically, we could request to update just the area
          ;; covered by this control, but this interacts badly with the map
          ;; widget where these controls are used, and I don't have the time
          ;; to hunt the problem down, so I'm requesting the update of the
          ;; entire snip here, or at least a large area of it.
          (send admin needs-update parent-snip 0 0 1000 1000))))

    ;; Get and set the position of this control relative to the parent snip.
    (define position
      (case-lambda
        (() (values x y))
        ((new-x new-y)
         (set! x new-x)
         (set! y new-y)
         (request-update))))

    ;; Get and set the size of this control.
    (define size
      (case-lambda
        (()
         (unless (and width height)
           (let ([admin (send parent-snip get-admin)])
             (when admin
               (let-values ([(w h) (send this min-size (send admin get-dc))])
                 (unless width (set! width w))
                 (unless height (set! height h))))))
         (values width height))
        ((new-width new-height)
         (set! width new-width)
         (set! height new-height)
         (request-update))))

    ;; Get and set whether this control is enabled -- a disabled control will
    ;; not process any events (e.g. you cannot click on disabled buttons) and
    ;; will also be drawn in a distinctive way to indicate that it is
    ;; disabled.
    (define enabled
      (case-lambda
        (() enabled?)
        ((flag)
         (set! enabled? flag)
         (request-update))))

    ;; Process a mouse event from the parent snip.  The parent snip needs to
    ;; call this method from its own `on-event` method and with the same
    ;; parameters.  If this method returns #t, it means the event was handled
    ;; by this control and should not pass it to other controls.  The parent
    ;; snip should call `on-event` in order for all its controls and stop for
    ;; the first one that returns #t.
    (define/public (on-event dc snip-x snip-y editorx editory event)
      (when enabled?
        (let ((cx (- (send event get-x) snip-x x))
              (cy (- (send event get-y) snip-y y)))
          (define inside? (and (<= 0 cx width) (<= 0 cy height)))
          (on-mouse-event dc cx cy inside? event))))

    ;; Process a keyboard event from the parent snip.  Same notes apply as for
    ;; `on-event`
    (define/public (on-char dc snip-x snip-y editorx editory event)
      (when enabled?
        (let ((cx (- (send event get-x) snip-x))
              (cy (- (send event get-y) snip-y)))
          (define inside? (and (<= 0 cx width) (<= 0 cy height)))
          (on-keyboard-event dc cx cy inside? event))))

    ;; This method can be overridden by children of this class to implement
    ;; mouse event handling.  The CONTROLX, CONTROLY parameters are the
    ;; coordinates where the event happened relative to the position of this
    ;; control.  INSIDE? indicates that the event happened inside this control
    ;; (as defined by its position and size), while EVENT is the mouse event
    ;; itself.
    (define/public (on-mouse-event dx controlx controly inside? event)
      #f)

    ;; This method can be overridden by children of this class to implement
    ;; mouse event handling.  Same notes apply as for `on-mouse-event` except
    ;; that EVENT is a keyboard event.
    (define/public (on-keyboard-event dx controlx controly inside? event)
      #f)

    (public position size enabled)

    ;; Children snips must implement a DRAW and a MIN-SIZE method.
    (abstract draw min-size)

    ))


;; A label control, displays a label and has no mouse interaction.  The label
;; can be a string or a bitmap%
(define esc-label%
  (class esc-control%
    (init-field [label "esc-label%"]
                [align 'left]           ; left right center
                [font control-font]
                [color "DarkSlateGray"]
                [disabled-color "Gray"])
    (super-new)
    (inherit position size enabled request-update)

    (define border 1)

    ;; Determine the minimum size of this widget, required to accommodate its
    ;; contents.  Note that the actual size of the widget might be different
    ;; and smaller than what this method returns.
    (define/override (min-size dc)
      (cond ((is-a? label bitmap%)
             (values (+ (send label get-width) border border)
                     (+ (send label get-height) border border)))
            ((string? label)
             (let-values ([(w h b v) (send dc get-text-extent label font #t)])
               ;; Add a border
               (values (+ w border border) (+ h border border))))
            (#t
             (values 30 30))))

    ;; Set the label to a new value
    (define/public (set-label n)
      (set! label n)
      (request-update))

    ;; Draw this control.  SNIP-X and SNIP-Y represent the position of the
    ;; snip inside the snip canvas.  The control should fetch its position and
    ;; size inside the snip with the `position` and `size` methods.
    (define/override (draw dc snip-x snip-y)
      (let-values ([(x y) (position)]
                   [(width height) (size)])
        (let ([old-pen (send dc get-pen)]
              [old-brush (send dc get-brush)]
              [old-font (send dc get-font)]
              [old-text-color (send dc get-text-foreground)])

          (cond ((is-a? label bitmap%)
                 (let ((x1 (/ (- width (send label get-width)) 2))
                       (y1 (/ (- height (send label get-height)) 2)))
                   (send dc draw-bitmap label (+ snip-x x x1) (+ snip-y y y1))))
                ((string? label)
                 (send dc set-text-foreground (if (enabled) color disabled-color))
                 (send dc set-font font)
                 (let-values ([(w h b v) (send dc get-text-extent label font #f)])
                   (let ((x1 (+ snip-x x
                                (case align
                                  ((left) border)
                                  ((right) (- width border w))
                                  (else (/ (- width w) 2)))))
                         (y1 (+ snip-y y (/ (- height h) 2))))
                     (send dc draw-text label x1 y1)))))

          ;; Restore old drawing parameters
          (send dc set-pen old-pen)
          (send dc set-brush old-brush)
          (send dc set-text-foreground old-text-color)
          (send dc set-font old-font))))

    ))


;; A button with a label and a callback which is invoked when the button is
;; pressed.  The LABEL can be a string, a bitmap% or one of the symbols 'menu,
;; 'cross, 'plus or 'minus -- in which case the corresponding glyph is drawn.
(define esc-button%
  (class esc-control%
    (init-field [callback (lambda () (void))] ; callback to be invoked on click
                [label 'cross]
                [font control-font]
                ;; Color to use when the mouse hovers over the button
                [hover-color "DarkGoldenrod"]
                ;; Color to use when the button is pushed in.
                [pushed-color "Goldenrod"]
                [text-color "DarkSlateGray"]
                [disabled-text-color "Gray"]
                ;; NOTE: color and disabled-color determine the button
                ;; background and they can be #f, in which case there is no
                ;; button background and the button will be highlighted only
                ;; when the mouse hovers over it.
                [color "Goldenrod"]
                [disabled-color "light gray"])
    (super-new)
    (inherit position size enabled request-update)

    (define hover? #f)                ; is the mouse hovering over the button?
    (define pushed? #f)               ; is the button being pushed?
    (define border 5)

    (define outline-pen
      (send the-pen-list find-or-create-pen "black" 0.5 'transparent))

    (define hover-brush
      (send the-brush-list find-or-create-brush hover-color 'solid))
    (define pushed-brush
      (send the-brush-list find-or-create-brush pushed-color 'solid))
    (define enabled-brush
      (if color
          (send the-brush-list find-or-create-brush color 'solid)
          (send the-brush-list find-or-create-brush "black" 'transparent)))
    (define disabled-brush
      (if disabled-color
          (send the-brush-list find-or-create-brush disabled-color 'solid)
          (send the-brush-list find-or-create-brush "black" 'transparent)))

    (define/override (min-size dc)
      (cond ((is-a? label bitmap%)
             (values (+ (send label get-width) border border)
                     (+ (send label get-height) border border)))
            ((string? label)
             (let-values ([(w h b v) (send dc get-text-extent label font #t)])
               ;; Add a border
               (values (+ w border border) (+ h border border))))
            (#t
             (values 30 30))))

    (define/override (draw dc snip-x snip-y)
      (let-values ([(x y) (position)]
                   [(width height) (size)])
        (let ([old-pen (send dc get-pen)]
              [old-brush (send dc get-brush)]
              [old-font (send dc get-font)]
              [old-text-color (send dc get-text-foreground)])

          ;; Draw the background and outline
          (send dc set-pen outline-pen)
          (send dc set-brush (cond (pushed? pushed-brush)
                                   (hover? hover-brush)
                                   ((enabled) enabled-brush)
                                   (#t disabled-brush)))
          (send dc draw-rounded-rectangle (+ snip-x x) (+ snip-y y) width height)

          (cond ((is-a? label bitmap%)
                 (let ((x1 (/ (- width (send label get-width)) 2))
                       (y1 (/ (- height (send label get-height)) 2)))
                   (send dc draw-bitmap label (+ snip-x x x1) (+ snip-y y y1))))
                ((string? label)
                 (send dc set-text-foreground (if (enabled) text-color disabled-text-color))
                 (send dc set-font font)
                 (let-values ([(w h b v) (send dc get-text-extent label font #f)])
                   (let ((x1 (+ snip-x x (/ (- width w) 2)))
                         (y1 (+ snip-y y (/ (- height h) 2))))
                     (send dc draw-text label x1 y1))))
                ((symbol? label)
                 (define size (min width height))
                 (send dc set-pen
                       (send the-pen-list find-or-create-pen
                             (if (enabled) text-color disabled-text-color) 2.0 'solid))
                 (case label
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

          ;; Restore old drawing parameters
          (send dc set-pen old-pen)
          (send dc set-brush old-brush)
          (send dc set-text-foreground old-text-color)
          (send dc set-font old-font))))

    (define/override (on-mouse-event dc control-x control-y inside? event)
      (define pushed-button? (and (send event button-down? 'left)
                                  inside?))
      (define clicked? (and (send event button-up? 'left)
                            inside? pushed?))
      (when clicked? (callback))
      ;; Request redraw for the parent snip if the state of the button
      ;; has changed.
      (unless (and (eq? hover? inside?)
                   (eq? pushed? pushed-button?))
        (set! hover? inside?)
        (set! pushed? pushed-button?)
        (request-update))
      (or pushed-button? clicked?))

    ))

;; A slider control with a value ranging between 0 and 1
(define esc-slider%
  (class esc-control%
    (init-field [callback (lambda (v) (void))]  ; callback to be invoked on click or drag
                [slider-height 7]
                [border 1]
                ;; Color to use for the knob when the mouse hovers over it
                [hover-color "DarkGoldenrod"]
                ;; Color to use when the knob is pushed and dragged
                [pushed-color "Goldenrod"]
                ;; Color for the control outline
                [color "DarkSlateGray"]
                [disabled-color "Gray"])

    (super-new)
    (inherit position size enabled request-update)

    (define the-value 0)

    (define hover? #f)                  ; is the mouse hovering over the control?
    (define knob-hover? #f)             ; the mouse is hovering over the knob
    (define knob-pushed? #f)
    (define knob-radius (* 1.0 slider-height))

    (define enabled-pen
      (send the-pen-list find-or-create-pen color 2.0 'solid))
    (define enabled-brush
      (send the-brush-list find-or-create-brush color 'solid))
    (define transparent-brush
      (send the-brush-list find-or-create-brush "black" 'transparent))
    (define disabled-pen
      (send the-pen-list find-or-create-pen disabled-color 2.0 'solid))
    (define hover-brush
      (send the-brush-list find-or-create-brush hover-color 'solid))
    (define pushed-brush
      (send the-brush-list find-or-create-brush pushed-color 'solid))
    (define disabled-brush
      (send the-brush-list find-or-create-brush disabled-color 'solid))

    (public value)
    (define value
      (case-lambda
        (() the-value)
        ((n) (set! the-value (max 0.0 (min 1.0 n)))
             (callback the-value)
             (request-update))))

    (define/override (min-size dc)
      (values 30 30))

    (define/override (draw dc snip-x snip-y)
      (let-values ([(x y) (position)]
                   [(width height) (size)])
        (let ((old-pen (send dc get-pen))
              (old-brush (send dc get-brush)))
          (define slider-width (- width border border))
          (define knob-x (+ snip-x x border (* (value) slider-width)))
          (send dc set-pen (if (enabled) enabled-pen disabled-pen))
          (send dc set-brush transparent-brush)
          (send dc draw-rounded-rectangle
                (+ snip-x x border) (+ snip-y y (/ (- height slider-height) 2))
                (- width border border) slider-height)
          (send dc set-brush (if knob-pushed? pushed-brush
                                 (if knob-hover? hover-brush
                                     (if (enabled) enabled-brush
                                         disabled-brush))))
          (send dc draw-ellipse (- knob-x knob-radius) (- (+ snip-y y (/ height 2)) knob-radius)
                (* 2 knob-radius) (* 2 knob-radius))
          (send dc set-pen old-pen)
          (send dc set-brush old-brush))))

    (define/override (on-mouse-event dc control-x control-y inside? event)
      (let-values ([(width height) (size)])
        (define slider-width (- width border border))
        (let ([kx (+ border (* (value) slider-width))]
              [ky (+ (/ height 2))])
          (define inside-knob?
            ;; NOTE: we assume a square knob!
            (and (<= (abs (- control-x kx)) knob-radius)
                 (<= (abs (- control-y ky)) knob-radius)))
          (define event-type (send event get-event-type))
          (define handled? #f)
          (define pushed?
            (or (and inside-knob? (eq? event-type 'left-down))
                ;; NOTE: get-left-down indicates that the left button is down,
                ;; but it was not pushed down this event!
                (and knob-pushed? (send event get-left-down))))

          (cond ((and pushed? (send event dragging?))
                 (value (/ (- control-x border) slider-width))
                 (set! handled? #t))
                ((and (eq? event-type 'left-down)
                      (<= (abs (- control-y ky)) slider-height))
                 (value (/ (- control-x border) slider-width))
                 (set! handled? #t))
                ((and (eq? event-type 'left-up) inside?)
                 (set! handled? #t)))

          ;; Request redraw for the parent snip if the state of the button has
          ;; changed.
          (unless (and (eq? hover? inside?)
                       (eq? knob-hover? inside-knob?)
                       (eq? knob-pushed? pushed?))
            (set! hover? inside?)
            (set! knob-hover? inside-knob?)
            (set! knob-pushed? pushed?)
            (request-update))

          handled?)))

    ))


;; A gauge displaying a value between 0 and 1 in a progress bar
(define esc-gauge%
  (class esc-control%
    (init-field [gauge-height 7]
                [border 1]
                ;; Color to use when the knob is pushed and dragged
                [fill-color "Goldenrod"]
                ;; Color for the control outline
                [color "DarkSlateGray"]
                [disabled-color "Gray"])
    (super-new)
    (inherit position size enabled request-update)

    (define the-value 0)

    (define transparent-pen
      (send the-pen-list find-or-create-pen "black" 0 'transparent))
    (define enabled-pen
      (send the-pen-list find-or-create-pen color 2.0 'solid))
    (define enabled-brush
      (send the-brush-list find-or-create-brush fill-color 'solid))
    (define transparent-brush
      (send the-brush-list find-or-create-brush "black" 'transparent))
    (define disabled-pen
      (send the-pen-list find-or-create-pen disabled-color 2.0 'solid))
    (define disabled-brush
      (send the-brush-list find-or-create-brush disabled-color 'solid))

    (public value)
    (define value
      (case-lambda
        (() the-value)
        ((n) (set! the-value (max 0.0 (min 1.0 n)))
             (request-update))))

    (define/override (min-size dc)
      (values 30 (+ border gauge-height border)))

    (define/override (draw dc snip-x snip-y)
      (let-values ([(x y) (position)]
                   [(width height) (size)])
        (let ((old-pen (send dc get-pen))
              (old-brush (send dc get-brush)))
          (define gauge-width (- width border border))
          (send dc set-pen transparent-pen)
          (send dc set-brush (if (enabled) enabled-brush disabled-brush))
          (send dc draw-rounded-rectangle
                (+ snip-x x border) (+ snip-y y (/ (- height gauge-height) 2))
                (ceiling (* (value) gauge-width)) gauge-height)
          (send dc set-pen (if (enabled) enabled-pen disabled-pen))
          (send dc set-brush transparent-brush)
          (send dc draw-rounded-rectangle
                (+ snip-x x border) (+ snip-y y (/ (- height gauge-height) 2))
                (- width border border) gauge-height)
          (send dc set-pen old-pen)
          (send dc set-brush old-brush))))

    ))

;; A checkbox control which a box which can be ticked and a label which can be
;; a string or a bitmap%
(define esc-checkbox%
  (class esc-control%
    (init-field [callback (lambda (x) (void))] ; callback to be invoked on click
                [label "esc-checkbox%"]
                [initial-value #f]
                [fill-color "Goldenrod"]
                ;; Color for the control outline
                [color "DarkSlateGray"]
                [disabled-color "Gray"]
                [font control-font])
    (super-new)
    (inherit position size enabled request-update)

    (define the-value initial-value)

    (define pushed? #f)               ; is the button being pushed?
    (define border 1)
    (define box-size #f)                ; initialized later
    (define box-spacing 5)

    (define transparent-pen
      (send the-pen-list find-or-create-pen "black" 0 'transparent))
    (define enabled-pen
      (send the-pen-list find-or-create-pen color 2.0 'solid))
    (define enabled-brush
      (send the-brush-list find-or-create-brush fill-color 'solid))
    (define transparent-brush
      (send the-brush-list find-or-create-brush "black" 'transparent))
    (define disabled-pen
      (send the-pen-list find-or-create-pen disabled-color 2.0 'solid))
    (define disabled-brush
      (send the-brush-list find-or-create-brush disabled-color 'solid))
    (define enabled-text-color color)
    (define disabled-text-color disabled-color)

    (public value)
    (define value
      (case-lambda
        (() the-value)
        ((n) (set! the-value (and n #t))
             (request-update))))

    (define/override (min-size dc)
      (unless box-size
        (set! box-size
              (let-values ([(w h b v) (send dc get-text-extent "X" font #t)])
                (max w h))))
      
      (define-values (w h)
        (cond ((is-a? label bitmap%)
               (values (+ (send label get-width) box-size box-spacing border border)
                       (+ (send label get-height) border border)))
              ((string? label)
               (let-values ([(w h b v) (send dc get-text-extent label font #t)])
                 ;; Add a border
                 (values (+ w box-size box-spacing border border) (+ h border border))))
              (#t
               (values (+ box-size box-size border border) (+ box-size border border)))))

      (values (+ border w box-size border) (+ border h border)))

    (define/override (draw dc snip-x snip-y)

      (unless box-size
        (set! box-size
              (let-values ([(w h b v) (send dc get-text-extent "X" font #t)])
                (max w h))))

      (let-values ([(x y) (position)]
                   [(width height) (size)])
        (let ([old-pen (send dc get-pen)]
              [old-brush (send dc get-brush)]
              [old-font (send dc get-font)]
              [old-text-color (send dc get-text-foreground)])

          ;; Draw the check box
          (send dc set-pen (if (enabled) enabled-pen disabled-pen))
          (send dc set-brush (if (enabled)
                                 (if (value) enabled-brush transparent-brush)
                                 (if (value) disabled-brush transparent-brush)))
          (let ([cx (+ snip-x x border)]
                [cy (+ snip-y y (/ (- height box-size) 2))])
            (send dc draw-rounded-rectangle cx cy box-size box-size))

          (send dc set-text-foreground (if (enabled) enabled-text-color disabled-text-color))
          (send dc set-font font)
          (let-values ([(w h b v) (send dc get-text-extent label font #f)])
            (let ((x1 (+ snip-x x border box-size box-spacing))
                  (y1 (+ snip-y y (/ (- height h) 2))))
              (send dc draw-text label x1 y1)))

          ;; Restore old drawing parameters
          (send dc set-pen old-pen)
          (send dc set-brush old-brush)
          (send dc set-text-foreground old-text-color)
          (send dc set-font old-font))))

    (define/override (on-mouse-event dc control-x control-y inside? event)
      (define pushed-button? (and (send event button-down? 'left) inside?))
      (define clicked? (and (send event button-up? 'left) inside? pushed?))
      (when clicked?
        (value (not (value)))
        (callback (value)))
      (set! pushed? pushed-button?)
      (or pushed-button? clicked?))

    ))
