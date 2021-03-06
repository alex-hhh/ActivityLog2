#lang racket
;; esc-demo.rkt -- demo for the embedded snip control widgets

;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015, 2018, 2019, 2021 Alex Harsányi <AlexHarsanyi@gmail.com>
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


(require racket/gui racket/draw)
(require "../rkt/widgets/esc-controls.rkt")

;; Colors to use for the controls -- most controls allow specifying the colors
;; to use for various functionality, so they look ok in whatever snip they are
;; placed.
(define background (make-object color% #xff #xf8 #xdc 0.75))
(define item-color (make-object color% #x2f #x4f #x4f))
(define button-color (make-object color% #xdc #xdc #xdc))
(define hover-color (make-object color% #xef #xef #xef))
(define pushed-color (make-object color% #xd0 #xd0 #xd0))
(define fill-color (make-object color% #x00 #x64 #x00))

;; The function below implement a basic container based alignment and
;; positioning for the embedded snip controls.  These are not part of the
;; library, as the placement code is not flexible enough.  ActivityLog2 uses
;; these controls with ad-hoc placement code for now.

;; A container holds a list of CONTROLS, which are spaced at SPACING pixels
;; and have BORDER pixels around them.  TYPE can be 'horizontal or 'vertical
;; and specifies how the controls in this container will be placed.  Note that
;; CONTROLS can be a list of an esc-control% instance or other sub-containers.
;; See esc-control-demo-snip% for how containers are used.
(struct container (controls spacing border type))

;; Determine the minimum size required by a container AI.  DC is a device
;; context used for drawing.
(define (ai-min-size ai dc)
  (match-define (container controls border spacing type) ai)
  (define-values (width height)
    (for/fold ([width 0]
               [height 0])
              ([c (in-list controls)])
      (define-values (cw ch) (if (container? c)
                                 (ai-min-size c dc)
                                 (send c size)))
      #;(if (eq? type 'vertical)
          (values (max width cw) (+ height ch))
          (values (+ width cw) (max height ch)))
      (values (max width cw) (max height ch))))
  (define total-spacing (* spacing (sub1 (length controls))))
  (define-values (mw mh)
    (if (equal? type 'vertical)
        (values (+ border width border)
                (+ border (* height (length controls)) total-spacing border))
        (values (+ border (* width (length controls)) total-spacing border)
                (+ border height border))))
  (values mw mh))

;; Place the controls withing the AI container at position X, Y within the
;; SNIP and taking WIDTH / HEIGHT dimensions
(define (ai-place ai x y width height)
  (match-define (container controls border spacing type) ai)
  (define total-spacing (* spacing (sub1 (length controls))))
  (define-values (cw ch)
    (if (equal? type 'vertical)
        (values (- width border border)
                (/ (- height border border total-spacing) (length controls)))
        (values (/ (- width border border total-spacing) (length controls))
                (- height border border))))
  (for ([(c index) (in-indexed (in-list controls))])
    (define-values (cx cy)
      (if (equal? type 'vertical)
          (values (+ x border)
                  (+ y border (* index (+ ch spacing))))
          (values (+ x border (* index (+ cw spacing)))
                  (+ y border))))
    (if (container? c)
        (ai-place c cx cy cw ch)
        (let-values ([(w h) (send c size)])
          (send c position (+ cx #;(/ (- cw w) 2)) (+ cy (/ (- ch h) 2)))))))

;; send the `draw` command to all the controls withing the AI container tree.
(define (ai-draw ai dc x y)
  (match-define (container controls border spacing type) ai)
  (for ([c (in-list controls)])
    (if (container? c)
        (ai-draw c dc x y)
        (send c draw dc x y))))

;; send the `on-event` command to all the controls withing the AI container
;; tree.  The message is sent until one of the controls returns #t, which
;; means that it handled the event.  Returns #t, or #f indicating if the event
;; was handled or not.
(define (ai-on-event ai dc x y editorx editory event)
  (match-define (container controls border spacing type) ai)
  (for/or ([c (in-list controls)])
    (if (container? c)
        (ai-on-event c dc x y editorx editory event)
        (send c on-event dc x y editorx editory event))))

;; send the `on-char` command to all the controls withing the AI container
;; tree.  The message is sent until one of the controls returns #t, which
;; means that it handled the event.  Returns #t, or #f indicating if the event
;; was handled or not.
(define (ai-on-char ai dc x y editorx editory event)
  (match-define (container controls border spacing type) ai)
  (for/or ([c (in-list controls)])
    (if (container? c)
        (ai-on-char c dc x y editorx editory event)
        (send c on-char dc x y editorx editory event))))

(define esc-control-demo-snip-class
  (make-object
   (class snip-class%
     (super-new)
     (send this set-classname "esc-control-demo-snip"))))
(send (get-the-snip-class-list) add esc-control-demo-snip-class)

;; Demo snip illustrating how to use esc-control widgets.
(define esc-control-demo-snip%
  (class snip%
    (init-field [width #f] [height #f])
    (super-new)

    (send this set-snipclass esc-control-demo-snip-class)

    ;; Tell our admin that we wish to handle all events, so we can pass them
    ;; on to our embedded controls
    (let ((flags (send this get-flags)))
      (unless (member 'handles-events flags)
        (set! flags (cons 'handles-events flags)))
      (unless (member 'handles-all-mouse-events flags)
        (set! flags (cons 'handles-all-mouse-events flags)))
      (send this set-flags flags))

    (define/private (get-editor)
      (let ((admin (send this get-admin)))
        (when admin
          (send admin get-editor))))

    (define/private (request-update)
      (let ((admin (send this get-admin)))
        (and admin (send admin needs-update this 0 0 width height))))

    ;; DrRacket will copy the snip before showing it in the GUI, so we need a
    ;; copy method.
    (define/override (copy)
      (new esc-control-demo-snip%))
    
    ;; These are the controls on the snip:

    ;; The "button" demo row contains a button which updates the label for the
    ;; BINFO control to indicate the number of times the button was clicked.
    (define blabel
      (new esc-label% [parent-snip this]
           [label "A button: "]
           [color item-color]))
    (define click-count 0)
    (define binfo
      (new esc-label% [parent-snip this]
           [label "Clicked 0 times"]
           [color item-color]))
    (define button
      (new esc-button% [parent-snip this]
           [label "Click Me"]
           [callback (lambda ()
                       (set! click-count (add1 click-count))
                       (define label (format "Clicked ~a times" click-count))
                       (send binfo set-label label))]
           [text-color item-color]
           [color button-color]
           [hover-color hover-color]
           [pushed-color pushed-color]))

    ;; The "gauge" demo row, the gauge is updated by the slider callback (see
    ;; below)
    (define glabel
      (new esc-label% [parent-snip this]
           [label "A gauge: "]
           [color item-color]))
    (define ginfo
      (new esc-label% [parent-snip this]
           [label "(linked with the slider)"]
           [color item-color]))
    (define gauge
      (new esc-gauge% [parent-snip this]
           [width 70]
           [fill-color fill-color]
           [color item-color]))

    ;; The "slider" demo row, the slider callback updates the SINFO label
    ;; indicating the value of the slider, as well as the gauge defined above
    ;; to represent the inverse value of the slider.
    (define slabel
      (new esc-label% [parent-snip this]
           [label "A slider: "]
           [color item-color]))
    (define sinfo
      (new esc-label% [parent-snip this]
           [align 'left]
           [label "Slider value: 0"]
           [color item-color]))
    (define slider
      (new esc-slider% [parent-snip this]
           [width 70]
           [callback (lambda (v)
                       (define label (format "Slider value: ~a" (~r v #:precision 2)))
                       (send sinfo set-label label)
                       (send gauge value (- 1 v)))]
           [color item-color]
           [hover-color hover-color]
           [pushed-color fill-color]))

    ;; The "checkbox" demo row, the checkbox callback updates the kinfo label
    ;; with info whether it is checked or unchecked.
    (define klabel
      (new esc-label% [parent-snip this]
           [label "A checkbox: "]
           [color item-color]))
    (define kinfo
      (new esc-label% [parent-snip this]
           [label "Unchecked"]
           [color item-color]))
    (define check-box
      (new esc-checkbox% [parent-snip this]
           [label "Click"]
           [initial-value #f]
           [color item-color]
           [fill-color fill-color]
           [callback (lambda (v) (send kinfo set-label (if v "Checked" "Unchecked")))]))

    (send gauge value (- 1 (send slider value))) ; link the slider value with the gauge

    (define border 5)
    (define vspacing 1)
    (define hspacing 1)

    ;; The container tree laying out the controls in the snip
    (define controls
      (container
       (list (container (list blabel button binfo) border hspacing 'horizontal)
             (container (list slabel slider sinfo) border hspacing 'horizontal)
             (container (list glabel gauge ginfo) border hspacing 'horizontal)
             (container (list klabel check-box kinfo) border hspacing 'horizontal))
       border vspacing 'vertical))

    ;; Implement the draw method for the snip interface.  We draw a background
    ;; for the entire snip area, than we draw the controls by calling
    ;; `ai-draw`
    (define/override (draw dc x y . _)
      (define old-smoothing (send dc get-smoothing))
      (send dc set-smoothing 'smoothed)
      (send dc set-brush (send the-brush-list find-or-create-brush background 'solid))
      (send dc set-pen (send the-pen-list find-or-create-pen item-color 0.5 'solid))
      (send dc draw-rectangle x y width height)
      ;; Draw all the controls
      (ai-draw controls dc x y)
      (send dc set-smoothing old-smoothing))

    ;; Handle mouse events sent to this snip.  We pass the events to the
    ;; controls using `ai-on-event` and if they are not handled we pass them
    ;; back to the editor via `on-default-event`
    (define/override (on-event dc x y editorx editory event)
      (unless (ai-on-event controls dc x y editorx editory event)
        ;; Since we didn't handle the event, we send it to the pasteboard%,
        ;; maybe it knows what to do with it.
        (let ((editor (get-editor)))
          (and editor (send editor on-default-event event)))))

    ;; Handle keyboard events sent to this snip.  We pass the events to the
    ;; controls using `ai-on-char` and if they are not handled we pass them
    ;; back to the editor via `on-default-event`
    (define/override (on-char dc x y editorx editory event)
      ;; On char is received when we have focus, so no need to check if we are
      ;; inside the snip -- this event is for us.
      (unless (ai-on-char controls dc x y editorx editory event)
        ;; Since we didn't handle the event, we send it to the pasteboard%,
        ;; maybe it knows what to do with it.
        (let ((editor (get-editor)))
          (and editor (send editor on-default-char event)))))

    ;; Handle snip resizing.  We re-calculate the layout of the controls using
    ;; `ai-place`
    (define/override (resize w h)
      (set! width w)
      (set! height h)
      (ai-place controls 0 0 width height)
      (send (send this get-admin) resized this #t))

    ;; Provide the size of this snip -- also calculates the size for the
    ;; container and places the controls in their initial position, if this is
    ;; the first time it is called.
    (define/override (get-extent dc x y [w #f] [h #f] [descent #f]
                                 [space #f] [lspace #f] [rspace #f])
      (unless (and width height)
        (let-values ([(w h) (ai-min-size controls dc)])
          (set! width w)
          (set! height h))
        (ai-place controls 0 0 width height))
      (when w (set-box! w width))
      (when h (set-box! h height))
      (when descent (set-box! descent 0.0))
      (when space (set-box! space 0.0))
      (when lspace (set-box! lspace 0.0))
      (when rspace (set-box! rspace 0.0)))

    ))

(define (show-demo-frame)
  (define f (new frame% [label "Hello"] [width 600] [height 300]))
  (define pb (new pasteboard%))
  (define canvas (new editor-canvas%
                      [parent f]
                      [style '(no-hscroll no-vscroll)]
                      [horizontal-inset 0]
                      [vertical-inset 0]
                      [editor pb]))
  (send f show #t)
  (define demo (new esc-control-demo-snip%))
  (send pb insert demo 10 10)
  ;;(send pb set-selection-visible #f)
  (send canvas wheel-step #f))

(printf "(1) Run (show-demo-frame) to show the demo snip in a separate frame~%")
(printf "(2) Create the snip inside DrRacket with (new esc-control-demo-snip%)~%")
