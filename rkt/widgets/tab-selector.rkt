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

(require racket/gui/base racket/class racket/match racket/math)
(require "../utilities.rkt")            ; for dbglog, put-pref
(provide tab-selector%)

;; Hold information about a label in the tab-selector%
(struct lbl (text width height ncolor mcolor scolor))

;; Display a set of labels and allow the user to click on one to select it.  A
;; notification is sent when a new label is selected.  The labels can be drawn
;; either horizontal or vertical and there's a small button allowing the user
;; to switch between the two.  The intent is to place this widget in the
;; toplevel frame to allow selecting several "sections" of an application.
;;
;; The widget has a similar purpose as a list-box%, but it is drawn in a
;;fancier way,
(define tab-selector%
  (class object%
    (init-field
     parent                             ; parent widget for this one
     ;; called when the user clicks on an item; it is called with two
     ;; parameters: the current object and the index of the object being
     ;; selected.
     [callback #f]
     ;; called when the layout of the widget changes from vertical to
     ;; horizontal or reverse; it is called with two parameters: the current
     ;; object, and a flag which is #t for vertical, #f for horizontal
     [layout-changed-cb #f]
     ;; Tag used by this object to store its preferences with
     ;; `save-visual-layout'
     [tag 'tab-selector-prefs])
    (super-new)

    ;; Font used to draw the labels
    (define font (send the-font-list find-or-create-font 9 'default 'normal 'normal
                       #f 'smoothed))
    ;; Colors for drawing the labels.  Can be overridden for individual
    ;; labels.
    (define normal-color (make-object color% #xc1 #xcd #xcd)) ; azure3
    (define mouse-over-color (make-object color% #x83 #x8b #x8b)) ; azure4
    (define selected-color (make-object color% #x13 #x90 #xff)) ; dodger blue

    (define pen (send the-pen-list find-or-create-pen normal-color 1 'transparent))
    (define normal-brush (send the-brush-list find-or-create-brush normal-color 'solid))
    (define mouse-over-brush (send the-brush-list find-or-create-brush mouse-over-color 'solid))
    (define selected-brush (send the-brush-list find-or-create-brush selected-color 'solid))

    (define label-spacing 2) ; space between labels (both horizontal and vertical)
    (define text-offset 2)   ; text offset inside the label
    (define vertical? #t)    ; when #t, layout is vertical
    (define labels '())      ; list of LBL structures
    (define lwidth #f)       ; label text width, use (label-width)
    (define lheight #f)      ; label text height, use (label-height)
    (define selected-index #f)  ; index in labels for the selected item
    (define hover-index #f)     ; index in labels for the item under the mouse
    (define expand-label #f)    ; created below
    (define collapse-label #f)  ; created below

    ;; Restore preferences here
    (let ((visual-layout (get-pref tag (lambda () '(gen1 #t)))))
      (when (and visual-layout (list? visual-layout) (> (length visual-layout) 1))
        ;; Visual layout is valid, check version
        (when (eq? (car visual-layout) 'gen1)
          (match-define (list 'gen1 v?) visual-layout)
          (set! vertical? v?))))

    ;; Returns the width of the label text.  Does the right thing if the
    ;; layout is vertical.
    (define (label-width) (if vertical? lheight lwidth))
    ;; Returns the height of the label text.  Does the right thing if the
    ;; layout is vertical.
    (define (label-height) (if vertical? lwidth lheight))
    ;; Returns the width of the label box.  Does the right thing if the layout
    ;; is vertical.
    (define (box-width) (+ text-offset (label-width) text-offset))
    ;; Returns the height of the label box.  Does the right thing if the
    ;; layout is vertical.
    (define (box-height) (+ text-offset (label-height) text-offset))

    ;; Setup the drawing context DC with the right pen and brushes to draw the
    ;; label L (a LBL struct), MODE is the state in which the label should be
    ;; drawn: 'normal 'selected or 'mouse-over
    (define (setup-draw-context-for-label dc l mode)
      (case mode
        ((mouse-over)
         (if (lbl-mcolor l)
             ;; If the label specifies its own color, use it
             (let ((brush (send the-brush-list find-or-create-brush (lbl-mcolor l) 'solid)))
               (send dc set-brush brush))
             (send dc set-brush mouse-over-brush))
         (send dc set-text-foreground "black"))
        ((selected)
         (if (lbl-scolor l)
             ;; If the label specifies its own color, use it
             (let ((brush (send the-brush-list find-or-create-brush (lbl-scolor l) 'solid)))
               (send dc set-brush brush))
             (send dc set-brush selected-brush))
         (send dc set-text-foreground "white"))
        (else
         (if (lbl-ncolor l)
             ;; If the label specifies its own color, use it
             (let ((brush (send the-brush-list find-or-create-brush (lbl-ncolor l) 'solid)))
               (send dc set-brush brush))
             (send dc set-brush normal-brush))
         (send dc set-text-foreground "black"))))

    ;; Return the width and the height of the label box as two values.  Does
    ;; the right thing if LABEL is one of collapse-label and expand-label.
    (define (label-box-dimensions label)
      (if (or (eq? label collapse-label) (eq? label expand-label))
          (values
           (+ text-offset (lbl-width label) text-offset)
           (+ text-offset (lbl-height label) text-offset))
          (values (box-width) (box-height))))

    ;; Return the X Y coordinates where the text of LABEL should be drawn as
    ;; two values.  Does the right thing if LABEL is expand-label or
    ;; collapse-label and takes the layout (vertical or horizontal) into
    ;; consideration.
    (define (label-text-origin label)
      (if (or (eq? label collapse-label) (eq? label expand-label))
          (values
           (+ label-spacing text-offset)
           text-offset
           0)
          (values
           (+ label-spacing text-offset)
           (+ text-offset
              (if vertical?
                  (- (label-height) (* 0.5 (- (label-height) (lbl-width label))))
                  0))
           (if vertical? (/ pi 2) 0))))

    ;; Draw a label onto the device context DC starting at the Y coordinate.
    ;; The label is always drawn at a precomputed X coordinate.
    (define (draw-label dc l y)
      (send dc set-pen pen)
      (let ((text (lbl-text l)))
        (let-values (((w h) (label-box-dimensions l))
                     ((lx ly angle) (label-text-origin l)))
          (send dc draw-rectangle label-spacing y w h)
          (send dc draw-text text lx (+ y ly) #t 0 angle))))

    (define (on-paint canvas dc)
      (send dc clear)
      (send dc set-font font)
      (send dc set-smoothing 'smoothed)

      (if vertical?
          (begin
            (setup-draw-context-for-label
             dc
             expand-label
             (if (eq? hover-index 0) 'mouse-over 'normal))
            (draw-label dc expand-label label-spacing))
          (begin
            (setup-draw-context-for-label
             dc
             collapse-label
             (if (eq? hover-index 0) 'mouse-over 'normal))
            (draw-label dc collapse-label label-spacing)))

      (let-values (((bw bh) (label-box-dimensions (if vertical? expand-label collapse-label))))
        (for/fold ([y (+ label-spacing bh label-spacing)])
                  ([(l idx) (in-indexed labels)])
          (let ((mode (cond
                        ((eq? (add1 idx) selected-index) 'selected)
                        ((eq? (add1 idx) hover-index) 'mouse-over)
                        (#t 'normal))))
            (setup-draw-context-for-label dc l mode)
            (draw-label dc l y))
          (+ y (box-height) label-spacing))))

    (define (on-key canvas event)
      #f)

    ;; Return the index of the label that the mouse is hovering over
    ;; (according to EVENT).  Returns 0 if this is the expand or collapse
    ;; button, 1 for the first label, and #f if the mouse is not hovering over
    ;; any label.
    (define (hover-candidate canvas event)
      (let ((x (send event get-x))
            (y (send event get-y)))
        (let-values (((vx vy) (send canvas get-view-start))
                     ((cw ch) (send canvas get-virtual-size))
                     ((bw bh) (label-box-dimensions (if vertical? expand-label collapse-label))))
          (cond ((< y label-spacing) #f)
                ((< y (+ label-spacing bh))
                 ;; Maybe it is the collapse/expand button
                 (and (> x label-spacing) (< x (+ label-spacing bw))) 0)
                ((< y (+ label-spacing bh (* (length labels) (+ (box-height) label-spacing))))
                 ;; It is one of the buttons
                 (and (> x label-spacing) (< x (+ label-spacing (label-width)))
                      (add1 (exact-truncate (/ (- y (+ label-spacing bh)) (+ (box-height) label-spacing))))))
                (#t
                 #f)))))

    (define (on-mouse canvas event)
      (let ((type (send event get-event-type))
            (candidate (hover-candidate canvas event)))
        (cond ((eq? type 'motion)
               (unless (equal? candidate hover-index)
                 (set! hover-index candidate)
                 (send canvas refresh)))
              ((eq? type 'leave)
               (set! hover-index #f)
               (send canvas refresh))
              ((eq? type 'left-up)
               (cond ((not candidate)
                      ;; Mouse button not released over any button
                      #f)
                     ((eq? candidate 0)
                      (set! vertical? (not vertical?))
                      (adjust-canvas-size canvas)
                      (when layout-changed-cb (layout-changed-cb this vertical?)))
                     ((not (equal? selected-index candidate))
                      (set! selected-index candidate)
                      (when (and callback selected-index (> selected-index 0))
                        ;; NOTE that selected-index of 0 means the
                        ;; expand/collapse button, which we don't want to send
                        ;; out.
                        (callback this (sub1 selected-index)))))
               (send canvas refresh)))))

    (define (on-paint-wrapped canvas dc)
      (with-handlers
        (((lambda (x) #t)
          (lambda (x) (dbglog "al-section-selector%/on-paint-wrapped: ~a" x))))
        (send dc clear)
        (unless (null? labels)
          (on-paint canvas dc))))

    ;; Adjust the minimum canvas size so that the entire set of labels can be
    ;; displayed.  Takes layout into consideration.
    (define (adjust-canvas-size canvas)
      (let-values (((bw bh) (label-box-dimensions (if vertical? expand-label collapse-label))))
        (if (null? labels)
            (begin
              (send canvas min-client-height (exact-truncate (+ label-spacing bh label-spacing)))
              (send canvas min-client-width (exact-truncate (+ label-spacing bw label-spacing))))
            (begin
              (let ((height (+ label-spacing bh label-spacing (* (length labels) (+ (box-height) label-spacing))))
                    (width (+ label-spacing (box-width) label-spacing)))
                (send canvas min-client-height (exact-truncate height))
                (send canvas min-client-width (exact-truncate width)))))))

    (define canvas
      (new
       (class canvas% (init) (super-new)
         (define/override (on-char event) (on-key this event))
         (define/override (on-event event) (on-mouse this event)))
       [parent parent]
       [min-width 1] [min-height 1]
       [stretchable-width #f] [stretchable-height #f]
       [paint-callback on-paint-wrapped]))

    ;; Create the expand and collapse labels
    (let ((dc (send canvas get-dc)))
      (let-values (((w h x y) (send dc get-text-extent "«" font #t)))
        (set! collapse-label (lbl "«" w h "white" #f #f)))
      (let-values (((w h x y) (send dc get-text-extent "»" font #t)))
        (set! expand-label (lbl "»" w h "white" #f #f))))

    ;; Clear the contents of the widget
    (define/public (clear)
      (set! labels '())
      (set! lwidth #f)
      (set! lheight #f)
      (adjust-canvas-size canvas)
      (send canvas refresh))

    ;; Add a new label to the widget.  The normal, mouse-over and selected
    ;; colors can be overridden for this label.
    (define/public (append label-text
                           #:normal-color (normal-color #f)
                           #:mouse-over-color (mouse-over-color #f)
                           #:selected-color (selected-color #f))
      (let ((dc (send canvas get-dc)))
        (let-values (((w h x y) (send dc get-text-extent label-text font #t)))
          (let ((l (lbl label-text w h normal-color mouse-over-color selected-color)))
            (set! labels (reverse (cons l (reverse labels))))
            (set! lwidth (if lwidth (max lwidth w) w))
            (set! lheight (if lheight (max lheight h) h)))))
      (adjust-canvas-size canvas)
      (send canvas refresh))

    ;; Return the index of the currently selected item (0 is the first actual
    ;; label, NOT the collapse button).  Returns #f if no item is selected.
    (define/public (get-selection)
      (and selected-index (sub1 selected-index)))

    ;; Set the selected item to the INDEX label (0 is the first actual label,
    ;; NOT the collapse button).
    (define/public (set-selection index)
      ;; NOTE that index 0 is reserved for the expand/collapse button, our
      ;; internal indexes start at 1.
      (unless (eq? selected-index (add1 index))
        (set! selected-index (add1 index))
        (send canvas refresh)))

    ;; Select or de-select the item at INDEX depending on SELECT? (0 is the
    ;; first actual label, NOT the collapse button).
    (define/public (select index select?)
      ;; NOTE that index 0 is reserved for the expand/collapse button, our
      ;; internal indexes start at 1.
      (if (eq? selected-index (add1 index))
          ;; De-select the current item, if selected.
          (unless select?
            (set! selected-index #f)
            (send canvas refresh))
          (when select?
            (set-selection index))))

    (define/public (save-visual-layout)
      (put-pref tag (list 'gen1 vertical?)))

    ))
