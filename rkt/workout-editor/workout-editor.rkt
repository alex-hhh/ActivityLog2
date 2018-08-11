#lang racket/base
;; workout-editor.rkt -- workout editor panel
;;
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

(require racket/gui/base racket/class pict
         "../widgets/main.rkt"
         "pict-util.rkt"
         "wkstep.rkt"
         "workout-editor-headline.rkt"
         "workout-pasteboard.rkt"
         "wk-estimate.rkt")

(provide workout-editor%)


;;...................................................... workout-editor% ....

;; The workout editor panel, allowing to edit the workout headline, step and
;; description.
(define workout-editor%
  (class object%
    (init-field parent [workout-changed-callback #f])
    (super-new)

    ;; the workout being edited in this editor
    (define current-workout #f)

    (define pane (make-vertical-pane parent))
    (define headline (new workout-editor-headline%
                          [parent pane]
                          [headline-updated-callback (lambda (h s) (on-headline-updated h s))]))

    (define pane1 (make-horizontal-pane pane))

    (define steps-box (make-group-box-panel pane1 "Workout Steps" #f #t))
    ;; Holds the buttons for the workout editor
    (define control-panel
      (new horizontal-panel%
           [parent steps-box] [spacing 10] [border 0]
           [alignment '(left center)]
           [stretchable-height #f]))
    ;; the workout steps editor
    (define wkpb
      (new workout-pasteboard%
           [undo-available-callback (lambda (flag) (on-undo-available flag))]
           [contents-modified-callback (lambda (flag)
                                         (queue-callback
                                          (lambda ()
                                            (on-contents-modified flag))))]))
    (define editor (new editor-canvas%
                        [parent steps-box]
                        [stretchable-width #t]
                        [min-width (+ total-width 50)]
                        [style '(no-hscroll auto-vscroll)]
                        [editor wkpb]))

    (define desc-panel (make-vertical-pane pane1))

    (define add-step-button
      (new button% [parent control-panel] [label "Add Step"]
           [callback (lambda (b e) (on-add-step))]))

    (define add-repeat-button
      (new button% [parent control-panel] [label "Add Repeat"]
           [callback (lambda (b e) (on-add-repeat))]))

    (define undo-button
      (new button% [parent control-panel] [label "Undo"]
           [callback (lambda (b e) (on-undo))]))

    (define spacer (make-spacer control-panel 5 #t))

    (define save-workout-button
      (new button% [parent control-panel] [label "Save"]
           [callback (lambda (b e) (on-workout-steps-updated))]
           [style '(deleted)]))

    (define revert-workout-button
      (new button% [parent control-panel] [label "Revert"]
           [callback (lambda (b e) (on-revert-workout-steps))]
           [style '(deleted)]))

    (define estimate-canvas
      (new canvas% [parent desc-panel]
           [stretchable-width #t]
           [stretchable-height #f]
           [min-height 300]
           [paint-callback (lambda (c dc) (on-estimate-canvas-paint c dc))]))

    (define description-field
      (new notes-input-field% [parent desc-panel]
           [on-save-callback (lambda (text) (on-description-updated text))]))

    (define (on-headline-updated name sport)
      (when current-workout
        (set! current-workout
              (struct-copy workout current-workout
                           [timestamp (current-seconds)]
                           [name name]
                           [sport sport]))
        (when workout-changed-callback
          (workout-changed-callback current-workout))))

    (define (on-workout-steps-updated)
      (when current-workout
        (define steps (send wkpb get-workout-steps))
        (set! current-workout
              (struct-copy workout current-workout
                           [timestamp (current-seconds)]
                           [steps steps]))
        (when workout-changed-callback
          (workout-changed-callback current-workout))
        (send wkpb clear-modified-flag)))

    (define (on-revert-workout-steps)
      (when current-workout
        (send wkpb put-workout-steps (workout-steps current-workout))
        (send wkpb clear-modified-flag)))

    (define (on-description-updated text)
      (when current-workout
        (set! current-workout
              (struct-copy workout current-workout
                           [timestamp (current-seconds)]
                           [description text]))
        (when workout-changed-callback
          (workout-changed-callback current-workout))))

    (define (enable flag)
      (send headline enable flag)
      (send description-field enable flag)
      (send add-step-button enable flag)
      (send add-repeat-button enable flag))

    (define (on-undo-available flag)
      (send undo-button enable flag))

    (define epict #f)

    (define (on-contents-modified flag)
      (send control-panel change-children
            (lambda (old)
              (if flag
                  (list add-step-button add-repeat-button undo-button
                        spacer save-workout-button revert-workout-button)
                  (list add-step-button add-repeat-button undo-button spacer))))
      (define wk (struct-copy workout current-workout
                              [timestamp (current-seconds)]
                              [steps (send wkpb get-workout-steps)]))
      (set! epict (estimate-workout/pict wk))
      (send estimate-canvas refresh))

    (define (on-add-step)
      (send wkpb add-new-step))

    (define (on-add-repeat)
      (send wkpb add-new-repeat))

    (define (on-undo)
      (send wkpb x-undo))

    (define (on-estimate-canvas-paint canvas dc)
      (send dc clear)
      (when epict
        (send dc set-smoothing 'smoothed)
        (let-values (((cw ch) (send canvas get-size)))
          (let ((x (* 0.5 (- cw (pict-width epict))))
                (y (* 0.5 (- ch (pict-height epict)))))
            (draw-pict epict dc x y)))))

    (define/public (set-workout wk)
      (set! current-workout wk)
      (send headline set-workout wk)
      (if current-workout
          (begin
            (send wkpb put-workout-steps (workout-steps current-workout))
            (set! epict (estimate-workout/pict wk)))
          (begin
            (set! epict #f)
            (send wkpb clear-steps)))
      (send estimate-canvas refresh)
      (send description-field set-contents
            (if current-workout
                (or (workout-description wk) "")
                ""))
      (enable (workout? current-workout)))

    (define/public (unsaved-edits?)
      (or (send headline unsaved-edits?)
          (send description-field unsaved-edits?)
          (send wkpb unsaved-edits?)))

    ))
