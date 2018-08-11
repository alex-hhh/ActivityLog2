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
         "../sport-charms.rkt"
         "../widgets/main.rkt"
         "wkstep.rkt")

(provide workout-editor-headline%)

(define *title-font* (send the-font-list find-or-create-font 18 'default 'normal 'normal))
(define *label-font* (send the-font-list find-or-create-font 10 'default 'normal 'normal))
(define *data-font* (send the-font-list find-or-create-font 10 'default 'normal 'bold))

;; Widget to display or edit the workout title and sport
(define workout-editor-headline%
  (class object%
    (init-field parent [headline-updated-callback #f])
    (super-new)

    (define headline "")
    (define sport 1)

    (define panel0 (new horizontal-panel%
			[parent parent]
			[border 0]
			[spacing 5]
			[stretchable-height #f]
			[alignment '(left center)]))

    (define begining-spacer (make-spacer panel0))

    (define sport-icon (new message% [parent panel0]
                            [label (get-sport-bitmap-colorized 1 #f)]
                            [stretchable-width #f]
                            [stretchable-height #f]))

    (define panel (new vertical-panel%
                       [parent panel0]
                       [border 5]
                       [spacing 1]
                       [stretchable-height #f]
                       [alignment '(left top)]))

    (define workout-title
      (new message% [parent panel]
           [label "Untitled"]
           [stretchable-width #t]
           [font *title-font*]))

    (define workout-title-edit
      (new text-field% [parent panel]
           [label ""]
           [style '(single deleted)]
           [stretchable-width #t]
           [font *title-font*]))

    (define sport-panel
      (new horizontal-panel%
           [parent panel]
           [spacing 1]
           [stretchable-height #f]
           [alignment '(left center)]))

    (new message% [parent sport-panel]
         [stretchable-width #f]
         [label "Activity type:"]
         [font *label-font*])

    (define sport-name
      (new message% [parent sport-panel]
           [stretchable-width #t]
           [label (get-sport-name 1 #f)]
           [font *data-font*]))

    (define sport-panel-edit
      (new horizontal-panel%
           [parent panel]
           [spacing 1]
           [style '(deleted)]
           [stretchable-height #f]
           [alignment '(left center)]))

    (new message% [parent sport-panel-edit]
         [stretchable-width #f]
         [label "Activity type:"]
         [font *label-font*])

    (define (on-sport-selected control event)
      (define index (send control get-selection))
      (define sport (add1 index))       ; funny, but works
      (send sport-icon set-label (get-sport-bitmap-colorized sport #f)))

    (define sport-name-edit
      (new choice%
           [parent sport-panel-edit]
           [label ""]
           [choices '("Run" "Bike")]
           [callback on-sport-selected]))

    (define edit-button
      (new button%
           [parent panel0]
           [label "Edit"]
           [callback (lambda (b e) (on-edit-headline))]))

    (define save-button
      (new button%
           [parent panel0]
           [label "Save"]
           [callback (lambda (b e) (on-save-headline))]
           [style '(deleted)]))

    (define revert-button
      (new button%
           [parent panel0]
           [label "Revert"]
           [callback (lambda (b e) (on-revert-headline))]
           [style '(deleted)]))

    (define end-spacer (make-spacer panel0))

    (define is-editing? #f)

    (define (on-edit-headline)
      (switch-to-edit-mode))

    (define (on-save-headline)
      (update-workout-headline)
      (switch-to-view-mode)
      (when headline-updated-callback
        (headline-updated-callback (get-headline) (get-sport))))

    (define (on-revert-headline)
      (switch-to-view-mode))

    (define (switch-to-edit-mode)
      (set! is-editing? #t)
      ;; Set the min-height of the top panel (panel0) to its current height.
      ;; This will prevent re-flowing of the entire viewwhen we switch to edit
      ;; mode which has a smaller height.
      (let-values (([w h] (send panel0 get-graphical-min-size)))
        (send panel0 min-height h))

      (send workout-title-edit set-value headline)
      (send sport-name-edit set-selection (sub1 sport))
      (send panel change-children
            (lambda (old)
              (list workout-title-edit sport-panel-edit)))
      (send panel0 change-children
            (lambda (old)
              (list begining-spacer sport-icon panel save-button revert-button end-spacer)))
      (send workout-title-edit focus))

    (define (switch-to-view-mode)
      (set! is-editing? #f)
      (send sport-icon set-label (get-sport-bitmap-colorized sport #f))
      (send sport-name set-label (get-sport-name sport #f))
      (send workout-title set-label headline)
      (send panel change-children
            (lambda (old)
              (list workout-title sport-panel)))
      (send panel0 change-children
            (lambda (old)
              (list begining-spacer sport-icon panel edit-button end-spacer))))

    (define (update-workout-headline)
      (set! headline (send workout-title-edit get-value))
      (let ((s (send sport-name-edit get-selection)))
        (set! sport (add1 s))))

    (define/public (set-workout workout)
      (if workout
          (begin
            (set! headline (workout-name workout))
            (set! sport (case (workout-sport workout)
                          ((running) 1)
                          ((cycling) 2)
                          (else #f))))
          (begin
            (set! headline "")
            (set! sport 1)))
      (switch-to-view-mode))

    (define/public (get-headline)
      headline)

    (define/public (get-sport)
      (case sport
        ((1) 'running)
        ((2) 'cycling)
        (else #f)))

    (define/public (unsaved-edits?) is-editing?)

    (define/public (enable flag)
      (send edit-button enable flag)
      (when flag
        (switch-to-view-mode)))

    ))
