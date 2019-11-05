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

(require racket/class
         racket/draw
         "../widgets/esc-controls.rkt"
         "pict-util.rkt"
         "wkstep-editor.rkt"
         "wkstep.rkt"
         "workout-step-snip-base.rkt")

(provide workout-step-snip%)

(define workout-step-editor #f)

(define (get-workout-step-editor)
  (unless workout-step-editor
    (set! workout-step-editor (new wkstep-editor%)))
  workout-step-editor)

(define workout-step-snip-class (define-snip-class "workout-step-snip"))

;; Snip% class to display a workout step (wkstep structure) and allow editing
;; it by providing an edit button.
(define workout-step-snip%
  (class workout-step-snip-base%
    (init-field [data (wkstep 'warmup 'time 600 'open #f #f #f)])
    (super-new
     [snip-class workout-step-snip-class]
     [pict (make-step-pict data)])

    (let ((edit-button (new esc-button%
                            [parent-snip this]
                            [label 'menu]
                            [width button-size]
                            [height button-size]
                            [color #f] [disabled-color #f] ; make the background transparent
                            [text-color (make-object color% #x2f #x4f #x4f)]
                            [hover-color (make-object color% #xda #xa5 #x20)]
                            [pushed-color (make-object color% #xee #xe8 #xaa)]
                            [callback (lambda () (on-edit))])))
      (send this add-additional-button edit-button))

    (define (get-editor)
      (let ((admin (send this get-admin)))
        (and admin (send admin get-editor))))

    (define (get-top-level-window)
      (let ((editor (get-editor)))
        (and editor
             (let ((canvas (send editor get-canvas)))
               (and canvas (send canvas get-top-level-window))))))

    ;; Callback for the edit button: invoke the editor and update the snip if
    ;; the changes are saved.
    (define (on-edit)
      (define toplevel (get-top-level-window))
      (when toplevel
        (define result (send (get-workout-step-editor) show-dialog toplevel data))
        (when result
          (let ((editor (get-editor))
                (old-data data))
            (when editor
              (send editor x-add-undo
                    (lambda ()
                      (set! data old-data)
                      (send this refresh-pict)))))
          (set! data result)
          (send this refresh-pict))))

    (define/override (make-pict nesting-level)
      (make-step-pict data nesting-level))

    ;; Return the wkstep managed by this snip
    (define/public (get-wkstep)
      data)

    ))
