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

(require racket/gui/base racket/class)
(require "../utilities.rkt"             ; thread/dbglog
         "widget-utilities.rkt")

(provide progress-dialog%)

(define progress-dialog%
  (class object%
    (init-field
     title
     [icon #f]
     [can-cancel? #t]
     [min-width 400])
    (super-new)

    (define (make-toplevel-dialog parent)
      (new
       (class dialog% (init) (super-new)
         (define/augment (can-close?) can-cancel?)
         (define/augment (on-close) (on-close-dialog)))
       [label title]
       [min-width min-width]
       [parent parent]))

    (define toplevel-window (make-toplevel-dialog #f))
    (define message-box #f)
    (define progress-bar #f)
    (define update-button #f)
    (define cancel-button #f)

    (define update-thread #f)
    (define task-fn #f)

    (define start-timestamp (current-inexact-milliseconds))
    (define update-complete-flag #f)
    (define terminate-update-flag #f)

    (define dialog-pane
      (let ((pane (new vertical-panel%
                       [parent toplevel-window] [border 20] [spacing 5]
                       [alignment '(left top)])))
        (let ((pane (new horizontal-pane% [parent pane])))
          (new message% [parent pane] [label title]
               [font (send the-font-list find-or-create-font 12 'default 'normal 'normal)]))
        (let ((pane (new horizontal-pane% [parent pane] [border 0] [spacing 20])))
          (when icon
            (new message% [parent pane] [label icon]
                 [stretchable-height #f] [stretchable-width #f]))
          (let ((pane (new vertical-pane% [parent pane] [spacing 1] [alignment '(left top)])))
            (set! progress-bar (new gauge% [parent pane] [label ""] [range 100]))
            (set! message-box (new message% [parent pane] [label ""] [min-width 200]))))
        (let ((pane (new horizontal-pane% [parent pane] [border 0]
                         [stretchable-height #f] [alignment '(right center)])))
          (set! update-button (new button%
                                   [label "Begin Update"]
                                   [parent pane]
                                   [callback (lambda (b e) (on-begin-update))]))
          (set! cancel-button (new button% [label "Cancel"]
                                   [parent pane]
                                   [callback (lambda (b e) (on-close-dialog))])))
        pane))

    (define/public (set-message msg)
      (send message-box set-label msg))

    (define/public (set-progress pct)
      (when (eqv? (inexact->exact pct) 0)
        (set! start-timestamp (current-inexact-milliseconds)))
      (send progress-bar set-value (inexact->exact pct))
      (when (> pct 0)
        (let* ((elapsed (- (current-inexact-milliseconds) start-timestamp))
               (remaining (- (/ elapsed (/ pct 100)) elapsed)))
          (send message-box set-label
                (format "Remaining time: ~a (elapsed ~a)"
                        (w-duration->string (/ remaining 1000))
                        (w-duration->string (/ elapsed 1000))))))
      (not terminate-update-flag))

    (define (on-begin-update)
      (if update-complete-flag
          (on-close-dialog)
          (begin
            (send update-button enable #f)
            (set! start-timestamp (current-inexact-milliseconds))
            (unless can-cancel? (send cancel-button enable #f))
            (set! update-thread
                  (thread/dbglog
                   #:name "al-progress-dialog% worker"
                   (lambda ()
                     (task-fn this)
                     (set! update-complete-flag #t)
                     (send update-button set-label "Close")
                     (send update-button enable #t)
                     (send cancel-button enable #f)))))))

    (define (on-close-dialog)
      (when update-thread
        (set! terminate-update-flag #t)
        (thread-wait update-thread))
      (send toplevel-window show #f))

    (define/public (run parent task)
      (let ((old-toplevel toplevel-window))
        (let ((toplevel (if parent (make-toplevel-dialog parent) toplevel-window)))
          (send dialog-pane reparent toplevel)
          (set! toplevel-window toplevel))
        (send update-button set-label "Begin Update")
        (send update-button enable #t)
        (send cancel-button enable #t)
        (send message-box set-label "")
        (send progress-bar set-value 0)
        (set! update-complete-flag #f)
        (set! terminate-update-flag #f)
        (set! update-thread #f)
        (set! task-fn task)
        (send toplevel-window show #t) ; will block
        (send dialog-pane reparent old-toplevel)
        (set! toplevel-window old-toplevel)))

    ))
