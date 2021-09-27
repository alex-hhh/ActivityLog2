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
         racket/gui/base
         "widget-utilities.rkt")

(provide edit-dialog-base%)

(define title-font
  (send the-font-list find-or-create-font 16 'default 'normal 'normal))

;; Base class for "edit dialogs" -- implements the Save/Cancel functionality,
;; and a common look and feel.  Each dialog has a title and an optional icon
;; plus "Save" and "Cancel" buttons.  Additional widgets can be added to the
;; dialog's internal pane%, which can be obtained by calling
;; `get-client-pane`.
;;
;; To use this class, derive a class from this one, create the dialog contents
;; (see `get-client-pane`) and run the dialog using `do-edit`.  You may also
;; wish to override the `has-valid-data?` and `on-finish-edit` methods (see
;; their documentation for details).
(define edit-dialog-base%
  (class object%
    (init-field title
          [icon #f]
          [save-button-name "Save"]
          [min-width 300]
          [min-height 300])
    (super-new)

    (define (make-toplevel-dialog parent)
      (new
       (class dialog% (init) (super-new)
         (define/augment (on-close) (on-cancel)))
       [label title]
       [min-width min-width]
       [min-height min-height]
       [parent (if parent (send parent get-top-level-window) #f)]))

    (define toplevel-window (make-toplevel-dialog #f))

    ;; The save button of the dialog box (created later), it is
    ;; enabled/disabled by validate-timer if data is valid and can be saved.
    (define save-button #f)

    ;; The icon displayed on the dialog (created later).  It can be changed by
    ;; calling `set-icon`.
    (define dialog-icon #f)
    
    ;; client-pane contains the user widgets for this dialog (created later).
    ;; A derived class may obtain this using `get-client-pane` and populate it
    ;; with widgets.
    (define-values (edit-pane client-pane)
      (let ((p (new vertical-panel% [parent toplevel-window]
                    [spacing 10]
                    [border 10]
                    [alignment '(left top)])))
        (let ((p0 (make-horizontal-pane p #f)))
          (set! dialog-icon
                (new message% [parent p0] [label icon]
                     [stretchable-width #f] [stretchable-height #f]))
          (new message% [label title] [parent p0] [font title-font]))

        (define client-pane
          (new vertical-panel%
               [parent p] [border 0] [spacing 10] [alignment '(left top)]))

        (let ((bp (new horizontal-pane% [parent p] [border 0] [spacing 10]
                       [stretchable-height #f] [alignment '(right center)])))
          (set! save-button
                (new button% [label save-button-name] [parent bp]
                     [callback (lambda (b e) (on-save))]))
          (new button% [label "Cancel"] [parent bp]
               [callback (lambda (b e) (on-cancel))]))

        (values p client-pane)))

    ;; Result of running the dialog, #t if data was saved, #f if it was not,
    ;; this is what edit-equipment returns.
    (define dialog-result #f)

    ;; Check for valid data in the dialog box and enable disable the save
    ;; button based on that.  This timer only runs while the dialog box is
    ;; shown.
    (define validate-timer
      (new timer%
           [notify-callback
            (lambda () (send save-button enable (has-valid-data?)))]))

    (define/private (finish-edit result)
      (when (on-finish-edit result)
        (set! dialog-result result)
        (send validate-timer stop)
        (send toplevel-window show #f)))

    (define/private (on-save)
      ;; Don't allow finishing the edit with invalid data.  Our validate timer
      ;; might not have caught the invalid data to disable the save button in
      ;; time...
      (when (has-valid-data?)
        (finish-edit #t)))

    (define/private (on-cancel)
      (finish-edit #f))

    ;; Return the client pane in which controls for this dialog may be added.
    ;; This needs to be used in derived classes to set up some content widgets
    ;; in this dialog.
    (define/public (get-client-pane)
      client-pane)

    ;; Return the toplevel window corresponding to this dialog
    (define/public (get-top-level-window)
      toplevel-window)

    ;; Set a new ICON for this dialog, replacing the existing one.
    (define/public (set-icon icon)
      (send dialog-icon set-label icon))

    ;; Called periodically to check if the dialog has valid data.  If the
    ;; method returns #t, the form is assumed to contain valid data and the
    ;; "Save" button is enabled, otherwise, the "Save" button is disabled.
    (define/public (has-valid-data?)
      #t)

    ;; Called before the dialog window is closed.  RESULT is #t if it is
    ;; closed by the "Save" button, #f otherwise.  If the method returns #t,
    ;; the dialog is closed, otherwise the dialog is left open.
    ;;
    ;; This method can be overridden in a sub-class to display further dialog
    ;; boxes that have this dialog as a parent.
    (define/public (on-finish-edit result)
      #t)

    ;; Show the dialog.  PARENT is the parent window for the dialog.  This
    ;; method does not return until either "Save" or "Cancel" are pressed, or
    ;; the dialog is closed.  Returns #t if save was pressed, #f otherwise.
    ;;
    ;; A derived class might want to provide a "show-dialog" method that wraps
    ;; this one, and sets up the dialog contents for editing and actually
    ;; saves the result when the dialog is closed.
    (define/public (do-edit parent)
      (let ((old-toplevel toplevel-window))
        (let ((toplevel (if parent (make-toplevel-dialog parent) toplevel-window)))
          (send edit-pane reparent toplevel)
          (set! toplevel-window toplevel))
        (set! dialog-result #f)
        (send save-button enable (has-valid-data?))
        (send validate-timer start 1000)
        (send toplevel-window show #t) ; will block until finish-dialog is called
        (send edit-pane reparent old-toplevel)
        (set! toplevel-window old-toplevel)
        dialog-result))

    ))
