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
         racket/gui/base)

(provide notes-input-field%)

(define headline-font
  (send the-font-list find-or-create-font 16 'default 'normal 'normal))

;; Widget used to edit description data for sessions and workouts.  It
;; contains a headline and a text editor.  When the text is modified a pair or
;; "Save"/"Revert" buttons are shown automatically, allowing the user to save
;; or revert the changes.  These buttons are only shown when the text is
;; modified, making the GUI less cluttered.
(define notes-input-field%
  (class object%
    (init-field parent
                ;; Label to show for the widget
                [label "Description/Notes"]
                ;; Callback invoked when the user clicks the "Save" button.
                [on-save-callback (lambda (text) #f)])
    (super-new)

    (define pane (new vertical-pane% [parent parent] [alignment '(center top)]))

    (define control-pane (new horizontal-pane% [parent pane]
                              [stretchable-height #f]
                              [alignment '(left center)]))

    (define headline (new message% [parent control-pane]
                          [stretchable-height #f]
                          [stretchable-width #t]
                          [font headline-font]
                          [label label]))

    (define save-button (new button% [parent control-pane]
                             [label "Save"]
                             [callback (lambda (b e) (on-save-description))]
                             [style '(deleted)]))

    (define revert-button (new button% [parent control-pane]
                               [label "Revert"]
                               [callback (lambda (b e) (on-revert-description))]
                               [style '(deleted)]))

    (define canvas (new editor-canvas% [parent pane] [style '(no-hscroll)]))

    (define text (new (class text% (init) (super-new)
                        
                        (define text-style
                          (let ([delta (new style-delta%)])
                            (send delta set-family 'modern)
                            (send delta set-size-add 2)
                            delta))

                        (send this change-style text-style 'start 'end #f)
                        
                        (define/augride (on-change)
                          ;; Use queue-callback so on-edit-description is
                          ;; called outside the on-change method which has the
                          ;; editor locked, so it interacts badly with the
                          ;; auto-wrap feature.
                          (queue-callback on-edit-description)))
                      [line-spacing 1.5]))

    (send canvas set-editor text)
    (send text set-max-undo-history 100)

    (define previous #f)
    (define is-editing? #f)

    ;; Called by the "on-change" method in the text% field.  If the editor is
    ;; modified, put on the save/revert buttons, otherwise remove them (for
    ;; example when the user has undo-ed all changes)
    (define (on-edit-description)
      (if (send text is-modified?)
          (unless is-editing?
            (set! is-editing? #t)
            (send control-pane change-children
                  (lambda (old) (list headline save-button revert-button))))
          (when is-editing?
            (set! is-editing? #f)
            (send control-pane change-children (lambda (old) (list headline))))))

    ;; Called when the user clicks "Save".  Calls the ON-SAVE-CALLBACK if the
    ;; text actually changed and hides the "Save"/"Revert" buttons.
    (define (on-save-description)

      (let ((contents (send text get-text 0 'eof #t)))
        (unless (equal? previous contents)
          (set! previous contents)
          (on-save-callback contents)))
      
      (send text set-modified #f)
      (send control-pane change-children (lambda (old) (list headline)))
      (set! is-editing? #f))

    ;; Called when the user clicks "Revert".  Restores the previous contents
    ;; of the editor and hides the "Save"/"Revert" buttons.
    (define (on-revert-description)
      (send text begin-edit-sequence)
      (send text select-all)
      (send text clear)
      (send text insert previous)
      (send text set-modified #f)
      (send text end-edit-sequence)
      (send control-pane change-children (lambda (old) (list headline)))
      (set! is-editing? #f))

    ;; Returns #t if the contents have been edited but not saved
    (define/public (unsaved-edits?)
      is-editing?)

    ;; Set new contents for the widget.  The previous contents are discarded
    ;; and the widget reverts to unmodified state.
    (define/public (set-contents contents)
      (set! previous (or contents ""))
      (send text begin-edit-sequence)
      (send text select-all)
      (send text clear)
      (send text insert previous)
      (send text clear-undos)
      (send text move-position 'home)
      (send text set-modified #f)
      (send text end-edit-sequence)
      (set! is-editing? #f)
      (send control-pane change-children (lambda (old) (list headline))))

    ;; Return the contents of the widget.  When UNSAVED? is #t, the possibly
    ;; edited contents are returned, otherwise the last saved contents are
    ;; returned.
    (define/public (get-contents [unsaved? #f])
      (if unsaved?
          (send text get-text 0 'eof #t)
          previous))

    ;; Enable/Disable the text widget according to FLAG
    (define/public (enable flag)
      (send canvas enable flag))

    ;; Do this last, as it will invoke the on-change method which will try to
    ;; call on-edit-description.
    (send text auto-wrap #t)

    ))
