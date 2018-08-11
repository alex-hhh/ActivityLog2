#lang racket/base
;; edit-labels.rkt -- edit the session labels (add, edit, delete)
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015 Alex Harsanyi (AlexHarsanyi@gmail.com)
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

(require db/base
         racket/class
         racket/gui/base
         "../widgets/main.rkt")

(provide get-label-editor)


;;............................................... edit-one-label-dialog% ....

;; Dialog box for editing the name and description of a single label
(define edit-one-label-dialog%
  (class edit-dialog-base%
    (init)
    (super-new [title "Edit Label"] [icon ""] [min-height 10])
    
    (define name-field #f)
    (define description-field #f)
    
    (let ((p (send this get-client-pane)))
      (let ((p1 (new vertical-pane%
                     [parent p] [spacing 5]
                     [alignment '(left center)])))
        (set! name-field
              (new text-field% [parent p1] [label "Name: "]
                   [min-width 100] [stretchable-width #f]))
        (set! description-field
              (new text-field% [parent p1] [label "Description: "]
                   [min-width 300]))))
    
    (define/override (has-valid-data?)
      (> (string-length (send name-field get-value)) 0))

    ;; Show the dialog and return any data.  LABEL and DESCRIPTION define the
    ;; intial values (can be #f).  The method returns #f if the user cancelled
    ;; or a (cons NAME DESCRIPTION) if the user hit save.
    (define/public (show-dialog parent label description)
      (send name-field set-value (if label label ""))
      (send description-field set-value (if description description ""))
      (if (send this do-edit parent)
          (cons (send name-field get-value)
                (send description-field get-value))
          #f))))


;;.................................................. edit-labels-dialog% ....

(define (label-id data) (vector-ref data 0))
(define (label-name data) (vector-ref data 1))
(define (label-use-count data) (vector-ref data 2))
(define (label-description data) (vector-ref data 3))

;; Column definitions for the qresults-list% object used by the label editor.
(define le-columns
  (list
   (qcolumn "Label" label-name label-name)
   (qcolumn "Use count" 
            (lambda (row) (number->string (label-use-count row)))
            label-use-count)
   (qcolumn "Description" label-description label-description)))

;; Query to fetch all labels and their use count.
(define le-query
  "select L.id, 
          L.name, 
          (select count(SL.label_id) 
             from SESSION_LABEL SL 
            where SL.label_id = L.id), 
          L.description 
     from LABEL L")

(define edit-labels-dialog%
  (class edit-dialog-base%
    (init)
    (super-new [title "Labels"] [icon (edit-icon)] [min-width 600] [min-height 400])
    
    (define label-lb #f)
    
    (define one-label-editor (new edit-one-label-dialog%))

    ;; List of deleted label id's.  These will be deleted from the database
    ;; only when the user clicks SAVE.
    (define deleted-label-ids '())

    (let ((p (send this get-client-pane)))
      (let ((p1 (new vertical-pane%
                     [stretchable-height #t]
                     [parent p] [spacing 5]
                     [alignment '(center center)])))
        (let ((p2 (new horizontal-pane%
                       [stretchable-height #f]
                       [parent p1] [spacing 5]
                       [alignment '(right center)])))
          (new button% [parent p2] [label "Add"]
               [callback (lambda (b e) (on-add-label))])
          (new button% [parent p2] [label "Edit"]
               [callback (lambda (b e) (on-edit-label))])
          (new button% [parent p2] [label "Delete"]
               [callback (lambda (b e) (on-delete-label))]))

        (set! label-lb
              (new qresults-list%
                   [parent p1] [pref-tag 'activity-log:label-editor]))))

    (send label-lb setup-column-defs le-columns)
    
    (define (on-add-label)
      (let ((new-label (send one-label-editor show-dialog 
                             (send this get-top-level-window) "" "")))
        (when new-label
          (send label-lb add-row 
                (vector #f (car new-label) 0 (cdr new-label))))))
    
    (define (on-delete-label)
      (let ((index (send label-lb get-selected-row-index)))
        (when index
          (let ((data (send label-lb get-data-for-row index)))
            (if (> (label-use-count data) 0)
                (message-box
                 "Cannot delete" 
                 "Cowardly refusing to delete a label that is attached to sessions."
                 (send this get-top-level-window)
                 '(ok stop))
                (begin
                  ;; Store the id of the label to delete, it will be deleted
                  ;; when the user saves the changes
                  (when (label-id data)
                    (set! deleted-label-ids (cons (label-id data) deleted-label-ids)))
                  (send label-lb delete-row index)))))))
    
    (define (on-edit-label)
      (let ((index (send label-lb get-selected-row-index)))
        (when index
          (let ((data (send label-lb get-data-for-row index)))
            (let ((new-label (send one-label-editor show-dialog
                                   (send this get-top-level-window)
                                   (label-name data)
                                   (label-description data))))
              (when new-label
                (send label-lb update-row index 
                      (vector
                       (label-id data)
                       (car new-label)
                       (label-use-count data)
                       (cdr new-label)))))))))
    
    (define (populate-labels db)
      (let ((rows (query-rows db le-query)))
        (send label-lb set-data rows)))

    (define (save-labels db)
      (call-with-transaction
       db
       (lambda ()
         ;; Delete rows which the user deleted
         (for ((id (in-list deleted-label-ids)))
           (query-exec db "delete from LABEL where id = ?" id))
         ;; Update or create the new labels.  Note that we always update all
         ;; labels, even if they didn't change.  We don't expect the number of
         ;; labels to be too large and also we don't expect editing to be a
         ;; frequent operation.
         (for ((row-index (in-range (send label-lb get-row-count))))
           (let ((data (send label-lb get-data-for-row row-index)))
             (if (label-id data)
                 (query-exec
                  db
                  "update LABEL set name = ?, description = ? where id = ?"
                  (label-name data) (label-description data) (label-id data))
                 (query-exec
                  db
                  "insert into LABEL(name, description) values (?, ?)"
                  (label-name data) (label-description data))))))))
    
    (define/public (show-dialog parent db)
      (set! deleted-label-ids '())
      (populate-labels db)
      (when (send this do-edit parent)
        (save-labels db))
      (send label-lb save-visual-layout))

    ))

(define the-label-editor #f)

(define (get-label-editor)
  (unless the-label-editor
    (set! the-label-editor (new edit-labels-dialog%)))
  the-label-editor)
