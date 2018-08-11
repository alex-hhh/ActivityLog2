#lang racket/base
;; edit-seasions.rkt -- manage the seasons in the database (add, edit,
;; delete).  Seasons are defined date ranges which can be used to filter
;; activities and reports.
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
         racket/list
         "../dbutil.rkt"
         "../fmt-util.rkt"
         "../widgets/main.rkt")

(provide get-season-editor)



;;.............................................. edit-one-season-dialog% ....

;; Dialog box for editing the name, description and date range of a single
;; season
(define edit-one-season-dialog%
  (class edit-dialog-base%
    (init)
    (super-new [title "Edit Season"] [icon ""] [min-height 10])

    (define name-field #f)
    (define description-field #f)
    (define start-date-field #f)
    (define end-date-field #f)

    (let ((p (send this get-client-pane)))
      (let ((p1 (new vertical-pane%
                     [parent p] [spacing 5]
                     [alignment '(left center)])))
        (set! name-field
              (new text-field% [parent p1] [label "Name: "]
                   [min-width 100] [stretchable-width #f]))
        (set! description-field
              (new text-field% [parent p1] [label "Description: "]
                   [min-width 300]))

        (let ((p (new horizontal-pane% [parent p1] [spacing 5])))
          (set! start-date-field
                (new date-input-field%
                     [parent p] [label "Start "]
                     [style '(single)] [stretchable-width #f] [min-width 1]))
          (set! end-date-field
                (new date-input-field%
                     [parent p] [label "End "]
                     [style '(single)] [stretchable-width #f] [min-width 1]))
          )))

    (define/override (has-valid-data?)
      (and (> (string-length (send name-field get-value)) 0)
           (send start-date-field has-valid-value?)
           (send end-date-field has-valid-value?)
           (let ((s (send start-date-field get-converted-value))
                 (e (send end-date-field get-converted-value)))
             (if (and (number? s) (number? e))
                 (if (<= e s)
                     (begin
                       (send start-date-field mark-valid #f)
                       #f)
                     #t)
                 #f))))

    (define/public (show-dialog parent [label #f] [description #f] [start #f] [end #f])
      (send name-field set-value (if label label ""))
      (send description-field set-value (if description description ""))
      (if start
          (send start-date-field set-date-value start)
          (send start-date-field set-value  ""))
      (if end
          (send end-date-field set-date-value end)
          (send end-date-field set-value ""))
      (if (send this do-edit parent)
          (list (send name-field get-value)
                (send description-field get-value)
                (send start-date-field get-converted-value)
                (send end-date-field get-converted-value))
          #f))
    
    ))


;;................................................. edit-seasons-dialog% ....

(define (season-id data) (sql-column-ref data 0 ""))
(define (season-name data) (sql-column-ref data 1 ""))
(define (season-description data) (sql-column-ref data 2 ""))
(define (season-start-date data) (sql-column-ref data 3))
(define (season-end-date data) (sql-column-ref data 4))

;; Column definitions for the qresults-list% object used by the season editor.
(define sn-columns
  (list
   (qcolumn "Name" season-name season-name)
   (qcolumn "Description"  season-description season-description)
   (qcolumn "Start Date"
            (lambda (row)
              (calendar-date->string (season-start-date row)))
            season-start-date)
   (qcolumn "End Date"
            (lambda (row)
              (calendar-date->string (season-end-date row)))
            season-end-date)
   ))

;; Query to fetch all labels and their use count.
(define sn-query
  "select SN.id, 
          SN.name, 
          SN.description, 
          SN.start_date,
          SN.end_date
     from SEASON SN")

(define edit-seasons-dialog%
  (class edit-dialog-base%
    (init)
    (super-new [title "Seasons"] [icon (edit-icon)] [min-width 600] [min-height 400])

    (define season-lb #f)
    
    (define one-season-editor (new edit-one-season-dialog%))

    ;; List of deleted season id's.  These will be deleted from the database
    ;; only when the user clicks SAVE.
    (define deleted-season-ids '())

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
               [callback (lambda (b e) (on-add-season))])
          (new button% [parent p2] [label "Edit"]
               [callback (lambda (b e) (on-edit-season))])
          (new button% [parent p2] [label "Delete"]
               [callback (lambda (b e) (on-delete-season))]))

        (set! season-lb
              (new qresults-list%
                   [parent p1] [pref-tag 'activity-log:season-editor]))))

    (send season-lb setup-column-defs sn-columns)
    
    (define (on-add-season)
      (let ((sn (send one-season-editor show-dialog 
                      (send this get-top-level-window))))
        (when sn
          (send season-lb add-row 
                (vector #f (first sn) (second sn) (third sn) (fourth sn))))))
    
    (define (on-delete-season)
      (let ((index (send season-lb get-selected-row-index)))
        (when index
          (let ((data (send season-lb get-data-for-row index)))
            ;; Store the id of the season to delete, it will be deleted when
            ;; the user saves the changes.
            ;;
            ;; TODO: ask the user for confirmation first.
            (when (season-id data)
              (set! deleted-season-ids (cons (season-id data) deleted-season-ids)))
            (send season-lb delete-row index)))))
    
    (define (on-edit-season)
      (let ((index (send season-lb get-selected-row-index)))
        (when index
          (let* ((data (send season-lb get-data-for-row index))
                 (sn (send one-season-editor show-dialog
                           (send this get-top-level-window)
                           (season-name data)
                           (season-description data)
                           (season-start-date data)
                           (season-end-date data))))
            (when sn
              (send season-lb update-row index
                    (vector 
                     (season-id data)
                     (first sn)
                     (second sn)
                     (third sn)
                     (fourth sn))))))))
    
    (define (populate-seasons db)
      (let ((rows (query-rows db sn-query)))
        (send season-lb set-data rows)))

    (define (save-seasons db)
      (call-with-transaction
       db
       (lambda ()
         ;; delete rows which the user deleted
         (for ((id (in-list deleted-season-ids)))
           (query-exec db "delete from SEASON where id = ?" id))
         ;; Update or create new seasons.  Note that we alwasy
         ;; udpate all seasons even when they don't change.
         (for ((row-index (in-range (send season-lb get-row-count))))
           (let ((data (send season-lb get-data-for-row row-index)))
             (if (season-id data)
                 (query-exec
                  db
                  "update SEASON 
                   set name =?, description = ?, start_date = ?, end_date = ?
                   where id = ?"
                  (season-name data)
                  (season-description data)
                  (season-start-date data)
                  (season-end-date data)
                  (season-id data))
                 (query-exec
                  db
                  "insert into SEASON (name, description, start_date, end_date)
                   values (?, ?, ?, ?)"
                  (season-name data)
                  (season-description data)
                  (season-start-date data)
                  (season-end-date data))))))))
    
    (define/public (show-dialog parent db)
      (set! deleted-season-ids '())
      (populate-seasons db)
      (when (send this do-edit parent)
        (save-seasons db))
      (send season-lb save-visual-layout))

    ))

(define the-season-editor #f)

(define (get-season-editor)
  (unless the-season-editor
    (set! the-season-editor (new edit-seasons-dialog%)))
  the-season-editor)

