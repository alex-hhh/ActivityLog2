#lang racket/base
;; view-last-import.rkt -- panel showing activies that were last imported
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
         "dialogs/activity-edit.rkt"
         "utilities.rkt"
         "fmt-util.rkt"
         "import.rkt"
         "sport-charms.rkt"
         "widgets/main.rkt"
         "dbutil.rkt")

(provide import-view%)
(provide import-dialog%)

(define (import-data-file data) (vector-ref data 0))
(define (import-data-status data) (vector-ref data 1))

;; Column definitions for the qresults-list% object used by the import dialog.
(define id-columns
  (list
   (qcolumn "File name" import-data-file import-data-file)
   (qcolumn "Status" import-data-status import-data-status)))

(define import-dialog%
  (class object%
    (init) (super-new)
    
    (define (make-toplevel-dialog parent)
      (new
       (class dialog% (init) (super-new)
         (define/override (on-superwindow-show shown?)
           (when shown? (begin-import)))
         (define/augment (on-close) #f))
       [label "Import new activities"]
       [min-width 600]
       [min-height 400]
       [parent parent]))
    
    (define toplevel-window (make-toplevel-dialog #f))
    (define import-list #f)
    (define status-message #f)
    (define close-button #f)
    (define export-button #f)
    
    (define import-pane
      (let ((p (new vertical-panel% [parent toplevel-window]
                    [spacing 10] [border 10]
                    [alignment '(left top)])))
        (let ((p0 (make-horizontal-pane p #f)))
          (new message% [parent p0]
               [label (import-icon)]
               [stretchable-width #f]
               [stretchable-height #f])
          
          (set! status-message
                (new message% [parent p0]
                     [stretchable-width #t]
                     [label "Importing new activities..."])))

        (set! import-list
              (new qresults-list% 
                   [parent p] [pref-tag 'activity-log:import-dialog]))

        (send import-list set-default-export-file-name "import-list.csv")
        
        (send import-list setup-column-defs id-columns)

        (let ((p1 (make-horizontal-pane p #f)))
          (send p1 set-alignment 'right 'center)
          (set! export-button
                (new button% [parent p1] [label "Export info..."]
                     [callback (lambda (b e) (on-export-info))]))
          (set! close-button 
                (new button% [parent p1] [label "Close"]
                     [callback (lambda (b e) (on-close-dialog))])))
        
        p))

    (define database #f)
    (define import-directory #f)

    (define (begin-import)
      (thread/dbglog
       #:name "import-dialog%/begin-import"
       (lambda ()
         (with-handlers
           (((lambda (e) #t)
             (lambda (e)
               (let ((message (if (exn? e) (exn-message e) (format "~a" e))))
                 (dbglog-exception "import-dialog%/begin-import" e)
                 (message-box "Import error" message toplevel-window '(ok stop))))))
           (import-new-activities-from-directory
            import-directory
            database 
            (lambda (file status detail)
              (queue-callback 
               (lambda () (on-file-progress file status detail))))
            (lambda (message)
              (queue-callback
               (lambda ()
                 (send status-message set-label message))))))
         (queue-callback 
          (lambda () 
            (send status-message set-label "Import complete.")
            (send export-button enable #t)
            (send close-button enable #t))))))
    
    (define (on-file-progress file status detail)
      (send import-list
            add-row
            (vector (if (path? file) (path->string file) file)
                    (cond ((eq? status 'ok) "Imported")
                          ((eq? status 'already-exists) "Already exists")
                          ((eq? status 'retired-device) "Retired device")
                          ((eq? status 'failed) (format "~a" detail))))))

    (define (on-export-info)
      (send import-list on-interactive-export-data #t))

    (define (on-close-dialog)
      (send toplevel-window show #f))

    (define/public (run parent db import-dir)
      (let ((old-toplevel toplevel-window))
        (let ((toplevel (if parent (make-toplevel-dialog parent) toplevel-window)))
          (send import-pane reparent toplevel)
          (set! toplevel-window toplevel))
        (set! database db)
        (set! import-directory import-dir)
        (send close-button enable #f)
        (send export-button enable #f)
        (send status-message set-label "Importing new activities...")
        (send import-list clear)
        ;; Import will begin as soon as the window is shown. The code below
        ;; will block until finish-dialog is called
        (send toplevel-window show #t)
        (send import-pane reparent old-toplevel)
        (set! toplevel-window old-toplevel)
        (send import-list save-visual-layout)
        (set! database #f)
        (set! import-directory #f)
        #t))

    ))

(define *display-columns*
  (list
   (let ((fn (lambda (row) (vector-ref row 1))))
     (qcolumn "Activity Name" fn fn))
   
   (let ((fn (lambda (row)
               (let ((sport (vector-ref row 5))
                     (sub-sport (vector-ref row 6)))
                 (get-sport-name sport sub-sport)))))
     (qcolumn "Sport" fn fn))
   
   (qcolumn "Start Time" 
            (lambda (row) (date-time->string (vector-ref row 2)))
            (lambda (row) (vector-ref row 2)))

   (qcolumn "Time" 
            (lambda (row) (duration->string (vector-ref row 3)))
            (lambda (row) (vector-ref row 3)))

   (qcolumn "Distance" 
            (lambda (row) 
              (let ((sport (vector-ref row 5))
                    (distance (vector-ref row 4)))
                (if (= sport 5)
                    (short-distance->string distance #t)
                    (distance->string distance #t))))
            (lambda (row) (vector-ref row 4)))
   
   (let ((fn (lambda (row) (vector-ref row 7))))
     (qcolumn "Activity-Guid" fn fn))
   
   (qcolumn "Session-Id" 
            (lambda (row) (format "~a" (vector-ref row 0)))
            (lambda (row) (vector-ref row 0)))
   
   ))

(define (get-last-imported-activities db)
  (query-rows 
   db
   "select S.id,
       ifnull(S.name, ''),
       ifnull(S.start_time, 0),
       ifnull(SS.total_timer_time, 0),
       ifnull(SS.total_distance, 0),
       ifnull(S.sport_id, 0),
       ifnull(S.sub_sport_id, 0),
       ifnull(A.guid, '')
  from LAST_IMPORT LI,
       A_SESSION S,
       SECTION_SUMMARY SS,
       ACTIVITY A
 where S.activity_id = LI.activity_id
   and S.summary_id = SS.id
   and S.activity_id = A.id
  order by S.start_time desc"))

;; NOTE: columns must match what get-last-imported-activities returns
(define (get-activity-1 db session-id)
  (query-row
   db
   "select S.id,
       ifnull(S.name, ''),
       ifnull(S.start_time, 0),
       ifnull(SS.total_timer_time, 0),
       ifnull(SS.total_distance, 0),
       ifnull(S.sport_id, 0),
       ifnull(S.sub_sport_id, 0),
       ifnull(A.guid, '')
  from A_SESSION S,
       SECTION_SUMMARY SS,
       ACTIVITY A
 where S.id = ?
   and S.summary_id = SS.id
   and S.activity_id = A.id" session-id))

(define (make-import-summary-label rows)
  (let ((nitems (length rows))
        (total-distance 0)
        (total-time 0))
    (for-each (lambda (row)
                (let ((time (vector-ref row 3))
                      (distance (vector-ref row 4)))
                  (set! total-time (+ total-time time))
                  (set! total-distance (+ total-distance distance))))
              rows)
    (format "~a activities, total time: ~a, total distance: ~a" 
            nitems 
            (duration->string total-time)
            (distance->string total-distance #t))))

(define import-view%
  (class* object% (activity-operations<%>)
    (init parent database select-activity-callback)
    (super-new)

    (define pane (new vertical-pane% 
                      [parent parent]
                      [alignment '(left center)]))

    (define the-database database)

    (define the-inspect-callback select-activity-callback)

    (let ((sel-pane (new horizontal-pane% [parent pane] 
                         [spacing 20]
                         [border 0]
                         [stretchable-height #f]
                         [stretchable-width #t]
                         [alignment '(left center)])))
      (make-spacer sel-pane)
      (new message% [parent sel-pane] [label (import-icon)])

      (let ((p (new horizontal-pane% [parent sel-pane]
                    [stretchable-width #t] [border 20]
                    [alignment '(right center)])))
        (new button% [parent p] [label "Import..."]
             [callback (lambda (b e) (on-import-new-activities))]))
      (make-spacer sel-pane))

    (define lb 
      (new (class qresults-list% (init) (super-new)
             (define/override (on-double-click row-index row-data)
               (when the-inspect-callback
                 (the-inspect-callback (vector-ref row-data 0)))))
           [parent pane]
           [pref-tag 'activity-log:import-view]
           [right-click-menu 
            (send (new activity-operations-menu% [target this]) get-popup-menu)]))

    (send lb set-default-export-file-name "last-import.csv")

    ;; The activty-operations interface

    (define selected-row-index #f)

    (define import-dialog (new import-dialog%))

    (define (on-import-new-activities)
      (send import-dialog run
            (send pane get-top-level-window)
            the-database)
      (refresh))

    (define/public (get-top-level-window)
      (send pane get-top-level-window))
    (define/public (get-database)
      the-database)
    (define/public (get-selected-sid)
      (when selected-row-index
        (let ((data (send lb get-data-for-row selected-row-index)))
          (if data (vector-ref data 0) #f))))
    (define/public (get-selected-sport)
      (when selected-row-index
        (let ((data (send lb get-data-for-row selected-row-index)))
          (if data (cons (sql-column-ref data 5 0) (sql-column-ref data 6 0)) #f))))
    (define/public (get-selected-guid)
      (when selected-row-index
        (let ((data (send lb get-data-for-row selected-row-index)))
          (if data 
              (let ((guid (vector-ref data 7)))
                (if (or (sql-null? guid) (equal? guid ""))
                    #f
                    guid))
              #f))))
    (define/public (after-update sid)
      (when selected-row-index
        (let ((new-data (get-activity-1 the-database sid)))
          (send lb update-row selected-row-index new-data))))
    (define/public (after-new sid)
      #f)
    (define/public (can-delete? sid)
      (not (eq? selected-row-index #f)))
    (define/public (after-delete sid)
      (when selected-row-index
        (send lb delete-row selected-row-index)))
    (define/public (before-popup)
      (set! selected-row-index (send lb get-selected-row-index)))
    (define/public (after-popdown)
      (set! selected-row-index #f))

    (define first-time? #t)
    (define/public (activated)
      (when first-time?
        (send lb setup-column-defs *display-columns*)
        (set! first-time? #f))
      (refresh))

    (define/public (refresh)
      (let* ((rows (get-last-imported-activities the-database))
             (status-label (make-import-summary-label rows)))
        (send lb set-data (get-last-imported-activities the-database))
        (send (send pane get-top-level-window) set-status-text status-label)))

    (define/public (save-visual-layout)
      (send lb save-visual-layout))
    
    ))
