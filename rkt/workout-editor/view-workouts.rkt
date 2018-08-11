#lang racket/base
;; view-workouts.rkt -- workouts management panel
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

(require racket/class
         racket/gui/base
         racket/match
         db/base
         "../dbutil.rkt"
         "../sport-charms.rkt"
         "../widgets/main.rkt"
         "wkstep.rkt"
         "wk-db.rkt"
         "wk-fit.rkt"
         "workout-editor.rkt")

(provide view-workouts%
         workout-operations-menu%
         workout-operations<%>)

(define message-font
  (send the-font-list find-or-create-font 12 'default 'normal 'normal))

;; Dialog box to enter or edit a library name.  Used by the "New Workout
;; Library..." and "Edit Workout Libary..." menu commands.
(define workout-library-editor%
  (class edit-dialog-base%
    (init)
    (super-new [title "Edit Workout Library"]
               [icon (edit-icon)]
               [min-width 600] [min-height 300])

    (define pane (make-vertical-pane (send this get-client-pane) 50))
    (define existing-names '())

    (define (check-library-name v)
      (not (member v existing-names)))

    (define library-name-input
      (new validating-input-field%
           [parent pane]
           [label "Library Name: "]
           [validate-fn (lambda (v) (check-library-name v))]
           [convert-fn values]
           [cue-text "library name"]
           [allow-empty? #f]))

    (define message-field
      (new message% [parent pane] [label ""] [stretchable-width #t] [font message-font]))

    (define/override (has-valid-data?)
      (define result (send library-name-input has-valid-value?))
      (define contents (send library-name-input get-value))
      (cond (result (send message-field set-label ""))
            ((equal? contents "")
             (send message-field set-label "Library name cannot be empty"))
            (#t
             (send message-field set-label "A library by this name already exists")))
      result)

    (define/public (show-dialog parent existing-library-names [library-name #f])
      (set! existing-names existing-library-names)
      (send library-name-input set-value (or library-name ""))
      (and (send this do-edit parent)
           (send library-name-input get-converted-value)))

    ))

;; Interface containing the basic methods used to implement the operations
;; listed in the "Workouts..." menu.  They are implemented by view-workouts%,
;; but toplevel.rkt contains a forwarder with this interface as well.
(define workout-operations<%>
  (interface ()
             get-top-level-window
             get-database
             before-popup
             after-popdown
             switch-to-view
             get-selected-library-id
             get-selected-workout-id
             after-new-library
             after-update-library
             can-delete-library?
             after-delete-library
             after-new-workout
             after-update-workout
             can-delete-workout?
             after-delete-workout))

;; The "Workouts" menu and its operations.
(define workout-operations-menu%
  (class object%
    (init-field target)
    (init [menu-bar #f])
    (super-new)

    (unless (is-a? target workout-operations<%>)
      (error "Target must implement the workout-operations<%> interface"))

    (define (on-demand m)
      (send target before-popup)
      (let ((have-library-id? (number? (send target get-selected-library-id)))
            (have-workout-id? (number? (send target get-selected-workout-id))))
        (send new-library-menu enable have-library-id?)
        (send rename-library-menu enable have-library-id?)
        (send delete-library-menu enable have-library-id?)
        (send new-workout-menu enable have-library-id?)
        (send import-workout-menu enable have-library-id?)
        (send duplicate-workout-menu enable have-workout-id?)
        (send delete-workout-menu enable have-workout-id?)
        (send export-workout-menu enable have-workout-id?)))

    (define (on-popdown m e)
      (send target after-popdown))

    (define (on-switch-to-view m e)
      (send target switch-to-view))

    (define (on-new-library m e)
      (let* ((db (send target get-database))
             (toplevel (send target get-top-level-window))
             (existing (query-list db "select name from WORKOUT_LIBRARY"))
             (library-name (send library-dialog show-dialog toplevel existing)))
        (when library-name
          (query-exec db "insert into WORKOUT_LIBRARY(name) values(?)" library-name)
          (define id (db-get-last-pk "WORKOUT_LIBRARY" db))
          (send target after-new-library id))))

    (define (on-rename-library m e)
      (let* ((db (send target get-database))
             (toplevel (send target get-top-level-window))
             (library-id (send target get-selected-library-id))
             (existing (query-rows db "select id, name from WORKOUT_LIBRARY")))
        (define library-name
          (for/first ([item (in-list existing)]
                      #:when (eqv? (vector-ref item 0) library-id))
            (vector-ref item 1)))
        (define others
          (for/list ([item (in-list existing)]
                     #:unless (eqv? (vector-ref item 0) library-id))
            (vector-ref item 1)))
        (define new-name (send library-dialog show-dialog toplevel others library-name))
        (when new-name
          (query-exec db "update WORKOUT_LIBRARY set name = ? where id = ?"
                      new-name library-id)
          (send target after-update-library library-id))))

    (define (on-delete-library m e)
      (let* ((db (send target get-database))
             (toplevel (send target get-top-level-window))
             (library-id (send target get-selected-library-id))
             (workout-count (query-value
                             db "select count(*) from WORKOUT where library_id = ?" library-id)))
        (if (eqv? workout-count 0)
            (begin
              (query-exec db "delete from WORKOUT_LIBRARY where id = ?" library-id)
              (send target after-delete-library library-id))
            (message-box
             "Cannot delete workout library"
             "Cannot delete workout library, because it contains workouts.\n\nOnly empty libraries can be deleted."
             toplevel '(stop ok)))))

    (define (on-new-workout m e)
      (let* ((db (send target get-database))
             (library-id (send target get-selected-library-id))
             (wk (workout "New Workout" "" 'running #f #f '())))
        (define-values (workout-id _) (store-workout db wk library-id))
        (send target after-new-workout workout-id)))

    (define (on-import-workout m e)
      (let* ((db (send target get-database))
             (toplevel (send target get-top-level-window))
             (library-id (send target get-selected-library-id))
             (file (get-file "Import FIT workout file" toplevel #f #f #f null
                             '(("FIT Files" "*.fit")
                               ("Any" "*.*")))))
        (when file
          (if (file-exists? file)
              (let ((wk (fit->workout file)))
                (define-values (workout-id _)
                  (store-workout db wk library-id #:may-replace-serial? #t))
                (send target after-new-workout workout-id))
              (message-box
               "Could not open file"
               (format "File ~a does not exist." file)
               toplevel '(stop ok))))))

    (define (on-duplicate-workout m e)
      (let* ((db (send target get-database))
             (toplevel (send target get-top-level-window))
             (library-id (send target get-selected-library-id))
             (workout-id (send target get-selected-workout-id))
             (wk (fetch-workout db workout-id)))
        (define new-workout
          (struct-copy workout wk
                       [name (string-append (workout-name wk) " - Copy")]
                       [serial #f]))
        (define-values (workout-id _)
          (store-workout db new-workout library-id #:may-replace-serial? #t))
        (send target after-new-workout workout-id)))

    (define (on-delete-workout m e)
      (let* ((db (send target get-database))
             (toplevel (send target get-top-level-window))
             (workout-id (send target get-selected-workout-id))
             (wname (query-value db "select name from WORKOUT where id = ?" workout-id)))
        (define result
          (message-box
           "Confirm delete"
           (format "Are you sure you want to delete the ~a workout" wname)
           toplevel
           '(caution yes-no)))
        (when (eq? result 'yes)
          (delete-workout db workout-id)
          (send target after-delete-workout workout-id))))

    (define (on-export-workout/fit m e)
      (let* ((db (send target get-database))
             (toplevel (send target get-top-level-window))
             (workout-id (send target get-selected-workout-id))
             (wk (fetch-workout db workout-id #:for-export? #t))
             (proposed-file (format "~a.fit" (workout-name wk))))
        (define file-name
          (put-file "Export workout as FIT file" toplevel
                    #f proposed-file ".fit" null
                    '(("FIT Files" "*.fit")
                      ("Any" "*.*"))))
        (when file-name
          (define data (workout->fit wk))
          (call-with-output-file file-name
            (lambda (out) (write-bytes data out))
            #:mode 'binary #:exists 'replace))))

        (define the-menu
      (if menu-bar
          (new menu% [parent menu-bar] [label "&Workout"]
               [demand-callback on-demand])
          (new popup-menu% [title "Workout"]
               [demand-callback on-demand]
               [popdown-callback on-popdown])))

    (define library-dialog (new workout-library-editor%))

    (define (make-menu-item label callback [shortcut #f])
      (new menu-item% [parent the-menu] [label label]
           [callback callback] [shortcut shortcut]))

    (define switch-to-view-menu
      (make-menu-item "Switch to Workout View" on-switch-to-view))
    (new separator-menu-item% [parent the-menu])
    (define new-library-menu
      (make-menu-item "New Workout Library ..." on-new-library))
    (define rename-library-menu
      (make-menu-item "Rename Workout Library ..." on-rename-library))
    (new separator-menu-item% [parent the-menu])
    (define delete-library-menu
      (make-menu-item "Delete Workout Library ..." on-delete-library))
    (new separator-menu-item% [parent the-menu])
    (define new-workout-menu
      (make-menu-item "New Workout" on-new-workout))
    (define import-workout-menu
      (make-menu-item "Import Workout..." on-import-workout))
    (define duplicate-workout-menu
      (make-menu-item "Duplicate Workout" on-duplicate-workout))
    (new separator-menu-item% [parent the-menu])
    (define delete-workout-menu
      (make-menu-item "Delete Workout..." on-delete-workout))
    (new separator-menu-item% [parent the-menu])
    (define export-workout-menu
      (make-menu-item "Export Workout (FIT File)..." on-export-workout/fit))

    (define/public (get-popup-menu) the-menu)
    ))

(define workout-colums
  (list
   (let ((fn (lambda (v) (sql-column-ref v 1))))
     (qcolumn "Name" fn fn))
   (let ((fn (lambda (v)
               (let ((sport (sql-column-ref v 2))
                     (sub-sport (sql-column-ref v 3)))
                 (get-sport-name sport sub-sport)))))
     (qcolumn "Sport" fn fn))))

(define (get-workout-libraries db)
  (query-rows db "select id, name from WORKOUT_LIBRARY order by name"))

(define (get-workouts-for-library db library-id)
  (query-rows db "
select id, name, sport_id, sub_sport_id, serial, library_id
  from WORKOUT
 where library_id = ?
order by name" library-id))

;; This function must return a single row with the same data as the rows
;; returned by `get-workout-libraries`
(define (get-workout-row db workout-id)
  (query-row db "
select id, name, sport_id, sub_sport_id, serial, library_id
  from WORKOUT
 where id = ?" workout-id))

(define view-workouts%
  (class* object% (workout-operations<%>)
    (init-field parent database)
    (super-new)

    (define tag 'activity-log:view-workouts)
    (define workout-libraries (get-workout-libraries database))

    (define pane (new horizontal-pane% [parent parent]))

    (define workouts-pane (make-vertical-pane pane #f))
    (define workouts-pane-top-bar (make-horizontal-pane workouts-pane #f))
    (send workouts-pane-top-bar border 30)

    (define selected-library-index #f)
    (define selected-workout-index #f)

    (define library-choice
      (new choice%
           [parent workouts-pane-top-bar]
           [font message-font]
           [label "Library: "]
           [choices (for/list ([l (in-list workout-libraries)])
                      (vector-ref l 1))]
           [callback (lambda (c e)
                       (let ((index (send c get-selection)))
                         (on-library-selected index)))]))

    (define workouts-list
      (new (class qresults-list%
             (init) (super-new)
             (define/override (on-select index data)
               (on-workout-selected index data)))
           [parent workouts-pane]
           [pref-tag 'activity-log:view-workouts-qresults]))

    (define workout-editor
      (new workout-editor%
           [parent pane]
           [workout-changed-callback (lambda (wk) (on-workout-changed wk))]))

    (define (get-current-workout-library-id)
      (define index (send library-choice get-selection))
      (match-define (vector id name) (list-ref workout-libraries index))
      id)

    (define (on-workout-changed workout)
      (define library-id (get-current-workout-library-id))
      (define-values (workout-id _) (store-workout database workout library-id))
      (define index (send workouts-list get-selected-row-index))
      (when index
        (define data (send workouts-list get-data-for-row index))
        (unless (eqv? workout-id (vector-ref data 0))
          (error "Workout ID's mismatch"))
        (define nrow (get-workout-row database workout-id))
        (send workouts-list update-row index nrow)))

    (define (can-discard-current-workout?)
      (if (send workout-editor unsaved-edits?)
          (let ((mresult (message-box/custom
                          "Unsaved Edits"
                          "Workout headline, notes or steps are not saved"
                          "Review" "Discard" #f
                          (send pane get-top-level-window)
                          '(stop default=1)
                          #f)))
            (cond ((or (eq? #f mresult) (eqv? 1 mresult))
                   #f)
                  ((eqv? 2 mresult)
                   ;; User wants the changes discarded
                   #t)
                  (#t #f)))
          #t))

    (define (on-library-selected index [selected-workout-id #f])
      (unless (eqv? index selected-library-index)
        (define proceed (can-discard-current-workout?))
        (if proceed
            (let ()
              (match-define (vector id name) (list-ref workout-libraries index))
              (define workout-data (get-workouts-for-library database id))
              (send workouts-list clear)
              (send workouts-list set-data workout-data)
              (set! selected-workout-index #f)
              (cond ((null? workout-data)
                     (send workout-editor set-workout #f))
                    ((eq? selected-workout-id #f)
                     (send workouts-list select-row 0)
                     (on-workout-selected 0 (car workout-data)))
                    (#t
                     (define index
                       (or
                        (for/first (((wk index) (in-indexed (in-list workout-data)))
                                    #:when (eq? (vector-ref wk 0) selected-workout-id))
                          index)
                        0))
                     (send workouts-list select-row index)
                     (on-workout-selected 0 (list-ref workout-data index))))
              (set! selected-library-index index))
            (begin
              (send library-choice set-selection selected-library-index)))))

    (define (on-workout-selected index data)
      (unless (eqv? index selected-workout-index)
        (define proceed (can-discard-current-workout?))
        (if proceed
            (let* ((id (vector-ref data 0))
                   (wk (fetch-workout database id)))
              (send workout-editor set-workout wk)
              (set! selected-workout-index index))
            (send workouts-list select-row selected-workout-index))))

    (define (refresh-workout-libraries
             [selected-library-id #f]
             [selected-workout-id #f])
      (set! workout-libraries (get-workout-libraries database))
      (send library-choice clear)
      (define selected-index 0)
      (set! selected-library-index #f)
      (for (([lib index] (in-indexed (in-list workout-libraries))))
        (match-define (vector id name) lib)
        (send library-choice append name)
        (when (eqv? id selected-library-id)
          (set! selected-index index)))
      (send library-choice set-selection selected-index)
      (on-library-selected selected-index selected-workout-id))

    (define first-activation? #t)

    (define/public (activated)
      (when first-activation?
        (send workouts-list setup-column-defs workout-colums)
        (let ((selection (send library-choice get-selection)))
          (when selection
            (on-library-selected selection)))
        (set! first-activation? #f)))

    (define/public (refresh)
      (define library-id (get-selected-library-id))
      (define workout-id (get-selected-workout-id))
      (refresh-workout-libraries library-id workout-id))

    (define/public (can-exit?)
      (can-discard-current-workout?))

    (define/public (save-visual-layout)
      ;; TODO: save the selected libary ID and workout ID as well, so we
      ;; restore to that same one when the application is restarted.
      (send workouts-list save-visual-layout))


    ;;.............................................. workout menu operations ....

    (define selected-library #f)
    (define selected-workout #f)

    (define/public (get-top-level-window)
      (send pane get-top-level-window))

    (define/public (get-database)
      database)

    (define/public (before-popup)
      (set! selected-library (send library-choice get-selection))
      (set! selected-workout (send workouts-list get-selected-row-index)))

    (define/public (after-popdown)
      (set! selected-library #f)
      (set! selected-workout #f))

    (define/public (switch-to-view)
      ;; this method is actually implemented in the forwarder in toplevel.rkt,
      ;; and the call never reaches us.
      #f)

    (define/public (get-selected-library-id)
      (define selection (send library-choice get-selection))
      (and selection (vector-ref (list-ref workout-libraries selection) 0)))

    (define/public (get-selected-workout-id)
      (define selection (send workouts-list get-selected-row-index))
      (if selection
          (let ((data (send workouts-list get-data-for-row selection)))
            (vector-ref data 0))
          #f))

    (define/public (after-new-library library-id)
      (refresh-workout-libraries library-id))

    (define/public (after-update-library library-id)
      (refresh-workout-libraries library-id))

    (define/public (can-delete-library? library-id)
      (eqv? (get-selected-library-id) library-id))

    (define/public (after-delete-library library-id)
      (define new-selected-library-id
        (let ((next (and selected-library (add1 selected-library))))
          (if (and next (< next (length workout-libraries)))
              (vector-ref (list-ref workout-libraries next) 0)
              #f)))
      (refresh-workout-libraries new-selected-library-id))

    (define/public (after-new-workout workout-id)
      (define row (get-workout-row database workout-id))
      (define library-id (vector-ref row 5))
      (if (eqv? (get-selected-library-id) library-id)
          ;; workout is in the same library
          (send workouts-list add-row row)
          ;; workout is in a different library, don't switch libraries for now
          ;; -- BTW, this should not happen :-)
          (error "New workout is not in the expected library")))

    (define/public (after-update-workout workout-id)
      ;; An updated workout might change libraries -- find the library, select
      ;; it, refresh the workouts, find the workout and select it.
      (define row (get-workout-row database workout-id))
      (define library-id (vector-ref row 5))
      (refresh-workout-libraries library-id workout-id))

    (define/public (can-delete-workout? workout-id)
      ;; We can delete the currently selected workout.  Note that prompting
      ;; the user for deletion and other checks are done elsewhere
      (eqv? (get-selected-workout-id) workout-id))

    (define/public (after-delete-workout workout-id)
      ;; note that it is still present in the workout list
      (if (eqv? (get-selected-workout-id) workout-id)
          ;; if the current workout was deleted, just remove the entry
          (let ((index (send workouts-list get-selected-row-index)))
            (send workout-editor set-workout #f) ; close any unsaved edits
            (send workouts-list delete-row index)
            ;; Select the next workout
            (let* ((nindex (min index (sub1 (send workouts-list get-row-count))))
                   (data (send workouts-list get-data-for-row nindex)))
              (send workouts-list select-row nindex)
              (on-workout-selected nindex data)))
          ;; refresh the entire libary
          (on-library-selected (send library-choice get-selection))))

    ))
