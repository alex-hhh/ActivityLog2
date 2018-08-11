#lang racket/base
;; activity-edit.rkt -- implement operations on an activity
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015, 2018 Alex Harsanyi <AlexHarsanyi@gmail.com>
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
         "../database.rkt"
         "../sport-charms.rkt"
         "../utilities.rkt"
         "../session-df.rkt"
         "../data-frame/csv.rkt"
         "../data-frame/gpx.rkt")

;; (lazy-require
;;  ("edit-session-summary.rkt" (get-edit-session-summary-dialog))
;;  ("edit-session-tss.rkt" (get-edit-session-tss-dialog))
;;  ("edit-session-weather.rkt" (get-weather-editor))
;;  ("edit-lap-swim.rkt" (get-lap-swim-editor))
;;  ("gpx.rkt" (df-write/gpx))
;;  ("elevation-correction.rkt" (interactive-fixup-elevation
;;                               clear-corrected-elevation-for-session)))

(require
 "edit-session-summary.rkt"
 "edit-session-tss.rkt"
 "edit-session-weather.rkt"
 "edit-lap-swim.rkt"
 "../elevation-correction.rkt")

(provide activity-operations<%>)
(provide activity-operations-menu%)
(provide set-inspect-callback)


;;............................................ activity-operations-menu% ....

;; This is the interface expected by the activity-operations-menu% allowing it
;; to get all the information it needs to do its work.
(define activity-operations<%>
  (interface ()
    get-top-level-window
    get-database
    get-selected-sid
    get-selected-guid
    get-selected-sport
    after-update
    can-delete?
    after-delete
    after-new
    before-popup
    after-popdown))

(define the-inspect-callback #f)

(define (set-inspect-callback cb)
  (set! the-inspect-callback cb))

(define (get-session-headline db sid)
  (let ((row (query-row db "
select ifnull(S.name, 'unnamed'), S.sport_id, S.sub_sport_id
  from A_SESSION S
 where S.id = ?" sid)))
    (let ((name (vector-ref row 0))
          (sport (vector-ref row 1))
          (sub-sport (vector-ref row 2)))
      (format "~a (~a)" name
              (get-sport-name (if (sql-null? sport) #f sport)
                              (if (sql-null? sub-sport) #f sub-sport))))))

(define (get-activity-original-file-name db guid)
  (query-value
   db "select file_name
         from ACTIVITY A, ACTIVITY_RAW_DATA ARD
        where A.guid = ? and ARD.activity_id = A.id" guid))

(define activity-operations-menu%
  (class object%
    (init-field target)
    (init [menu-bar #f])
    (super-new)

    (unless (is-a? target activity-operations<%>)
      (error "Target must implement the activity-operations<%> interface"))

    (define (on-demand m)
      (send target before-popup)
      ;; Enable/disable appropiate menus
      (let ((sid (and target (send target get-selected-sid)))
            (have-sid? (and target (number? (send target get-selected-sid))))
            (have-guid? (and target (string? (send target get-selected-guid))))
            (is-lap-swim? (and target (equal? (send target get-selected-sport) '(5 . 17)))))
        (send inspect-menu-item enable (and have-sid? the-inspect-callback))
        (send edit-menu-item enable have-sid?)
        (send fixup-elevation-menu-item enable have-sid?)
        (send clear-elevation-menu-item enable have-sid?)
        (send delete-menu-item enable
              (and have-sid? (send target can-delete? sid)))
        (send copy-guid-menu-item enable have-guid?)
        (send copy-sid-menu-item enable have-sid?)
        (send edit-tss-menu-item enable have-sid?)
        (send edit-weather-menu-item enable have-sid?)
        (send edit-lap-swim-menu-item enable is-lap-swim?)
        ;; TODO: we need to enable it only if there's an actual file to export.
        (send export-original-menu-item enable have-sid?)
        (send export-csv-menu-item enable have-sid?)
        (send export-gpx-menu-item enable have-sid?)))

    (define (on-popdown m e)
      (send target after-popdown))

    (define (on-inspect m e)
      (the-inspect-callback (send target get-selected-sid)))

    (define (on-edit m e)
      (let ((sid (send target get-selected-sid))
            (db (send target get-database))
            (toplevel (send target get-top-level-window)))
        (when (send (get-edit-session-summary-dialog) show-dialog toplevel db sid)
          (clear-session-df-cache sid) ; remove this session from the cache
          (log-event 'session-updated sid)
          (send target after-update sid))))

    (define (on-new m e)
      (let ((db (send target get-database))
            (toplevel (send target get-top-level-window)))
        (let ((sid (send (get-edit-session-summary-dialog) show-dialog toplevel db #f)))
          (when sid
            (log-event 'session-created sid)
            (send target after-new sid)))))

    (define (on-fixup-elevation m e)
      (let ((sid (send target get-selected-sid))
            (db (send target get-database))
            (toplevel (send target get-top-level-window)))
        (clear-session-df-cache sid) ; remove this session from the cache
        (interactive-fixup-elevation db sid toplevel)
        (log-event 'session-updated sid)
        (log-event 'session-updated-data sid)
        (send target after-update sid)))

    (define (on-clear-corrected-elevation m e)
      (let ((sid (send target get-selected-sid))
            (db (send target get-database))
            (toplevel (send target get-top-level-window)))
        (let ((mresult (message-box/custom
                        "Confirm clear corrected elevation"
                        (format "Really clear the corrected elevation for \"~a\"?~%You can re-create this data again using Fixup Elevation."
                                (get-session-headline db sid))
                        #f "Clear" "Cancel" toplevel '(caution default=3))))
          (when (equal? mresult 2)
            (clear-session-df-cache sid) ; remove this session from the cache
            (clear-corrected-elevation-for-session db sid)
            (log-event 'session-updated sid)
            (log-event 'session-updated-data sid)
            (send target after-update sid)))))

    (define (on-edit-weather m e)
      (let ((sid (send target get-selected-sid))
            (db (send target get-database))
            (toplevel (send target get-top-level-window)))
        (when (send (get-weather-editor) begin-edit toplevel db sid)
          ;; NOTE: weather-data-changed event is raised by the weather editor!
          (send target after-update sid))))

    (define (on-edit-lap-swim m e)
      (let ((sid (send target get-selected-sid))
            (sport (send target get-selected-sport))
            (db (send target get-database))
            (toplevel (send target get-top-level-window)))
        (when (equal? sport '(5 . 17))  ; lap swimming
          (when (send (get-lap-swim-editor) begin-edit toplevel db sid)
            (clear-session-df-cache sid) ; remove this session from the cache
            (log-event 'session-updated sid)
            (log-event 'session-updated-data sid)
            (send target after-update sid)))))

    (define (on-edit-tss m e)
      (let ((sid (send target get-selected-sid))
            (db (send target get-database))
            (toplevel (send target get-top-level-window)))
        (when (send (get-edit-session-tss-dialog) run toplevel db sid)
          (log-event 'session-updated sid)
          (send target after-update sid))))

    (define (on-delete m e)
      (let ((sid (send target get-selected-sid))
            (db (send target get-database))
            (toplevel (send target get-top-level-window)))
        (when (send target can-delete? sid)
          (let ((mresult (message-box/custom
                          "Confirm delete"
                          (format "Really delete activity \"~a\"?~%This cannot be undone."
                                  (get-session-headline db sid))
                          #f "Delete" "Cancel" toplevel '(caution default=3))))
            (when (equal? mresult 2)
              (db-delete-session sid db)
              (clear-session-df-cache sid) ; remove this session from the cache
              (log-event 'session-deleted sid)
              (send target after-delete sid))))))

    (define (on-copy-guid-to-clipboard m e)
      (send the-clipboard set-clipboard-string
            (send target get-selected-guid)
            (send e get-time-stamp)))

    (define (on-copy-sid-to-clipboard m e)
      (send the-clipboard set-clipboard-string
            (format "~s" (send target get-selected-sid))
            (send e get-time-stamp)))

    (define (on-export-original-file m e)
      (let* ((guid (send target get-selected-guid))
             (db (send target get-database))
             (toplevel (send target get-top-level-window))
             (aid (query-value db "select id from ACTIVITY where guid = ?" guid))
             (fname (get-activity-original-file-name db guid))
             (file (put-file "Select file to export to" toplevel #f fname #f '()
                             '(("Any" "*.*")))))
        (when file
          (db-export-raw-data aid db file))))

    (define (on-export-csv m e)
      (let ((sid (send target get-selected-sid))
            (db (send target get-database))
            (toplevel (send target get-top-level-window)))
        (let ((df #f) (fname #f))
          ;; Triksy Hobbit, fetch the data frame while the user is selecting
          ;; the file name, so that response time is improved (we will do some
          ;; unnecessary work if the user changes their mind.)
          (queue-task "activity-edit/on-export-csv"
                      (lambda ()
                        (let ((df1 (session-df db sid)))
                          (queue-callback
                           (lambda ()
                             (set! df df1))))))
          (set! fname
                (put-file "Select file to export to" toplevel #f
                          (format "track-data-~a.csv" sid) ".csv" '()
                          '(("CSV files" "*.csv") ("Any" "*.*"))))
          (when fname
            (unless df      ; wait for the data frame if it did not arrive yet
              (for ((_ (in-range 20)) #:unless df) (sleep/yield 0.1)))
            (if df
                (let* ((sn (get-series/ordered df)))
                  (call-with-output-file fname (lambda (port) (apply df-write/csv df port sn))
                    #:mode 'text #:exists 'truncate/replace ))
                (message-box "Failed to fetch data frame" "Failed to fetch data frame"
                             toplevel '(ok stop)))))))

    (define (on-export-gpx m e)
      (let ((sid (send target get-selected-sid))
            (db (send target get-database))
            (toplevel (send target get-top-level-window)))
        (let ((df #f) (fname #f))
          ;; Triksy Hobbit, fetch the data frame while the user is selecting
          ;; the file name, so that response time is improved (we will do some
          ;; unnecessary work if the user changes their mind.)
          (queue-task "activity-edit/on-export-gpx"
                      (lambda ()
                        (let ((df1 (session-df db sid)))
                          (queue-callback
                           (lambda ()
                             (set! df df1))))))
          (set! fname
                (put-file "Select file to export to" toplevel #f
                          (format "track-data-~a.gpx" sid) ".gpx" '()
                          '(("GPX files" "*.gpx") ("Any" "*.*"))))
          (when fname
            (unless df      ; wait for the data frame if it did not arrive yet
              (for ((_ (in-range 20)) #:unless df) (sleep/yield 0.1)))
            (if df
                (call-with-output-file fname
                  (lambda (port)
                    (df-write/gpx df port #:name (get-session-headline db sid)))
                  #:mode 'text #:exists 'truncate/replace )
                (message-box "Failed to fetch data frame" "Failed to fetch data frame"
                             toplevel '(ok stop)))))))

    (define (switch-to-view m e)
      (send target switch-to-view))

    (define the-menu
      (if menu-bar
          (new menu% [parent menu-bar] [label "&Activity"]
               [demand-callback on-demand])
          (new popup-menu% [title "Activtiy"]
               [demand-callback on-demand]
               [popdown-callback on-popdown])))

    (define (make-menu-item label callback [shortcut #f])
      (new menu-item% [parent the-menu] [label label]
           [callback callback] [shortcut shortcut]))

    (define switch-to-view-menu-item
      (if menu-bar
          (make-menu-item "Switch to Activity View" switch-to-view)
          #f))
    (when menu-bar
      (new separator-menu-item% [parent the-menu]))
    (define inspect-menu-item
      (make-menu-item "Inspect ..." on-inspect))
    (define edit-menu-item
      (make-menu-item "Edit ..." on-edit #\E))
    (define fixup-elevation-menu-item
      (make-menu-item "Fixup elevation ..." on-fixup-elevation))
    (define clear-elevation-menu-item
      (make-menu-item "Clear corrected elevation ..." on-clear-corrected-elevation))
    (define edit-tss-menu-item
      (make-menu-item "Edit effort ..." on-edit-tss))
    (define edit-weather-menu-item
      (make-menu-item "Edit weather ..." on-edit-weather))
    (define edit-lap-swim-menu-item
      (make-menu-item "Edit lap swim ..." on-edit-lap-swim))
    (new separator-menu-item% [parent the-menu])
    (define new-menu-item
      (make-menu-item "New activity ..." on-new #\N))
    (new separator-menu-item% [parent the-menu])
    (define delete-menu-item
      (make-menu-item "Delete ..." on-delete))
    (new separator-menu-item% [parent the-menu])
    (define copy-guid-menu-item
      (make-menu-item "Copy GUID to clipboard" on-copy-guid-to-clipboard))
    (define copy-sid-menu-item
      (make-menu-item "Copy session id to clipboard" on-copy-sid-to-clipboard))
    (define export-original-menu-item
      (make-menu-item "Export original file..." on-export-original-file))
    (define export-csv-menu-item
      (make-menu-item "Export track data (CSV)..." on-export-csv))
    (define export-gpx-menu-item
      (make-menu-item "Export track data (GPX)..." on-export-gpx))

    (define/public (get-popup-menu) the-menu)

    ))
