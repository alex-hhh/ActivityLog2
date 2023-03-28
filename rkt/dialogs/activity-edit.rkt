#lang racket/base
;; activity-edit.rkt -- implement operations on an activity
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015, 2018, 2020, 2021, 2022, 2023 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require data-frame
         data-frame/gpx
         db/base
         racket/class
         racket/gui/base
         racket/port
         "../al-widgets.rkt"
         "../database.rkt"
         "../session-df/session-df.rkt"
         "../sport-charms.rkt"
         "../utilities.rkt")

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
 "../models/ec-util.rkt"
 "fthr-analysis.rkt"
 "power-spikes.rkt")

(provide activity-operations<%>)
(provide activity-operations-menu%)


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
    inspect-session
    after-update
    can-delete?
    after-delete
    after-new
    before-popup
    after-popdown
    get-aerolab-analysis-status
    show-or-hide-aerolab-tab
    ))

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
      (let* ((sport (send target get-selected-sport))
             (sid (send target get-selected-sid))
             (have-sid? (number? (send target get-selected-sid)))
             (have-guid? (string? (send target get-selected-guid)))
             (is-lap-swim? (equal? sport '(5 . 17))))
        (send inspect-menu-item enable have-sid?)
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
        (send export-gpx-menu-item enable have-sid?)
        ;; FTHR analysis is only available for bike and run activities
        (send fthr-menu-item enable (and have-sid? sport (member (car sport) '(1 2))))
        ;; Power Spikes is only available for bike activities.  Only
        ;; activities with power -- but we might need to load the session to
        ;; know that the activity has power data or not, so we just check if
        ;; this is a cycling activity.
        (send power-spikes-menu-item enable (and have-sid? sport (equal? (car sport) 2)))
        (send df-describe-menu-item enable have-sid?)

        (when aerolab-menu-item
          (let ([aerolab-status (send target get-aerolab-analysis-status)])
            (case aerolab-status
              ((none)
               (send aerolab-menu-item enable #f)
               (send aerolab-menu-item set-label "Show Aerolab Analysis..."))
              ((enable)
               (send aerolab-menu-item enable #t)
               (send aerolab-menu-item set-label "Show Aerolab Analysis..."))
              ((disable)
               (send aerolab-menu-item enable #t)
               (send aerolab-menu-item set-label "Hide Aerolab Analysis...")))))

        ))

    (define (on-popdown m e)
      (send target after-popdown))

    (define (on-inspect m e)
      (send target inspect-session (send target get-selected-sid)))

    (define (on-edit m e)
      (let ((sid (send target get-selected-sid))
            (db (send target get-database))
            (toplevel (send target get-top-level-window)))
        (when (send (get-edit-session-summary-dialog) show-dialog toplevel db sid)
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
            (clear-corrected-elevation-for-session db sid)
            (log-event 'session-updated sid)
            (log-event 'session-updated-data sid)
            (send target after-update sid)))))

    (define (on-edit-weather m e)
      (let ((sid (send target get-selected-sid))
            (db (send target get-database))
            (toplevel (send target get-top-level-window)))
        (define c (query-value db "select count(*) from SESSION_WEATHER where session_id = ?" sid))
        (if (> c 1)
            ;; NOTE: this is a limitation of the application: FIT files can
            ;; have multiple weather records, all of them imported in the DB,
            ;; but the weather editor was designed to handle a single record
            ;; when fetching weather data from an online service.
            (message-box/custom
             "Cannot edit weather"
             (format "Cowardly refusing to edit weather for ~a: multiple weather records are present"
                     (get-session-headline db sid))
             #f #f "OK" toplevel '(stop default=3))
            (when (send (get-weather-editor) begin-edit toplevel db sid)
              ;; NOTE: weather-data-changed event is raised by the weather editor!
              (send target after-update sid)))))

    (define (on-edit-lap-swim m e)
      (let ((sid (send target get-selected-sid))
            (sport (send target get-selected-sport))
            (db (send target get-database))
            (toplevel (send target get-top-level-window)))
        (when (equal? sport '(5 . 17))  ; lap swimming
          (when (send (get-lap-swim-editor) begin-edit toplevel db sid)
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

    (define (on-show-session-data-frame-summary m e)
      (let ((sid (send target get-selected-sid))
            (db (send target get-database))
            (toplevel (send target get-top-level-window)))
        (when (and sid db toplevel)
          (define df (session-df db sid))
          (define text (call-with-output-string
                        (lambda (out)
                          (parameterize ([current-output-port out])
                            (df-describe df)))))
          (send (get-text-export-dialog)
                show-dialog
                toplevel
                (format "df-describe sid = ~a" sid)
                text
                #:width 1000 #:height 600))))

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
                (message-box "Failed to fetch data frame" "Failed to fetch data frame (timeout?)"
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
                (with-handlers
                  ;; GPX export will fail if some data series are missing
                  ;; (e.g. lat, lon)
                  ((exn? (lambda (e)
                           (message-box "GPX Export Failed" (exn-message e) toplevel '(ok stop)))))
                  (call-with-output-file fname
                    (lambda (port)
                      (df-write/gpx df port #:name (get-session-headline db sid)))
                    #:mode 'text #:exists 'truncate/replace ))
                (message-box "Failed to fetch data frame" "Failed to fetch data frame (timeout?)"
                             toplevel '(ok stop)))))))

    (define (on-fthr-analysis m e)
      (let ((sid (send target get-selected-sid))
            (db (send target get-database))
            (toplevel (send target get-top-level-window)))
        (show-fthr-analisys-dashboard toplevel db sid)))

    (define (on-power-spikes m e)
      (let ([sid (send target get-selected-sid)]
            [db (send target get-database)]
            [toplevel (send target get-top-level-window)])
        (show-power-spikes-dashboard toplevel db sid)))

    (define (on-show-hide-aerolab m e)
      (send target show-or-hide-aerolab-tab))

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
    (define fthr-menu-item
      (make-menu-item "FTHR Analysis ..." on-fthr-analysis))
    (new separator-menu-item% [parent the-menu])
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
    (define power-spikes-menu-item
      (make-menu-item "Clear Power Spikes ..." on-power-spikes))

    ;; NOTE: only show the Aerolab item on the menu, not the right-click menus
    ;; in the "Activities" or "Calendar" views.
    (define aerolab-menu-item
      (if menu-bar
          (make-menu-item "Show Aerolab Analysis..." on-show-hide-aerolab)
          #f))

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
    (define df-describe-menu-item
      (make-menu-item "Show session data frame summary..." on-show-session-data-frame-summary))
    (define export-original-menu-item
      (make-menu-item "Export original file..." on-export-original-file))
    (define export-csv-menu-item
      (make-menu-item "Export track data (CSV)..." on-export-csv))
    (define export-gpx-menu-item
      (make-menu-item "Export track data (GPX)..." on-export-gpx))

    (define/public (get-popup-menu) the-menu)

    ))
