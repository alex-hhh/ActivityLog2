#lang racket/base
;; toplevel.rkt -- toplevel form for the application
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
         framework/splash
         racket/class
         racket/gui/base
         racket/math
         "database.rkt"
         "dbapp.rkt"
         "dbutil.rkt"
         "dialogs/about-frame.rkt"
         "dialogs/activity-edit.rkt"
         "dialogs/edit-cp.rkt"
         "dialogs/edit-labels.rkt"
         "dialogs/edit-preferences.rkt"
         "dialogs/edit-seasons.rkt"
         "dialogs/edit-sz.rkt"
         "dialogs/export-fit-settings.rkt"
         "elevation-correction.rkt"
         "import.rkt"
         "metrics.rkt"
         "session-df.rkt"
         "session-inspector/view-session.rkt"
         "time-in-zone.rkt"
         "trend-charts/view-trends.rkt"
         "utilities.rkt"
         "view-activities.rkt"
         "view-athlete-metrics.rkt"
         "view-calendar.rkt"
         "view-equipment.rkt"
         "view-last-import.rkt"
         "view-reports.rkt"
         "weather.rkt"
         "widgets/main.rkt"
         "widgets/map-widget/map-tiles.rkt"
         "widgets/map-widget/map-util.rkt"
         "workout-editor/view-workouts.rkt")

(provide toplevel-window%)

(define (make-dbglog-sink . loggers)
  (define sources (for/list ([logger loggers])
                    (make-log-receiver logger 'info)))
  (define (do-logging)
    (let* ((log-item (apply sync sources))
           (level (vector-ref log-item 0))
           (message (vector-ref log-item 1)))
      (dbglog "~a: ~a" level message))
    (do-logging))
  (thread do-logging)
  (void))

;; notifications from our loggers go do the dbglog sink
(make-dbglog-sink map-widget-logger user-notification-logger)

;; Create a notification banner
(define (make-log-output-window parent)

  (define banner (new notification-banner% [parent parent]))

  (define (insert-log-messages source)
    (let* ((log-item (sync source))
           (level (vector-ref log-item 0))
           (message (vector-ref log-item 1)))
      (queue-callback
       (lambda () (send banner notify message)))
      (insert-log-messages source)))

  (define receiver (make-log-receiver user-notification-logger 'info))
  (thread (lambda () (insert-log-messages receiver)))
  (void))


;;....................................................... make-file-menu ....

(define (make-file-menu menu-bar toplevel)

  ;; Return the GUI object that can export data.  We search from the focus
  ;; window through the parents for an object that has an
  ;; 'interactive-export-image method.
  (define (get-data-exporter)

    (define (can-export-data? o)
      (object-method-arity-includes? o 'interactive-export-data 1))

    (define (search o)
      (if (can-export-data? o)
          o
          (let ((parent (send o get-parent)))
            (if parent (search parent) #f))))
    (let ((w (send (send toplevel get-frame) get-focus-window)))
      (if w (search w) #f)))

  (define (data-exporter-demand-cb m)
    (let ((e (get-data-exporter)))
      (send m enable (not (eq? e #f)))))

  ;; Return the GUI object that can export images.  We search from the focus
  ;; window through the parents for an object that has an
  ;; 'interactive-export-image method.
  (define (get-img-exporter)

    (define (can-export-image? o)
      (object-method-arity-includes? o 'interactive-export-image 0))

    (define (search o)
      (if (can-export-image? o)
          o
          (let ((parent (send o get-parent)))
            (if parent (search parent) #f))))
    (let ((w (send (send toplevel get-frame) get-focus-window)))
      (if w (search w) #f)))

  (define (img-exporter-demand-cb m)
    (let ((e (get-img-exporter)))
      (send m enable (not (eq? e #f)))))

  ;; Return the GUI object that can export SQL queries.  We search from the
  ;; focus window through the parents for an object that has an
  ;; 'interactive-export-sql-query method.
  (define (get-sql-query-exporter)
    (define (can-export-sql-query? o)
      (object-method-arity-includes? o 'interactive-export-sql-query 0))
    (define (search o)
      (if (can-export-sql-query? o)
          o
          (let ((parent (send o get-parent)))
            (if parent (search parent) #f))))
    (let ((w (send (send toplevel get-frame) get-focus-window)))
      (if w (search w) #f)))

  (define (sql-query-exporter-demand-cb m)
    (let ((e (get-sql-query-exporter)))
      (send m enable (not (eq? e #f)))))

  ;; Return the GUI object that can generate heat maps.  We search from the
  ;; focus window through the parents for an object that has an
  ;; 'interactive-generate-heatmap' method.
  (define (get-heatmap-generator)
    (define (can-generate-heatmap? o)
      (object-method-arity-includes? o 'interactive-generate-heatmap 0))
    (define (search o)
      (if (can-generate-heatmap? o)
          o
          (let ((parent (send o get-parent)))
            (if parent (search parent) #f))))
    (let ((w (send (send toplevel get-frame) get-focus-window)))
      (if w (search w) #f)))

  (define (generate-heatmap-demand-cb m)
    (let ((e (get-heatmap-generator)))
      (send m enable (not (eq? e #f)))))

  (define file-menu (new menu% [parent menu-bar] [label "&File"]))

  (new menu-item%
       [parent file-menu] [label "&New database..."]
       [callback (lambda (m e) (send toplevel on-new-database))])

  (new menu-item%
       [parent file-menu] [label "&Open database..."]
       [shortcut #\O]
       [callback (lambda (m e) (send toplevel on-open-database))])

  (new separator-menu-item% [parent file-menu])

  (new menu-item%
       [parent file-menu] [label  "Import &activity..."]
       [callback (lambda (m e) (send toplevel on-import-activity))])

  (new menu-item%
       [parent file-menu] [label  "Import from &directory..."]
       [shortcut #\I]
       [callback (lambda (m e) (send toplevel on-import-from-directory))])

  (new separator-menu-item% [parent file-menu])

  (new menu-item%
       [parent file-menu] [label "Export data (formatted)..."]
       [demand-callback data-exporter-demand-cb]
       [callback
        (lambda (m e)
          (let ((q (get-data-exporter)))
            (when q (send q interactive-export-data #t))))])

  (new menu-item%
       [parent file-menu] [label "Export data (unformatted)..."]
       [demand-callback data-exporter-demand-cb]
       [callback
        (lambda (m e)
          (let ((q (get-data-exporter)))
            (when q (send q interactive-export-data #f))))])

  (new menu-item%
       [parent file-menu] [label "Export image ..."]
       [demand-callback img-exporter-demand-cb]
       [callback
        (lambda (m e)
          (let ((w (get-img-exporter)))
            (when w (send w interactive-export-image))))])

  (new menu-item%
       [parent file-menu] [label "Export SQL query ..."]
       [demand-callback sql-query-exporter-demand-cb]
       [callback
        (lambda (m e)
          (let ((w (get-sql-query-exporter)))
            (when w (send w interactive-export-sql-query))))])

  (new menu-item%
       [parent file-menu] [label "Generate HeatMap ..."]
       [demand-callback generate-heatmap-demand-cb]
       [callback
        (lambda (m e)
          (let ((w (get-heatmap-generator)))
            (when w (send w interactive-generate-heatmap))))])

  (new separator-menu-item% [parent file-menu])

  (new menu-item%
       [parent file-menu] [label "E&xit"]
       [shortcut #\Q]
       [callback (lambda (m e) (send toplevel on-exit))])

  file-menu)


;;....................................................... make-edit-menu ....

(define (make-edit-menu menu-bar toplevel)

  (define edit-menu (new menu% [parent menu-bar] [label "&Edit"]))
  (append-editor-operation-menu-items edit-menu #t)

  (new separator-menu-item% [parent edit-menu])

  (new menu-item%
       [parent edit-menu] [label "Edit Labels..."]
       [callback
        (lambda (m e)
          (send (get-label-editor) show-dialog
                (send toplevel get-frame)
                (send toplevel get-database)))])

  (new menu-item%
       [parent edit-menu] [label "Edit Seasons..."]
       [callback
        (lambda (m e)
          (send (get-season-editor) show-dialog
                (send toplevel get-frame)
                (send toplevel get-database)))])

  (new separator-menu-item% [parent edit-menu])

  (new menu-item%
       [parent edit-menu] [label "Edit Preferences..."]
       [shortcut #\;]
       [callback
        (lambda (m e)
          (when (send (get-preferences-dialog) run
                      (send toplevel get-frame))
            (send toplevel refresh-current-view)))])

  edit-menu)


;;....................................................... make-view-menu ....

(define (make-view-menu menu-bar toplevel the-sections)

  (define (is-qresults-object? o)
    (object-method-arity-includes? o 'get-qresults-object 0))

  (define (qresults-demand-cb m)
    (let ((w (send (send toplevel get-frame) get-focus-window)))
      (send m enable (and w (is-qresults-object? w)))))

  (define (get-qresults-object)
    (let ((w (send (send toplevel get-frame) get-focus-window)))
      (if (and w (is-qresults-object? w))
          (send w get-qresults-object)
          #f)))


  (define view-menu (new menu% [parent menu-bar] [label "&View"]))

  (new menu-item%
       [parent view-menu] [label "Refresh current view"]
       [shortcut #\R]
       [callback
        (lambda (m e)
          (send toplevel refresh-current-view))])

  (new separator-menu-item% [parent view-menu])

  (new menu-item%
       [parent view-menu] [label "Setup columns..."]
       [demand-callback qresults-demand-cb]
       [callback
        (lambda (m e)
          (let ((q (get-qresults-object)))
            (when q (send q interactive-setup-visible-columns))))])

  (new separator-menu-item% [parent view-menu])

  (for ((s the-sections))
    (new menu-item%
         [parent view-menu]
         [label (tl-section-name s)]
         [callback
          (lambda (m e)
            (send toplevel select-section (tl-section-tag s)))]))

  view-menu)


;;.................................................... make-athlete-menu ....

(define (make-athlete-menu menu-bar target)

  (define operations-menu
    (new athlete-metrics-operations-menu% [menu-bar menu-bar] [target target]))

  (let ((menu (send operations-menu get-popup-menu)))
    (new separator-menu-item% [parent menu])
    (new menu-item%
         [parent menu] [label "Edit Sport Zones..."]
         [callback
          (lambda (m e)
            (send (get-sz-editor) show-dialog
                  (send target get-top-level-window)
                  (send target get-database)))])
    (new menu-item%
         [parent menu] [label "Edit Critical Power..."]
         [callback
          (lambda (m e)
            (send (get-cp-editor) show-dialog
                  (send target get-top-level-window)
                  (send target get-database)))])
    (new menu-item%
         [parent menu] [label "Export Settings to Device..."]
         [callback
          (lambda (m e)
            (send (get-export-settings-dialog) show-dialog
                  (send target get-top-level-window)
                  (send target get-database)))])
    menu))


;;................................................... make-activtiy-menu ....

(define (make-activtiy-menu menu-bar target)
  (define activity-menu
    (new activity-operations-menu% [menu-bar menu-bar] [target target]))
  (send activity-menu get-popup-menu))


;;.................................................... make-workout-menu ....

(define (make-workout-menu menu-bar target)
  (define workout-menu
    (new workout-operations-menu% [menu-bar menu-bar] [target target]))
  (send workout-menu get-popup-menu))


;;...................................................... make-tools-menu ....

(define (make-tools-menu menu-bar toplevel)
  (define tools-menu (new menu% [parent menu-bar] [label "&Tools"]))

  (new menu-item%
       [parent tools-menu] [label "Rebuild elevation data..."]
       [callback
        (lambda (m e)
          (send toplevel rebuild-elevation-data))])

  (new menu-item%
       [parent tools-menu] [label "Rebuild metrics..."]
       [callback
        (lambda (m e)
          (send toplevel rebuild-time-in-zone-data))])

  (new menu-item%
       [parent tools-menu] [label "Optimize database..."]
       [callback
        (lambda (m e)
          (send toplevel vacuum-database))])

  )


;....................................................... make-help-menu ....

(define (make-help-menu menu-bar toplevel)
  (define help-menu (new menu% [parent menu-bar] [label "&Help"]))

  (new menu-item%
       [parent help-menu] [label "About..."]
       [callback
        (lambda (m e) (send toplevel on-show-about))]))


;;........................................ activity-operations-forwarder ....

;; Forward the activity operations to the selected section in the toplevel
;; window.  This is used by the toplevel menubar to send the menu commands to
;; the correct section (e.g. Activity List, Import View, etc)

(define activity-operations-forwarder%
  (class* object% (activity-operations<%>)
    (init-field toplevel-application)
    (super-new)

    (define/public (get-top-level-window)
      (send toplevel-application get-frame))

    (define/public (get-database)
      (send toplevel-application get-database))

    ;; Return the selected section, but only if it implements the
    ;; activity-operations<%> interface, return #f otherwise.  The activity
    ;; menus will be disabled if the selected section does not support
    ;; activity operations.
    (define (get-target-section)
      (let ((section (send toplevel-application get-selected-section)))
        (if (and section (is-a? section activity-operations<%>))
            section
            #f)))

    (define/public (get-selected-sid)
      (let ((target (get-target-section)))
        (and target (send target get-selected-sid))))

    (define/public (get-selected-sport)
      (let ((target (get-target-section)))
        (and target (send target get-selected-sport))))

    (define/public (get-selected-guid)
      (let ((target (get-target-section)))
        (and target (send target get-selected-guid))))

    (define/public (after-update sid)
      (let ((target (get-target-section)))
        (and target (send target after-update sid))))

    (define/public (after-new sid)
      (let ((target (get-target-section)))
        (and target (send target after-new sid))))

    (define/public (can-delete? sid)
      (let ((target (get-target-section)))
        (and target (send target can-delete? sid))))

    (define/public (after-delete sid)
      (let ((target (get-target-section)))
        (and target (send target after-delete sid))))

    (define/public (before-popup)
      (let ((target (get-target-section)))
        (and target (send target before-popup))))

    (define/public (after-popdown)
      (let ((target (get-target-section)))
        (and target (send target before-popup))))

    (define/public (switch-to-view)
      (send toplevel-application select-section 'activity-list))

    ))


;;........................................... athlete-metrics-forwarder% ....

;; Forward methods from the athlete-metrics-operations<%> interface to the
;; athlete view, if it is the selected section.  This acts as a "glue" between
;; the Athlete menu and the view-athlete-metrics% object instance.
(define athlete-metrics-forwarder%
  (class* object% (athlete-metrics-operations<%>)
    (init-field toplevel-application)
    (super-new)

    (define/public (get-top-level-window)
      (send toplevel-application get-frame))

    (define/public (get-database)
      (send toplevel-application get-database))

    ;; Return the selected section, but only if it implements the
    ;; athlete-metrics-operations<%> interface, return #f otherwise.  The
    ;; activity menus will be disabled if the selected section does not
    ;; support activity operations.
    (define (get-target-section)
      (let ((section (send toplevel-application get-selected-section)))
        (if (and section (is-a? section athlete-metrics-operations<%>))
            section
            #f)))

    (define/public (get-selected-id)
      (let ((target (get-target-section)))
        (and target (send target get-selected-id))))

    (define/public (after-update id)
      (let ((target (get-target-section)))
        (and target (send target after-update id))))

    (define/public (after-new id)
      (let ((target (get-target-section)))
        (and target (send target after-new id))))

    (define/public (can-delete? id)
      (let ((target (get-target-section)))
        (and target (send target can-delete? id))))

    (define/public (after-delete id)
      (let ((target (get-target-section)))
        (and target (send target after-delete id))))

    (define/public (before-popup)
      (let ((target (get-target-section)))
        (and target (send target before-popup))))

    (define/public (after-popdown)
      (let ((target (get-target-section)))
        (and target (send target before-popup))))

    (define/public (switch-to-view)
      (send toplevel-application select-section 'athlete-metrics))))


;;.................................................. workouts-forwarder% ....

;; Forward methods from the workout-operations<%> interface to the workout
;; view, if it is the selected section.  This acts as a "glue" between the
;; Workout menu and the view-workouts% object instance.
(define workout-forwarder%
  (class* object% (workout-operations<%>)
    (init-field toplevel-application)
    (super-new)

    (define/public (get-top-level-window)
      (send toplevel-application get-frame))

    (define/public (get-database)
      (send toplevel-application get-database))

    ;; Return the selected section, but only if it implements the
    ;; workout-operations<%> interface, return #f otherwise.  The workout menu
    ;; items will be disabled if the selected section does not support workout
    ;; operations.
    (define (get-target-section)
      (let ((section (send toplevel-application get-selected-section)))
        (if (and section (is-a? section workout-operations<%>))
            section
            #f)))

    (define/public (before-popup)
      (let ((target (get-target-section)))
        (and target (send target before-popup))))

    (define/public (after-popdown)
      (let ((target (get-target-section)))
        (and target (send target after-popdown))))

    (define/public (switch-to-view)
      (send toplevel-application select-section 'workouts))

    (define/public (get-selected-library-id)
      (let ((target (get-target-section)))
        (and target (send target get-selected-library-id))))

    (define/public (get-selected-workout-id)
      (let ((target (get-target-section)))
        (and target (send target get-selected-workout-id))))

    (define/public (after-new-library library-id)
      (let ((target (get-target-section)))
        (and target (send target after-new-library library-id))))

    (define/public (after-update-library library-id)
      (let ((target (get-target-section)))
        (and target (send target after-update-library library-id))))

    (define/public (can-delete-library? library-id)
      (let ((target (get-target-section)))
        (and target (send target can-delete-library? library-id))))

    (define/public (after-delete-library library-id)
      (let ((target (get-target-section)))
        (and target (send target after-delete-library library-id))))

    (define/public (after-new-workout workout-id)
      (let ((target (get-target-section)))
        (and target (send target after-new-workout workout-id))))

    (define/public (after-update-workout workout-id)
      (let ((target (get-target-section)))
        (and target (send target after-update-workout workout-id))))

    (define/public (can-delete-workout? workout-id)
      (let ((target (get-target-section)))
        (and target (send target can-delete-workout? workout-id))))

    (define/public (after-delete-workout workout-id)
      (let ((target (get-target-section)))
        (and target (send target after-delete-workout workout-id))))

    ))



;;....................................................... Open db dialog ....

(define (interactive-open-database database-path)

  (define frame #f)
  (define message-field #f)
  (define progress-bar #f)
  (define last-msg #f)

  (define (make-frame)
    (shutdown-splash)
    (close-splash)
    (set! frame (new frame%
                     [width 400]
                     [height 250]
                     [stretchable-width #f]
                     [stretchable-height #f]
                     [label (if (file-exists? database-path)
                                "Opening database"
                                "Creating database")]))
    (let ((pane (new horizontal-pane% [parent frame] [border 20] [spacing 20])))
      (new message% [parent pane] [label (sql-export-icon)]
           [stretchable-height #f] [stretchable-width #f])
      (let ((pane (new vertical-pane% [parent pane] [spacing 20] [alignment '(left top)])))
        (set! message-field (new message% [parent pane] [label ""] [min-width 200]))
        (set! progress-bar (new gauge% [parent pane] [label ""] [range 100]))))
    (send progress-bar set-value 0)
    (send frame center 'both)
    (send frame show #t))

  (define (cb msg crt max)
    ;; Make the frame the first time we are called.  If we are not called at
    ;; all, the frame it not shown.
    (unless frame (make-frame))
    ;; Setting the same message causes it to flicker.  Avoid doing that.
    (when (and msg (not (equal? last-msg msg)))
      (set! last-msg msg)
      (send message-field set-label msg))
    (when (and crt max)
      (let ((new-progress (exact-round (* 100 (/ crt max)))))
        (send progress-bar set-value new-progress))))

  (define database #f)

  (define (db-open-thread)
    (with-handlers
      (((lambda (e) #t)
        (lambda (e)
          (let ((msg (cond ((db-exn-bad-version? e)
                            (format (string-append
                                     "This version of ActivityLog2 requires database version ~a, "
                                     "and the database file ~a is at version ~a. "
                                     "Don't know how to upgrade the database.")
                                    (db-exn-bad-version-expected e)
                                    database-path
                                    (db-exn-bad-version-actual e)))
                           (#t
                            (format "~a : ~a" database-path e)))))
            (dbglog "interactive-open-database: ~a" msg)
            (message-box "Database open error" msg frame '(ok stop))))))
      (let ((db (open-activity-log database-path cb)))
        (set! database db))))

  (yield
   (thread/dbglog
    #:name "interactive-open-database-dialog%/interactive-open-database"
    db-open-thread))

  ;; Close the frame if it was created.
  (when frame (send frame show #f))

  database)


;;..................................................... toplevel-window% ....

(struct tl-section (name tag panel content))

(define toplevel-window%
  (class object%
    (init-field database-path)
    (super-new)

    (define database (interactive-open-database database-path))
    (unless database
      (raise (format "failed to open database at ~a" database-path)))
    (set-current-database database)     ; set this as current

    ;;; Construct the toplevel frame and initial panels
    (define tl-frame
      (let-values (((dir file _1) (split-path database-path)))
        (let ((dims (get-pref 'activity-log:frame-dimensions (lambda () (cons 1200 750)))))
          (new
           (class frame% (init) (super-new)
             ;; Note: the default implementation of on-exit is to call
             ;; on-close and hide the window.
             (define/augment (on-close) (send this show #f) (on-toplevel-close #t))
             (define/augment (can-close?) (can-close-toplevel?))
             (define/override (can-exit?) (can-close-toplevel?)))
           [width (car dims)] [height (cdr dims)]
           [style '(fullscreen-button)]
           [label (format "~a (~a) - ActivityLog2" file dir)]))))
    ;; Restore the maximization state of the frame (if any)
    (let ((maximized? (get-pref 'activity-log:frame-maximized (lambda () #f))))
      (send tl-frame maximize maximized?))
    (send tl-frame create-status-line)
    (make-log-output-window tl-frame)   ; notification banner
    (queue-callback
     (lambda () (shutdown-splash) (close-splash))
     #f)

    (define tl-panel            ; Holds all the widgets in the toplevel window
      (new horizontal-pane% [parent tl-frame] [spacing 0]))

    (define left-panel             ; Holds the section selector and log window
      (new vertical-pane%
           [parent tl-panel]
           [stretchable-width #f]
           [spacing 5]))

    ;;; Construct the individual views (sections) of the application

    (define the-sections '())
    (define the-selected-section #f)

    (define section-selector
      (new tab-selector%
           [parent left-panel]
           [callback (lambda (c index) (switch-to-section-by-num index))]))

    (begin

      (define (add-section name tag content-constructor-fn)
        (let* ((panel (new horizontal-panel% [parent tl-panel] [style '(deleted)]))
               (content (content-constructor-fn panel)))
          (set! the-sections (cons (tl-section name tag panel content) the-sections))))

      ;; NOTE: sections need to be added in the reverse order in which they
      ;; will appear in the `section-selector'

      (add-section "Session" 'session-view
                   (lambda (parent) (new view-session% [parent parent] [database database])))

      (add-section "Last Import" 'import
                   (lambda (parent)
                     (new import-view% [parent parent] [database database]
                          [select-activity-callback (lambda (dbid) (inspect-session dbid))])))

      (add-section "Equipment" 'equipment
                   (lambda (parent)
                     (new view-equipment% [parent parent] [database database])))

      (add-section "Workouts" 'workouts
                   (lambda (parent)
                     (new view-workouts% [parent parent] [database database])))

      (add-section "Trends" 'trends
                   (lambda (parent)
                     (new view-trends% [parent parent] [database database])))

      (add-section "Reports" 'reports
                   (lambda (parent)
                     (new view-reports% [parent parent] [database database])))

      (add-section "Athlete" 'athlete-metrics
                   (lambda (parent)
                     (new view-athlete-metrics% [parent parent] [database database])))

      (add-section "Calendar" 'calendar
                   (lambda (parent)
                     (new view-calendar% [parent parent]
                          [database database]
                          [select-activity-callback (lambda (dbid) (inspect-session dbid))])))

      (add-section "Activities" 'activity-list
                   (lambda (parent)
                     (new view-activities% [parent parent]
                          [database database]
                          [select-activity-callback (lambda (dbid) (inspect-session dbid))])))
      )

    ;; Configure the section-selector

    (send section-selector clear)
    (for ((section the-sections))
      (send section-selector append (tl-section-name section)))

    ;;; Construct the toplevel menu bar

    (let ((mb (new menu-bar% [parent tl-frame]))
          (aop (new activity-operations-forwarder% [toplevel-application this]))
          (amop (new athlete-metrics-forwarder% [toplevel-application this]))
          (wop (new workout-forwarder% [toplevel-application this])))
      (make-file-menu mb this)
      (make-edit-menu mb this)
      (make-view-menu mb this the-sections)
      (make-athlete-menu mb amop)
      (make-activtiy-menu mb aop)
      (make-workout-menu mb wop)
      (make-tools-menu mb this)
      (make-help-menu mb this))

    (define (can-close-toplevel?)
      (and (check-unsaved-edits)
           (let* ((section (get-section-by-tag 'workouts))
                  (result (send (tl-section-content section) can-exit?)))
             (unless result
               (switch-to-section section)
               (send section-selector select (get-section-index 'workouts) #t))
             result)))

    ;; Check if there are any unsaved edits, and prompts the user if there
    ;; are.  Returns #t if there are no unsaved edits, or the user does not
    ;; care about them (and therefore can be discarded)
    (define (check-unsaved-edits)
      (define section (get-section-by-tag 'session-view))
      (define unsaved-edits? (send (tl-section-content section) unsaved-edits?))
      (if unsaved-edits?
          (let ((mresult (message-box/custom "Unsaved Edits" "Session notes are unsaved"
                                             "Review" "Discard" #f
                                             tl-frame
                                             '(stop default=1)
                                             #f)))
            (cond ((eq? #f mresult)
                   ;; Just cancel the close
                   #f)
                  ((eq? 1 mresult)
                   (switch-to-section section)
                   (send section-selector select (get-section-index 'session-view) #t)
                   #f)
                  ((eq? 2 mresult)
                   ;; User wants the changes discarded
                   #t)
                  (#t #f)))
          #t))

    (define (on-toplevel-close (exit-application? #f))
      ;; NOTE: we might be called twice
      (dbglog "closing toplevel% for ~a" database-path)

      ;; Tell all our sections to save their visual layout
      (for-each (lambda (section)
                  (send (tl-section-content section) save-visual-layout))
                the-sections)

      (send section-selector save-visual-layout)

      ;; Save the size of the frame, so we can re-open it with the same
      ;; dimensions
      (unless (or (send tl-frame is-maximized?) (send tl-frame is-fullscreened?))
        (let-values (([w h] (send tl-frame get-size)))
          (put-pref 'activity-log:frame-dimensions (cons w h))))
      (put-pref 'activity-log:frame-maximized (send tl-frame is-maximized?))

      (when database

        ;; (call-with-output-file "profile.txt"
        ;;   #:mode 'text
        ;;   #:exists 'append
        ;;   profile-display)

        (disconnect database)
        (clear-metrics-cache)
        (clear-session-df-cache)
        (set! database #f))

      (when exit-application?
        (shutdown-event-sink-listener-threads)
        (shutdown-workers)
        (shutdown-map-tile-workers)
        (exit 0)))

    (define (get-section-by-tag tag)
      (findf (lambda (s) (eq? tag (tl-section-tag s))) the-sections))

    (define (get-section-index tag)
      (let loop ((index 0)
                 (sections the-sections))
        (cond ((null? sections) #f)
              ((eq? tag (tl-section-tag (car sections))) index)
              (#t (loop (+ index 1) (cdr sections))))))

    (define (inspect-session dbid)
      (when (check-unsaved-edits)
        (with-busy-cursor
          (lambda ()
            (let ((s (get-section-by-tag 'session-view)))
              (send (tl-section-content s) set-session dbid)
              (switch-to-section s)
              (send section-selector select (get-section-index 'session-view) #t))))))
    (set-inspect-callback inspect-session)

    (define (switch-to-section section)
      (unless (eq? the-selected-section section)
        (when the-selected-section
          (send tl-panel delete-child (tl-section-panel the-selected-section)))
        (set! the-selected-section section)
        (when the-selected-section
          (send tl-panel add-child (tl-section-panel the-selected-section))
          (send tl-frame set-status-text "")
          (send (tl-section-content section) activated))
        (send tl-panel reflow-container)))

    (define (switch-to-section-by-num n)
      (let ((p (list-ref the-sections n)))
        (switch-to-section (if n p #f))))

    (define/public (select-section tag)
      (let ((index (get-section-index tag)))
        (when index
          (switch-to-section-by-num index)
          (send section-selector set-selection index))))

    (define/public (get-frame) tl-frame)
    (define/public (get-database) database)
    (define/public (get-selected-section)
      (and the-selected-section (tl-section-content the-selected-section)))

    (define/public (refresh-current-view)
      (when the-selected-section
        (send (tl-section-content the-selected-section) refresh)))

    ;; Tools implementation
    (define/public (rebuild-elevation-data)
      (interactive-fixup-elevation database #f tl-frame))

    (define/public (rebuild-time-in-zone-data)
      (interactive-update-time-in-zone-data database tl-frame))

    (define/public (vacuum-database)

      (define progress-dialog
        (new progress-dialog%
             [title "Optimize database (vacuum and analyze)"]
             [can-cancel? #f]
             [icon (sql-export-icon)]))

      (define (task progress-dialog)
        (dbglog "vacuum-database started (main database)")
        (send progress-dialog set-message "Vacuum database ...")
        (query-exec database "vacuum")
        (dbglog "analyze-database started (main database)")
        (send progress-dialog set-message "Analyze database ...")
        (query-exec database "analyze")
        ;; The command below forces the statistics to be reloaded in the
        ;; current session.
        (query-exec database "analyze sqlite_master")
        (dbglog "vacuum-database started (tile cache database)")
        (send progress-dialog set-message "Vacuum tile cache database ...")
        (vacuum-tile-cache-database)
        (dbglog "vacuum-database completed")
        (send progress-dialog set-message "Completed."))

      (when database
        (send progress-dialog run tl-frame task)))

    (define/public (run)
      ;; The current arhitecture makes the toplevel-window% object useless
      ;; after it was closed.  A new one must be re-created and run.
      (unless database
        (error "toplevel-window%/run: trying to run the toplevel window after it was closed"))

      (send section-selector set-selection 0)
      (switch-to-section-by-num 0)

      (send tl-frame show #t)

      ;; Check for some basic things and log them, might be useful to diagnose
      ;; problems.
      (unless (get-pref 'activity-log:allow-tile-download (lambda () #t))
        (dbglog "map tile download disabled"))
      (unless (wu-api-key)
        (dbglog "No Wundergdound API key set"))
      (unless (get-pref 'activity-log:allow-weather-download (lambda () #t))
        (dbglog "weather data download disabled"))
      (let ((equipment (get-section-by-tag 'equipment)))
        (send (tl-section-content equipment) log-due-items))
      (let ((nsessions (query-value database "select count(*) from A_SESSION")))
        (when (or (sql-null? nsessions) (zero? nsessions))
          (notify-user 'info "There are no activities in the database.  Import some using the \"File\" menu.")))
      (collect-garbage 'major))

    (define (open-another-activity-log file)
      (dbglog "open-another-activity-log: will try to open ~a" file)
      (with-handlers
        (((lambda (e) #t)
          (lambda (e)
            (dbglog "open-another-activity-log: ~a" e))))
        (let ((new-tl (new toplevel-window% [database-path file])))
          ;; Toplevel window was successfully created, save the database file
          ;; as the new default to open next time.
          (put-pref 'activity-log:database-file
                       (if (path? file) (path->string file) file))
          ;; close this window than open the other one.  note that at this
          ;; moment we cannot have multiple databases open beacuse of
          ;; `init-sport-charms'.  This could be fixed with a medium effort.
          (send tl-frame show #f)
          (on-toplevel-close #f)
          (send new-tl run))))

    (define/public (on-exit)
      (when (can-close-toplevel?)
        (send tl-frame show #f)
        (on-toplevel-close #t)))

    (define/public (on-new-database)
      (when (can-close-toplevel?)
        (let ((file (get-file "Create database..." tl-frame
                              (find-system-path 'doc-dir)
                              "ActivityLog.db")))
          (when file (open-another-activity-log file)))))

    (define/public (on-open-database)
      (when (can-close-toplevel?)
        (let ((file (get-file "Open database..." tl-frame
                              (find-system-path 'doc-dir))))
          (when file (open-another-activity-log file)))))

    (define/public (on-import-activity)
      (let ((file (get-file "Select activity..." tl-frame)))
        (when file
          (query-exec database "delete from LAST_IMPORT")
          (let* ((iresult (db-import-activity-from-file file database))
                 (ecode (car iresult))
                 (edata (cdr iresult)))
            (cond ((eq? ecode 'failed)
                   (message-box
                    "Import failed" (format "Failed to import ~a: ~a" file edata)
                    tl-frame '(stop ok)))

                  ((eq? ecode 'already-exists)
                   (let ((mresult (message-box/custom
                                   "Import failed"
                                   (format "~a was previously imported. Force re-import?" file)
                                   "Re-import" "Cancel" #f tl-frame '(caution default=2))))
                     (when (eqv? mresult 1)
                       (let ((aid (db-get-activity-id edata database)))
                         (db-delete-activity-hard aid database)
                         (let ((iresult (db-import-activity-from-file file database)))
                           (refresh-current-view)
                           (if (eq? (car iresult) 'ok)
                               (begin
                                 (do-post-import-tasks database)
                                 (let ((equipment (get-section-by-tag 'equipment)))
                                   (send (tl-section-content equipment) log-due-items)))
                               (message-box
                                "Import failed" (format "Failed to import ~a: ~a" file ecode)
                                tl-frame '(stop ok))))))))

                  ((eq? ecode 'retired-device)
                   ;; TODO: force the re-import by un-retiring the device and
                   ;; retiring it back again.
                   (message-box
                    "Import failed" (format "Failed to import ~a: retired device" file)
                    tl-frame '(stop ok)))

                  ((eq? ecode 'ok)
                   (do-post-import-tasks database)
                   (let ((equipment (get-section-by-tag 'equipment)))
                     (send (tl-section-content equipment) log-due-items)))

                  (#t
                   (message-box
                    "Import failed" (format "Failed to import ~a: ~a" file edata) tl-frame '(stop ok)))))
          (refresh-current-view))))

    (define/public (on-import-from-directory)
      (let* ((last-import-dir (get-pref 'activity-log:last-import-dir (lambda () #f)))
             ;; NOTE: we use the platform independent directory selection
             ;; dialog, because on Windows, the OS one does not accept our
             ;; `last-import-dir' value.
             (dir (get-directory "Select directory for import..."
                                 tl-frame last-import-dir '(common))))
        (when dir
          (if (directory-exists? dir)
              (begin
                (put-pref 'activity-log:last-import-dir (path->string dir))
                (send (new import-dialog%) run tl-frame database dir)
                (refresh-current-view)
                (let ((equipment (get-section-by-tag 'equipment)))
                  (send (tl-section-content equipment) log-due-items))
                dir)
              ;; This can happen since the "get-directory" will not check if
              ;; the initial directory exists (and it might not if it is on a
              ;; mapped drive that is disconnected.
              (message-box "Cannot import"
                           (format "Directory ~a does not exist." dir)
                           tl-frame
                           '(stop ok))))
        #f))

    (define about-frame #f)

    (define/public (on-show-about)
      (unless about-frame
        (set! about-frame (make-about-frame)))
      (send about-frame show #t))

    ))
