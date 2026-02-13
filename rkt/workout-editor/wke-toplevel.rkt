#lang racket/base
;; wke-toplevel.rkt -- toplevel form for al2-workout-editor
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2025, 2026 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(provide toplevel-window%)
(require db/base
         framework/splash
         racket/gui
         "../dbutil-gui.rkt"
         "../sport-charms.rkt"
         "../utilities.rkt"
         "view-workouts.rkt"
         "wke-db.rkt")


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

  (new separator-menu-item% [parent file-menu])

  (new menu-item%
       [parent file-menu] [label "E&xit"]
       [shortcut #\Q]
       [callback (lambda (m e) (send toplevel on-exit))])

  file-menu)



;;....................................................... make-view-menu ....

(define (make-view-menu menu-bar toplevel)

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

  view-menu)


;;.................................................... make-workout-menu ....

(define (make-workout-menu menu-bar target)
  (define workout-menu
    (new workout-operations-menu% [menu-bar menu-bar] [target target]))
  (send workout-menu get-popup-menu))


;....................................................... make-help-menu ....

(define (make-help-menu menu-bar toplevel)
  (define help-menu (new menu% [parent menu-bar] [label "&Help"]))

  (new menu-item%
       [parent help-menu] [label "About..."]
       [callback
        (lambda (m e) (send toplevel on-show-about))]))


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

(define toplevel-window%
  (class object%
    (init-field database-path)
    (super-new)

    (define database
      (interactive-open-database database-path wke-open-database))
    (define sport-charms (new sport-charms% [dbc database]))
    (unless database
      (raise (format "failed to open database at ~a" database-path)))

    ;;; Construct the toplevel frame and initial panels
    (define tl-frame
      (let-values (((dir file _1) (split-path database-path)))
        (let ((dims (get-pref 'al2-workout-editor:frame-dimensions (lambda () (cons 1200 750)))))
          (new
           (class frame%
             (init)
             (super-new)
             ;; Note: the default implementation of on-exit is to call
             ;; on-close and hide the window.
             (define/augment (on-close) (send this show #f) (on-toplevel-close #t))
             (define/augment (can-close?) (can-close-toplevel?))
             (define/override (can-exit?) (can-close-toplevel?)))
           [width (car dims)] [height (cdr dims)]
           [style '(fullscreen-button)]
           [label (format "~a (~a) - AL2-Workout-Editor" file dir)]))))

    ;; Restore the maximization state of the frame (if any)
    (let ((maximized? (get-pref 'al2-workout-editor:frame-maximized (lambda () #f))))
      (send tl-frame maximize maximized?))

    (send tl-frame create-status-line)
    (queue-callback
     (lambda () (shutdown-splash) (close-splash))
     #f)

    (define workouts-view
      (new view-workouts%
           [parent tl-frame]
           [database database]
           [sport-charms sport-charms]))

    (send workouts-view activated)

    ;;; Construct the toplevel menu bar
    (let ((mb (new menu-bar% [parent tl-frame]))
          (wop (new workout-forwarder% [toplevel-application this])))
      (make-file-menu mb this)
      (make-view-menu mb this) ; NOTE: we will miss the Session view here...
      (make-workout-menu mb wop)
      (make-help-menu mb this))

    (define (can-close-toplevel?)
      (send workouts-view can-exit?))

    (define (on-toplevel-close (exit-application? #f))
      ;; NOTE: we might be called twice
      (dbglog "closing toplevel% for ~a" database-path)

      ;; Tell all our sections to save their visual layout
      (send workouts-view save-visual-layout)

      ;; Save the size of the frame, so we can re-open it with the same
      ;; dimensions
      (unless (or (send tl-frame is-maximized?) (send tl-frame is-fullscreened?))
        (let-values (([w h] (send tl-frame get-size)))
          (put-pref 'al2-workout-editor:frame-dimensions (cons w h))))
      (put-pref 'al2-workout-editor:frame-maximized (send tl-frame is-maximized?))

      (when database

        #;(call-with-output-file "profile.txt"
          #:mode 'text
          #:exists 'append
          profile-display)

        ;; AB#40 SQLite documentation recommends running an optimize before
        ;; closing the connection -- this will perform optimizations based on
        ;; the queries that were run during the session.  Most of the time,
        ;; this is a no-op and should run fast...
        ;;
        ;; https://sqlite.org/pragma.html#pragma_optimize
        (query-exec database "pragma optimize")

        (disconnect database)
        (set! database #f))

      (when exit-application?
        (exit 0)))

    (define/public (get-frame) tl-frame)
    (define/public (get-database) database)
    (define/public (get-selected-section) workouts-view)

    (define/public (refresh-current-view)
      (send workouts-view refresh))

    (define/public (run)
      ;; The current architecture makes the toplevel-window% object useless
      ;; after it was closed.  A new one must be re-created and run.
      (unless database
        (error "toplevel-window%/run: trying to run the toplevel window after it was closed"))

      (send tl-frame show #t))

    (define (open-another-activity-log file)
      (dbglog "open-another-activity-log: will try to open ~a" file)
      (with-handlers
        (((lambda (e) #t)
          (lambda (e)
            (dbglog "open-another-activity-log: ~a" e))))
        (let ((new-tl (new toplevel-window% [database-path file])))
          ;; Toplevel window was successfully created, save the database file
          ;; as the new default to open next time.
          (put-pref 'al2-workout-editor:database-file
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
        (let ((file (put-file "Create database..." tl-frame
                              (find-system-path 'doc-dir)
                              "AL2-Workout-Editor.db")))
          (when file (open-another-activity-log file)))))

    (define/public (on-open-database)
      (when (can-close-toplevel?)
        (let ((file (get-file "Open database..." tl-frame
                              (find-system-path 'doc-dir))))
          (when file (open-another-activity-log file)))))
    ))
