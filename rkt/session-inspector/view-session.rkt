#lang racket/base
;; view-session.rkt -- view information about a sesion (graphs, laps, etc)
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015, 2018, 2020, 2021, 2022, 2023, 2024, 2025 Alex Harsányi <AlexHarsanyi@gmail.com>
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
         db/base
         racket/async-channel
         racket/class
         racket/dict
         racket/gui/base
         racket/match
         "../aerolab/aerolab-storage.rkt"
         "../al-widgets.rkt"
         "../database.rkt"
         "../dbutil.rkt"
         "../dialogs/activity-edit.rkt"
         "../fit-file/activity-util.rkt"
         "../fmt-util-ut.rkt"
         "../fmt-util.rkt"
         "../session-df/session-df.rkt"
         "../sport-charms.rkt"
         "../utilities.rkt"
         "../widgets/main.rkt"
         "inspect-aerolab.rkt"
         "inspect-best-avg.rkt"
         "inspect-graphs.rkt"
         "inspect-histogram.rkt"
         "inspect-laps.rkt"
         "inspect-map.rkt"
         "inspect-model-parameters.rkt"
         "inspect-overview.rkt"
         "inspect-quadrant.rkt"
         "inspect-traffic.rkt"
         "inspect-scatter.rkt"
         "inspect-similar-routes.rkt")

(provide view-session%)


;............................................................ resources ....

(define *title-font* (make-object font% 18 'default))
(define *label-font* (make-object font% 10 'default))
(define *data-font* (make-object font% 10 'default 'normal 'bold))


;;..................................................... session-header% ....

(define session-header%
  (class object%
    (init parent)
    (super-new)

    (define database #f)
    (define session-id #f)
    (define headline "")
    (define sport #f)
    (define sub-sport #f)
    (define perceived-effort 0)

    (define panel0 (new horizontal-panel%
                        [parent parent]
                        [border 0]
                        [spacing 5]
                        [stretchable-height #f]
                        [alignment '(left center)]))

    (define begining-spacer (make-spacer panel0))

    (define sport-icon (new message% [parent panel0]
                            [label (get-sport-bitmap-colorized 0 0)]
                            [stretchable-width #f]
                            [stretchable-height #f]))

    (define panel (new vertical-panel%
                       [parent panel0]
                       [border 5]
                       [spacing 1]
                       [stretchable-height #f]
                       [alignment '(left top)]))

    (define session-title
      (new message% [parent panel]
           [label "Untitled"]
           [stretchable-width #t]
           [font *title-font*]))

    (define session-title-edit
      (new text-field% [parent panel]
           [label ""]
           [style '(single deleted)]
           [stretchable-width #t]
           [font *title-font*]))

    (define start-time
      (new message% [parent panel]
           [stretchable-width #t]
           [font *label-font*]
           [label "No start time"]))

    (define sport-panel
      (new horizontal-panel%
           [parent panel]
           [spacing 1]
           [stretchable-height #f]
           [alignment '(left center)]))

    (new message% [parent sport-panel]
         [stretchable-width #f]
         [label "Activity type:"]
         [font *label-font*])

    (define sport-name
      (new message% [parent sport-panel]
           [stretchable-width #f]
           [auto-resize #t]
           [label "Other"]
           [font *data-font*]))

    (make-spacer sport-panel 10)

    (new message% [parent sport-panel]
         [stretchable-width #f]
         [label "Perceived Effort:"]
         [font *label-font*])

    (define rpe-name
      (new message% [parent sport-panel]
           [stretchable-width #t]
           [auto-resize #t]
           [label "Not Specified"]
           [font *data-font*]))

    (define sport-panel-edit
      (new horizontal-panel%
           [parent panel]
           [spacing 1]
           [style '(deleted)]
           [stretchable-height #f]
           [alignment '(left center)]))

    (new message% [parent sport-panel-edit]
         [stretchable-width #f]
         [label "Activity type:"]
         [font *label-font*])

    (define sport-name-edit
      (new sport-selector%
           [parent sport-panel-edit]
           [sports-in-use-only? #f]
           [label ""]
           [callback (lambda (v)
                       (send sport-icon set-label (get-sport-bitmap-colorized (car v) (cdr v))))]))

    (new message% [parent sport-panel-edit]
         [stretchable-width #f]
         [label "Perceived Effort:"]
         [font *label-font*])

    (define rpe-name-choice
      (new choice%
           [parent sport-panel-edit]
           [label ""]
           [choices
            (for/list ([rpe (in-range 11)])
              (rpe->string rpe))]
           [callback (lambda (control event)
                       (set! perceived-effort (send control get-selection))
                       (send rpe-name set-label (rpe->string perceived-effort)))]))

    (define edit-button
      (new button%
           [parent panel0]
           [label "Edit"]
           [callback (lambda (b e) (on-edit-headline))]))

    (define save-button
      (new button%
           [parent panel0]
           [label "Save"]
           [callback (lambda (b e) (on-save-headline))]
           [style '(deleted)]))

    (define revert-button
      (new button%
           [parent panel0]
           [label "Revert"]
           [callback (lambda (b e) (on-revert-headline))]
           [style '(deleted)]))

    (define end-spacer (make-spacer panel0))

    (define is-editing? #f)

    (define (on-edit-headline)
      (switch-to-edit-mode))

    (define (on-save-headline)
      (unless session-id
        (error (format "on-save-headline called with bad session-id: ~a~%" session-id)))
      (update-session-healine session-id database)
      (switch-to-view-mode)
      (log-event 'session-updated session-id))

    (define (on-revert-headline)
      (switch-to-view-mode))

    (define (switch-to-edit-mode)
      (set! is-editing? #t)
      ;; Set the min-height of the top panel (panel0) to its current height.
      ;; This will prevent re-flowing of the entire viewwhen we switch to edit
      ;; mode which has a smaller height.
      (let-values (([w h] (send panel0 get-graphical-min-size)))
        (send panel0 min-height h))

      (send session-title-edit set-value headline)
      (send sport-name-edit set-selected-sport sport sub-sport)
      (send rpe-name-choice set-selection perceived-effort)
      (send panel change-children
            (lambda (old)
              (list session-title-edit sport-panel-edit)))
      (send panel0 change-children
            (lambda (old)
              (list begining-spacer sport-icon panel save-button revert-button end-spacer)))
      (send sport-panel-edit reflow-container)
      (send session-title-edit focus))

    (define (switch-to-view-mode)
      (set! is-editing? #f)
      (send sport-icon set-label
            (get-sport-bitmap-colorized sport sub-sport))
      (send sport-name set-label (get-sport-name sport sub-sport))
      (send session-title set-label headline)
      (send rpe-name set-label (rpe->string perceived-effort))
      (send panel change-children
            (lambda (old)
              (list session-title start-time sport-panel)))
      (send panel0 change-children
            (lambda (old)
              (list begining-spacer sport-icon panel edit-button end-spacer)))
      ;; Edit button is only enabled if the session id is valid (i.e. we
      ;; actually have a session)
      (send edit-button enable (exact-positive-integer? session-id))
      (send sport-panel reflow-container))

    (define (update-session-healine sid db)
      (when (and db sid)
        (set! headline (send session-title-edit get-value))
        (let ((s (send sport-name-edit get-selection)))
          (set! sport (car s))
          (set! sub-sport (cdr s)))
        (call-with-transaction
         db
         (lambda ()
           (query-exec db "
update A_SESSION set name = ?, sport_id = ?, sub_sport_id = ?, rpe_scale = ?
 where id = ?" headline (or sport sql-null) (or sub-sport sql-null) (if (> perceived-effort 0) perceived-effort sql-null) sid)))))

    (define/public (set-session session)
      (if session
          (begin
            (set! headline (dict-ref  session 'name "Untitled"))
            (set! sport (session-sport session))
            (set! sub-sport (session-sub-sport session))
            (set! session-id (dict-ref session 'database-id #f))
            (set! perceived-effort (or (session-rpe session) 0))
            (send rpe-name set-label (rpe->string perceived-effort))
            (send start-time set-label
                  (format-date (session-start-time session)
                               (session-time-zone session))))
          (begin
            (set! headline "")
            (set! sport #f)
            (set! sub-sport #f)
            (set! session-id #f)
            (set! perceived-effort 0)
            (send rpe-name set-label (rpe->string perceived-effort))
            (send start-time set-label "")))
      (switch-to-view-mode))

    (define/public (set-database db)
      (set! database db))

    (define/public (unsaved-edits?) is-editing?)

    ))


;;.................................................. session-view% ....

;; Data corresponding to a TAB in the session view. 'GENERATION' is used to
;; avoid calling set-session on the contents more often than needed.
(struct tdata (name panel contents (generation #:mutable)))

(define (make-tdata name parent constructor-fn)
  (let* ((panel (new horizontal-panel% [parent parent] [style '(deleted)]
                     [alignment '(center top)] [stretchable-height #t]))
         (contents (constructor-fn panel)))
    (tdata name panel contents 0)))

(define view-session%
  (class* object% (activity-operations<%>)
    (init parent database select-activity-callback)
    (super-new)

    (define session-panel (new vertical-panel%
                               [parent parent]
                               [border 1]
                               [spacing 1]
                               [alignment '(center top)]))

    (define header (new session-header% [parent session-panel]))

    (define detail-panel
      (new tab-panel%
           [stretchable-height #t]
           [choices '("* None *")]
           [callback (lambda (p c)
                       (switch-tabs (send p get-selection)))]
           [parent session-panel]))

    (define overview
      (make-tdata "Overview" detail-panel
                  (lambda (panel)
                    (new inspect-overview-panel%
                         [parent panel]
                         [database database]))))
    (define laps
      (make-tdata "Laps" detail-panel
                  (lambda (panel)
                    (new laps-panel%
                         [parent panel]))))
    (define charts
      (make-tdata "Charts" detail-panel
                  (lambda (panel)
                    (new graph-panel%
                         [parent panel]
                         [get-preference
                          (lambda (name fail-thunk)
                            (db-get-pref database name (lambda () (get-pref name fail-thunk))))]
                         [put-preference
                          (lambda (name value)
                            (db-put-pref database name value))]))))
    (define scatter
      (make-tdata "Scatter" detail-panel
                  (lambda (panel)
                    (new scatter-plot-panel%
                         [parent panel]
                         [get-preference
                          (lambda (name fail-thunk)
                            (db-get-pref database name (lambda () (get-pref name fail-thunk))))]
                         [put-preference
                          (lambda (name value)
                            (db-put-pref database name value))]))))
    (define histogram
      (make-tdata "Histogram" detail-panel
                  (lambda (panel)
                    (new histogram-plot-panel%
                         [parent panel]
                         [get-preference
                          (lambda (name fail-thunk)
                            (db-get-pref database name (lambda () (get-pref name fail-thunk))))]
                         [put-preference
                          (lambda (name value)
                            (db-put-pref database name value))]))))
    (define mean-max
      (make-tdata "Mean Max" detail-panel
                  (lambda (panel)
                    (new mean-max-plot-panel%
                         [parent panel]
                         [get-preference
                          (lambda (name fail-thunk)
                            (db-get-pref database name (lambda () (get-pref name fail-thunk))))]
                         [put-preference
                          (lambda (name value)
                            (db-put-pref database name value))]))))
    (define quadrant
      (make-tdata "Quadrant" detail-panel
                  (lambda (panel)
                    (new quadrant-plot-panel%
                         [parent panel]
                         [get-preference
                          (lambda (name fail-thunk)
                            (db-get-pref database name (lambda () (get-pref name fail-thunk))))]
                         [put-preference
                          (lambda (name value)
                            (db-put-pref database name value))]))))
    (define maps
      (make-tdata "Map" detail-panel
                  (lambda (panel)
                    (new map-panel%
                         [parent panel]
                         [get-preference
                          (lambda (name fail-thunk)
                            (db-get-pref database name (lambda () (get-pref name fail-thunk))))]
                         [put-preference
                          (lambda (name value)
                            (db-put-pref database name value))]))))
    (define model-params
      (make-tdata "Model Params" detail-panel
                  (lambda (panel) (new model-parameters-panel% [parent panel]))))
    (define aerolab
      (make-tdata
       "Aerolab"
       detail-panel
       (lambda (panel)
         (new aerolab-panel%
              [parent panel]
              [get-aerolab-parameters
               (lambda () aerolab-parameters)]
              [put-aerolab-parameters
               (lambda (p)
                 (set! aerolab-parameters p)
                 (store-aerolab-parameters the-database session-id p))]))))
    (define traffic
      (make-tdata
       "Traffic"
       detail-panel
       (lambda (panel) (new traffic-panel% [parent panel]))))
    (define similar-routes
      (make-tdata
       "Similar Routes"
       detail-panel
       (lambda (panel)
         (new similar-routes-panel%
              [parent panel]
              [database database]
              [select-activity-callback select-activity-callback]))))

    (define installed-tabs '())

    (define session-id #f)
    (define session #f)
    (define data-frame #f)                      ; data frame for the session
    (define aerolab-parameters #f)
    (define generation 0)
    (define the-database database)

    (define/private (switch-tabs selected)
      (let ((tab (list-ref installed-tabs selected)))
        (send detail-panel begin-container-sequence)
        (send detail-panel change-children (lambda (o) (list (tdata-panel tab))))
        (with-busy-cursor
          (lambda ()
            (unless (equal? (tdata-generation tab) generation)
              (set-tdata-generation! tab generation)
              (send (tdata-contents tab) set-session session data-frame))))
        (send detail-panel end-container-sequence)))

    (define/private (set-session-df sdf ap)
      (unless (data-frame? sdf)
        (error "view-session%/set-session-df: expecting a data-frame"))
      (set! data-frame sdf)
      (set! aerolab-parameters ap)
      (define sport (df-get-property data-frame 'sport))

      ;; The overview panel also uses the data frame, so tell it that it is
      ;; available now.
      (send (tdata-contents overview) set-session session sdf)

      ;; Determine which tabs are needed, and only show those.  The Overview
      ;; panel always exists.
      (let ((tabs (list overview)))

        ;; Graphs, Scatter, Histogram and Laps panels exist if we have some
        ;; data.
        (when (> (df-row-count data-frame) 0)
          (set! tabs (cons charts tabs))
          (set! tabs (cons scatter tabs))
          (set! tabs (cons histogram tabs))
          (set! tabs (cons mean-max tabs))
          (when (send (tdata-contents quadrant) should-display-for-data-frame? data-frame)
            (set! tabs (cons quadrant tabs)))
          (set! tabs (cons laps tabs))
          (when (df-contains? data-frame "lat" "lon")
            (set! tabs (cons maps tabs))
            (set! tabs (cons similar-routes tabs)))

          (when (df-contains? data-frame "mbrt_vehicle_count" "mbrt_absolute_speed")
            (set! tabs (cons traffic tabs)))

          ;; Display the "Aerolab" tab only if we have aerolab parameters,
          ;; which exist only if the user enabled them from the Activities
          ;; menu.
          (when aerolab-parameters
            (set! tabs (cons aerolab tabs)))

          ;; Display the "Model Parameters" tab only for Swim, Bike or Run
          ;; activities, as these are the ones that we allow defining Sport
          ;; Zones and Critical Power parameters -- note that the DB schema
          ;; allows these to be defined for any activity, but the application
          ;; does not support that.
          (when (or (is-runnig? sport) (is-cycling? sport) (is-swimming? sport))
            (set! tabs (cons model-params tabs))))


        (set! installed-tabs (reverse tabs))
        (send detail-panel set (map tdata-name installed-tabs))))

    (define/public (set-session sid)
      (set! generation (add1 generation))
      (set! session-id sid)
      (set! session (and session-id the-database (db-fetch-session sid the-database)))

      ;; The session data frame and aerolab parameters take a while to fetch,
      ;; so we do it in a separate thread.
      (set! data-frame #f)
      (set! aerolab-parameters #f)
      (when (and session-id the-database)
        (queue-task
         "fetch-session-data-frame"
         (lambda ()
           (let ([df (session-df the-database sid)]
                 [ap (fetch-aerolab-parameters the-database session-id)])
             (queue-callback
              (lambda ()
                (set-session-df df ap)))))))

      (send header set-session session)
      (send header set-database the-database)

      ;; Install the overview panel only, as this does not require the data
      ;; frame
      (let ((tabs (list overview)))
        (set! installed-tabs (reverse tabs))
        (send detail-panel set (map tdata-name installed-tabs)))

      (send detail-panel set-selection 0)
      (switch-tabs 0))

    (define (refresh-session-summary)
      (when (and session-id the-database)
        (if (db-session-exists? session-id the-database)
            (begin
              (set! session (db-fetch-session session-id the-database))
              (send header set-session session)
              (send (tdata-contents overview) set-session session data-frame))
            (set-session #f))))

    (define (refresh-session-data)
      (when (and session-id the-database)
        (if (db-session-exists? session-id the-database)
            (set-session session-id)
            (set-session #f))))

    (define change-notification-source (make-log-event-source))

    ;; Monitor the change-notification-source in a separate thread and update
    ;; the view if the session we display has changed.  This thread will run
    ;; even if the view is not active, so there is the potential of slowing
    ;; things down when there are a lot of updates.  Unfortunately, hooking
    ;; into 'activated' does not work, as updates while this view is active
    ;; (e.g. changing critical power params) will not work.
    (define change-processing-thread
      (thread/dbglog
       #:name "view-session%/change-notification-thread"
       (lambda ()
         (let loop ((item (async-channel-get change-notification-source)))
           (when item
             (match-define (list tag data) item)
             (case tag
               ((session-deleted)
                (queue-callback
                 (lambda ()
                   (when (eq? session-id data)
                     (set-session #f)))))
               ((session-updated weather-data-changed)
                (queue-callback
                 (lambda ()
                   (when (eq? session-id data)
                     (refresh-session-summary)))))
               ((session-updated-data)
                (queue-callback
                 (lambda ()
                   (when (eq? session-id data)
                     (refresh-session-data))))))
             (loop (async-channel-get change-notification-source)))))))

    (define/public (activated)
      (void))

    (define/public (refresh)
      (set-session session-id))

    (define/public (save-visual-layout)
      (send (tdata-contents laps) save-visual-layout)
      (send (tdata-contents charts) save-visual-layout)
      (send (tdata-contents scatter) save-visual-layout)
      (send (tdata-contents histogram) save-visual-layout)
      (send (tdata-contents mean-max) save-visual-layout)
      (send (tdata-contents quadrant) save-visual-layout)
      (send (tdata-contents maps) save-visual-layout)
      (send (tdata-contents aerolab) save-visual-layout)
      (send (tdata-contents traffic) save-visual-layout)
      (send (tdata-contents similar-routes) save-visual-layout))

    (define/public (unsaved-edits?)
      (or (send (tdata-contents overview) unsaved-edits?)
          (send (tdata-contents aerolab) unsaved-edits?)
          (send header unsaved-edits?)))

    ;; Activity operations interface implementation

    (define/public (get-top-level-window)
      (send session-panel get-top-level-window))
    (define/public (get-database) the-database)

    (define/public (get-selected-sid) session-id)
    (define/public (get-selected-guid)
      (if session-id
          (query-maybe-value
           the-database
           "select A.guid
              from ACTIVITY A, A_SESSION S
             where S.activity_id = A.id and S.id = ?"
           session-id)
          #f))
    (define/public (get-selected-sport)
      (if session
          (let ((sport (session-sport session))
                (sub-sport (session-sub-sport session)))
            (cons sport sub-sport))
          (cons #f #f)))
    (define/public (after-update sid)
      (activated))
    (define/public (after-new sid)
      ;; We only receive the 'after-new' message if a new session was created
      ;; while this view is active, so switch to the new session now.
      (set-session sid))
    (define/public (can-delete? sid)
      ;; We don't allow deleting from here
      #f)
    (define/public (after-delete sid)
      (activated))
    (define/public (before-popup) #f)
    (define/public (after-popdown) #f)
    (define/public (inspect-session sid)
      ;; Do nothing...
      (void))

    (define/public (get-aerolab-analysis-status)
      (cond
        ((hash? aerolab-parameters)
         ;; If we have aerolab params, the menu should offer to disable the
         ;; analysis tab
         'disable)
        ((and session
               the-database
               (equal? (session-sport session) 2)
               data-frame
               (and (df-contains? data-frame "spd" "pwr" "lat" "lon")
                    (df-contains/any? data-frame "alt" "calt")))
         ;; Aerolab analysis is available for cycling sessions with power
         ;; data, speed, latitude/longitude and elevation data
         'enable)
        (#t
         'none)))

    (define/public (show-or-hide-aerolab-tab)
      (case (get-aerolab-analysis-status)
        ((none)                     ; This method should not have been invoked
         (void))
        ((enable)
         (set! aerolab-parameters (hash)) ; start empty
         (store-aerolab-parameters the-database session-id aerolab-parameters)
         (set! installed-tabs (append installed-tabs (list aerolab)))
         (let ([index (sub1 (length installed-tabs))])
           (send detail-panel set (map tdata-name installed-tabs))
           (send detail-panel set-selection index)
           (switch-tabs index)))
        ((disable)
         (let ((mresult
                (message-box/custom
                 "Confirm clear Aerolab Parameters"
                 "Aerolab parameters will be deleted for this session.  Are you sure?"
                 #f
                 "Delete Parameters"
                 "Cancel"
                 (get-top-level-window)
                 '(caution default=3)
                 #:dialog-mixin al2-message-box-mixin)))
          (when (equal? mresult 2)
            (drop-aerolab-parameters the-database session-id)
            (send (tdata-contents aerolab) clear)
            (set! installed-tabs (filter (lambda (x) (not (equal? "Aerolab" (tdata-name x)))) installed-tabs))
            (send detail-panel set (map tdata-name installed-tabs))
            (send detail-panel set-selection 0)
            (switch-tabs 0))))))
    ))
