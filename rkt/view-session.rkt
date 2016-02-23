#lang racket/base
;; view-session.rkt -- view information about a sesion (graphs, laps, etc)
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

(require db
         racket/class
         racket/date
         racket/gui/base
         (rename-in srfi/48 (format format-48))
         "activity-edit.rkt"
         "activity-util.rkt"
         "al-widgets.rkt"
         "database.rkt"
         "inspect-best-avg.rkt"
         "inspect-graphs.rkt"
         "inspect-histogram.rkt"
         "inspect-laps.rkt"
         "inspect-map.rkt"
         "inspect-overview.rkt"
         "inspect-scatter.rkt"
         "sport-charms.rkt"
         "utilities.rkt"
         "widgets.rkt")

(provide view-session%)


;............................................................ resources ....

(define *title-font* (make-object font% 24 'default))
(define *label-font* (make-object font% 12 'default))
(define *data-font* (make-object font% 12 'default 'normal 'bold))


;;..................................................... session-header% ....

(define (unix-time->date-time-string seconds)
  (let ((old-fmt (date-display-format)))
    (date-display-format 'american)
    (let ((str (date->string (seconds->date seconds) #t)))
      (date-display-format old-fmt)
      str)))

(define session-header%
  (class object%
    (init parent)
    (super-new)

    (define database #f)
    (define session-id #f)
    (define headline "")
    (define sport #f)
    (define sub-sport #f)

    (define panel0 (new horizontal-panel%
			[parent parent]
			[border 0]
			[spacing 5]
			[stretchable-height #f]
			[alignment '(left center)]))

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
           [stretchable-width #t]
           [label "Other"]
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

    (define (on-edit-headline)
      (switch-to-edit-mode))

    (define (on-save-headline)
      (update-session-healine session-id database)
      (switch-to-view-mode))

    (define (on-revert-headline)
      (switch-to-view-mode))

    (define (switch-to-edit-mode)
      ;; Set the min-height of the top panel (panel0) to its current height.
      ;; This will prevent re-flowing of the entire viewwhen we switch to edit
      ;; mode which has a smaller height.
      (let-values (([w h] (send panel0 get-graphical-min-size)))
        (send panel0 min-height h))

      (send session-title-edit set-value headline)
      (send sport-name-edit set-selected-sport sport sub-sport)
      (send panel change-children
            (lambda (old)
              (list session-title-edit sport-panel-edit)))
      (send panel0 change-children
            (lambda (old)
              (list sport-icon panel save-button revert-button)))
      (send session-title-edit focus))

    (define (switch-to-view-mode)
      (send sport-icon set-label 
            (get-sport-bitmap-colorized sport sub-sport))
      (send sport-name set-label (get-sport-name sport sub-sport))
      (send session-title set-label headline)
      (send panel change-children
            (lambda (old)
              (list session-title start-time sport-panel)))
      (send panel0 change-children
            (lambda (old)
              (list sport-icon panel edit-button))))

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
update A_SESSION set name = ?, sport_id = ?, sub_sport_id = ?
 where id = ?" headline (or sport sql-null) (or sub-sport sql-null) sid)))))

    (define/public (set-session session)
      (set! headline (or (assq1 'name session) "Untitled"))
      (set! sport (session-sport session))
      (set! sub-sport (session-sub-sport session))
      (set! session-id (assq1 'database-id session))
      (send start-time
	    set-label
	    (unix-time->date-time-string (assq1 'start-time session)))
      (switch-to-view-mode))

    (define/public (set-database db)
      (set! database db))

    ))


;;.................................................. session-view% ....

(define (make-panel parent)
  (new horizontal-panel%
       [parent parent]
       [style '(deleted)]
       [alignment '(center top)]
       [stretchable-height #t]))

(define view-session%
  (class* object% (activity-operations<%>)
    (init parent database)
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
           [choices '("Overview" "Graphs" "Scatter" "Histogram" "Best Avg" "Laps" "Map")]
           [callback (lambda (p c)
                       (switch-tabs (send p get-selection)))]
           [parent session-panel]))

    (define overview-panel (make-panel detail-panel))
    (define charts-panel (make-panel detail-panel))
    (define scatter-panel (make-panel detail-panel))
    (define histogram-panel (make-panel detail-panel))
    (define laps-panel (make-panel detail-panel))
    (define best-avg-panel (make-panel detail-panel))
    (define map-panel (make-panel detail-panel))

    (define overview (new inspect-overview-panel% [parent overview-panel] [database database]))
    (define laps (new laps-panel% [parent laps-panel]))
    (define charts (new graph-panel% [parent charts-panel]))
    (define scatter (new scatter-plot-panel% [parent scatter-panel]))
    (define histogram (new histogram-plot-panel% [parent histogram-panel]))
    (define best-avg (new best-avg-plot-panel% [parent best-avg-panel]))
    (define map (new map-panel% [parent map-panel]))

    (define tabs (list 
                  (cons overview-panel overview)
                  (cons charts-panel charts) 
                  (cons scatter-panel scatter)
                  (cons histogram-panel histogram)
                  (cons best-avg-panel best-avg)
                  (cons laps-panel laps)
                  (cons map-panel map)))

    (define session-id #f)
    (define session #f)
    (define generation -1)
    (define the-database database)

    (define (switch-tabs selected)
      (send detail-panel change-children
            (lambda (old) (list (car (list-ref tabs selected)))))
      (let ((v (cdr (list-ref tabs selected))))
        (with-busy-cursor
         (lambda ()
           (when (< (send v get-generation) generation)
             (send v set-session session))))))

    (define/public (set-session sid)
      (set! generation (+ 1 generation))
      (set! session-id sid)
      (set! session (db-fetch-session sid the-database))

      (send header set-session session)
      (send header set-database the-database)

      (define is-lap-swimming?
        (let ((sport (session-sport session))
              (sub-sport (session-sub-sport session)))
          (and (equal? sport 5) (equal? sub-sport 17))))

      ;; Determine which tabs are needed, and only show those.  The Overview
      ;; panel always exists.
      (let ((labels '("Overview"))
            (tabs-1 (list (cons overview-panel overview))))

        ;; Graphs, Scatter, Histogram and Laps panels exist if we have some
        ;; laps.  The Map panel requires GPS data as well.
        (when (> (length (session-laps session)) 0)

          (set! labels (cons "Graphs" labels))
          (set! tabs-1 (cons (cons charts-panel charts) tabs-1))

          (set! labels (cons "Scatter" labels))
          (set! tabs-1 (cons (cons scatter-panel scatter) tabs-1))

          (set! labels (cons "Histogram" labels))
          (set! tabs-1 (cons (cons histogram-panel histogram) tabs-1))

          (unless is-lap-swimming?
            (set! labels (cons "Best Avg" labels))
            (set! tabs-1 (cons (cons best-avg-panel best-avg) tabs-1)))

          (set! labels (cons "Laps" labels))
          (set! tabs-1 (cons (cons laps-panel laps) tabs-1))

          (let ((have-gps-track? (with-handlers
                                  (((lambda (e) #t)
                                    (lambda (e) e)))
                                  (for-each-session-trackpoint
                                   session
                                   (lambda (prev current)
                                     (when (assq 'position-lat current)
                                       (raise 'found))))
                                  #f)))
            (when have-gps-track?
              (set! labels (cons "Map" labels))
              (set! tabs-1 (cons (cons map-panel map) tabs-1)))))

        (send detail-panel set (reverse labels))
        (set! tabs (reverse tabs-1)))

      (send detail-panel set-selection 0)
      (switch-tabs 0))

    (define/public (activated)
      #f)

    (define/public (refresh)
      (set-session session-id))

    (define/public (save-visual-layout)
      (send laps save-visual-layout)
      (send charts save-visual-layout)
      (send scatter save-visual-layout)
      (send histogram save-visual-layout)
      (send best-avg save-visual-layout)
      (send map save-visual-layout))

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
    (define/public (after-update sid) (refresh))
    (define/public (after-new sid) (set-session sid)) ; show the new session
    (define/public (can-delete? sid)
      ;; We don't allow deleting from here
      #f)
    (define/public (after-delete sid) #f)
    (define/public (before-popup) #f)
    (define/public (after-popdown) #f)

    ))
