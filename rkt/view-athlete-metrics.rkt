#lang racket/gui
;; view-athlete-metrics.rkt -- athelte metrics panel
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
         "al-widgets.rkt"
         "database.rkt"
         "edit-athlete-metrics.rkt"
         "fmt-util.rkt"
         "icon-resources.rkt"
         "utilities.rkt"
         "widgets.rkt")

(provide athlete-metrics-operations<%>)
(provide athlete-metrics-operations-menu%)
(provide view-athlete-metrics%)
(provide get-athlete-metrics)

(define (index a b)
  (let ((tail (member a (reverse b))))
    (if tail (length (cdr tail)) #f)))


;;........................................ athlete-metrics-operations<%> ....

(define athlete-metrics-operations<%>
  (interface ()
             get-top-level-window
             get-database
             get-selected-id
             after-update
             after-delete
             after-new
             before-popup
             after-popdown))


;;..................................... athlete-metrics-operations-menu% ....

(define (delete-athlete-metrics db id)
  (query-exec db "delete from ATHLETE_METRICS where id = ?" id))

(define athlete-metrics-operations-menu%
  (class object% (init-field target [menu-bar #f]) (super-new)
    
    (unless (is-a? target athlete-metrics-operations<%>)
      (error "Target must implement the athlete-metrics-operations<%> interface"))
    
    (define (on-demand m)
      (send target before-popup)
      (let ((have-id? (send target get-selected-id)))
        (send edit-menu-item enable have-id?)
        (send delete-menu-item enable have-id?)))

    (define (on-popdown m e)
      (send target after-popdown))

    (define (on-edit m e)
      (let ((id (send target get-selected-id))
            (db (send target get-database))
            (tl (send target get-top-level-window)))
        (unless id
          (error "athlete-metrics-operations-menu%/on-edit: bad id" id))
        (when (send (get-edit-athlete-metrics-dialog) show-dialog tl db id)
          (send target after-update id))))

    (define (on-new m e)
      (let ((db (send target get-database))
            (tl (send target get-top-level-window)))
        (let ((id (send (get-edit-athlete-metrics-dialog) show-dialog tl db #f)))
          (when id
            (send target after-new id)))))

    (define (on-delete m e)
      (let ((id (send target get-selected-id))
            (db (send target get-database))
            (tl (send target get-top-level-window)))
        (unless id
          (error "athlete-metrics-operations-menu%/on-edit: bad id" id))
        (let ((mresult (message-box/custom
                        "Confirm delete"
                        (format "Really delete metrics?~%This cannot be undone.")
                        #f "Delete" "Cancel" tl '(caution default=3))))
          (when (eqv? mresult 2)
            (delete-athlete-metrics db id)
            (send target after-delete id)))))

    (define the-menu
      (if menu-bar
          (new menu% [parent menu-bar] [label "Athlete"]
               [demand-callback on-demand])
          (new popup-menu% [title "Athlete"]
               [demand-callback on-demand]
               [popdown-callback on-popdown])))
    
    (define edit-menu-item
      (new menu-item% [parent the-menu]
           [label "Edit athlete metrics ..."] [callback on-edit]))
    (define new-menu-item
      (new menu-item% [parent the-menu]
           [label "New athlete metrics ..."]
           [callback on-new]
           [shortcut #\N]
           [shortcut-prefix '(ctl shift)]))
    (new separator-menu-item% [parent the-menu])
    (define delete-menu-item
      (new menu-item% [parent the-menu]
           [label "Delete athlete metrics..."] [callback on-delete]))

    (define/public (get-popup-menu) the-menu)

    ))


;;................................................ view-athlete-metrics% ....

;; Produce the text of an SQL query that returns data for the view.
(define (get-athlete-metrics-sql-query date-range)
  (format "select AM.id,
       AM.timestamp,
       AM.body_weight,
       AM.sleep_time,
       (select SQ.name from E_SLEEP_QUALITY SQ where SQ.id = AM.sleep_quality),
       (select OF.name from E_OVERALL_FEELING OF where OF.id = AM.overall_feeling),
       AM.description
  from ATHLETE_METRICS AM
 where ~a and ~a
 order by AM.timestamp desc"
           (if (and date-range (car date-range))
               (format "AM.timestamp >= ~a" (car date-range))
               "1 = 1")
           (if (and date-range (cdr date-range))
               (format "AM.timestamp <= ~a" (cdr date-range))
               "1 = 1")))

;; Return data for the view.
(define (get-athlete-metrics db date-range)
  (query-rows
   db
   (get-athlete-metrics-sql-query date-range)))

;; Return one row of data corresponding to the ATHLETE_METRICS.id.  This is
;; used to update an entry in the view after it was added or edited.
;;
;; NOTE: the rows returned here must match the ones returned by
;; get-athlete-metrics
(define (get-athlete-metrics-1 db id)
  (query-row
   db
   "select AM.id,
       AM.timestamp,
       AM.body_weight,
       AM.sleep_time,
       (select SQ.name from E_SLEEP_QUALITY SQ where SQ.id = AM.sleep_quality),
       (select OF.name from E_OVERALL_FEELING OF where OF.id = AM.overall_feeling),
       AM.description
  from ATHLETE_METRICS AM
 where AM.id = ?" id))

(define athlete-metrics-display-columns
  (list
   (let ((fn (lambda (row) (vector-ref row 1))))
     (column-info "Date/Time" (lambda (row) (date-time->string (fn row))) fn))

   (let ((fn (lambda (row) (sql-column-ref row 2 0))))
     (column-info "Bodyweight"
                  (lambda (row) (weight->string (fn row) #t))
                  fn))

   (let ((fn (lambda (row) (sql-column-ref row 3 0))))
     (column-info "Sleep duration"
                  (lambda (row) (let ((v (fn row)))
                                  (if (> v 0)
                                      (duration->string v)
                                      "")))
                  fn))

   (let ((fn (lambda (row) (sql-column-ref row 4 ""))))
     (column-info "Sleep quality"
                  (lambda (row) (format "~a" (fn row)))
                  fn))

   (let ((fn (lambda (row) (sql-column-ref row 5 ""))))
     (column-info "Overall feeling"
                  (lambda (row) (format "~a" (fn row)))
                  fn))

   (let ((fn (lambda (row) (sql-column-ref row 6 ""))))
     (column-info "Notes"
                  (lambda (row) (format "~a" (fn row)))
                  fn))))

(define (make-athlete-metrics-summary-label rows)
  (let ((nitems (length rows))
        (total-bw 0)
        (bw-samples 0)
        (total-sleep 0)
        (sleep-samples 0))
    (if (> nitems 0)
        (begin
          (for-each (lambda (row)
                      (let ((bw (sql-column-ref row 2 #f))
                            (sleep (sql-column-ref row 3 #f)))
                        (when bw
                          (set! total-bw (+ total-bw bw))
                          (set! bw-samples (+ bw-samples 1)))
                        (when sleep
                          (set! total-sleep (+ total-sleep sleep))
                          (set! sleep-samples (+ sleep-samples 1)))))
                    rows)
          (let ((avg-bw (if (> bw-samples 0) (/ total-bw bw-samples) #f))
                (avg-sleep (if (> sleep-samples 0) (/ total-sleep sleep-samples) #f)))
            (cond ((and avg-bw avg-sleep)
                   (format "~a items, avg body weight: ~a, avg sleep: ~a"
                           nitems (weight->string avg-bw #t) (duration->string avg-sleep)))
                  (avg-bw
                   (format "~a items, avg body weight: ~a" nitems (weight->string avg-bw #t)))
                  (avg-sleep
                   (format "~a items, avg sleep: ~a" nitems (duration->string avg-sleep)))
                  (#t
                   (format "~a items" nitems)))))
        "")))

(define view-athlete-metrics%
  (class* object% (athlete-metrics-operations<%>)
    (init parent)
    (init-field database)
    (super-new)

    (define tag 'activity-log:athlete-metrics)
    (define date-range '(#f . #f))

    (define pane (new (class vertical-panel%
                        (init)(super-new)
                        (define/public (interactive-export-sql-query)
                          (on-interactive-export-sql-query)))
                      [parent parent]
                      [alignment '(left center)]))

    (define date-range-selector #f)

    (define (on-date-range dr)
      (set! date-range dr)
      (on-filter-changed))

    (let ((sel-pane (new horizontal-pane% [parent pane] 
                        [spacing 20]
                         [border 0]
                         [stretchable-height #f]
                         [stretchable-width #t]
                         [alignment '(left center)])))
      (new message% [parent sel-pane] [label wscale-icon])
      (let ((p (new horizontal-pane% [parent sel-pane]
                    [stretchable-width #f] [border 20]
                    [alignment '(left center)])))

        (let ((drf (new date-range-selector% [parent p]
                        [initial-selection 'last-30-days]
                        [callback (lambda (s) (on-date-range s))])))
          (send drf set-seasons (db-get-seasons database))
          (set! date-range (send drf get-selection))
          (set! date-range-selector drf))))

    (define lb (new qresults-list% [parent pane]
                    [tag 'activity-log:athlete-metrics]
                    [right-click-menu
                     (send (new athlete-metrics-operations-menu% [target this]) get-popup-menu)]
                    ))

    (define (on-filter-changed)
      (let ((rows (get-athlete-metrics database date-range)))
        (send lb set-data rows)
        (send (send pane get-top-level-window) 
            set-status-text 
            (make-athlete-metrics-summary-label rows))))

    (send lb setup-column-defs athlete-metrics-display-columns)

    (define/public (activated)
      (refresh))

    (define/public (refresh)
      (send date-range-selector set-seasons (db-get-seasons database))
      (on-filter-changed))

    (define/public (save-visual-layout)
      (send lb save-visual-layout))

    ;; Target

    (define selected-row-index #f)

    (define/public (before-popup)
      (set! selected-row-index (send lb get-selected-row-index)))

    (define/public (after-popdown)
      (set! selected-row-index #f))

    (define/public (get-selected-id)
      (if selected-row-index
          (let ((data (send lb get-data-for-row selected-row-index)))
            (if data (vector-ref data 0) #f))
          #f))

    (define/public (get-database) database)
    (define/public (get-top-level-window) (send pane get-top-level-window))

    (define/public (after-update id)
      (when selected-row-index
        (let ((new-data (get-athlete-metrics-1 database id)))
          (send lb update-row selected-row-index new-data))))

    (define/public (after-new id)
      (let ((new-data (get-athlete-metrics-1 database id)))
        (send lb add-row new-data)))

    (define/public (after-delete id)
      (when selected-row-index
        (send lb delete-row selected-row-index)))

    (define/public (on-interactive-export-sql-query)
      (let ((query (get-athlete-metrics-sql-query date-range)))
        (send (get-sql-export-dialog) 
              show-dialog (send pane get-top-level-window) query)))

    ))
