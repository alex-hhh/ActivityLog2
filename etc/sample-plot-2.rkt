#lang racket/gui

;; This file ilustrates how to get plots of data series for sessions from the
;; database.

(require plot)
(require "../rkt/al-prefs.rkt")         ; so we know what the default database is
(require "../rkt/database.rkt")         ; so we can fetch sessions from the database
(require "../rkt/plot-builder.rkt")     ; so we can extract data series from the session
(require "../rkt/plot-axis-def.rkt")    ; so we know what data series to extract

;; The session we will use. "Copy session-id to clipboard..." can be used to
;; choose session id's
(define session-id 1522)

;; Open the default database
(define *db*
  (let ((db-file (al-get-pref 'activity-log:database-file (lambda () #f))))
    (unless db-file
      (error "No default database"))
    (db-open-activity-log db-file)))

;; Fetch the session from the database.
(define session (db-fetch-session session-id *db*))

;; See plot-axis-def.rkt for what axis are available
(define base axis-timer-time)           ; axis-distance is also good here
(define series axis-hr-bpm)             ; try axis-speed, axis-pace, etc.
(define filter-amount 5)                ; can be 0 for no filtering

;; Extract the HR data series form the session
(define data-series
  (let-values (([data lap-markers min-x max-x min-y max-y]
                (extract-data session base series filter-amount)))
    ;; We are only interested in the data series
    data))

;; Construct the plot frame.  Note that the axis definitions contain the
;; correct labels and axis-ticks, so we use them
(define pf 
  (parameterize ([plot-x-ticks (send base get-axis-ticks)]
                 [plot-x-label (send base get-axis-label)]
                 [plot-y-ticks (send series get-axis-ticks)]
                 [plot-y-label (send series get-axis-label)])
    (plot-frame (lines data-series))))

(send pf show #t)
