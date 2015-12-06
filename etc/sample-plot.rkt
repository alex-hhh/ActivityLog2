#lang racket/gui
;; This file ilustrates how to plot data series for sessions read from FIT
;; files.

(require plot)
(require "../rkt/fit-file.rkt")         ; so we can read activities
(require "../rkt/plot-builder.rkt")     ; so we can extract data series from the session
(require "../rkt/plot-axis-def.rkt")    ; so we know what data series to extract

;; The activity we will use
(define activity-file "5BN53355.FIT")

;; Read the activity from the file
(define activity (read-activity-from-file activity-file))

;; An activity contains a number of sessions (most often, only one, except for
;; multi-sport activities). We use the first session and assume it exists
(define session (car (cdr (assq 'sessions activity))))

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

