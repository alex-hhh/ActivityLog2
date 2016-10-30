#lang racket/base
;; inspect-best-avg.rkt -- best-avg plot view for a session.  This is not
;; supported for swimming activites.
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
         plot
         racket/class
         racket/date
         racket/gui/base
         racket/list
         racket/match
         "activity-util.rkt"
         "al-prefs.rkt"
         "series-meta.rkt"
         "data-frame.rkt"
         "dbapp.rkt"
         "metrics.rkt"
         "plot-hack.rkt"
         "snip-canvas.rkt"
         "spline-interpolation.rkt"
         "workers.rkt")

(provide best-avg-plot-panel%)

;; Filter AXIS-LIST to remove any axis definition that don't have a data
;; series in DF, a data-frame%
(define (filter-axis-list df axis-list)

  (define (valid? axis)
    (if (list? axis)
        (let ()
          (match-define (list name a1 a2) axis)
          (send df contains?
                (send a1 series-name)
                (send a2 series-name)))
        (send df contains? (send axis series-name))))

  (define (sort-key axis)
    (if (list? axis) (first axis) (send axis headline)))

  (sort (filter valid? axis-list) string<? #:key sort-key))

;; Find an axis that works in SERIES-NAME and return its position in
;; AXIS-LIST.  Return #f is not found
(define (find-axis series-name axis-list)

  (define (match? axis)
    (let ((sn (if (list? axis)
                  (car axis)
                  (send axis series-name))))
      (equal? series-name sn)))

  (for/first ([(axis index) (in-indexed axis-list)]
              #:when (match? axis))
    index))

;; Axis choices for all non lap swimming sports.  note that some axis choices
;; don't make sense, so they are not listed here, to keep the list smaller.
(define default-axis-choices
  (list
   axis-speed
   axis-pace
   axis-speed-zone
   axis-grade
   axis-grade-inverted
   axis-hr-bpm
   axis-hr-pct
   axis-hr-zone
   axis-cadence
   axis-vertical-oscillation
   axis-stance-time
   axis-stance-time-percent
   axis-stride
   axis-vratio
   axis-power
   axis-power-zone
   axis-left-torque-effectiveness
   axis-right-torque-effectiveness
   axis-left-pedal-smoothness
   axis-right-pedal-smoothness
   axis-left-power-phase-angle
   axis-left-peak-power-phase-angle
   axis-right-power-phase-angle
   axis-right-peak-power-phase-angle
   ))

;; Return the start of today as a UNIX timestamp (in seconds)
(define (this-day-start)
  (let ((now (current-date)))
    (date->seconds
     (date 0 0 0
           (date-day now) (date-month now) (date-year now)
           0 0 0 (date-time-zone-offset now)))))

;; Default time periods for the "Show bests" plot.
(define default-periods
  (list
   (list "None" (lambda () #f))
   (list "Last Month"
         (lambda ()
           (let ((end (this-day-start)))
             (cons (- end (* 30 24 3600)) (+ end (* 24 3600))))))
   (list "Last 6 Weeks"
         (lambda ()
           (let ((end (this-day-start)))
             (cons (- end (* 42 24 3600)) (+ end (* 24 3600))))))
   (list "Last 3 Months"
         (lambda ()
           (let ((end (this-day-start)))
             (cons (- end (* 90 24 3600)) (+ end (* 24 3600))))))
   (list "Last 6 Months"
         (lambda ()
           (let ((end (this-day-start)))
             (cons (- end (* 180 24 3600)) (+ end (* 24 3600))))))
   (list "Last 12 Months"
         (lambda ()
           (let ((end (this-day-start)))
             (cons (- end (* 365 24 3600)) (+ end (* 24 3600))))))
   (list "All Time"
         (lambda ()
           (let ((end (this-day-start)))
             (cons #f #f))))))

;; Return a list of time intervals for each defined season in the database.
;; The list is in the same format as `default-periods'
(define (make-periods-from-seasons db)
  (for/list (((name start end)
              (in-query db "select name, start_date, end_date from SEASON order by start_date")))
    (list (format "~a season" name) (lambda () (cons start end)))))

;; Find an axis that works in SERIES-NAME and return its position in
;; AXIS-LIST.  Return #f is not found
(define (find-period name period-list)

  (define (match? ti) (equal? name (first ti)))

  (for/first ([(ti index) (in-indexed period-list)]
              #:when (match? ti))
    index))

;; Return a list of session id based that match SPORT, a (Vectorof sport-id
;; sub-sport-id) and TIME-INTERVAL, a (Cons start end).
(define (get-candidate-sessions db sport period)
  (if period
      (match-let (((vector sport-id sub-sport-id) sport)
                  ((cons start end) period))
        (fetch-candidate-sessions db sport-id sub-sport-id start end))
      '()))

;; Return a valid Y-RANGE for BAVG, a best-avg set.  The interval is slightly
;; larger than the min and max Y value in the set.
(define (bavg-y-range bavg)
  (let ((min-y #f)
        (max-y #f))
    (for ([item bavg])
      (match-define (list sid ts duration value) item)
      (set! min-y (if min-y (min min-y value) value))
      (set! max-y (if max-y (max max-y value) value)))
    (when (and min-y max-y)
      (let ((padding (* 0.05 (- max-y min-y))))
        (set! min-y (- min-y padding))
        (set! max-y (+ max-y padding))))
    (values min-y max-y)))

;; Return suitable plot bounds for a best-avg plor, considering all input
;; data.  Returns four values: MIN-X, MAX-X, MIN-Y, MAX-Y.
(define (plot-bounds axis zero-base? best-avg-data bests-data)
  (let-values (((min-x max-x min-y max-y) (get-best-avg-bounds best-avg-data))
               ((bmin-y bmax-y) (bavg-y-range bests-data)))
    (let ((inverted? (send axis inverted-best-avg?)))
      (values
       min-x
       max-x
       (if zero-base? 0 (if inverted? (if bmin-y (min bmin-y min-y) min-y) min-y))
       (if (not inverted?) (if bmax-y (max bmax-y max-y) max-y) max-y)
      ))))

(define best-avg-plot-panel%
  (class object% (init parent) (super-new)
    (define pref-tag 'activity-log:best-avg-plot)

    (define axis-choices '())
    (define period-choices '())
    
    (define selected-axis 0)
    (define selected-aux-axis 0)
    (define selected-period 0)
    (define zero-base? #f)

    ;; Store plot parameters by sport type, to be restored when a session from
    ;; the same sport is used.  See `save-params-for-sport',
    ;; 'restore-params-for-sport'.
    ;;
    ;; maps SPORT => params-by-series hash
    (define params-by-sport (make-hash))

    ;; Store plot parameters for each selected series (what AUX axis and what
    ;; Bests period to show).  See `save-params-for-series',
    ;; 'restore-params-for-series'
    ;;
    ;; maps SERIES NAME -> (List AUX-SERIES-NAME BESTS-PERIOD-NAME ZERO-BASE?)
    (define params-by-series (make-hash))

    ;; Restore the saved preferences now.
    (let ((pref (al-get-pref pref-tag (lambda () #f))))
      (when (and pref (eqv? (length pref) 1))
        (set! params-by-sport (hash-copy (first pref)))))

    ;; Root widget of the entire scatter plot panel
    (define panel
      (new (class vertical-panel%
             (init) (super-new)
             (define/public (interactive-export-image)
               (on-interactive-export-image))
             (define/public (interactive-export-data formatted?)
               (on-interactive-export-data formatted?)))
           [parent parent] [border 5] [spacing 5]
           [alignment '(center top)]))

    ;; Holds the widgets that control the look of the plot
    (define control-panel
      (new horizontal-panel%
           [parent panel] [spacing 10] [border 0]
           [alignment '(center center)]
           [stretchable-height #f]))

    (define axis-choice-box
      (new choice% [parent control-panel] [choices '()]
           [min-width 300] [label "Best Avg: "]
           [callback (lambda (c e) (on-axis-changed (send c get-selection)))]))

    (define aux-axis-choice-box
      (new choice% [parent control-panel] [choices '()]
           [min-width 300] [label "Auxiliary: "]
           [callback (lambda (c e) (on-aux-axis-changed (send c get-selection)))]))

    (define period-choice-box
      (new choice% [parent control-panel] [choices '()]
           [min-width 300] [label "Show Best: "]
           [callback (lambda (c e) (on-period-changed (send c get-selection)))]))

    (define zero-base-check-box
      (new check-box% [parent control-panel]
           [value zero-base?] [label "Zero Base"]
           [callback (lambda (c e) (on-zero-base (send c get-value)))]))

    ;; Pasteboard to display the actual BEST-AVG plot
    (define plot-pb (new snip-canvas% [parent panel]))

    ;; Graph data
    (define data-frame #f)
    (define best-avg-data '())
    (define best-avg-aux-data '())
    (define inhibit-refresh 0)
    (define plot-rt #f)
    ;; The plot render tree for the "bests" plot
    (define best-rt #f)
    (define best-rt-generation 0)
    (define bests-data '())
    (define data-cache (make-hash))
    ;; The name of the file used by 'on-interactive-export-image'. This is
    ;; remembered between subsequent exports, but reset when one of the axis
    ;; changes.
    (define img-export-file-name #f)
    (define data-export-file-name #f)

    (define (current-sport)
      (if data-frame (send data-frame get-property 'sport) #f))

    (define (get-series-axis)
      (list-ref axis-choices selected-axis))

    (define (get-aux-axis)
      (and (> selected-aux-axis 0)
           (list-ref axis-choices (sub1 selected-aux-axis))))

    (define (get-best-rt-generation) best-rt-generation)

    (define (install-axis-choices)
      (set! axis-choices (filter-axis-list data-frame default-axis-choices))
      (send axis-choice-box clear)
      (send aux-axis-choice-box clear)
      (send aux-axis-choice-box append "None")
      (for ([a axis-choices])
        (let ((n (send a axis-label)))
          (send axis-choice-box append n)
          (send aux-axis-choice-box append n))))

    (define (install-period-choices)
      (set! period-choices
            (append default-periods
                    (make-periods-from-seasons (current-database))))
      (send period-choice-box clear)
      (for ([t period-choices])
        (send period-choice-box append (car t))))

    (define (on-period-changed new-index)
      (unless (equal? selected-period new-index)
        (set! selected-period new-index)
        (refresh-bests-plot)))

    (define (on-axis-changed new-index)
      (unless (equal? selected-axis new-index)
        (save-params-for-series)
        (set! selected-axis new-index)
        (set! img-export-file-name #f)
        (set! data-export-file-name #f)
        (restore-params-for-series)
        (refresh-bests-plot)
        (refresh-plot)))

    (define (on-aux-axis-changed new-index)
      (unless (equal? selected-aux-axis new-index)
        (set! selected-aux-axis new-index)
        (set! img-export-file-name #f)
        (set! data-export-file-name #f)
        (refresh-plot)))

    (define (on-zero-base flag)
      (unless (equal? zero-base? flag)
        (set! zero-base? flag)
        (refresh-plot)))

    (define (put-plot-snip)
      (when plot-rt
        (let ((rt (list (tick-grid) plot-rt)))
          (when best-rt
            (set! rt (cons best-rt rt)))
          (let ((best-avg-axis (get-series-axis))
                (aux-axis (get-aux-axis)))
            (let-values (((min-x max-x min-y max-y) (plot-bounds (get-series-axis) zero-base? best-avg-data bests-data)))
              ;; aux data might not exist, if an incorrect/invalid aux-axis is
              ;; selected
              (if best-avg-aux-data
                  (let ((ivs (mk-inverse best-avg-aux-data best-avg-data zero-base?)))
                    (parameterize ([plot-x-ticks (best-avg-ticks)]
                                   [plot-x-label "Duration"]
                                   [plot-x-transform log-transform]
                                   [plot-y-ticks (send best-avg-axis plot-ticks)]
                                   [plot-y-label (send best-avg-axis axis-label)]
                                   [plot-y-far-ticks (transform-ticks (send aux-axis plot-ticks) ivs)]
                                   [plot-y-far-label (send aux-axis axis-label)])
                      (plot-snip/hack plot-pb rt
                                      #:x-min min-x #:x-max max-x
                                      #:y-min min-y #:y-max max-y
                                      )))
                (parameterize ([plot-x-ticks (best-avg-ticks)]
                               [plot-x-label "Duration"]
                               [plot-x-transform log-transform]
                               [plot-y-ticks (send best-avg-axis plot-ticks)]
                               [plot-y-label (send best-avg-axis axis-label)])
                  (plot-snip/hack plot-pb rt
                                  #:x-min min-x #:x-max max-x
                                  #:y-min min-y #:y-max max-y))))))))

    (define (refresh-plot)
      (set! plot-rt #f)
      (send plot-pb set-background-message "Working...")
      (send plot-pb set-snip #f)
      (unless (> inhibit-refresh 0)
        ;; Capture all needed data, as we will work in a different thread.
        (let ((df data-frame)
              (cache data-cache)
              (axis (get-series-axis))
              (aux-axis (get-aux-axis))
              (zerob? zero-base?))
          (queue-task
           "inspect-best-avg%/refresh-plot"
           (lambda ()
             (define data
               (or (hash-ref cache axis #f)
                   (let ((inverted? (send axis inverted-best-avg?))
                         (series (send axis series-name)))
                     (and (send df contains? series)
                          (df-best-avg df series #:inverted? inverted?)))))
             (hash-set! cache axis data)
             ;; rebuild auxiliary data here
             (define aux-data
               (and data aux-axis
                    (let ((series (send aux-axis series-name)))
                      (and (send df contains? series)
                           (df-best-avg-aux df series data)))))
             (define rt
               (and data
                    (make-best-avg-renderer
                     data aux-data
                     #:color1 (send axis plot-color)
                     #:color2 (and aux-axis (send aux-axis plot-color))
                     #:zero-base? zerob?)))
             (queue-callback
              (lambda ()
                (cond (rt
                       (set! best-avg-data data)
                       (set! best-avg-aux-data aux-data)
                       (set! plot-rt rt)
                       (set! cache data-cache)
                       (put-plot-snip))
                      (#t
                       (send plot-pb set-background-message "No data for plot"))))))))))

    (define (refresh-bests-plot)
      (define debug-tag "inspect-best-avg%/refresh-bests-plot")
      (unless (> inhibit-refresh 0)
        (set! best-rt-generation (add1 best-rt-generation))
        (set! best-rt #f)
        (let ((axis (get-series-axis))
              (generation best-rt-generation)
              (time-interval selected-period))
          (queue-task
           debug-tag
           (lambda ()
             (define candidates
               (get-candidate-sessions
                (current-database)
                (send data-frame get-property 'sport)
                ((second (list-ref period-choices time-interval)))))

             (define inverted? (send axis inverted-best-avg?))
             (define sname (send axis series-name))
             (define bavg (get-best-avg/merged candidates sname inverted?))

             (let ((fn (bavg->spline-fn bavg)))
               (define brt
                 (and fn
                      (function-interval
                       ;; The bottom (or top) of the shaded area needs to
                       ;; cover the entire plot and it is too difficult to
                       ;; find out the true min/max X value of the plot, so we
                       ;; just put what we hope are large enough values.  The
                       ;; plot will be clipped at the right spot.
                       (if inverted? (lambda (x) 10000) (lambda (x) -10000))
                       fn
                       #:color (send axis plot-color)
                       #:alpha 0.1
                       #:line2-color "black"
                       #:line2-width 0.5
                       #:line1-style 'transparent)))
               (queue-callback
                (lambda ()
                  ;; Discard changes if there was a new request since ours was
                  ;; sumbitted.
                  (let ((current-generation (get-best-rt-generation)))
                    (when (= generation current-generation)
                      (set! best-rt brt)
                      (set! bests-data bavg)
                      (put-plot-snip)))))))))))

    (define (save-params-for-sport)
      (when (current-sport)
        (save-params-for-series)
        (let ((axis (get-series-axis)))
          (hash-set! params-by-sport
                     (current-sport)
                     (list 'gen1 (send axis series-name) params-by-series)))))
               
    (define (restore-params-for-sport)
      (set! inhibit-refresh (add1 inhibit-refresh))
      (let ((params (hash-ref params-by-sport (current-sport) #f)))
        (if (and params (eq? (car params) 'gen1))
            (match-let (((list tag series-name pbs) params))
              (let ((selection 0))
                (when series-name
                  (let ((index (find-axis series-name axis-choices)))
                    
                    (set! selection (min (or index 0) (sub1 (length axis-choices))))))
                (on-axis-changed selection)
                (set! params-by-series (hash-copy pbs))))
            (begin
              (on-axis-changed 0)
              (set! params-by-series (make-hash))))
        (send axis-choice-box set-selection selected-axis)
        (restore-params-for-series)
        (set! inhibit-refresh (sub1 inhibit-refresh))))

    (define (save-params-for-series)
      (when (current-sport)
        (let ((axis (get-series-axis))
              (aux-axis (get-aux-axis))
              (period (list-ref period-choices selected-period)))
          (hash-set!
           params-by-series
           (send axis series-name)
           (list 'gen1
                 (and aux-axis (send aux-axis series-name))
                 (first period)
                 zero-base?)))))

    (define (restore-params-for-series)
      (set! inhibit-refresh (add1 inhibit-refresh))
      (let* ((axis (get-series-axis))
             (series-name (send axis series-name))
             (params (hash-ref params-by-series series-name #f)))
        (if (and params (eq? (car params) 'gen1))
            (match-let (((list tag aux-series-name period-name zb?) params))
              (let ((selection 0))
                (when aux-series-name
                  (let ((index (find-axis aux-series-name axis-choices)))
                    (set! selection (add1 (min (or index 0) (sub1 (length axis-choices)))))))
                (on-aux-axis-changed selection))
              (let ((selection 0))
                (when period-name
                  (let ((index (find-period period-name period-choices)))
                    (set! selection (min (or index 0) (sub1 (length period-choices))))))
                (on-period-changed selection)))
            (begin
              (on-aux-axis-changed 0)
              (on-period-changed 0))))
      (send aux-axis-choice-box set-selection selected-aux-axis)
      (send period-choice-box set-selection selected-period)
      (set! inhibit-refresh (sub1 inhibit-refresh)))

    (define/public (save-visual-layout)
      (save-params-for-sport)
      (al-put-pref pref-tag (list params-by-sport)))

    ;; Return a suitable file name for use by 'on-interactive-export-image'.
    ;; If 'export-file-name' is set, we use that, otherwise we compose a file
    ;; name from the session id and axis names of the plot.
    (define (get-default-export-file-name (extenstion "png"))
      (let ((sid (send data-frame get-property 'session-id))
            (axis1 (get-series-axis))
            (axis2 (get-aux-axis)))
        (cond ((and sid axis1 axis2)
               (format "best-avg-~a-~a-~a.~a" sid
                       (send axis1 series-name)
                       (send axis2 series-name)
                       extenstion))
              ((and sid axis1)
               (format "best-avg-~a-~a.~a" sid
                       (send axis1 series-name)
                       extenstion))
              (#t
               (format "best-avg.~a" extenstion)))))

    (define/public (on-interactive-export-image)
      (let ((file (put-file "Select file to export to" #f #f
                            (or img-export-file-name (get-default-export-file-name "png"))
                            "png" '()
                            '(("PNG Files" "*.png") ("Any" "*.*")))))
        (when file
          (set! img-export-file-name file)
          (send plot-pb export-image-to-file file))))

    (define/public (on-interactive-export-data formatted?)
      (let ((file (put-file "Select file to export to" #f #f
                            (or data-export-file-name (get-default-export-file-name "csv"))
                            "csv" '()
                            '(("CSV Files" "*.csv") ("Any" "*.*")))))
        (when file
          (set! data-export-file-name file)
          (call-with-output-file file
            (lambda (out) (export-data-as-csv out formatted?))
            #:mode 'text #:exists 'truncate))))

    (define (export-data-as-csv out formatted?)
      (define have-aux? (and best-avg-aux-data (= (length best-avg-aux-data) (length best-avg-data))))
      (define have-bests? (and bests-data (<= (length best-avg-data) (length bests-data))))
      (write-string "Timestamp, Duration, Value" out)
      (when have-aux? (write-string ", Aux Value" out))
      (when have-bests? (write-string ", Best SID, Best Timestamp, Best Value" out))
      (newline out)
      (for ((index (in-range (length best-avg-data))))
        (match-define (vector d m s)
          (list-ref best-avg-data index))
        (if m
            (write-string (format "~a, ~a, ~a"
                                  (exact->inexact s)
                                  (exact->inexact d)
                                  (exact->inexact m))
                          out)
            (write-string (format ", , ") out))
        (when have-aux?
          (match-define (vector d m s)
            (list-ref best-avg-aux-data index))
          (if m
              (write-string (format ", ~a" (exact->inexact m)) out)
              (write-string ", " out)))
        (when have-bests?
          (match-define (list sid ts d m)
            (list-ref bests-data index))
          (format "bests: ~a ~a ~a ~a~%" sid ts d m)
          (if m
              (write-string (format ", ~a, ~a, ~a" sid (exact->inexact ts) (exact->inexact m)) out)
              (write-string (format ", , ,") out)))
        (newline out)))

    (define/public (set-session s df)
      (set! inhibit-refresh (add1 inhibit-refresh))
      (save-params-for-sport)
      (set! data-frame df)
      (set! data-cache (make-hash))
      (set! img-export-file-name #f)
      (set! data-export-file-name #f)
      (set! plot-rt #f)
      (set! best-rt #f)
      (install-axis-choices)
      (install-period-choices)
      (restore-params-for-sport)
      (set! inhibit-refresh (sub1 inhibit-refresh))
      (refresh-bests-plot)
      (refresh-plot))

    ))
