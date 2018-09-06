#lang racket/base
;; trends-bw.rkt -- bodyweight trend chart
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2016, 2018 Alex Harsányi <AlexHarsanyi@gmail.com>
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
         plot/no-gui
         racket/class
         racket/gui/base
         racket/match
         racket/hash
         math/statistics
         "../database.rkt"
         "../fmt-util.rkt"
         "../plot-util.rkt"
         "../widgets/main.rkt"
         "../data-frame/df.rkt"
         "../data-frame/sql.rkt"
         "../data-frame/csv.rkt"
         "../data-frame/least-squares-fit.rkt"
         "../data-frame/statistics.rkt"
         "../data-frame/describe.rkt"
         "../data-frame/colors.rkt"
         "../utilities.rkt"
         "../dbutil.rkt"
         "trends-chart.rkt")

(provide bw-trends-chart%)

;; SQL query to retrieve bodyweight data from the database
(define-sql-statement sql-query "../../sql/queries/tc-bodyweight.sql")

;; Read bodyweight data from the database and return a data frame with the
;; contents.  PARAMS define the parameters for the query, as returned by
;; `chart-settings-dialog%/get-chart-settings`.  In addition to the bodyweight
;; data, the least squares fit and the plot bounds (min and max timestamps and
;; bodyweight) are added as properties of the returned data frame.
(define (read-data db params)
  (match-define (cons start end) (hash-ref params 'timestamps))
  (define trendline (hash-ref params 'trendline))
  (define df (df-read/sql db (sql-query) start end))
  (if (> (df-row-count df) 0)
      (begin
        (df-put-property
         df
         'least-squares-fit
         (case trendline
           ((none) #f)
           ((linear)
            (df-least-squares-fit df "timestamp" "body_weight" #:mode 'linear))
           ((poly-2)
            (df-least-squares-fit df "timestamp" "body_weight"
                                  #:mode 'polynomial #:polynomial-degree 2))
           ((poly-3)
            (df-least-squares-fit df "timestamp" "body_weight"
                                  #:mode 'polynomial #:polynomial-degree 3))
           (else
            (dbglog "unknown least squares fit mode: %s" trendline)
            #f)))

        ;; Compute X and Y limits for the plot, otherwise some points
        ;; will be right on the edge of the plot.
        (let ((bwstats (df-statistics df "body_weight")))
          (df-put-property df 'bwmin (statistics-min bwstats))
          (df-put-property df 'bwmax (statistics-max bwstats))
          (df-put-property df 'bwrange (- (statistics-max bwstats)
                                          (statistics-min bwstats))))
        (let ((tsmin (df-ref df 0 "timestamp"))
              (tsmax (df-ref df (sub1 (df-row-count df)) "timestamp")))
          (df-put-property df 'tsmin tsmin)
          (df-put-property df 'tsmax tsmax)
          (df-put-property df 'tsrange (- tsmax tsmin)))

        (df-set-sorted df "timestamp" <)

        ;; Add a date series to the data frame, this makes the resulting CVS
        ;; easier to use.  The series is lazy and will only be materialized if
        ;; the data is exported.
        (df-add-lazy
           df "date" '("timestamp")
           (lambda (v)
             (match-define (list ts) v)
             (date-time->string ts)))

        df)
      #f))

;; Return the closest entry in the data frame DF for the TIMESTAMP/BODYWEIGHT
;; pair.  This is used by the hover callback to find the closest point to
;; highlight, and TIMESTAMP/BODYWEIGHT are plot coordinates.
(define (lookup-closest-entry df timestamp bodyweight)
  (define tsrange (df-get-property df 'tsrange 1))
  (define bwrange (df-get-property df 'bwrange 1))

  ;; Find the normalized Cartesian distance between TS/BW and
  ;; TIMESTAMP/BODYWEIGHT.  We use the normalized distance, because there is a
  ;; big difference in magnitudes of timestamps (UNIX timestamps) vs
  ;; bodyweight.
  (define (distance ts bw)
    (let ((delta-ts (/ (- timestamp ts) tsrange))
          (delta-bw (/ (- bodyweight bw) bwrange)))
      (+ (* delta-ts delta-ts) (* delta-bw delta-bw))))

  ;; Find the closest position for TIMESTAMP in the data frame
  (define index (df-index-of df "timestamp" timestamp))

  ;; Iterate a few points around INDEX to find the closest point...
  (match-define (list d ts bw)
    (df-fold df '("timestamp" "body_weight")
             '(#f #f #f)
             (lambda (accum val)
               (match-define (list min-distance-sqared ts bw) accum)
               (match-define (list ts0 bw0) val)
               (define distance-squared (distance ts0 bw0))
               (cond ((eq? min-distance-sqared #f)
                      (list distance-squared ts0 bw0))
                     ((< distance-squared min-distance-sqared)
                      (list distance-squared ts0 bw0))
                     (#t
                      accum)))
             #:start (max (- index 5) 0)
             #:stop (min (df-row-count df) (+ index 5))))

  ;; We will always find a closest point, but only return something if it is
  ;; actually close to the point...
  (and (< d 0.0001) (vector ts bw)))

;; Construct a hover callback function for the the bodyweight plot.
;; Highlights the BW measurement closest to the cursor and displays the data
;; and actual measurement in a label next to the value.
(define (plot-hover-callback df)
  (lambda (snip event x y)
    (define info '())
    (define (add-info tag val) (set! info (cons (list tag val) info)))
    (define renderers '())
    (define (add-renderer r) (set! renderers (cons r renderers)))

    (when (good-hover? x y event)
      (let ((entry (lookup-closest-entry df x y)))
        (when entry
          (add-renderer (points (list entry)
                                #:sym 'fullcircle
                                #:size (* (point-size) 3.0)
                                #:fill-color *trendline-color*
                                #:color *trendline-color*
                                #:alpha 1.0))
          ;; (add-renderer (pu-vrule x))
          (match-define (vector ts bw) entry)
          (add-info "Date" (calendar-date->string ts))
          (add-info "Bodyweight" (weight->string bw #t))
          (unless (null? info)
            (add-renderer (pu-label x y (make-hover-badge info)))))))

    (set-overlay-renderers snip renderers)))

(define *bw-color* (make-object color% #x2e #x8b #x57))
(define *trendline-color* (complement *bw-color*))

;; Make a plot renderer tree for the bodyweight data in the data frame DF.
;; Fit line is added if least squares fit data is available in the data frame.
(define (make-renderer-tree df)

  (define xadjust (* 0.02 (df-get-property df 'tsrange 0)))
  (define xmin (- (df-get-property df 'tsmin 0) xadjust))
  (define xmax (+ (df-get-property df 'tsmax 0) xadjust))

  (define yadjust (* 0.1 (df-get-property df 'bwrange 0)))
  (define ymin (- (df-get-property df 'bwmin 0) yadjust))
  (define ymax (+ (df-get-property df 'bwmax 0) yadjust))

  (define pts (points (df-select* df "timestamp" "body_weight")
                      #:sym 'fullcircle
                      #:size (* (point-size) 2)
                      #:fill-color *bw-color*
                      #:color *bw-color*
                      #:alpha 0.8
                      #:x-min xmin
                      #:x-max xmax
                      #:y-min ymin
                      #:y-max ymax))

  (define fit (df-get-property df 'least-squares-fit #f))

  (if fit
      (list (tick-grid)
            (function fit #:width 4 #:style 'long-dash #:color *trendline-color*)
            pts)
      (list (tick-grid)
            pts)))

(define (generate-plot output-fn renderer-tree)
  (parameterize ([plot-x-ticks (pmc-date-ticks)]
                 [plot-x-label #f]
                 [plot-y-label "Bodyweight"])
    (output-fn renderer-tree)))

(define (insert-plot-snip canvas renderer-tree)
  (generate-plot
   (lambda (renderer-tree)
     (plot-to-canvas renderer-tree canvas))
   renderer-tree))

(define (save-plot-to-file file-name width height renderer-tree)
  (generate-plot
   (lambda (renderer-tree)
     (plot-file renderer-tree file-name #:width width #:height height))
   renderer-tree))

;; Display the settings dialog for the bodyweight trends chart
(define chart-settings-dialog%
  (class* edit-dialog-base% (chart-settings-interface<%>)
    (init-field database
                [default-name "Trends"]
                [default-title "Trends Chart"])

    (super-new [title "Chart Settings"]
               [icon (edit-icon)]
               [min-height 10])

    (define name-gb (make-group-box-panel (send this get-client-pane)))
    (define name-field (new text-field% [parent name-gb] [label "Name "]))
    (send name-field set-value default-name)
    (define title-field (new text-field% [parent name-gb] [label "Title "]))
    (send title-field set-value default-title)

    (define time-gb (make-group-box-panel (send this get-client-pane)))
    (define date-range-selector (new date-range-selector% [parent time-gb]))

    (define trendline
      '(("none" . none)
        ("linear" . linear)
        ("2nd degree polynomial" . poly-2)
        ("3rd degree polynomial" . poly-3)))

    (define trendline-gb (make-group-box-panel (send this get-client-pane)))
    (define trendline-choice
      (new choice% [parent trendline-gb] [label "Trendline "]
           [choices (map car trendline)]))

    (define/public (get-chart-settings)
      (hash
       'name (send name-field get-value)
       'title (send title-field get-value)
       'date-range (send date-range-selector get-restore-data)
       'timestamps (send date-range-selector get-selection)
       'trendline (cdr (list-ref trendline (send trendline-choice get-selection)))))

    (define/public (put-chart-settings data)
      (when database
        (send date-range-selector set-seasons (db-get-seasons database)))
      (let ((name (hash-ref data 'name ""))
            (title (hash-ref data 'title ""))
            (dr (hash-ref data 'date-range #f))
            (tl (hash-ref data 'trendline 'none)))
        (send name-field set-value name)
        (send title-field set-value title)
        (when dr
          (send date-range-selector restore-from dr))
        (define index (for/first ([(v x) (in-indexed trendline)]
                                  #:when (eq? tl (cdr v)))
                        x))
        (send trendline-choice set-selection index)))

    (define/public (show-dialog parent)
      (when database
        (send date-range-selector set-seasons (db-get-seasons database)))
      (and (send this do-edit parent) (get-chart-settings)))

    ))

(define bw-trends-chart%
  (class trends-chart%
    (init-field database)
    (super-new)

    ;; Data frame holding bodyweight data, retrieved from the database
    (define df #f)

    (define/override (make-settings-dialog)
      (new chart-settings-dialog%
           [default-name "BodyWeight"]
           [default-title "Body Weight"]
           [database database]))

    (define/override (invalidate-data)
      (set! df #f))

    (define/override (is-invalidated-by-events? events)
      (or (hash-ref events 'athlete-metrics-deleted #f)
          (hash-ref events 'athlete-metrics-updated #f)
          (hash-ref events 'athlete-metrics-created #f)))

    (define/override (export-data-to-file file formatted?)
      (when df
        (df-write/csv df file "date" "timestamp" "body_weight")))

    (define/override (put-plot-snip canvas)
      (unless df
        (let ((params (send this get-chart-settings)))
          (when params
            (set! df (read-data database params)))))
      (if df
          (let ((snip (insert-plot-snip canvas (make-renderer-tree df))))
            (set-mouse-event-callback snip (plot-hover-callback df)))
          (begin
            (send canvas set-snip #f)
            (send canvas set-background-message "No data to plot"))))

    (define/override (save-plot-image file-name width height)
      ;; We assume the data is ready, and don't do anything if it is not.
      (when df
        (define rt (make-renderer-tree df))
        (save-plot-to-file file-name width height rt)))

    ))
