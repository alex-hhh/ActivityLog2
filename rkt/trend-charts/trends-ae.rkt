#lang racket/base

;; trends-ae.rkt -- aerobic efficiency trend charts
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2018 Alex Harsányi <AlexHarsanyi@gmail.com>
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
;;
;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.

(require db/base
         plot/no-gui
         racket/class
         racket/gui/base
         racket/match
         racket/hash
         math/statistics
         racket/format
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

(provide ae-trends-chart%)

(define-sql-statement sql-running "../../sql/queries/tc-aeff-running.sql")
(define-sql-statement sql-cycling "../../sql/queries/tc-aeff-cycling.sql")

(define *run-color* (mix-neutral (make-object color% 0 0 255)))
(define *cycle-color* (mix-neutral (make-object color% 255 0 0)))
(define *run-trendline-color* (complement *run-color*))
(define *cycle-trendline-color* (complement *cycle-color*))

;; Read aerobic efficiency data from the database and return a data frame with
;; the contents.  PARAMS contains the parameters for the plot, as returned by
;; `chart-settings-dialog%/get-chart-settings`.  Depending on the sport in
;; PARAMS, either the SQL-RUNNING or SQL-CYCLING queries are used -- these
;; queries actually compute the AE value for each session.
;;
;; In addition to this, some properties are added to the data frame for easier
;; plotting: a least squares fit structure (if requested by PARAMS) as well as
;; the bounds of the data for plotting purposes.  Returns a data frame or #f
;; if no data could be retrieved.
(define (read-data db params)
  (match-define (cons start end) (hash-ref params 'timestamps))
  (define trendline (hash-ref params 'trendline))
  (define sport (hash-ref params 'sport))
  ;; sport will be #f when trying to create this chart on a database that has
  ;; no running or cycling activities.
  (and
   sport
   (let* ((running? (case (car sport) ((1) #t) ((2) #f)))
          (query (if running? (sql-running) (sql-cycling)))
          (df (df-read/sql db query start end)))
     (and
      (> (df-row-count df) 0)
      (begin
        (df-put-property df 'running? running?)
        (df-put-property
         df
         'least-squares-fit
         (case trendline
           ((none) #f)
           ((linear)
            (df-least-squares-fit df "timestamp" "ae" #:mode 'linear))
           ((poly-2)
            (df-least-squares-fit df "timestamp" "ae"
                                  #:mode 'polynomial #:polynomial-degree 2))
           ((poly-3)
            (df-least-squares-fit df "timestamp" "ae"
                                  #:mode 'polynomial #:polynomial-degree 3))
           (else
            (dbglog "unknown least squares fit mode: %s" trendline)
            #f)))

        ;; Compute X and Y limits for the plot, otherwise some points
        ;; will be right on the edge of the plot.
        (let ((aestats (df-statistics df "ae")))
          (df-put-property df 'aemin (statistics-min aestats))
          (df-put-property df 'aemax (statistics-max aestats))
          (df-put-property df 'aerange (- (statistics-max aestats)
                                          (statistics-min aestats))))
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

        df)))))

;; Find the closest entry in the data frame DF for the
;; `timestamp`/`aerobic-efficiency` pair.  The TIMESTAMP/AEROBIC-EFFICIENCY
;; pair comes from a plot hover callback and represents the position where the
;; mouse is hovering on the plot.  This function returns the actual
;; TIMESTAMP/AEROBIC-EFFICIENCY point from the data frame DF which is closest
;; to this mouse position.  The function will return #f if the mouse is too
;; far away from any point in the data frame.
(define (lookup-closest-entry df timestamp aerobic-efficiency)
  (define tsrange (df-get-property df 'tsrange 1))
  (define aerange (df-get-property df 'aerange 1))

  ;; Find the normalized Cartesian distance between TS/AE and TIMESTAMP/AE.
  ;; We use the normalized distance, because there is a big difference in
  ;; magnitudes of timestamps (UNIX timestamps) vs ae.
  (define (distance ts ae)
    (let ((delta-ts (/ (- timestamp ts) tsrange))
          (delta-ae (/ (- aerobic-efficiency ae) aerange)))
      (+ (* delta-ts delta-ts) (* delta-ae delta-ae))))

  ;; Find the closest position for TIMESTAMP in the data frame, this should be
  ;; fast, as the "timestamp" series is sorted.
  (define index (df-index-of df "timestamp" timestamp))

  ;; Iterate a few points around INDEX to find the closest point...
  (match-define (list d ts ae)
    (df-fold df '("timestamp" "ae")
             '(#f #f #f)
             (lambda (accum val)
               (match-define (list min-distance-sqared ts ae) accum)
               (match-define (list ts0 ae0) val)
               (define distance-squared (distance ts0 ae0))
               (cond ((eq? min-distance-sqared #f)
                      (list distance-squared ts0 ae0))
                     ((< distance-squared min-distance-sqared)
                      (list distance-squared ts0 ae0))
                     (#t
                      accum)))
             #:start (max (- index 5) 0)
             #:stop (min (df-row-count df) (+ index 5))))

  ;; We will always find a closest point, but only return something if it is
  ;; actually close to the point...
  (and (< d 0.0001) (vector ts ae)))

;; Construct a hover callback function for the data frame DF.  The hover
;; callback function will highlight the AE point closest to the mouse and will
;; display information about it.
(define (plot-hover-callback df)
  (define running? (df-get-property df 'running?))
  (define trendline-color (if running? *run-trendline-color* *cycle-trendline-color*))
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
                                #:fill-color trendline-color
                                #:color trendline-color
                                #:alpha 1.0))
          ;; (add-renderer (pu-vrule x))
          (match-define (vector ts ae) entry)
          (add-info "Date" (calendar-date->string ts))
          (if running?
              (match-let ([(vector duration distance hr ae)
                           (df-lookup
                            df "timestamp"
                            '("duration" "distance" "heart_rate" "ae") ts)])
                (add-info "Duration" (duration->string duration))
                (add-info "Distance" (distance->string distance #t))
                (add-info "Avg Heart Rate" (heart-rate->string/bpm hr))
                (add-info "Aerobic Efficiency" (~r #:precision 3 ae)))
              (match-let ([(vector duration distance pwr np hr ae)
                          (df-lookup
                           df "timestamp"
                           '("duration" "distance" "power" "npower" "heart_rate" "ae") ts)])
                (add-info "Duration" (duration->string duration))
                (add-info "Distance" (distance->string distance #t))
                (add-info "Avg Heart Rate" (heart-rate->string/bpm hr))
                (add-info "Avg Power" (power->string pwr #t))
                (add-info "Iso Power" (power->string np #t))
                (add-info "Aerobic Efficiency" (~r #:precision 3 ae))))
          (unless (null? info)
            (add-renderer (pu-label x y (make-hover-badge info)))))))

    (set-overlay-renderers snip renderers)))

;; Construct a plot renderer tree for the aerobic efficiency data in the data
;; frame DF.
(define (make-renderer-tree df)

  (define color (if (df-get-property df 'running?) *run-color* *cycle-color*))
  (define trendline-color
    (if (df-get-property df 'running?)
        *run-trendline-color*
        *cycle-trendline-color*))

  (define xadjust (* 0.02 (df-get-property df 'tsrange 0)))
  (define xmin (- (df-get-property df 'tsmin 0) xadjust))
  (define xmax (+ (df-get-property df 'tsmax 0) xadjust))

  (define yadjust (* 0.1 (df-get-property df 'aerange 0)))
  (define ymin (- (df-get-property df 'aemin 0) yadjust))
  (define ymax (+ (df-get-property df 'aemax 0) yadjust))

  (define pts (points (df-select* df "timestamp" "ae")
                      #:sym 'fullcircle
                      #:size (* (point-size) 2)
                      #:fill-color color
                      #:color color
                      #:alpha 0.8
                      #:x-min xmin
                      #:x-max xmax
                      #:y-min ymin
                      #:y-max ymax))

  (define fit (df-get-property df 'least-squares-fit #f))

  (if fit
      (list (tick-grid)
            (function fit #:width 4 #:style 'long-dash #:color trendline-color)
            pts)
      (list (tick-grid)
            pts)))

(define (generate-plot output-fn renderer-tree)
  (parameterize ([plot-x-ticks (pmc-date-ticks)]
                 [plot-x-label #f]
                 [plot-y-label "Aerobic Efficiency"])
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

;; Display the settings dialog for the aerobic efficiency trends chart and
;; manage the chart settings.
(define chart-settings-dialog%
  (class edit-dialog-base%
    (init-field database
                [default-name "AE"]
                [default-title "Aerobic Efficiency"])

    (super-new [title "Chart Settings"]
               [icon (edit-icon)]
               [min-height 10])

    (define name-gb (make-group-box-panel (send this get-client-pane)))
    (define name-field (new text-field% [parent name-gb] [label "Name "]))
    (send name-field set-value default-name)
    (define title-field (new text-field% [parent name-gb] [label "Title "]))
    (send title-field set-value default-title)

    (define (on-sport-selected sport)
      (void))

    (define session-filter
      (new session-filter%
           [parent (send this get-client-pane)]
           [database database]
           ;; Only show running and cycling (and their
           ;; sub-sports in the selector.
           [sport-filter (lambda (sport)
                           (memq (vector-ref sport 1) '(1 2)))]
           [sport-selected-callback on-sport-selected]))

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
      (hash-union
       (send session-filter get-restore-data)
       (hash
        'name (send name-field get-value)
        'title (send title-field get-value)
        'trendline (cdr (list-ref trendline (send trendline-choice get-selection))))))

    (define/public (put-chart-settings data)
      (send session-filter restore-from data)
      (let ((name (hash-ref data 'name ""))
            (title (hash-ref data 'title ""))
            (tl (hash-ref data 'trendline 'none)))
        (send name-field set-value name)
        (send title-field set-value title)
        (define index (for/first ([(v x) (in-indexed trendline)]
                                  #:when (eq? tl (cdr v)))
                        x))
        (send trendline-choice set-selection index)))

    (define/public (show-dialog parent)
      (send session-filter on-before-show-dialog)
      (and (send this do-edit parent) (get-chart-settings)))

    ))

;; Aerobic efficiency trends chart
(define ae-trends-chart%
  (class trends-chart%
    (init-field database)
    (super-new)

    ;; Data frame holding aerobic efficiency data, retrieved from the database
    (define df #f)

    (define/override (make-settings-dialog)
      (new chart-settings-dialog%
           [default-name "Aerobic Efficiency"]
           [default-title "Aerobic Efficiency"]
           [database database]))

    (define/override (invalidate-data)
      (set! df #f))

    (define/override (is-invalidated-by-events? events)
      (or (hash-ref events 'session-created #f)
          (hash-ref events 'session-updated #f)
          (hash-ref events 'session-deleted #f)))

    (define/override (export-data-to-file file formatted?)
      (when df
        (if (df-get-property df 'running?)
            (df-write/csv df file "date" "timestamp"
                          "duration" "distance" "speed" "heart_rate" "ae")
            (df-write/csv df file "date" "timestamp"
                          "duration" "distance" "power" "npower" "heart_rate" "ae"))))

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
