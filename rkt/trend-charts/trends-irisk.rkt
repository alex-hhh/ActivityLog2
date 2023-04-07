#lang racket/base

;; trends-irisk.rkt -- Injuri Risk Trends chart for swimming, biking and running
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2020, 2021, 2023 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require
 racket/runtime-path
 data-frame
 data-frame/spline
 racket/class
 racket/draw
 racket/format
 racket/math
 math/statistics
 plot
 plot-container/hover-util
 racket/gui/base
 gui-widget-mixins
 racket/match
 plot-container
 "trends-chart.rkt"
 "../dbutil.rkt"
 "../widgets/grid-pane.rkt"
 "../widgets/main.rkt"
 "../utilities.rkt")

(define weeks-input-field%
  (validate-mixin
   string->number number->string
   (tooltip-mixin
    (decorate-mixin
     (decorate-with "weeks" #:validate string->number)
     (cue-mixin "weeks" text-field%)))))

(define percent-input-field%
  (validate-mixin
   (lambda (s)
     (define n (string->number s))
     (if (and n (>= n 0) (<= n 100)) n #f))
   number->string
   (tooltip-mixin
    (decorate-mixin
     (decorate-with "%" #:validate string->number)
     (cue-mixin "0 .. 100%" text-field%)))))


;;............................................... data source connection ....

;; This is the location of the SQL query we run to retrieve the data.  It is
;; defined using `define-runtime-path` so the file is packaged when a
;; stand-alone executable is created.
(define-runtime-path query-file "../../sql/queries/tc-irisk.sql")

;; The query read from the above file.  Note that this is a function and needs
;; to be called as (query)
(define irisk-sql (define-sql-statement query-file))



;;............................................... fetch and process data ....

;; Fetch the data from the database by running irisk-sql and storing data in a
;; data frame.  Note that the query requires two parameters, which are unix
;; timestamps representing the start and end timestamps (end might be in the
;; future).  This function accepts two parameters WEEKS-BACK and WEEKS-FORWARD
;; representing the number of weeks before and after the current time -- it is
;; easier to work in weeks than in UNIX timestamps.
(define (fetch-irisk-data db weeks-back weeks-forward)
  (define now (current-seconds))
  (define df (df-read/sql
              db
              (irisk-sql)
              (- now (* weeks-back 7 24 3600))
              (+ now (* weeks-forward 7 24 3600))))
  ;; Mark the timestamp series as sorted, so we can do lookups on it.
  (df-set-sorted! df "timestamp" <)
  df)

;; Add a series that is a smoothing of BASE-SERIES.  The smoothing is done by
;; applying a low pass filter where:
;;
;;     smoothed = (1 - factor) * smoothed + factor * current
;;
(define (add-smoothed-series df base-series load-series factor)
  (define smoothed 0)
  (df-add-derived!
   df
   load-series                          ; series name to create
   (list base-series)                   ; selected series
   (lambda (l)
     ;; 'l' is a list containing a "row" of the selected series, since we only
     ;; select one series (base-series), the list contains one element, which
     ;; we extract here:
     (define value (list-ref l 0))

     ;; this weeks volume contributes to next weeks load, so we return the
     ;; previous smoothed value (`begin0`) and computing the next one.
     (begin0 smoothed
       (set! smoothed (+ (* (- 1 factor) smoothed) (* factor (or value 0))))))))

;; Add all smoothed series for all "volumes" we retrieved from the database
;; (swim, bike, run) x (duration, distance, effort)
(define (add-all-smoothed-series df factor)

  (add-smoothed-series df "sDuration" "sDurationLoad" factor)
  (add-smoothed-series df "sDist" "sDistLoad" factor)
  (add-smoothed-series df "sTss" "sTssLoad" factor)

  (add-smoothed-series df "bDuration" "bDurationLoad" factor)
  (add-smoothed-series df "bDist" "bDistLoad" factor)
  (add-smoothed-series df "bTss" "bTssLoad" factor)

  (add-smoothed-series df "rDuration" "rDurationLoad" factor)
  (add-smoothed-series df "rDist" "rDistLoad" factor)
  (add-smoothed-series df "rTss" "rTssLoad" factor))


;;.................................................. colors for the plot ....

;; Default plot colors are basic, and don't look very nice.  Pick some nicer
;; ones from the color database.  For a list and previous of available colors,
;; see
;;
;; https://docs.racket-lang.org/draw/color-database___.html?q=the-color-database

;; Color for the "current value" dots on the plot
(define dot-color (send the-color-database find-color "SeaGreen"))

;; Color for the currently highlighted dot.
(define hl-dot-color (send the-color-database find-color "DeepSkyBlue"))

;; Color for the warning line on the plot
(define warning-color (send the-color-database find-color "DarkGoldenrod"))

;; Color for the danger line on the plot
(define danger-color (send the-color-database find-color "Firebrick"))

;; Color for the vertical line indicating the current week
(define this-week-color (send the-color-database find-color "RoyalBlue"))

;; Background color for swim plots
(define swim-bg-color (send the-color-database find-color "Ivory"))

;; Background color for bike plots
(define bike-bg-color (send the-color-database find-color "LavenderBlush"))

;; Background color for the run plots
(define run-bg-color (send the-color-database find-color "AliceBlue"))



;.................................................... creating the plots ....

;; This routine creates a plot for a single data series and its corresponding
;; load series.
(define (make-plot df series load-series
                   #:title (title #f)
                   #:y-label (y-label #f)
                   #:offset (offset 10)
                   #:background (bg 0)
                   #:format-value (format-value ~a)
                   #:width width
                   #:height height)

  ;; Select the data points from the data frame, these will be displayed as
  ;; individual dots on the plot
  (define data (df-select* df "timestamp" series #:start offset))

  ;; Select the load points from the data frame
  (define load-data (df-select* df "timestamp" load-series #:start offset))

  ;; Make a spline function out of the load points, this will make the load
  ;; lines smooth and will look much nicer.
  (define load-fn (spline load-data))

  ;; Make the "warning line" 20% above the current load
  (define (warning-line x) (* (or (load-fn x) 0) 1.2))

  ;; Make the "danger line" 60% above the current load
  (define (danger-line x) (* (or (load-fn x) 0) 1.6))

  ;; Obtain the maximum Y value for the plot, so we can make the plot taller
  ;; (by default, the plot library will make the plot so that the data just
  ;; fits, but it looks nicer if we add some extra room at the top...
  (define stats (df-statistics df series))
  (define load-stats (df-statistics df load-series))
  (define max-y (* 1.2 (max (statistics-max stats)
                            (* 1.6 (statistics-max load-stats)))))

  ;; Determine the position of the current week by looking up the current time
  ;; in the "timestamp" series, note that we won't find this value exactly,
  ;; but `df-index-of` returns the closest match.
  (define now-index (df-index-of df "timestamp" (current-seconds)))
  (define now-value (df-ref df (max 0 (sub1 now-index)) "timestamp"))

  ;; This is the plot snip, if this value is output in DrRacket, it will be
  ;; displayed in the REPL, but for us, it will be used in a plot-container%.
  (define snip
    (parameterize ([plot-title title]
                   [plot-background bg]
                   [plot-x-ticks (date-ticks)]
                   [plot-y-label y-label]
                   [plot-x-label #f])
      (plot-snip
       (list
        (tick-grid)
        (function warning-line #:color warning-color #:width 3 #:style 'dot)
        (function danger-line #:color danger-color #:width 3 #:style 'dot)
        (points data
                #:sym 'fullcircle
                #:size (* (point-size) 1.5)
                #:fill-color dot-color
                #:color dot-color)
        (vrule now-value
               #:color this-week-color
               #:width 2.0
               #:style 'short-dash))
       #:y-min 0 #:y-max (max max-y 1)
       #:width width #:height height)))

  ;; A callback to be invoked when the mouse hovers over the plot.  It is
  ;; invoked with the plot snip, the mouse event and the X, Y coordinates in
  ;; plot coordinates (for us, X will be the timestamp)
  (define (hover-callback snip event x y)

    (define renderers '())
    (define (add-renderer r) (set! renderers (cons r renderers)))

    ;; The callback might be invoked when the mouse is outside the plot area.
    ;; `good-hover? determines if we should display hover information
    (when (good-hover? snip x y event)
      ;; Find the position of the mouse in the current data frame, extract the
      ;; information for the current location and display a vertical line plus
      ;; a pict containing information about the plot.
      (define index (df-index-of df "timestamp" x))
      (when (and index (< index (df-row-count df)))
        (define ts (df-ref df index "timestamp"))
        (define week (df-ref df index "week"))
        (define vol (df-ref df index series))
        (define vol-warning (warning-line ts))
        (define vol-danger (danger-line ts))
        (add-renderer (vrule ts #:width 2.0 #:color this-week-color))
        (add-renderer (points (list (vector ts vol))
                              #:sym 'fullcircle
                              #:size (* (point-size) 2.0)
                              #:fill-color hl-dot-color
                              #:color hl-dot-color))

        (define badge (make-hover-badge
                       (list
                        (list "Danger" (format-value vol-danger))
                        (list "Warning" (format-value vol-warning))
                        (list "Actual" (format-value vol))
                        (list "Week" week))))

        ;; Normally, we would display the badge using `point-label`, but that
        ;; clips the label to the current plot area.  Given that the canvas
        ;; contains more plots and the badge can safely overlap those, we can
        ;; use plot-container%/set-hover-pict to display the badge.  The
        ;; interface is not very nice and has a lot of steps -- this could be
        ;; improved.
        (define admin (send snip get-admin))
        (define pb (send admin get-editor))
        (define canvas (send pb get-canvas))
        (send canvas set-hover-pict-at-mouse-event badge event)))

    ;; NOTE: we need to call both set-hover-pict and set-overlay-renderers
    ;; even if we didn't have any information to display, as calling these
    ;; functions with #f is what clears the previous renderers.

    (when (null? renderers)
      (define admin (send snip get-admin))
      (define pb (send admin get-editor))
      (define canvas (send pb get-canvas))
      (send canvas set-hover-pict #f 0 0))

    (send snip set-overlay-renderers (if (null? renderers) #f renderers)))

  ;; Attach the hover callback to the plot snip
  (send snip set-mouse-event-callback hover-callback)

  ;; return the snip.  This can be displayed in DrRacket, or inserted into a
  ;; plot container.
  snip)

;; Convert a distance in km into a string, this is done for "short distances"
;; where the meters value is displayed.
(define (short-distance->string distance [unit-label #f])
  (if (number? distance)
      (string-append
       (~r (* distance 1000.0) #:precision 0)
       (if unit-label (" m")  ""))
      ""))

;; Convert a distance in km into a string, the resulting string is printed as
;; the kilometer value.
(define (distance->string distance [unit-label #f])
  (if (number? distance)
      (string-append
       (~r distance #:precision 1)
       (if unit-label (" km")  ""))
      ""))

;; Convert a duration (in seconds) into a string.  E.g. 3600 is converted to
;; "1:00:00"
(define (duration->string hours)
  (if (number? hours)
      (let* ((seconds (* hours 3600.0))
             (h (exact-truncate (/ seconds 3600.0)))
             (m (exact-truncate (/ (- seconds (* h 3600.0)) 60.0)))
             (s (exact-truncate (- seconds (* h 3600.0) (* m 60.0)))))
        (string-append
         (~r h #:precision 0)
         ":"
         (~r m #:precision 0 #:min-width 2 #:pad-string "0")
         ":"
         (~r s #:precision 0 #:min-width 2 #:pad-string "0")))
      ""))

(define (effort->string effort)
  (if (number? effort)
      (~r effort #:precision 1)
      ""))

;; Read in the data based on the chart settings (PARAMS)
(define (read-data db params)
  (define load (hash-ref params 'load))
  (define before (hash-ref params 'before))
  (define after (hash-ref params 'after))
  (define extra (* 3 (exact-round (/ 100 load))))
  (define df (fetch-irisk-data db (+ extra before) after))
  (add-all-smoothed-series df (/ load 100))
  df)

;; Create plots from the data frame DF and plot parameters PARAMS.  CONTAINER
;; is the plot-container to which plots will be added, but this function does
;; not add them -- the container is used to determine the ideal size of the
;; plot snips, to avoid resizing them
(define (make-plot-snips container df params)

  ;; Because we filter the data, the filtered values will always start at 0,
  ;; to avoid this, we fetch some EXTRA weeks to seed the filter, these weeks
  ;; will not be plotted, because an OFFSET is calculated later and all plots
  ;; will actually discard the first OFFSET entries.

  (define-values (width height) (send container cell-dimensions 9 #:columns 3 #:spacing 5))

  (define before (hash-ref params 'before))
  (define after (hash-ref params 'after))
  (define offset (max 0 (- (df-row-count df) (+ before after))))

  (define all-plot-data
    (list
     (list "Swim Distance" "sDist" "sDistLoad" short-distance->string swim-bg-color)
     (list "Swim Duration" "sDuration" "sDurationLoad" duration->string swim-bg-color)
     (list "Swim Effort" "sTss" "sTssLoad" effort->string swim-bg-color)

     (list "Bike Distance" "bDist" "bDistLoad" distance->string bike-bg-color)
     (list "Bike Duration" "bDuration" "bDurationLoad" duration->string bike-bg-color)
     (list "Bike Effort" "bTss" "bTssLoad" effort->string bike-bg-color)

     (list "Run Distance" "rDist" "rDistLoad" distance->string run-bg-color)
     (list "Run Duration" "rDuration" "rDurationLoad" duration->string run-bg-color)
     (list "Run Effort" "rTss" "rTssLoad" effort->string run-bg-color)))

  (for/list ((data (in-list all-plot-data)))
    (match-define (list title series load-series format-value color) data)
    (make-plot df series load-series
               #:title title
               #:format-value format-value
               #:offset offset
               #:background color
               #:width width #:height height)))

;; Display the settings dialog for the bodyweight trends chart
(define chart-settings-dialog%
  (class* edit-dialog-base% (chart-settings-interface<%>)
    (init-field database
                [default-name "IRisk"]
                [default-title "Injury Risk"])

    (super-new [title "Chart Settings"]
               [icon (edit-icon)]
               [min-height 10])

    (define name-gb (make-group-box-panel (send this get-client-pane)))
    (define name-field (new text-field% [parent name-gb] [label "Name "]))
    (send name-field set-value default-name)
    (define title-field (new text-field% [parent name-gb] [label "Title "]))
    (send title-field set-value default-title)

    (define settings-gb (make-group-box-panel (send this get-client-pane)))
    (define settings (new grid-pane% [parent settings-gb] [columns 2]))
    (define wbefore-label (new message% [parent settings] [label "Past"]))
    (define wbefore (new weeks-input-field%
                         [parent settings]
                         [stretchable-width #f]
                         [min-width 150]
                         [label ""]
                         [tooltip "Number of weeks in the past"]
                         [init-value (~a 15)]))
    (define wafter-label (new message% [parent settings] [label "Future"]))
    (define wafter (new weeks-input-field%
                        [parent settings]
                        [stretchable-width #f]
                        [min-width 150]
                        [label ""]
                        [tooltip "Number of weeks to predict in the future"]
                        [init-value (~a 5)]))
    (define wload-label (new message% [parent settings] [label "Load"]))
    (define wload (new percent-input-field%
                       [parent settings]
                       [tooltip "Load percentage of current week"]
                       [stretchable-width #f]
                       [label ""]
                       [min-width 150]
                       [init-value (~a 25)]))

    (define/public (put-chart-settings data)
      (let ((name (hash-ref data 'name ""))
            (title (hash-ref data 'title ""))
            (before (hash-ref data 'before 10))
            (after (hash-ref data 'after 5))
            (load (hash-ref data 'load 25)))
        (send name-field set-value name)
        (send title-field set-value title)
        (send wbefore set-value before)
        (send wafter set-value after)
        (send wload set-value load)))

    (define/public (get-chart-settings)
      (hash
       'name (send name-field get-value)
       'title (send title-field get-value)
       'before (send wbefore get-value/validated)
       'after (send wafter get-value/validated)
       'load (send wload get-value/validated)))

    (define/public (show-dialog parent)
      (and (send this do-edit parent) (get-chart-settings)))

    ))

(define irisk-trends-chart%
  (class trends-chart%
    (init-field database)
    (super-new)

    ;; Data frame holding bodyweight data, retrieved from the database
    (define df #f)

    (define/override (make-settings-dialog)
      (new chart-settings-dialog%
           [default-name "IRisk"]
           [default-title "Injury Risk"]
           [database database]))

    (define/override (invalidate-data)
      (set! df #f))

    (define/override (is-invalidated-by-events? events)
      (or (hash-ref events 'session-created #f)
          (hash-ref events 'session-updated #f)
          (hash-ref events 'session-deleted #f)))

    (define/override (export-data-to-file file formatted?)
      (when df
        (df-write/csv df file "week"
                      "sDist" "sDuration" "sTss" "sDistLoad" "sDurationLoad" "sTssLoad"
                      "bDist" "bDuration" "bTss" "bDistLoad" "bDurationLoad" "bTssLoad"
                      "rDist" "rDuration" "rTss" "rDistLoad" "rDurationLoad" "rTssLoad")))

    (define generation 0)
    (define (get-generation) generation)

    (define/override (put-plot-snip canvas)
      (send canvas clear-all)
      (send canvas set-background-message "Working...")
      (set! generation (add1 generation))
      (let ((previous-data df)
            (params (send this get-chart-settings))
            (saved-generation generation))
        (if params
            (queue-task
             "irisk-trends-chart%/put-plot-snip"
             (lambda ()
               (define data (or previous-data (read-data database params)))
               (define plots (make-plot-snips canvas data params))
               (queue-callback
                (lambda ()
                  (when (= saved-generation (get-generation))
                    (set! df data)
                    (send canvas set-snips/layout (apply cgroup 3 plots)))))))
            (begin
              (send canvas clear-all)
              (send canvas set-background-message "No params for plot")))))

    (define/override (save-plot-image file-name width height)
      ;; We assume the data is ready, and don't do anything if it is not.
      #;(send canvas export-image-to-file file-name width height)
      (void))

    ))

(provide irisk-trends-chart%)
