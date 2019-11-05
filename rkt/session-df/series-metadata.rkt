#lang racket/base

;; series-metadata.rkt --meta data about data series in session data frames
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2018, 2019 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require plot/no-gui
         racket/class
         racket/contract
         racket/draw
         racket/format
         (prefix-in ct: "../color-theme.rkt")
         "../utilities.rkt")

;; Provides meta data information about a series in a data frame, mostly
;; related on how to plot values of this series.
(define series-metadata%
  (class object% (init) (super-new)
    ;; When #t, we attempt to detect stop points and generate additional data
    ;; points with 0 Y values, this makes graphs look nicer.
    (define/public (has-stop-detection?) #f)

    ;; Whether to filter values in this series using a low pass filter.
    ;; Whether filtering happens or not is defined on an Y axis (e.g. we may
    ;; want to filter heart rate but not elevation).
    (define/public (should-filter?) #f)

    ;; The width of the low pass filter.  The filter width is determined by
    ;; the X axis (e.g. distance and time will have different widths).
    (define/public (filter-width) #f)

    ;; Return the base multiplier for determining the final bucket width in a
    ;; histogram.  This value is multiplied with the user specified bucket
    ;; size.  It is used when measurements have fractional values which are
    ;; significant, as actual bucket sizes have 1 as the default and minimum
    ;; value.
    (define/public (histogram-bucket-slot) 1.0)

    ;; Returns #t if BEST-AVG values for this series are "better" when they
    ;; are smaller.  This is used for series like pace, where a smaller value
    ;; means you are running faster, or for series which need to be minimized,
    ;; for example ground contact time or vertical oscillation.
    (define/public (inverted-mean-max?) #f)

    ;; Return the ticks to use for plotting values of this series.
    (define/public (plot-ticks) (linear-ticks))

    ;; Returns the text to display for this series when it appears in various
    ;; selection boxes.
    (define/public (headline) (axis-label))

    ;; Return a string to use as the axis label when this series is used in a
    ;; plot.
    (define/public (axis-label) (raise "no axis label defined"))

    ;; Return a string to use as the legend value on a plot for this series
    (define/public (plot-label) #f)

    (define color #f)

    ;; Return the color to use for plotting this series.  We look it up in the
    ;; (series-colors) ALIST for a tag the same as (series-name)
    (define/public (plot-color)
      (unless color
        (let* ((tag (string->symbol (series-name)))
               (color-item (assq tag (ct:series-colors))))
          (when color-item
            (set! color (cdr color-item))))
        ;; Could not find it, use a default
        (unless color
          (set! color (make-object color% 0 148 255))))
      color)

    ;; When true, color the plot by swim stroke colors.  Only makes sense for
    ;; swimming activities.
    (define/public (plot-color-by-swim-stroke?) #f)

    ;; Return the Y range for ploting this series.  Returns a (cons LOW HIGH),
    ;; either of them can be #f, in which case the range is automatically
    ;; detected.  This can be used to force Y ranges for certain series (for
    ;; example the balance series can be centered arount 50%, regardless of
    ;; the data in the plot).
    (define/public (y-range) #f)

    ;; Return the name of the series in the data frame
    (define/public (series-name) #f)

    ;; Return the number of fractional digits to keep when truncating values
    ;; in this series.
    (define/public (fractional-digits) 0)

    ;; Value to replace missing values in this series
    (define/public (missing-value) 0)

    ;; Return a function that can classify values for this series into
    ;; discrete elements (tags), returns #f if there is no such function.  The
    ;; method accepts SPORT and a session id (SID) arguments, allowing factor
    ;; functions to be sport and/or session specific.  This is currently used
    ;; to provide factor functions based on sport zones -- the SID allows
    ;; retrieving a sport zone that was valid at the time when the session
    ;; occurred.  An implementation can ignore these arguments if they don't
    ;; make sense, and it should also be prepared to handle both of them being
    ;; #f
    (define/public (factor-fn sport (sid #f)) #f)

    ;; Return an alist mapping factor names to colors
    (define/public (factor-colors) (ct:factor-colors))

    ;; Return #t if we can estimate Critical Power for this data series.
    (define/public (have-cp-estimate?) #f)
    ;; Estimate CP for this data series given a best-avg function (as returned
    ;; by `aggregate-bavg') and search parameters (a CP2PARAMS instance).
    ;; Returns a CP2 structure.
    (define/public (cp-estimate bavg-fn params) #f)
    ;; Return a PD function for the supplied critical power parameters (a CP2
    ;; instance)
    (define/public (pd-function cp-params) #f)
    ;; Return a pict displaying information about the supplied critical power
    ;; parameters (a CP2 instance).
    (define/public (pd-data-as-pict cp-params bavgfn) #f)

    ;; Return a function (-> number? string?) which formats a value of this
    ;; series into a string.  The method accepts SPORT and a session id (SID)
    ;; arguments, allowing formatters to be sport and/or session specific.
    ;; This is currently used to provide formatters based on sport zones --
    ;; the SID allows retrieving a sport zone that was valid at the time when
    ;; the session occurred.  An implementation can ignore these arguments if
    ;; they don't make sense, and it should also be prepared to handle both of
    ;; them being #f
    (define/public (value-formatter sport (sid #f))
      (lambda (p)
        (if (rational? p)
            (~r p #:precision (fractional-digits))
            (~a p))))

    ;; Return the name of the values in this series (e.g. "Pace", "Power",
    ;; etc).
    (define/public (name) "Unnamed")

    ))

(define the-metadata-registry (make-hash))
(define the-swim-metadata-registry (make-hash))

(define (register-series-metadata m (is-lap-swimming? #f))
  (define sn (send m series-name))
  (define registry (if is-lap-swimming?
                       the-swim-metadata-registry
                       the-metadata-registry))
  (when (hash-ref registry sn #f)
    (dbglog "register-series-metadata: overriding metadata for ~a" sn))
  (hash-set! registry (send m series-name) m)
  (void))

(define (unregister-series-metadata m (is-lap-swimming? #f))
  (define sn (send m series-name))
  (define registry (if is-lap-swimming?
                       the-swim-metadata-registry
                       the-metadata-registry))
  (hash-remove! registry (send m series-name))
  (void))

(define (find-series-metadata series-name (is-lap-swimming? #f))
  (define registry (if is-lap-swimming?
                       the-swim-metadata-registry
                       the-metadata-registry))
  (define metadata (hash-ref registry series-name #f))
  (if metadata
      metadata
      (raise-argument-error 'series-name
                            (format "valid series name~a"
                                    (if is-lap-swimming? " for swimming" ""))
                            series-name)))


;;............................................................. provides ....

(provide series-metadata%)

(provide/contract
 (register-series-metadata (->* ((is-a?/c series-metadata%)) (boolean?) void?))
 (unregister-series-metadata (->* ((is-a?/c series-metadata%)) (boolean?)  void?))
 (find-series-metadata (->* (string?) (boolean?) (is-a?/c series-metadata%))))
