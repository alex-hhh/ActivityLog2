#lang racket/base
;; box-and-whiskers.rkt -- box-and-whiskers plot renderer
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2021 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require math/statistics
         plot/utils
         plot
         racket/match
         racket/contract
         racket/sequence)

;; See also https://en.wikipedia.org/wiki/Box_plot, and the Matplotlib
;; reference
;; https://matplotlib.org/3.2.1/api/_as_gen/matplotlib.pyplot.boxplot.html

;; Hold the quantile and other information for the box and whiskers plot: Q1
;; is the 1st quantile, 25% of the points in the data set are less than this
;; value.  MEDIAN is the median, 50% of the points in the data set are less
;; than this, while Q3 is the 3rd quantile (75%).  The LOWER-WHISKER and
;; UPPWER-WHISKER are lower and upper limits below and above which points are
;; considered outliers, see `samples->bnw-data`.  Finally, OUTLIERS represents
;; a list of outlier points.
(struct bnw-data
  (q1 median q3 lower-whisker uppwer-whisker outliers)
  #:transparent)

;; Construct a BNW-DATA object from the data points VS.  If WS is present, it
;; represents the weights of each data point (if missing, each point has the
;; same weight), see `quantile` from the `racket/math` package on how WS is
;; used.
;;
;; IQR-SCALE is the multiplier used to determine the lower an upper limits
;; below and above which points are considered outliers.  These limits are
;; calculated as (* IQR-SCALE (- Q3 Q1)) above and below the median, with the
;; caveat that the limits are adjusted to represent the highest (or lowest)
;; actual data point at that limit.
(define (samples->bnw-data vs [ws #f] #:iqr-scale [iqr-scale 1.5])
  (let* ([q1 (quantile 0.25 < vs ws)]
         [median (quantile 0.5 < vs ws)]
         [q3 (quantile 0.75 < vs ws)]
         [iqr (- q3 q1)]
         [lower-limit (- q1 (* iqr-scale iqr))]
         [upper-limit (+ q3 (* iqr-scale iqr))]
         [lower-whisker (sequence-fold
                         (lambda (a sample)
                           (if (>= sample lower-limit) (min a sample) a))
                         q1 vs)]
         [upper-whisker (sequence-fold
                         (lambda (a sample)
                           (if (<= sample upper-limit) (max a sample) a))
                         q3 vs)]
         [outliers (sequence-filter
                    (lambda (sample) (or (> sample upper-whisker)
                                         (< sample lower-whisker))) vs)])
    (bnw-data q1 median q3 lower-whisker upper-whisker (sequence->list outliers))))

;; Create a box-and-whiskers plot from data obtained from a call to
;; `samples->bnw-data`.  X represents the x axis position where the box and
;; whiskers plot should be placed (by default the plot is drawn in a vertical
;; position).  Various X values allows multiple box plots to be drawn side by
;; side.  If INVERT? is #t, the plot is drawn horizontally, and X represents a
;; value on the Y axis.  GAP represents the "width" of the box plot (but the
;; actual plot will be narrower than GAP.  If multiple box plots are
;; displayed, they should be placed GAP interval between each other, in order
;; to look good.
(define (box-and-whiskers
         data

         #:x [x 0]

         #:invert? (invert? #f)

         #:gap (gap (discrete-histogram-gap))

         ;; Rectangles options
         #:box-color (box-color (rectangle-color))
         #:box-style (box-style (rectangle-style))
         #:box-line-color (box-line-color (rectangle-line-color))
         #:box-line-width (box-line-width (rectangle-line-width))
         #:box-line-style (box-line-style (rectangle-line-style))
         #:box-alpha (box-alpha (rectangle-alpha))

         #:show-outliers? (show-outliers? #t)
         #:outlier-color (outlier-color (point-color))
         #:outlier-sym (outlier-sym (point-sym))
         #:outlier-fill-color (outlier-fill-color 'auto)
         #:outlier-size (outlier-size (point-size))
         #:outlier-line-width (outlier-line-width (point-line-width))
         #:outlier-alpha (outlier-alpha (point-alpha))

         #:show-whiskers? (show-whiskers? #t)
         #:whiskers-color (whiskers-color (line-color))
         #:whiskers-width (whiskers-width (line-width))
         #:whiskers-style (whiskers-style (line-style))
         #:whiskers-alpha (whiskers-alpha (line-alpha))

         #:show-median? (show-median? #t)
         #:median-color (median-color (line-color))
         #:median-width (median-width (line-width))
         #:median-style (median-style (line-style))
         #:median-alpha (median-alpha (line-alpha))
         )
  (match-define (bnw-data q1 median q3 lower-whisker upper-whisker outliers) data)
  (define half-width (* 1/2 (- 1 gap)))
  (define quater-width (* 1/4 (- 1 gap)))
  (define maybe-invert (if invert? (lambda (x y) (vector y x)) vector))
  (list
   (rectangles
    (list (maybe-invert (ivl (- x half-width) (+ x half-width)) (ivl q1 q3)))
    #:color box-color
    #:style box-style
    #:line-color box-line-color
    #:line-width box-line-width
    #:line-style box-line-style
    #:alpha box-alpha)
   ;; Median line
   (if show-median?
       (lines (list (maybe-invert (- x half-width) median)
                    (maybe-invert (+ x half-width) median))
              #:color median-color
              #:width median-width
              #:style median-style
              #:alpha median-alpha)
       null)

   (if show-whiskers?
       (list
        (lines (list (maybe-invert x lower-whisker) (maybe-invert x q1)
                     (vector +nan.0 +nan.0)
                     (maybe-invert x q3) (maybe-invert x upper-whisker))
               #:color whiskers-color
               #:width whiskers-width
               #:style whiskers-style
               #:alpha whiskers-alpha)
        (lines (list
                (maybe-invert (- x quater-width) lower-whisker)
                (maybe-invert (+ x quater-width) lower-whisker)
                (vector +nan.0 +nan.0)
                (maybe-invert (- x quater-width) upper-whisker)
                (maybe-invert (+ x quater-width) upper-whisker))
               #:color whiskers-color
               #:width whiskers-width
               #:style 'solid
               #:alpha whiskers-alpha))
       null)

   (if show-outliers?
       (points (for/list ([o outliers]) (maybe-invert x o))
               #:color outlier-color
               #:sym outlier-sym
               #:fill-color outlier-fill-color
               #:size outlier-size
               #:line-width outlier-line-width
               #:alpha outlier-alpha)
       null)))



;;............................................................. provides ....

(provide
 (struct-out bnw-data))
(provide/contract
 (samples->bnw-data (->* ((sequence/c real?))
                         ((or/c (sequence/c real?) #f) #:iqr-scale real?)
                         bnw-data?)))
(provide box-and-whiskers)
