#lang racket/base

;; SPDX-License-Identifier: GPL-3.0-or-later
;; grade-series.rkt -- calculate the grade (or slope) in a data frame
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2022 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require data-frame
         data-frame/private/bsearch
         racket/match
         racket/contract
         racket/math
         racket/list
         (only-in map-widget/utils map-distance/degrees))

;; Add a grade (slope) series to the data frame DF.  We assume that an
;; altidute and lat/lot series exist (use ADD-GRADE-SERIES, which performs the
;; checks).
;;
;; This algorithm tries to be smart about building the slope series, since
;; simply looking at elevation changes between adjacent data points will
;; result in wildly varying slope values, which is not very useful.  Several
;; comments are present inside the implementation to explain what it does.
(define (add-grade-series! df)

  (unless (df-contains? df "lat" "lon")
    (error "add-grade-series! : missing lat/lon series"))
  (unless (df-contains/any? df "alt" "calt")
    (error "add-grade-series! : missing alt or calt series"))

  ;; Maximum distance between points for which we assume a monotonic grade.
  ;; For distances less than this we calculate a constant grade between the
  ;; start and end point.
  (define maximum-monotonic 50.0)

  ;; Minimum distance between points for which we calculate the grade.  If
  ;; iteration resulted in a segment smaller than this, we will interpolate
  ;; the grade between the adjacent points of this segment (small segments
  ;; result in grade spikes)
  (define minimum-valid 3.0)

  ;; Minimum altidute difference in a range for which we split the range.  If
  ;; the altidute difference in a range is less than this, we consider the
  ;; range monotonic.
  (define minimum-altitude 3.0)

  ;; The altitude series data, we make some modifications to it below, so we
  ;; make a copy.  Technically, we don't always modify it, so we could save
  ;; the 'vector-copy' call in some cases.  This is left for a future
  ;; improvement.
  (define alt
    (let ((series (cond ((df-contains? df "calt") "calt")
                        ((df-contains? df "alt") "alt")
                        (#t #f))))
      (df-select df series)))

  ;; Fixup #f's in the alt series, this works OK for one-off missing values,
  ;; if whole ranges are missing, this will not produce nice results.
  (for ([(a idx) (in-indexed alt)] #:unless a)
    (if (= idx 0)
        ;; Scan forward for the first good altitude value
        (vector-set! alt idx (for/first ([a alt] #:when a) a))
        ;; Use the previous value
        (vector-set! alt idx (vector-ref alt (sub1 idx)))))

  ;; Compute a distance data from the GPS points, don't use the "dst" series,
  ;; as it might have stop points which would mess up our calculations.
  (define dst
    (let ((adst 0))
      (df-map
       df
       '("lat" "lon")
       (lambda (prev val)
         (when prev
           (match-define (list plat plon) prev)
           (match-define (list lat lon) val)
           (when (and plat plon lat lon)
             (set! adst (+ adst (map-distance/degrees plat plon lat lon)))))
         adst))))

  ;; When entering a longer tunnel and loosing the GPS signal, the Garmin
  ;; Device will continue to log the last known GPS location, but will
  ;; interpolate the altitude (and distance data).  When it exits the tunnel
  ;; and the GPS signal is re-acquired, it will adjust the distance data
  ;; (usually with a big distance jump which is quite off)
  ;;
  ;; Since we recompute the distance form the GPS data, this behavior will
  ;; result in a lot of points with the same distance but the altitude
  ;; changing, resulting in large and unrealistic grade values.
  ;;
  ;; For now, we make all altitude values match the first one.  This will make
  ;; the grade inside the tunnel effectively 0, with possibly a big grade
  ;; adjustment at the exit of the tunnel.
  ;;
  ;; NOTE: On a device with a barometric altimeter, the altitude data may well
  ;; be accurate, but since we cannot calculate the distance correctly, we
  ;; cannot really use it for grade calculations.
  (for ((idx (in-range 1 (vector-length dst))))
    (define delta (- (vector-ref dst idx)
                     (vector-ref dst (sub1 idx))))
    (when (< delta 0.1)
      (vector-set! alt idx (vector-ref alt (sub1 idx)))))

  ;; The grade series we will fill in
  (define grade (make-vector (vector-length alt) #f))

  (define delayed-segments '())

  ;; NOTE: in all functions below, the START, END range is inclusive!!!

  ;; Compute the distance between START and END (indexes in the DST vector)
  (define (delta-dist start end)
    (- (vector-ref dst end) (vector-ref dst start)))
  ;; Compute the altitude change between START and END (indexes in the DST
  ;; vector).  This will be negative if the slope is downhill.
  (define (delta-alt start end)
    (- (vector-ref alt end) (vector-ref alt start)))
  ;; Fill in a monotonic slope between START and END.  The slope based on the
  ;; start and end points is calculated and filled in for all in-between
  ;; points.
  (define (monotonic-slope start end)
    (let* ((dist (delta-dist start end))
           (alt (delta-alt start end))
           (slp (if (> dist 0) (* 100.0 (/ alt dist)) #f))
           ;; Round the slope to 0.1% values, we cannot really compute the
           ;; slope with greater precision than that, and the extra false
           ;; precision creates problems with the plot.
           (rslp (and slp (/ (round (* slp 10.0)) 10.0))))
      (if (< dist minimum-valid)
          (set! delayed-segments (cons (cons start end) delayed-segments))
          (for ([idx (in-range (add1 start) (add1 end))])
            (vector-set! grade idx rslp)))))
  ;; Find the minimum and maximum altitude between START and END and return 4
  ;; values: min-alt, min-alt position, max-alt, max-alt position.
  (define (find-min-max-alt start end)
    (let ((min-alt (vector-ref alt start))
          (min-alt-idx start)
          (max-alt (vector-ref alt start))
          (max-alt-idx start))
      (for ([idx (in-range start (add1 end))])
        (define a (vector-ref alt idx))
        (when (< a min-alt)
          (set! min-alt a)
          (set! min-alt-idx idx))
        (when (> a max-alt)
          (set! max-alt a)
          (set! max-alt-idx idx)))
      (values min-alt min-alt-idx max-alt max-alt-idx)))
  ;; Return the position of the middle point between START and END.  This is
  ;; done based on distances, not indices, and depending on sampling rates and
  ;; stop points, it might be "off" from the middle. Still, the "best" middle
  ;; point is returned.
  (define (find-middle start end)
    (let* ((sdist (vector-ref dst start))
           (half (/ (delta-dist start end) 2))
           (mid (lower-bound dst (+ sdist half) #:start start #:stop end)))
      ;; happens if points are unevenly placed (e.g. a GPX file containing a
      ;; route.
      (if (or (= mid end) (= mid start))
          (exact-floor (/ (+ start end) 2))
          mid)))

  (define (order-points p1 p2 p3 p4)
    (let ((points (list p1 p2 p3 p4)))
      (remove-duplicates (sort points <))))

  (define (iterate start end)
    (cond
      ((<= (- end start) 2)    ; less than two data points in the range...
       (monotonic-slope start end))
      ((< (delta-dist start end) maximum-monotonic)
       (monotonic-slope start end))
      (#t
       (let-values (((min-alt min-alt-idx max-alt max-alt-idx)
                     (find-min-max-alt start end)))
         (if (< (- max-alt min-alt) minimum-altitude)
             (monotonic-slope start end)
             (let ((ranges (order-points start min-alt-idx max-alt-idx end)))
               (if (= (length ranges) 2)
                   ;; range is monotonic, split it in two and recurse
                   (let ((mid (find-middle start end)))
                     (iterate start mid)
                     (iterate mid end))
                   ;; Else, iterate over the defined ranges
                   (for ([s ranges] [e (cdr ranges)])
                     (iterate s e)))))))))

  (iterate 0 (sub1 (vector-length grade)))

  ;; Fix up the segments which were too small to have their slope
  ;; calculated...  We interpolate or extend the grade values from adjacent
  ;; segments.
  (for ([segment (in-list delayed-segments)])
    (match-define (cons start end) segment)
    (define nstart (and (> start 0) (sub1 start)))
    (define nend (and (< end (sub1 (vector-length grade))) (add1 end)))
    (define gstart (and nstart (vector-ref grade nstart)))
    (define gend (and nend (vector-ref grade nend)))
    (cond ((and gstart gend)
           (let ([dist (delta-dist nstart nend)]
                 [dgrade (- gend gstart)])
             (if (> dist 0)
                 (for ([i (in-range start (add1 end))])
                   (vector-set! grade i (+ gstart (* dgrade (/ (delta-dist nstart i) dist)))))
                 (for ([i (in-range start (add1 end))])
                   (vector-set! grade i gstart)))))
          (gstart
           (for([i (in-range start (add1 end))])
             (vector-set! grade i gstart)))
          (gend
           (for([i (in-range start (add1 end))])
             (vector-set! grade i gend)))))

  ;; Only add the grade series if there are actually any values in it...
  (when (for/first ([g (in-vector grade)] #:when g) #t)
    (df-add-series! df (make-series "grade" #:data grade))))

;; Check that the data frame DF has the required data and add the grade series
;; if it does.
(define (maybe-add-grade-series! df)
  (define alt-series
    (cond ((df-contains? df "calt") "calt")
          ((df-contains? df "alt") "alt")
          (#t #f)))
  (when (and alt-series (df-contains? df "lat" "lon"))
    (add-grade-series! df)))

(provide/contract
 (add-grade-series! (-> data-frame? any/c))
 (maybe-add-grade-series! (-> data-frame? any/c)))
