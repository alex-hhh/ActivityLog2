#lang racket/base
;; rdp-simplify.rkt -- Ramer–Douglas–Peucker line simplification algorithm
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2018 Alex Harsányi <AlexHarsanyi@gmail.com>
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
(require racket/vector
         racket/contract)

;; Return a function which calculates the perpendicular distance of a point to
;; the line defined by the two points P1, P2.  Points are defined as vectors
;; with the first two elements being the X and Y component, they can have any
;; number of additional components.
(define (perpendicular-distance p1 p2)
  (define x1 (vector-ref p1 0))
  (define y1 (vector-ref p1 1))
  (define x2 (vector-ref p2 0))
  (define y2 (vector-ref p2 1))
  (define yd (- y2 y1))
  (define xd (- x2 x1))
  (define denom (sqrt (+ (* yd yd) (* xd xd))))
  (lambda (p0)
    (define x0 (vector-ref p0 0))
    (define y0 (vector-ref p0 1))
    (define nom (abs (- (+ (- (* yd x0) (* xd y0)) (* x2 y1)) (* y2 x1))))
    (/ nom denom)))

;; Run the Ramer–Douglas–Peucker simplification algorithm on DATA, a (vectorof
;; (vector X Y ...)), representing a line.  Returns a new vector containing
;; fewer points by dropping some of the points from the original data set.  No
;; new points are created, no interpolation or filtering is done.  The
;; resulting line drawn from the simplified data set should resemble the
;; original line drawn from the original data set.
;;
;; EPSILON determines how hard to simplify the data -- bigger values will
;; result in fewer data points, but the simplified line will resemble the
;; original less.
;;
;; DESTROY? when #t the original data set will be modified in place, if #f a
;; copy of the data set will be created.  If the data set is large and it is
;; not needed after the simplification, it will be faster and more memory
;; efficient to destroy the original data set.
;;
;; KEEP represents a list of positions in the data which will not be
;; simplified, and will be kept in the output.  When empty, it is only
;; guaranteed that the first and last elements of the input data are kept.
;; When not empty, two points around each KEEP position are kept: the point
;; itself and the one that immediately follows it.
;;
;; NOTE: RDP-SIMPLIFY can be used to reduce the size of a data set to be
;; plotted by the LINES plot renderer, while still keeping the look of the
;; resulting plot the same, or nearly the same.  This can make plotting
;; faster, especially for large data sets of several thousand points.
;;
;; The line simplification algorithm, is described here:
;; https://en.wikipedia.org/wiki/Ramer%E2%80%93Douglas%E2%80%93Peucker_algorithm
;;
(define (rdp-simplify data
                      #:epsilon [eps 0.1]
                      #:destroy-original? [destroy? #f]
                      #:keep-positions [keep '()])

  (define dropped 0)
  (define wdata (if destroy? data (vector-copy data)))

  ;; Run the simplification algorithm on WDATA a (vectorof (vector X Y))
  ;; between the START and STOP indexes.  STOP is inclusive.  The algorithm
  ;; updates WDATA in place, replacing data that should be removed with #f.

  (define (loop start stop)
    (when (> (- stop start) 1)
      (let ((distance (perpendicular-distance
                       (vector-ref wdata start)
                       (vector-ref wdata stop))))
        (define-values (maximum mindex)
          (for/fold ([maximum -1] [mindex -1])
                    ([index (in-range (add1 start) stop)])
            (define epsilon (distance (vector-ref wdata index)))
            (if (> epsilon maximum)
                (values epsilon index)
                (values maximum mindex))))
        (if (> maximum eps)
            (begin
              (loop start mindex)
              (loop mindex stop))
            (begin
              ;; All points inside this range are below our threshold, clear
              ;; them out.
              (for ([index (in-range (add1 start) stop)])
                (vector-set! wdata index #f))
              (set! dropped (+ dropped (- stop (add1 start)))))))))

  (let ((limit (vector-length wdata)))
    (if (null? keep)
        (loop 0 (sub1 limit))
        (let ((skeep (sort (append (list -1) keep (list (sub1 limit))) <)))
          (for ([start skeep] [stop (cdr skeep)] #:when (and (>= start 0) (< stop limit)))
            (loop (add1 start) stop)))))

  ;; Construct the simplified data set by omitting all the #f values from
  ;; wdata
  (for/vector #:length (- (vector-length wdata) dropped)
              ([p (in-vector wdata)] #:when p)
    p))


;;............................................................. provides ....

(provide/contract
 (rdp-simplify (->* (vector?)
                    (#:epsilon (and/c real? positive?)
                     #:destroy-original? boolean?
                     #:keep-positions (listof exact-nonnegative-integer?))
                    (vectorof vector?))))
