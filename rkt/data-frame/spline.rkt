#lang typed/racket/base
;; spline.rkt -- construct spline interpolation functions from data points
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

;; Construct a spline interpolation function based on a set of data points.
;; The implementation is based on
;; https://en.wikipedia.org/wiki/Spline_interpolation

(require math/matrix
         racket/match)

(define-type Data-Point (Vector Real Real))
(define-type Delta-Point (Vector Real Real))
(define-type Data-Series (U (Listof Data-Point) (Vectorof Data-Point)))
(define-type Delta-Series (Listof Delta-Point))

;; Calculate the delta series for a set of data points.  The delta is the
;; difference between adjacent points in the data series.  It will have
;; one-less element than data series.
(: ->delta-series (-> Data-Series Delta-Series))
(define (->delta-series data-series)
  (cond ((list? data-series)
         (for/list ([d0 (in-list data-series)] [d1 (in-list (cdr data-series))])
           (match-define (vector x0 y0) d0)
           (match-define (vector x1 y1) d1)
           (vector (- x1 x0) (- y1 y0))))
        ((vector? data-series)
         (for/list ([d0 (in-vector data-series)] [d1 (in-vector data-series 1)])
           (match-define (vector x0 y0) d0)
           (match-define (vector x1 y1) d1)
           (vector (- x1 x0) (- y1 y0))))
        (#t
         (raise-argument-error "->delta-series -- unknown data-series type" data-series))))


;; Construct a B term for solving the equation system.
(: b-term (-> Delta-Point Real))
(define (b-term delta)
  (match-define (vector dx dy) delta)
  (* 3 (/ dy (* dx dx))))

;; Construct the B terms for the equation system.  From a delta series, we add
;; together the b-term of each delta, except for the first and last element
;; which stand by themselves.
(: ->b-terms (-> Delta-Series (Listof Real)))
(define (->b-terms delta-series)
  (for/list ([d0 (in-sequences (in-value #f) delta-series)]
             [d1 (in-sequences delta-series (in-value #f))])
    (+ (if d0 (b-term d0) 0) (if d1 (b-term d1) 0))))

;; Construct the column matrix of the B terms
(: b-matrix (-> Delta-Series (Matrix Real)))
(define (b-matrix delta-series)
  (->col-matrix (->b-terms delta-series)))

;; Construct an A term for solving the equation system
(: a-term (-> Delta-Point Real))
(define (a-term delta)
  (match-define (vector dx dy) delta)
  (/ 1 dx))

;; Construct the A matrix
(: a-matrix (-> Delta-Series (Matrix Real)))
(define (a-matrix delta-series)
  (define n (+ (length delta-series) 1))
  (build-matrix
   n n
   (lambda ((x : Integer) (y : Integer))
     (cond ((eqv? x y)                  ; diagonal
            (cond ((eqv? x 0)           ; first element
                   (* 2 (a-term (list-ref delta-series 0))))
                  ((eqv? x (- n 1))     ; last element
                   (* 2 (a-term (list-ref delta-series (- n 2)))))
                  (#t                   ; other diagonal element
                   (* 2 (+ (a-term (list-ref delta-series (- x 1)))
                           (a-term (list-ref delta-series x)))))))
           ((or (eqv? x (- y 1)) (eqv? y (- x 1)))
            (a-term (list-ref delta-series (min x y))))
           (#t
            0)))))


;; Given the K-TERMS and delta-series, compute the a_i, b_i terms for each
;; interpolation polynomial
(: ab-terms (-> (Listof Real) Delta-Series (Listof (Vector Real Real))))
(define (ab-terms k-terms delta-series)
  (for/list ([k0 k-terms] [k1 (cdr k-terms)] [delta delta-series])
    (match-define (vector dx dy) delta)
    (vector
     (- (* k0 dx) dy)
     (+ (* (- k1) dx) dy))))

;; Construct the spline terms necessary for doing the interpolation.  Given
;; the data-series, k-terms and delta series, we calculate the a_i, b_i terms
;; for each interpolation polynomial and return a list of vectors containing
;; the X, Y coordinates of the two points between which the polynomial will
;; interpolate; the k0 and k1 tangents at the start and end of the
;; interpolated range, followed by the a and b term for the polynomial.
(: spline-terms (-> Data-Series (Listof Real) Delta-Series
                    (Listof (Vector Real Real Real Real Real Real Real Real))))
(define (spline-terms data-series k-terms delta-series)
  (cond ((list? data-series)
         (for/list ([p0 (in-list data-series)]
                    [p1 (in-list (cdr data-series))]
                    [k0 (in-list k-terms)]
                    [k1 (in-list (cdr k-terms))]
                    [delta (in-list delta-series)])
           (match-define (vector dx dy) delta)
           (match-define (vector x0 y0) p0)
           (match-define (vector x1 y1) p1)
           (vector x0 y0 x1 y1 k0 k1 (- (* k0 dx) dy) (+ (* (- k1) dx) dy))))
        ((vector? data-series)
         (for/list ([p0 (in-vector data-series)]
                    [p1 (in-vector data-series 1)]
                    [k0 (in-list k-terms)]
                    [k1 (in-list (cdr k-terms))]
                    [delta (in-list delta-series)])
           (match-define (vector dx dy) delta)
           (match-define (vector x0 y0) p0)
           (match-define (vector x1 y1) p1)
           (vector x0 y0 x1 y1 k0 k1 (- (* k0 dx) dy) (+ (* (- k1) dx) dy))))
        (#t
         (raise-argument-error "spline-terms: unknown data-series type" data-series))))

;; Compute the a, b terms for each interpolation polynomial for a list of data
;; points in DATA-SERIES
(: poly-terms (-> Data-Series (Listof (Vector Real Real Real Real Real Real Real Real))))
(define (poly-terms data-series)
  (define delta-series (->delta-series data-series))
  (define A (a-matrix delta-series))
  (define B (b-matrix delta-series))
  (define k-terms (matrix->list (matrix-solve A B)))
  (spline-terms data-series k-terms delta-series))

;; Build a function that interpolates between points in the DATA-SERIES.
;; DATA-SERIES is a list of 2 or more data points, each data point is a
;; (vector/c real? real?).  If only 2 data points are given, the resulting
;; spline will be a line.
;;
;; The returned function accepts one argument, X and returns the interpolated
;; value, Y.  A spline function is only defined between the first and last
;; point in data-series and produces a function that follows these points
;; smoothly.  Our function will extend this over the entire domain, but
;; outside the data points the spline will turn into a line with the same
;; slope as the first and last data point.
(: spline (-> Data-Series (-> Real (U False Real))))
(define (spline data-series)
  (when (or (and (vector? data-series) (< (vector-length data-series) 3))
            (and (list? data-series) (< (length data-series) 3)))
    (raise-argument-error
     'spline "data series needs 2 or more points" data-series))
  (define spline-terms (poly-terms data-series))
  (lambda ((x : Real))
    (let loop ([sterms spline-terms])
      (match-define (vector x0 y0 x1 y1 k0 k1 a b) (car sterms))
      (cond
        ;; At each of the endpoints, the value of the spline is y0 or y1
        ;; respectively (the spline goes through the specified points.
        ((= x x0) y0)
        ((= x x1) y1)
        ;; Before the first point, spline turns into a straight line, with k0
        ;; as the slope
        ((< x x0)
         (+ (* k0 x) (- y0 (* k0 x0))))
        ;; Between these points, we interpolate according to the polynomial.
        ((<= x0 x x1)
         (let* ((t (/ (- x x0) (- x1 x0)))
                (^t (- 1 t)))
           (+ (* t y1) (* ^t y0)
              (* t ^t (+ (* a ^t) (* b t))))))
        ;; After last point, spline turns into a straight line, with k1 as the
        ;; slope.
        ((and (> x x1) (null? (cdr sterms)))
         (+ (* k1 x) (- y1 (* k1 x1))))
        (#t
         (loop (cdr sterms)))))))


;;............................................................. provides ....

(provide spline)
