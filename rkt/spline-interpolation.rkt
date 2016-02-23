#lang typed/racket/base
;; spline-interpolation.rkt -- provides spline interpolation for plots
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

;;; Comentary:
;;
;; Construct a spline interpolation function based on a set of datapoints.
;; The implementation is based on
;; https://en.wikipedia.org/wiki/Spline_interpolation

(require math/matrix
         racket/match)
(provide mk-spline-fn)

(define-type Data-Point (Vector Real Real))
(define-type Delta-Point (Vector Real Real))
(define-type Data-Series (Listof Data-Point))
(define-type Delta-Series (Listof Delta-Point))

;; Calculate the delta series for a set of data points.  The delta is the
;; difference between adjacent points in the data series.  It will have
;; one-less element than data series.
(: ->delta-series (-> Data-Series Delta-Series))
(define (->delta-series data-series)
  (for/list ([d0 data-series] [d1 (cdr data-series)])
    (match-define (vector x0 y0) d0)
    (match-define (vector x1 y1) d1)
    (vector (- x1 x0) (- y1 y0))))

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

;; Contstruct an A term for solving the equation system
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

;; Compute the a, b terms for each interpolation polynomial for a list of data
;; points in DATA-SERIES
(: poly-terms (-> Data-Series (Listof (Vector Real Real))))
(define (poly-terms data-series)
  (define delta-series (->delta-series data-series))
  (define A (a-matrix delta-series))
  (define B (b-matrix delta-series))
  (define k-terms (matrix->list (matrix-solve A B)))
  (ab-terms k-terms delta-series))

;; Build a function that interpolates between points in the DATA-SERIES.  The
;; function accepts one arg, X and returns the interpolated value, Y.  For
;; arguments outside the DATA-SERIES the function will return #f.
(: mk-spline-fn (-> Data-Series (-> Real (U False Real))))
(define (mk-spline-fn data-series)
  (define ab-terms (poly-terms data-series))
  (lambda ((x : Real))
    (let loop ((p0 data-series)
               (p1 (cdr data-series))
               (a-b ab-terms))
      (if (null? p1)
          #f
          (let ()
            (match-define (vector x0 y0) (car p0))
            (match-define (vector x1 y1) (car p1))
            (match-define (vector a b) (car a-b))

            (if (<= x0 x x1)
                (cond ((eqv? x x0) y0)
                      ((eqv? x x1) y1)
                      (#t
                       (let* ((t (/ (- x x0) (- x1 x0)))
                              (^t (- 1 t)))
                         (+ (* t y1) (* ^t y0)
                            (* t ^t (+ (* a ^t) (* b t)))))))
                (loop (cdr p0) (cdr p1) (cdr a-b))))))))
  
