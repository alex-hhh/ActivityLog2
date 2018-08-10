#lang racket/base
;; least-square-fitting.rkt -- fit curves based on least squares method
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

(require "df.rkt" "annealing.rkt" math/matrix racket/contract racket/match)

;; Return polynomial coefficients for a polynomial that fits the XS/YS data
;; set.  NITEMS is the number if items in XS and YS and DEGREE is the degree
;; of the requested polynomial.
;;
;; The algorithm is described here:
;; http://mathworld.wolfram.com/LeastSquaresFittingPolynomial.html
;;
(define (polynomial-fit-coefficients xs ys nitems degree)

  (define y-matrix (list->matrix nitems 1 ys))
  (define x-matrix (vandermonde-matrix xs (add1 degree)))
  (define x-matrix-transposed (matrix-transpose x-matrix))
  (define x (matrix* x-matrix-transposed x-matrix))
  (define y (matrix* x-matrix-transposed y-matrix))
  (matrix->list (matrix-solve x y)))

;; Return a function that evaluates a polynomial from COEFFICIENTS.
(define (make-polynomial coefficients)
  (lambda (x)
    (define-values (y p)
      (for/fold ([y 0] [p 1])
                ([c (in-list coefficients)])
        (values (+ y (* p c)) (* p x))))
    y))

;; Return the coefficients for an exponential fit function for the data set
;; XS/YS. NITEMS is the number of items in the data set.
;;
;; The algorithm is described below, the simplified version, where bigger
;; weight is given to smaller values, is implemented here:
;;
;; http://mathworld.wolfram.com/LeastSquaresFittingExponential.html
;;
(define (exponential-fit-coefficients xs ys nitems)

  (define miny
    (for/fold ([m #f]) ([y (in-list ys)]) (if m (min m y) y)))

  (define ys1
    (if (< miny 0.1)
        (for/list ([y (in-list ys)]) (+ y (+ (- miny) 0.1)))
        ys))

  (define xs-squared (map (lambda (x) (* x x)) xs))
  (define sum-xs-squared (foldl + 0 xs-squared))
  (define sum-xs (foldl + 0 xs))

  (define ln-ys (map log ys1))
  (define sum-ln-ys (foldl + 0 ln-ys))
  (define sum-x-ln-ys (for/sum ([x (in-list xs)] [ln-y (in-list ln-ys)]) (* x ln-y)))

  (define denom (- (* nitems sum-xs-squared) (* sum-xs sum-xs)))

  (define a (/ (- (* sum-ln-ys sum-xs-squared) (* sum-xs sum-x-ln-ys)) denom))
  (define b (/ (- (* nitems sum-x-ln-ys) (* sum-xs sum-ln-ys)) denom))

  (list (exp a) b 0))

;; Return the coefficients for an exponential fit function for the data set
;; XS/YS. NITEMS is the number of items in the data set.
;;
;; Y = A * e^B + C
;;
;; The algorithm is described below, the more complex version, which gives
;; equal weight to all values, is implemented here:
;;
;; http://mathworld.wolfram.com/LeastSquaresFittingExponential.html
;;
(define (exponential-fit-coefficients-2 xs ys nitems)
  (define miny
    (for/fold ([m #f]) ([y (in-list ys)]) (if m (min m y) y)))

  (define ys1
    (if (< miny 0.1)
        (for/list ([y (in-list ys)]) (+ y (+ (- miny) 0.1)))
        ys))

  (define sum-xs-squared-ys
    (for/sum ([x (in-list xs)] [y (in-list ys1)]) (* x x y)))
  (define sum-xs-ys
    (for/sum ([x (in-list xs)] [y (in-list ys1)]) (* x y)))
  (define sum-ys-ln-ys
    (for/sum ([y (in-list ys1)]) (* y (log y))))
  (define sum-xs-ys-ln-ys
    (for/sum ([x (in-list xs)] [y (in-list ys1)]) (* x y (log y))))
  (define sum-ys (foldl + 0 ys1))

  (define denom (- (* sum-ys sum-xs-squared-ys) (* sum-xs-ys sum-xs-ys)))

  (define a (/ (- (* sum-xs-squared-ys sum-ys-ln-ys) (* sum-xs-ys sum-xs-ys-ln-ys)) denom))
  (define b (/ (- (* sum-ys sum-xs-ys-ln-ys) (* sum-xs-ys sum-ys-ln-ys)) denom))

  (list (exp a) b (if (< miny 0.1) (- miny 0.1) 0)))

;; Construct the exponential fit function from COEFFICIENTS.
(define (make-exponential coefficients)
  (match-define (list a b c) coefficients)
  (lambda (x) (+ (* a (exp (* b x))) c)))

;; Optimize the exponential fit COEFFICIENTS for the data set XS/YS by running
;; the `annealing` algorithm ITERATIONS number of times.  
(define (find-better-exponential-coefficients coefficients xs ys iterations)

  (define (neighbour coefficients temp)
    (match-define (list a b c) coefficients)
    (define r1 (+ 1 (* temp (- (* 2 (random)) 1))))
    (define r2 (+ 1 (* temp (- (* 2 (random)) 1))))
    (define r3 (+ 1 (* temp (- (* 2 (random)) 1))))
    (define r (list (* r1 a) (* r2 b) (* r3 c)))

    r)

  (annealing
   ;; We start in the middle of the range
   #:initial coefficients
   ;; Our goal is the cost function
   #:goal (lambda (coefficients)
            (calculate-residual (make-exponential coefficients) xs ys))
   #:neighbour neighbour
   #:iterations iterations))

;; Return the coefficients for an logarithmic fit function for the data set
;; XS/YS. NITEMS is the number of items in the data set.
;;
;; y = A + B * ln(x)
;;
;; The algorithm is described here:
;; http://mathworld.wolfram.com/LeastSquaresFittingLogarithmic.html
;;
(define (logarithmic-fit-coefficients xs ys nitems)

  (define ln-xs (map log xs))
  (define sum-ys-ln-xs (for/sum ([x (in-list ln-xs)] [y (in-list ys)]) (* y x)))
  (define sum-ys (foldl + 0 ys))
  (define sum-ln-xs (foldl + 0 ln-xs))
  (define sum-ln-xs-squared (for/sum ([x (in-list ln-xs)]) (* x x)))

  (define b (/ (- (* nitems sum-ys-ln-xs) (* sum-ys sum-ln-xs))
               (- (* nitems sum-ln-xs-squared) (* sum-ln-xs sum-ln-xs))))
  (define a (/ (- sum-ys (* b sum-ln-xs)) nitems))

  (list a b))

;; Construct the logarithmic fit function from COEFFICIENTS
(define (make-logarithmic coefficients)
  (match-define (list a b) coefficients)
  (lambda (x) (+ a (* b (log x)))))

;; Return the coefficients for a power law fit function for the data set
;; XS/YS. NITEMS is the number of items in the data set
;;
;; y = A * x ^ B
;;
;; The algorithm is described here:
;; http://mathworld.wolfram.com/LeastSquaresFittingPowerLaw.html
;;
(define (power-law-fit-coefficients xs ys nitems)
  
  (define ln-xs (map log xs))
  (define ln-ys (map log ys))
  (define sum-ln-xs-ln-ys (for/sum ([x (in-list ln-xs)] [y (in-list ln-ys)]) (* x y)))
  (define sum-ln-xs (foldl + 0 ln-xs))
  (define sum-ln-ys (foldl + 0 ln-ys))
  (define sum-ln-xs-squared (for/sum ([x (in-list ln-xs)]) (* x x)))

  (define b (/ (- (* nitems sum-ln-xs-ln-ys) (* sum-ln-xs sum-ln-ys))
               (- (* nitems sum-ln-xs-squared) (* sum-ln-xs sum-ln-xs))))
  (define a (/ (- sum-ln-ys (* b sum-ln-xs)) nitems))

  (list (exp a) b))

;; Construct the power law fit function based on COEFFICIENTS.
(define (make-power-law coefficients)
  (match-define (list a b) coefficients)
  (lambda (x) (* a (expt x b))))

;; Optimize the power law fit COEFFICIENTS for the data set XS/YS by running
;; the `annealing` algorithm ITERATIONS number of times.
(define (find-better-power-law-coefficients coefficients xs ys iterations)

  (define (neighbour coefficients temp)
    (match-define (list a b) coefficients)
    (define r1 (+ 1 (* temp (- (* 2 (random)) 1))))
    (define r2 (+ 1 (* temp (- (* 2 (random)) 1))))
    (define r (list (* r1 a) (* r2 b)))
    r)

  (annealing
   ;; We start in the middle of the range
   #:initial coefficients
   ;; Our goal is the cost function
   #:goal (lambda (coefficients)
            (calculate-residual (make-power-law coefficients) xs ys))
   #:neighbour neighbour
   #:iterations iterations))

;; Calculate the residual between the fit function FN for the data set XS/YS.
;; The residual is the sum of squared differences between the Y values and the
;; fit function value at the corresponding X value.
(define (calculate-residual fn xs ys)
  (for/sum ([x (in-list xs)] [y (in-list ys)])
    (define d (- y (fn x)))
    (* d d)))

;; Return value for the `df-least-squares-fit` function.  The structure can be
;; applied directly as a procedure and acts as the fit function.
(struct least-squares-fit
  (;; The type of the fit ('linear, 'polynomial, etc) -- this is the same as
   ;; the #:mode parameter for the `df-least-squares-fit` function
   type
   ;; Coefficients for the fit function -- this is a list of values, but the
   ;; coefficients depend on the fit mode.
   coefficients
   ;; Residuals for the fit functions w.r.t the original data -- smaller
   ;; values indicate a better fit.
   residual
   ;; The fit function itself (-> real? real?) -- this function is called when
   ;; an instance of this structure is applied.
   fn)
  #:transparent
  #:property prop:procedure
  (lambda (s x) ((least-squares-fit-fn s) x)))

;; Return a best fit function for the XSERIES and YSERIES in the data frame
;; DF.  START and STOP specify the start and end position in the series, by
;; default all values are considered for the fit.
;;
;; MODE determines the type of the function being fitted and can have one of
;; the following values:
;;
;; 'linear -- a function Y = a * X + b is fitted where 'a' and 'b' are fitted;
;;         this is equivalent of fitting a 'polynomial of degree 1 (see below)
;;
;; 'polynomial or 'poly -- a polynomial Y = a0 + a1 * X + a2 * X^2 + ... is
;;         fitted.  The degree of the polynomial is specified by the DEGREE
;;         parameter, by default this is 2
;;
;; 'exponential or 'exp -- a function of Y = a * e ^ (b * X) + c is fitted.
;;         Note that this fit is not very good, and annealing needs to be used
;;         to improve it (see below)
;;
;; 'logarithmic or 'log -- a function of type Y = a + b * ln(X) is fitted.
;;         This will only return a "real" fit function (as opposed to an
;;         imaginary one) if all values in YSERIES are positive
;;
;; 'power -- a function of type Y = a * X ^ b is fitted. This will only return
;;         a "real" fit function (as opposed to an imaginary one) if all
;;         values in YSERIES are positive.  Note that this fit is not very
;;         good, and annealing needs to be used to improve it (see below)
;;
;; RESIDUAL? when #t indicates that the residual value is also returned in the
;; `least-squares-fit` structure.  Setting it to #f will avoid some
;; unnecessary computations.
;;
;; ANNEALING? when #t indicates that the fit coefficients should be further
;; refined using the `annealing` function.  This is only used for 'exponential
;; or 'power fit functions as these ones do not produce "best fit"
;; coefficients -- I don't know why, I am not a mathematician, I only used the
;; formulas.  Using annealing will significantly improve the fit for these
;; functions, but will still not determine the best one.  Note that the
;; annealing algorithm is probabilistic, so applying it a second time on the
;; same arguments will produce a slightly different result.
;;
;; ITERATIONS represents the number of annealing iterations, see the
;; #:iterations parameter to the `annealing` function.
;;
;; This function returns a least-squares-fit structure instance.  The instance
;; can be applied directly as a function, being the best fit function for the
;; input data.
;;
(define (df-least-squares-fit
         df xseries yseries
         #:start (start 0)
         #:stop (stop (df-row-count df))
         #:mode (mode 'linear)
         #:polynomial-degree (degree 2)
         #:residual? (residual? #f)
         #:annealing? (annealing? #f)
         #:annealing-iterations (iterations 500))

  (define-values (xs ys n)
    (for/fold
        ([xs '()] [ys '()] [n 0])
        ;; NOTE: we traverse the series in reverse, so `xs` and `ys` are
        ;; cons-ed in the right order.
        ([(x y) (in-data-frame df xseries yseries #:start (sub1 stop) #:stop (sub1 start))]
         #:when (and (real? x) (real? y)))
      (values (cons x xs) (cons y ys) (add1 n))))

  (case mode
    ((linear)
     (let* ((coefficients (polynomial-fit-coefficients xs ys n 1))
            (fn (make-polynomial coefficients)))
       (least-squares-fit
        'linear
        coefficients
        (if residual? (calculate-residual fn xs ys) #f)
        fn)))
    ((polynomial)
     (let* ((coefficients (polynomial-fit-coefficients xs ys n degree))
            (fn (make-polynomial coefficients)))
       (least-squares-fit
        'polynomial
        coefficients
        (if residual? (calculate-residual fn xs ys) #f)
        fn)))
    ((exp exponential)
     (let* ((c1 (exponential-fit-coefficients-2 xs ys n))
            (c2 (if annealing? (find-better-exponential-coefficients c1 xs ys iterations) c1))
            (fn (make-exponential c2)))
       (least-squares-fit
        'exponential
        c2
        (if residual? (calculate-residual fn xs ys) #f)
        fn)))
    ((log logarithmic)
     (let* ((coefficients (logarithmic-fit-coefficients xs ys n))
            (fn (make-logarithmic coefficients)))
       (least-squares-fit
        'logarithmic
        coefficients
        (if residual? (calculate-residual fn xs ys) #f)
        fn)))
    ((power)
     (let* ((c1 (power-law-fit-coefficients xs ys n))
            (c2 (if annealing? (find-better-power-law-coefficients c1 xs ys iterations) c1))
            (fn (make-power-law c2)))
       (least-squares-fit
        'power
        c2
        (if residual? (calculate-residual fn xs ys) #f)
        fn)))
    (else
     (raise-argument-error 'mode "valid fit method" mode))))

(provide/contract
 (df-least-squares-fit (->* (data-frame? string? string?)
                            (#:start exact-nonnegative-integer?
                             #:stop exact-nonnegative-integer?
                             #:mode (or/c 'linear
                                          'polynomial 'poly
                                          'power
                                          'exponential 'exp
                                          'logarithmic 'log)
                             #:polynomial-degree exact-nonnegative-integer?
                             #:residual? boolean?
                             #:annealing? boolean?
                             #:annealing-iterations exact-nonnegative-integer?)
                            least-squares-fit?)))
(provide (struct-out least-squares-fit))
