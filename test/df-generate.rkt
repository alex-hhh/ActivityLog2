#lang racket/base

;; df-generate.rkt -- generate some test files

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

(require racket/match
         "../rkt/data-frame/df.rkt"
         "../rkt/data-frame/series.rkt"
         "../rkt/data-frame/csv.rkt")

;; Generate the test data for the least squares fit, and save it as a CSV file
;; to OUTPUT -- we don't want to run the tests with different random data each
;; time, as this makes them non-reproducible.  Instead, we generate one random
;; file and use it every time we run the tests.

(define (generate-least-squares-fit-test-file output)
  (define tdf (make-data-frame))
  (df-add-series
   tdf
   (make-series "base"
                #:data (for/vector ([n (in-range 100)]) (- n 50))))
  ;; Add a second base, which contains only positive values, logarithmic and
  ;; power law fitting can only be done on positive values.
  (df-add-derived
   tdf
   "base2"
   '("base")
   (lambda (v)
     (match-define (list x) v)
     (+ x 50.1)))

  ;; Add a linear series for fitting a linear function.  The data points will
  ;; be mildly randomized
  (df-add-derived
   tdf "linear" '("base")
   (lambda (v)
     (match-define (list x) v)
     (define r (/ (- (random 1000) 500) 10.0))
     (+ r (+ (* 5 x) 12))))

  ;; Add a second degree polynomial series, the data points being mildly
  ;; randomized.
  (df-add-derived
   tdf "second" '("base")
   (lambda (v)
     (match-define (list x) v)
     (define r (/ (- (random 10000) 5000) 10.0))
     (+ r (+ (* 1.5 x x) (* -2 x) 12))))

  ;; Add a third degree polynomial series, the data points being mildly
  ;; randomized.
  (df-add-derived
   tdf "third" '("base")
   (lambda (v)
     (match-define (list x) v)
     (define r (/ (- (random 1000000) 500000) 10.0))
     (+ r (+ (* -2.1 x x x) (* 1.5 x x) (* -2 x) 12))))

  ;; Add an exponential series, the data points being mildly randomized.
  (df-add-derived
   tdf "exp" '("base")
   (lambda (v)
     (match-define (list x) v)
     (define r (/ (- (random 100) 50) 10.0))
     (+ r (* 3.5 (exp (* 0.1 x))))))

  ;; Add a logarithmic series, the data points being mildly randomized.
  (df-add-derived
   tdf "log" '("base2")
   (lambda (v)
     (match-define (list x) v)
     (define r (/ (- (random 100) 50) 10.0))
     (+ r 5.8 (* 7.3 (log x)))))

  ;; Add a power series, the data points being mildly randomized
  (df-add-derived
   tdf "pow" '("base2")
   (lambda (v)
     (match-define (list x) v)
     (define r (/ (random 10000) 10.0))
     (+ r (* 5.8 (expt x 7.3)))))

  (df-write/csv tdf output "base" "linear" "second" "third" "base2" "exp" "pow" "log"))

