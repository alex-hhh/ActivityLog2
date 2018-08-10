#lang racket/base

;; df-lsq-plots.rkt -- generate plots for the `df-least-squares-fit`

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

;; This file plots the data used in the `df-least-squares-fit` test, so the
;; fit functions can be visualized.  It plots the same file that `df-test.rkt`
;; is testing.

(require "../rkt/data-frame/csv.rkt"
         "../rkt/data-frame/df.rkt"
         "../rkt/data-frame/least-squares-fit.rkt"
         plot
         racket/class)

(define df (df-read/csv "./test-data/lsq-test.csv"))

(define fit-linear (df-least-squares-fit
                    df "base" "linear"
                    #:mode 'linear #:residual? #t))

(define plot-linear-frame
  (plot-frame
   (list (points (df-select* df "base" "linear"))
         (function fit-linear))
   #:title "linear fit"))

(send plot-linear-frame show #t)

(define fit-second (df-least-squares-fit
                    df "base" "second"
                    #:mode 'polynomial
                    #:polynomial-degree 2
                    #:residual? #t))

(define plot-second-frame
  (plot-frame
   (list (points (df-select* df "base" "second"))
         (function fit-second))
   #:title "second degree"))

(send plot-second-frame show #t)

(define fit-third (df-least-squares-fit
                    df "base" "third"
                    #:mode 'polynomial
                    #:polynomial-degree 3
                    #:residual? #t))

(define plot-third-frame
  (plot-frame
   (list (points (df-select* df "base" "third"))
         (function fit-third))
   #:title "third degree"))

(send plot-third-frame show #t)

(define fit-exp (df-least-squares-fit
                    df "base" "exp"
                    #:mode 'exponential
                    #:residual? #t
                    #:annealing? #t
                    #:annealing-iterations 1000))

(define plot-exp-frame
  (plot-frame
   (list (points (df-select* df "base" "exp"))
         (function fit-exp))
   #:title "exponential"))

(send plot-exp-frame show #t)

(define fit-log (df-least-squares-fit
                 df "base2" "log"
                 #:mode 'logarithmic
                 #:residual? #t))

(define plot-log-frame
  (plot-frame
   (list (points (df-select* df "base2" "log"))
         (function fit-log))
   #:title "logarithmic"))
(send plot-log-frame show #t)

(define fit-pow (df-least-squares-fit
                 df "base2" "pow"
                 #:mode 'power
                 #:residual? #t
                 #:annealing? #t
                 #:annealing-iterations 1000))

(define plot-pow-frame
  (plot-frame
   (list (points (df-select* df "base2" "pow"))
         (function fit-pow))
   #:title "power"))
(send plot-pow-frame show #t)

