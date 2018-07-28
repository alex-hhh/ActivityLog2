#lang racket/base
;; statistics.rkt -- statistics calculations for data frames
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

(require math/statistics
         racket/contract
         racket/match
         racket/vector
         "df.rkt")

;; Set the default weight series for a data frame.  This will be used as a
;; weight series if none is specified for `df-statistics` or `df-quantile`.
;; Set it to #f for no weight series to be used for statistics.
;;
;; A weight series needs to be used when samples in the data frame don't have
;; equal weight: For example, if a parameter (e.g. heart rate) is recorded at
;; variable intervals, simply averaging the values will not produce an
;; accurate average, if a timer series is also present, it can be used as a
;; weight series to produce a better average.
(define (df-set-default-weight-series df series)
  (df-put-property df 'weight-series series))

;; Return the name of the weight series for this data frame.
(define (df-get-default-weight-series df)
  (df-get-property df 'weight-series))

(define (weighted-statistics stats prev-val val)
  (if prev-val
      (match-let (((list pws pv) prev-val)
                  ((list ws v) val))
        (if (and (real? pws) (real? pv) (real? ws) (real? v))
            (let ([dx (- ws pws)]
                  [dy (/ (+ pv v) 2)])
              (if (> dx 0)       ; can happen for timer series, w/ stop points
                  (update-statistics stats dy dx)
                  stats))
            stats))
      stats))

(define (unweighted-statistics stats val)
  (define v (car val))
  (if (real? v)
      (update-statistics stats v)
      stats))

;; Compute statistics for a series in a data frame.  The statistics will use
;; weighting if a weight series is defined for the data frame.
(define (df-statistics df column
                       #:weight-series [weight (df-get-default-weight-series df)]
                       #:start (start 0)
                       #:stop [stop (df-row-count df)])
  (if (and (df-contains? df column)
           (or (not weight) (df-contains? df weight)))
      (if weight
          (df-fold df (list weight column)
                   empty-statistics weighted-statistics
                   #:start start #:stop stop)
          (df-fold df (list column)
                   empty-statistics unweighted-statistics
                   #:start start #:stop stop))
      #f))

;; Return the quantiles for the series COLUMN in the dataframe DF.  A list of
;; quantiles is returned as specified by QVALUES, or if no quantiles are
;; specified, the list (0 0.25 0.5 1) is used. #:weight-series has the usual
;; meaning, #:less-than is the ordering function passed to the `quantile`
;; function.
(define (df-quantile df column
                     #:weight-series [weight (df-get-default-weight-series df)]
                     #:less-than (lt <)
                     . qvalues)

  (define (dirty? xs ws)
    (or (vector-memq #f xs)
        (and ws (for/first ([item ws] #:unless (> item 0.0)) #t))))

  (define (good? xs ws index)
    (and (vector-ref xs index) (or (not ws) (> (vector-ref ws index) 0))))

  (and (df-contains? df column)
       (or (not weight) (df-contains? df weight))
       (let ((xs-base (df-select df column))
             (ws-base (and weight
                           (df-map df
                                   (list weight)
                                   (lambda (prev current)
                                     (if prev
                                         (- (list-ref current 0) (list-ref prev 0))
                                         (list-ref current 0))))))
             (quantiles (if (null? qvalues) (list 0 0.25 0.5 0.75 1) qvalues)))
         (and (> (vector-length xs-base) 0)
              (if (dirty? xs-base ws-base)
                  (let ((xs (for/vector ([(x idx) (in-indexed xs-base)]
                                         #:when (good? xs-base ws-base idx))
                              x))
                        (ws (and ws-base
                                 (for/vector ([(w idx) (in-indexed ws-base)]
                                              #:when (good? xs-base ws-base idx))
                                   w))))
                    (and (> (vector-length xs) 0)
                         (for/list ([q quantiles]) (quantile q lt xs ws))))
                  (for/list ([q quantiles]) (quantile q lt xs-base ws-base)))))))


;;............................................................. provides ....

(provide/contract
 (df-set-default-weight-series (-> data-frame? (or/c #f string?) any/c))
 (df-get-default-weight-series (-> data-frame? (or/c #f string?)))
 (df-statistics (->* (data-frame? string?)
                     (#:weight-series string?
                      #:start exact-nonnegative-integer?
                      #:stop exact-nonnegative-integer?)
                     (or/c #f statistics?)))
 (df-quantile (->* (data-frame? string?)
                   (#:weight-series string?
                    #:less-than (-> any/c any/c boolean?))
                   #:rest (listof (between/c 0 1))
                   (or/c #f (listof real?)))))
