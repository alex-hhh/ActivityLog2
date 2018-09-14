#lang racket/base
;; mmax-util.rkt -- utilities for Best Avg calculations
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2017, 2018 Alex Harsányi <AlexHarsanyi@gmail.com>
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
(require racket/contract
         racket/class
         racket/match
         "session-df/series-metadata.rkt"
         "session-df/native-series.rkt"
         "data-frame/df.rkt"
         "data-frame/meanmax.rkt"
         "metrics.rkt"
         "fmt-util.rkt")

(provide/contract
 (get-session-mmax (-> data-frame?
                       (is-a?/c series-metadata%)
                       mean-max/c))
 (get-aggregate-mmax (-> (listof exact-positive-integer?)
                         (is-a?/c series-metadata%)
                         (or/c #f (-> real? any/c))
                         (or/c #f aggregate-mmax/c)))
 (get-aggregate-mmax-heat-map (-> (listof exact-positive-integer?)
                                  aggregate-mmax/c
                                  (and/c real? positive?)
                                  (is-a?/c series-metadata%)
                                  (listof (vector/c (and/c real? positive?)
                                                    (and/c real? positive?)))))
 (lookup-duration (-> aggregate-mmax/c (and/c real? positive?)
                      (or/c #f (cons/c aggregate-mmax-item/c aggregate-mmax-item/c))))
 (lookup-duration/closest (-> aggregate-mmax/c (and/c real? positive?)
                              (or/c #f aggregate-mmax-item/c))))

;; Return the best avg for the session in DF (a data-frame%) and AXIS.  See
;; `df-mean-max`.
;;
;; If axis is pace, the best avg is computed for the "spd" series and
;; converted into pace.  See #17 and `get-aggregate-mmax` for details.
(define (get-session-mmax df axis)
  (define is-pace? (eq? axis axis-pace))
  (define is-gap? (eq? axis axis-gap))
  (define is-swim-pace? (eq? axis axis-swim-pace))

  (let* ((sname (cond (is-gap? "gaspd")
                      ((or is-pace? is-swim-pace?) "spd")
                      (#t (send axis series-name))))
         (inverted? (if  (or is-gap? is-pace? is-swim-pace?)
                         #f
                         (send axis inverted-mean-max?)))
         (mean-max-fn (if (df-get-property df 'is-lap-swim?)
                          df-mean-max/lap-swim
                          df-mean-max))
         (data (and (df-contains? df sname)
                    (mean-max-fn df sname #:inverted? inverted?))))
    ;; NOTE: df-mean-max, df-mean-max/lap-swim will return an empty list if
    ;; the activity is too short to compute at least one best avg position
    ;; (minimum is about 10 seconds)
    (if (or is-gap? is-pace? is-swim-pace?)
        (let ((convert-fn (if (or is-gap? is-pace?)
                              convert-m/s->pace
                              convert-m/s->swim-pace)))
          (for/list ((item data))
            (match-define (vector duration value pos) item)
            (vector duration (and value (convert-fn value)) pos)))
        data)))

;; Return the aggregate best avg for CANDIDATES (a list of session ids) and
;; AXIS.  See `aggregate-mmax`
;;
;; If axis is "pace", the best avg is computed for the "spd" series and the
;; result converted into pace.  This has two advantages: first, pace does not
;; average nicely over time (see #17), second, we convert the pace into
;; measurement specific pace values (metric or imperial)
(define (get-aggregate-mmax candidates axis progress-callback)
  (define is-pace? (eq? axis axis-pace))
  (define is-gap? (eq? axis axis-gap))
  (define is-swim-pace? (eq? axis axis-swim-pace))

  (let* ((sname (cond (is-gap? "gaspd")
                      ((or is-pace? is-swim-pace?) "spd")
                      (#t (send axis series-name))))
         (inverted? (if  (or is-gap? is-pace? is-swim-pace?)
                         #f
                         (send axis inverted-mean-max?)))
         (data (aggregate-mmax
                candidates sname
                #:inverted? inverted?
                #:progress-callback progress-callback)))
    (if (or is-gap? is-pace? is-swim-pace?)
        (let ((convert-fn (if (or is-gap? is-pace?)
                              convert-m/s->pace
                              convert-m/s->swim-pace)))
          (for/list ((item data))
            (match-define (list sid timestamp duration value) item)
            (list sid timestamp duration (convert-fn value))))
        data)))

;; Return the heat map for CANDIDATES (a list of session-ids) given a mean-max
;; MMAX data set, as produced by `get-aggregate-mmax`.  The heat map
;; represents the number of sessions that come within PCT percentage points of
;; the MMAX curve at a certain duration point.  AXIS represents the series
;; meta-data for which the values are computed. See `aggregate-mmax-heat-map`.
;;
;; The pace series is treated specially, see `get-aggregate-mmax` and #17 for
;; more details.
(define (get-aggregate-mmax-heat-map candidates mmax pct axis)
  (define is-gap? (eq? axis axis-gap))
  (define is-pace? (eq? axis axis-pace))
  (define is-swim-pace? (eq? axis axis-swim-pace))

  (let* ((sname (cond (is-gap? "gaspd")
                      ((or is-pace? is-swim-pace?) "spd")
                      (#t (send axis series-name))))
         (inverted? (if  (or is-pace? is-swim-pace?)
                         #f
                         (send axis inverted-mean-max?)))
         (data (if (or is-gap? is-pace? is-swim-pace?)
                   ;; MMAX is in "pace" values, convert them back to speed
                   ;; (meters/second)
                   (let ((convert-fn (if (or is-gap? is-pace?)
                                         convert-m/s->pace
                                         convert-m/s->swim-pace)))
                     (for/list ((item mmax))
                       (match-define (list sid timestamp duration value) item)
                       (list sid timestamp duration (convert-fn value))))
                   mmax)))
    (aggregate-mmax-heat-map
     data pct candidates
     sname #:inverted? inverted? #:as-percentage? #t)))

;; Lookup the entries for DURATION in AGGREGATE-MMAX-DATA. Returns the two
;; entries between which DURATION lies. since the data contains entries at
;; discrete points). Returns #f if DURATION is outside the range of the data.
(define (lookup-duration aggregate-mmax-data duration)
  (and (not (null? aggregate-mmax-data))
       (for/or ((prev (in-list aggregate-mmax-data))
                (next (in-list (cdr aggregate-mmax-data))))
         (match-define (list sid1 ts1 duration1 value1) prev)
         (match-define (list sid2 ts2 duration2 value2) next)
         (if (<= duration1 duration duration2)
             (cons prev next)
             #f))))

;; Lookup the closest entry for DURATION in AGGREGATE-MMAX-DATA. Since the
;; data contains entries at discrete points, this function returns the closest
;; point we have in the data.  To return both points use 'lookup-duration'.
;; Returns #f if DURATION is outside the range of the data.
(define (lookup-duration/closest aggregate-mmax-data duration)
  (and (not (null? aggregate-mmax-data))
       (for/or ((prev (in-list aggregate-mmax-data))
                (next (in-list (cdr aggregate-mmax-data))))
        (match-define (list sid1 ts1 duration1 value1) prev)
        (match-define (list sid2 ts2 duration2 value2) next)
        (if (<= duration1 duration duration2)
            (if (< (- duration duration1) (- duration2 duration)) prev next)
            #f))))
