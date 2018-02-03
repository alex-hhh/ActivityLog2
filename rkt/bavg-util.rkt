#lang racket/base
;; bavg-util.rkt -- utilities for Best Avg calculations
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2017 Alex Harsanyi (AlexHarsanyi@gmail.com)
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
         "series-meta.rkt"
         "data-frame.rkt"
         "metrics.rkt"
         "fmt-util.rkt")

(provide/contract
 (get-session-bavg (-> (is-a?/c data-frame%)
                       (is-a?/c series-metadata%)
                       best-avg/c))
 (get-aggregate-bavg (-> (listof exact-positive-integer?)
                         (is-a?/c series-metadata%)
                         (or/c #f (-> real? any/c))
                         aggregate-bavg/c))
 (get-aggregate-bavg-heat-map (-> (listof exact-positive-integer?)
                                  aggregate-bavg/c
                                  (and/c real? positive?)
                                  (is-a?/c series-metadata%)
                                  (listof (vector/c (and/c real? positive?)
                                                    (and/c real? positive?)))))
 (lookup-duration (-> aggregate-bavg/c (and/c real? positive?)
                      (or/c #f (cons/c aggregate-bavg-item/c aggregate-bavg-item/c)))))

;; Return the best avg for the session in DF (a data-frame%) and AXIS.  See
;; `df-best-avg`.
;;
;; If axis is pace, the best avg is computed for the "spd" series and
;; converted into pace.  See #17 and `get-aggregate-bavg` for details.
(define (get-session-bavg df axis)
  (define is-pace? (eq? axis axis-pace))
  (define is-gap? (eq? axis axis-gap))
  (define is-swim-pace? (eq? axis axis-swim-pace))

  (let* ((sname (cond (is-gap? "gaspd")
                      ((or is-pace? is-swim-pace?) "spd")
                      (#t (send axis series-name))))
         (inverted? (if  (or is-gap? is-pace? is-swim-pace?)
                         #f
                         (send axis inverted-best-avg?)))
         (best-avg-fn (if (send df get-property 'is-lap-swim?)
                          df-best-avg/lap-swim
                          df-best-avg))
         (data (and (send df contains? sname)
                    (best-avg-fn df sname #:inverted? inverted?))))
    (if data
        (if (or is-gap? is-pace? is-swim-pace?)
            (let ((convert-fn (if (or is-gap? is-pace?)
                                  convert-m/s->pace
                                  convert-m/s->swim-pace)))
              (for/list ((item data))
                (match-define (vector duration value pos) item)
                (vector duration (and value (convert-fn value)) pos)))
            data)
        #f)))

;; Return the aggregate best avg for CANDIDATES (a list of session ids) and
;; AXIS.  See `aggregate-bavg`
;;
;; If axis is "pace", the best avg is computed for the "spd" series and the
;; result converted into pace.  This has two advantages: first, pace does not
;; average nicely over time (see #17), second, we convert the pace into
;; measurement specific pace values (metric or imperial)
(define (get-aggregate-bavg candidates axis progress-callback)
  (define is-pace? (eq? axis axis-pace))
  (define is-gap? (eq? axis axis-gap))
  (define is-swim-pace? (eq? axis axis-swim-pace))

  (let* ((sname (cond (is-gap? "gaspd")
                      ((or is-pace? is-swim-pace?) "spd")
                      (#t (send axis series-name))))
         (inverted? (if  (or is-gap? is-pace? is-swim-pace?)
                         #f
                         (send axis inverted-best-avg?)))
         (data (aggregate-bavg
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

;; Return the heat map for CANDIDATES (a list of session-ids) given a best-avg
;; BAVG data set, as produced by `get-aggregate-bavg`.  The heat map
;; represents the number of sessions that come within PCT percentage points of
;; the BAVG curve at a certain duration point.  AXIS represents the series
;; meta-data for which the values are computed. See `aggregate-bavg-heat-map`.
;;
;; The pace series is treated specially, see `get-aggregate-bavg` and #17 for
;; more details.
(define (get-aggregate-bavg-heat-map candidates bavg pct axis)
  (define is-gap? (eq? axis axis-gap))
  (define is-pace? (eq? axis axis-pace))
  (define is-swim-pace? (eq? axis axis-swim-pace))

  (let* ((sname (cond (is-gap? "gaspd")
                      ((or is-pace? is-swim-pace?) "spd")
                      (#t (send axis series-name))))
         (inverted? (if  (or is-pace? is-swim-pace?)
                         #f
                         (send axis inverted-best-avg?)))
         (data (if (or is-gap? is-pace? is-swim-pace?)
                   ;; BAVG is in "pace" values, convert them back to speed
                   ;; (meters/second)
                   (let ((convert-fn (if (or is-gap? is-pace?)
                                         convert-m/s->pace
                                         convert-m/s->swim-pace)))
                     (for/list ((item bavg))
                       (match-define (list sid timestamp duration value) item)
                       (list sid timestamp duration (convert-fn value))))
                   bavg)))
    (aggregate-bavg-heat-map
     data pct candidates
     sname #:inverted? inverted? #:as-percentage? #t)))

;; Lookup the entries for DURATION in AGGREGATE-BAVG-DATA. Returns the two
;; entries between which DURATION lies. since the data contains entries at
;; discrete points). Returns #f if DURATION is outside the range of the data.
(define (lookup-duration aggregate-bavg-data duration)
  (for/or ((prev (in-list aggregate-bavg-data))
           (next (in-list (cdr aggregate-bavg-data))))
    (match-define (list sid1 ts1 duration1 value1) prev)
    (match-define (list sid2 ts2 duration2 value2) next)
    (if (<= duration1 duration duration2)
        (cons prev next)
        #f)))
