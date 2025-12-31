#lang racket/base
;; intervals.rkt -- find various types of intervals in session data frame
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2017-2023, 2025 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require data-frame
         data-frame/private/rdp-simplify
         math/statistics
         racket/dict
         racket/match
         "models/aerobic-decoupling.rkt"
         "models/coggan.rkt"
         "models/fiets-score.rkt")

(provide
 make-split-intervals
 make-climb-intervals
 make-best-pace-intervals
 make-best-power-intervals
 total-ascent-descent
 make-interval-summary)


;;; AMBIGUITIES in segment distances and times
;;
;; Data samples are recorded at 1 second intervals (or less) so they don't
;; have sufficient precision to create exact distance splits, or sub-second
;; precision for the time splits.  Even a modest running speed will make the
;; distance between samples 2 - 3 meters, faster speed will cause the distance
;; to be greater.
;;
;; For example, when finding the best 1000 meter split in a session, there
;; might not be an exact sample to compute the exact 1000 meter distance.  The
;; code will consider samples such that there are *approximately* 1000 meter
;; in the segment.  So, a best 1000 meter, might find a best 1003.17 meter
;; segment instead.


;; Split the data frame DF into pieces of equal AMOUNT for SERIES.  Return a
;; list of positions (indexes) where the splits start.  SERIES needs to be one
;; of the sorted series (e.g. time or distance based)
;;
;; This can be used to split the series into equal distance or equal time
;; chunks:
;;
;; (split-by df "dst" 1000) ; km splits
;; (split-by df "timer" 300) ; 5 minute splits
;;
;; See also the "AMBIGUITIES" section at the beginning of the file.
(define (split-by df series amount)
  ;; NOTE: the values in SERIES need not start at 0.  For example, in
  ;; multi-sport activities, the distance series in a session starts where the
  ;; previous session left off.  For example, in a HIM race, the distance
  ;; series for the bike split starts at approx 1.8 km.

  (define limit (df-row-count df))

  (let loop ((mark (df-ref df 0 series))
             (positions '()))
    (let ((pos (df-index-of df series mark)))
      (if (< pos limit)
          (loop (+ mark amount) (cons pos positions))
          (if (or (null? positions)
                  ;; Prevent creating an empty interval at the end
                  (>= (car positions) (sub1 limit)))
              (reverse positions)
              (reverse (cons limit positions)))))))

;; Describe summary information that should appear in an interval summary
;; constructed by `make-interval-summary'.
(define summary-def
  '((start-time "timestamp" first)
    (total-distance "dst" total)
    (total-timer-time "timer" total)
    (total-elapsed-time "elapsed" total)
    ;; (avg-speed "spd" avg)
    (max-speed "spd" max)
    (avg-heart-rate "hr" avg)
    (max-heart-rate "hr" max)
    (avg-cadence "cad" avg)
    (max-cadence "cad" max)
    (avg-vertical-oscillation "vosc" avg)
    (avg-stance-time "gct" avg)
    (avg-stance-time-percent "pgct" avg)
    (left-right-balance "lrbal" avg)
    (avg-temperature "tempe" avg)
    (max-temperature "tempe" max)

    ;; 'total-corrected-ascent
    ;; 'total-ascent
    ;; 'total-corrected-descent
    ;; 'total-descent

    (avg-power "pwr" avg)
    (max-power "pwr" max)
    (avg-left-torque-effectiveness "lteff" avg)
    (avg-right-torque-effectiveness "rteff" avg)
    (avg-left-pedal-smoothness "lpsmth" avg)
    (avg-right-pedal-smoothness "rpsmth" avg)
    (avg-combined-pedal-smoothness "cpsmth" avg)
    (avg-left-pco "lpco" avg)
    (avg-right-pco "rpco" avg)
    (avg-left-pp-start "lpps" avg)
    (avg-left-pp-end "lppe" avg)
    (avg-right-pp-start "rpps" avg)
    (avg-right-pp-end "rppe" avg)
    (avg-left-ppp-start "lppps" avg)
    (avg-left-ppp-end "lpppe" avg)
    (avg-right-ppp-start "rppps" avg)
    (avg-right-ppp-end "rpppe" avg)

    ))

;; Return the "total-cycles" (steps, or pedal rotations) in data frame DF
;; between START-INDEX and END-INDEX.  Assumes DF has "timer" and "cad"
;; series.
(define (total-cycles df start-index end-index)
  (df-fold df '("timer" "cad") 0
           (lambda (accum prev next)
             (if prev
                 (match-let (((list pts pcad) prev)
                             ((list nts ncad) next))
                   (if (and pts pcad nts ncad)
                       (let ((dt (- nts pts))
                             (dcad (/ (+ pcad ncad) 2)))
                         (+ accum (* dt (/ dcad 60.0))))
                       accum))
                 accum))
           #:start start-index #:stop end-index))

;; Calculate the total corrected ascent and descent in the data frame DF
;; between START-INDEX and END-INDEX using the "alt" series.  Returns a (cons
;; ASCENT DESCENT)
(define (total-ascent-descent df series start-index end-index)

  ;; NOTE: we only accumulate ascent and descent if the elevation gain or loss
  ;; is greater than 1 meter -- this avoids accumulating lots of very small
  ;; elevation changes, which would artificially inflate the total elevation
  ;; gain.

  (match-define
    (list ascent descent _)
    (df-fold df series
             '(0 0 #f)
             (lambda (accum val)
               (match-define (list alt) val)
               (if alt
                   (match-let ([(list ascent descent current) accum])
                     (cond ((equal? current #f)
                            (list ascent descent alt))
                           ((> alt (add1 current))
                            (list (+ ascent (- alt current)) descent alt))
                           ((< alt (sub1 current))
                            (list ascent (+ descent (- current alt)) alt))
                           (#t
                            accum)))
                   accum))
             #:start start-index #:stop end-index))
  (values ascent descent))

;; Construct ascent and descent lap information from the data frame DF between
;; START-INDEX and END-INDEX
(define (make-ascent-descent df start-index end-index)
  (define-values (ascent descent)
    (if (df-contains? df "alt")
        (total-ascent-descent df "alt" start-index end-index)
        (values #f #f)))
  (define-values (cascent cdescent)
    (if (df-contains? df "calt")
        (total-ascent-descent df "calt" start-index end-index)
        (values #f #f)))
  (list
   (cons 'total-ascent ascent)
   (cons 'total-descent descent)
   (cons 'total-corrected-ascent cascent)
   (cons 'total-corrected-descent cdescent)))

;; Construct "total cycles" lap information from the data frame DF between
;; START-INDEX and END-INDEX.
(define (make-total-cycles df start-index end-index)
  (list
   (cons 'total-cycles (total-cycles df start-index end-index))))

;; Construct an interval summary for the data frame DF between START-INDEX and
;; END-INDEX.  The interval summary contains averages and maximums for the
;; interval, and has the same structure as the stored laps for a session, the
;; usual 'lap-*' accessors from "activity-util.rkt" will work on the returned
;; object.
(define (make-interval-summary df start-index end-index #:ftp ftp)
  (define stats-cache (make-hash))

  (define (get-stats series)
    (let ((stats (hash-ref stats-cache series #f)))
      (unless stats
        (set! stats (df-statistics df series #:start start-index #:stop end-index))
        (hash-set! stats-cache series stats))
      stats))

  (define (first-value series)
    (for*/first ([index (in-range start-index end-index 1)]
                 [val (in-value (df-ref df index series))]
                 #:when val)
      val))

  (define (last-value series)
    (let ((upper-limit (min end-index (sub1 (df-row-count df)))))
      (for*/first ([index (in-range upper-limit start-index -1)]
                   [val (in-value (df-ref df index series))]
                   #:when val)
        val)))

  (define base
    (for/list ([field summary-def] #:when (df-contains? df (list-ref field 1)))
      (match-define (list tag series what) field)
      (define val
        (case what
          ((first) (first-value series))
          ((total)
           (let ((start (first-value series))
                 (end (last-value series)))
             (and start end (- end start))))
          ((avg)
           (let ((stats (get-stats series)))
             (statistics-mean stats)))
          ((max)
           (let ((stats (get-stats series)))
             (statistics-max stats)))))
      (cons tag val)))

  (when (df-contains/any? df "alt" "calt")
    (set! base (append base (make-ascent-descent df start-index end-index))))
  (when (df-contains? df "timer" "cad")
    (set! base (append base (make-total-cycles df start-index end-index))))

  ;; Calculate the average speed from total time and distance, averaging the
  ;; "spd" series does not produce nice results.
  (let ((total-distance (dict-ref base 'total-distance #f))
        (total-timer-time (dict-ref base 'total-timer-time #f)))
    (when (and total-distance total-timer-time (> total-timer-time 0))
      (set! base (cons (cons 'avg-speed (/ total-distance total-timer-time)) base))))

  (let ([adec (aerobic-decoupling df #:start start-index #:stop end-index)])
    (when adec
      (set! base (cons (cons 'aerobic-decoupling adec) base))))

  ;; Normalized power is only available if a FTP value is set.
  (when (and (df-contains? df "pwr") ftp)
    (let ([cg (cg-metrics df #:start start-index #:stop end-index)])
      (set! base (cons (cons 'normalized-power (cg-np cg)) base))))

  ;; Mark this lap as custom
  (set! base (append (list '(custom-lap . #t)) base))
  base)

;; Construct split intervals (a list of interval summary objects) obtained by
;; splitting the data frame DF into equal AMOUNT parts for SERIES. This can be
;; used to split a session into new set of laps, independent of the original
;; laps that were recorded.  For example:
;;
;; (make-split-intervals df "dst" 1000) ; lap every km
;; (make-split-intervals df "timer" 300) ; lap every 5 minutes
;;
;; See also the "AMBIGUITIES" section at the beginning of the file.
(define (make-split-intervals df series amount #:ftp ftp)
  (if (df-contains? df series)
      (let ((positions (split-by df series amount)))
        (for/list ([start positions]
                   [end (cdr positions)])
          (make-interval-summary df start end #:ftp ftp)))
      '()))


;;................................................. make-climb-intervals ....

;; Return intervals based on the climb (or descent sections) in the data frame
;; DF.
(define (make-climb-intervals df
                              #:descents (descents? #f)
                              #:min-grade (mg 0.5)
                              #:min-length (ml 100)
                              #:ftp ftp)

  ;; A segment between points p1 and p2 is valid if it is a climb or descent,
  ;; as per DESCENTS?, and has a grade greater than the minimum grade MG.
  (define (valid-segment? p1 p2)
    (match-define (vector d1 a1 _ ...) p1)
    (match-define (vector d2 a2 _ ...) p2)
    (and (if descents? (> a1 a2) (< a1 a2))
         (> d2 d1)
         (> (abs (* (/ (- a2 a1) (- d2 d1)) 100.0)) mg)))

  (define (do-it alt-series)
    (define data (df-select* df "dst" alt-series "timestamp" #:filter valid-only))
    (define-values (low high)
      (for/fold ([low #f] [high #f])
                ([item (in-vector data)])
        (define a (vector-ref item 1))  ; altitude
        (values
         (if low (min a low) a)
         (if high (max a high) a))))
    (define epsilon (max 1e-2 (/ (- high low) 20)))
    (define peaks(rdp-simplify data #:epsilon epsilon #:destroy-original? #t))
    (define segments
      (if (> (vector-length peaks) 1)
          (for/fold ([result '()])
                    ([p1 (in-vector peaks 0)]
                     [p2 (in-vector peaks 1)]
                     #:when (valid-segment? p1 p2))
            (if (null? result)
                (cons (cons p1 p2) result)
                (let ((previous (car result)))
                  (if (eq? p1 (cdr previous))
                      ;; concatenate two adjacent segments
                      (cons (cons (car previous) p2) (cdr result))
                      (cons (cons p1 p2) result)))))
          '()))
    (for/list ([s (in-list (reverse segments))])
      (define d1 (vector-ref (car s) 2))
      (define d2 (vector-ref (cdr s) 2))
      (match-define (list start end) (df-index-of* df "timestamp" d1 d2))
      (define summary (make-interval-summary df start end #:ftp ftp))
      (define fiets (fiets-score df #:start start #:stop end))
      (if fiets
          (cons
           (cons 'fiets-score fiets)
           summary)
          summary)))

  (define intervals
    (if (df-contains? df "timestamp" "dst")
        (cond ((df-contains? df "calt") (do-it "calt"))
              ((df-contains? df "alt") (do-it "alt"))
              (#t '()))
        '()))

  (filter (lambda (i)
            (let ([distance (dict-ref i 'total-distance)]
                  [elevation (if descents?
                                 (or (dict-ref i 'total-corrected-descent #f)
                                     (dict-ref i 'total-descent))
                                 (or (dict-ref i 'total-corrected-ascent #f)
                                     (dict-ref i 'total-ascent)))])
              (and (>= distance ml)
                   (>= (abs (* (/ elevation distance) 100)) (abs mg)))))
          intervals))


;;............................................. make-best-pace-intervals ....

(define best-pace-distances
  '(400 800 1000
        1600 3200                       ; 1 mile, 2 miles
        5000
        10000
        15000
        16000                           ; 10 miles
        21095                           ; Half Marathon
        42190                           ; Full Marathon
        ))

;; Return intervals based on the best pace for a set of predefined distances
;; (e.g. the fastest 1km in a 5 km run).
;;
;; See also the "AMBIGUITIES" section at the beginning of the file.
(define (make-best-pace-intervals df #:ftp ftp)
  (if (df-contains? df "spd" "dst")
      (let ((bavg (df-mean-max df "spd" #:weight-series "dst" #:durations best-pace-distances)))
        (for/list ((item (in-list bavg)))
          (match-define (vector duration value position) item)
          (match-define (list start end) (df-index-of* df "dst" position (+ position duration)))
          (make-interval-summary df start end #:ftp ftp)))
      '()))


;;............................................ make-best-power-intervals ....

(define best-power-durations
  '(10 30 60 180 300 600 900 1200 1800 3600 5400 7200 10800))

;; Return intervals based on the best power maintained for a set of predefined
;; durations (e.g. the best 20 minute power)
(define (make-best-power-intervals df #:ftp ftp)
  (if (df-contains? df "pwr" "elapsed")
      (let ((bavg (df-mean-max df "pwr" #:weight-series "elapsed" #:durations best-power-durations)))
        (for/list ((item (in-list bavg)))
          (match-define (vector duration value position) item)
          (match-define (list start end) (df-index-of* df "elapsed" position (+ position duration)))
          (make-interval-summary df start end #:ftp ftp)))
      '()))
