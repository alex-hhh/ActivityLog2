#lang racket/base
;; intervals.rkt -- find various types of intervals in session data frame
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

(require racket/class
         racket/match
         math/statistics
         "data-frame.rkt"
         "session-df.rkt")

(provide
 make-split-intervals
 make-climb-intervals
 make-best-pace-intervals
 make-best-power-intervals)

;; Split the data frame DF into pieces of equal AMOUNT for SERIES.  Return a
;; list of positions (indexes) where the splits start.  SERIES needs to be one
;; of the sorted series (e.g. time or distance based)
;;
;; This can be used to split the series into equal distance or equal time
;; chunks:
;;
;; (split-by df "dst" 1000) ; km splits
;; (split-by df "timer" 300) ; 5 minute splits
(define (split-by df series amount)
  (define positions '())
  (let loop ((val 0))
    (let ((pos (send df get-index series val)))
      (if pos
          (begin
            (set! positions (cons pos positions))
            (loop (+ val amount)))
          (if (or (null? positions)
                  (equal? (car positions) (send df get-row-count)))
              (reverse positions)
              (reverse (cons (send df get-row-count) positions)))))))

;; Describe summary information that should appear in an interval summary
;; constructed by `make-interval-summary'.
(define summary-def
  '((start-time "timestamp" first)
    (total-distance "dst" total)
    (total-timer-time "timer" total)
    (total-elapsed-time "elapsed" total)
    (avg-speed "spd" avg)
    (max-speed "spd" max)
    (avg-heart-rate "hr" avg)
    (max-heart-rate "hr" max)
    (avg-cadence "cad" avg)
    (max-cadence "cad" max)
    (avg-vertical-oscillation "vosc" avg)
    (avg-stance-time "gct" avg)
    (avg-stance-time-percent "pgct" avg)
    (left-right-balance "lrbal" avg)

    ;; 'total-corrected-ascent
    ;; 'total-ascent
    ;; 'total-corrected-descent
    ;; 'total-descent

    (avg-power "pwr" avg)
    (max-power "pwr" max)
    (avg-left-torque-effectiveness "lteff" avg)
    (avg-right-torque-effectiveness "rteff" avg)
    (avg-left-pedal-smoothness "lpsmth" avg)
    (avg-left-pedal-smoothness "rpsmth" avg)
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
  (send df fold '("timer" "cad") 0
        (lambda (accum prev next)
          (if prev
              (match-let (((vector pts pcad) prev)
                          ((vector nts ncad) next))
                (if (and pts pcad nts ncad)
                    (let ((dt (- nts pts))
                          (dcad (/ (+ pcad ncad) 2)))
                      (+ accum (* dt (/ dcad 60.0))))
                    accum))
              accum))
        #:start start-index #:end end-index))

;; Calculate the total ascent and descent in the data frame DF between
;; START-INDEX and END-INDEX using the "alt" series.  Returns a (cons ASCENT
;; DESCENT)
(define (total-ascent-descent df start-index end-index)
  (send df fold "alt" '(0 . 0)
        (lambda (accum prev next)
          (if prev
              (match-let (((vector palt) prev)
                          ((vector nalt) next))
                (if (and palt nalt)
                    (let ((diff (- nalt palt)))
                      (if (> diff 0)
                          (cons (+ (car accum) diff) (cdr accum))
                          (cons (car accum) (+ (cdr accum) (- diff)))))
                    accum))
              accum))
        #:start start-index #:end end-index))

;; Calculate the total corrected ascent and descent in the data frame DF
;; between START-INDEX and END-INDEX using the "alt" series.  Returns a (cons
;; ASCENT DESCENT)
(define (total-cascent-cdescent df start-index end-index)
  (send df fold "calt" '(0 . 0)
        (lambda (accum prev next)
          (if prev
              (match-let (((vector palt) prev)
                          ((vector nalt) next))
                (if (and palt nalt)
                    (let ((diff (- nalt palt)))
                      (if (> diff 0)
                          (cons (+ (car accum) diff) (cdr accum))
                          (cons (car accum) (+ (cdr accum) (- diff)))))
                    accum))
              accum))
        #:start start-index #:end end-index))

;; Construct ascent and descent lap information from the data frame DF between
;; START-INDEX and END-INDEX
(define (make-ascent-descent df start-index end-index)
  (define ad (total-ascent-descent df start-index end-index))
  (define cad (total-cascent-cdescent df start-index end-index))
  (list
   (cons 'total-ascent (car ad))
   (cons 'total-descent (cdr ad))
   (cons 'total-corrected-ascent (car cad))
   (cons 'total-corrected-descent (cdr cad))))

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
(define (make-interval-summary df start-index end-index)
  (define stats-cache (make-hash))

  (define (get-stats series)
    (let ((stats (hash-ref stats-cache series #f)))
      (unless stats
        (set! stats (df-statistics df series #:start start-index #:end end-index))
        (hash-set! stats-cache series stats))
      stats))

  (define (first-value series)
    (for*/first ([index (in-range start-index end-index 1)]
                 [val (in-value (send df ref index series))]
                 #:when val)
      val))

  (define (last-value series)
    (let ((upper-limit (min end-index (sub1 (send df get-row-count)))))
      (for*/first ([index (in-range upper-limit start-index -1)]
                   [val (in-value (send df ref index series))]
                   #:when val)
        val)))

  (define base
    (for/list ([field summary-def] #:when (send df contains? (list-ref field 1)))
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

  (when (send df contains? "alt" "calt")
    (set! base (append base (make-ascent-descent df start-index end-index))))
  (when (send df contains? "timer" "cad")
    (set! base (append base (make-total-cycles df start-index end-index))))
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
(define (make-split-intervals df series amount)
  (define positions (split-by df series amount))
  (for/list ([start positions]
             [end (cdr positions)])
    (make-interval-summary df start end)))


;;................................................. make-climb-intervals ....

;; Hold information about a climb or descent section in the data frame:
;;
;; START-IDX, END-IDX are indexes in the data frame for the start/end of the
;; climb
;;
;; START-DST, END-DST are distance values in the "dst" series for the
;; start/end of the climb.
;;
;; HDIFF is the height difference in the climb (always positive even if it is
;; a descent)
;;
;; HARD-END? means that this climb cannot be joined with the next one, as it
;; ends at a teleport point.
(struct climb (start-idx end-idx start-dst end-dst hdiff hard-end?)
  #:transparent)

;; Find climb sections (or descents if the DESCENTS? arg is #t) in the data
;; frame DF.  MH determines the minimum height change for a section to be
;; considered a climb or descent (e.g. a value of 20 indicates that only
;; climbs at least 20 meters in height are found).  FW is the width, in
;; meters, of the low-pass filter for grades, this allows finding longer
;; climbs where there are small downhill sections.  Return a list of CLIMB
;; structures, one for each climb.
(define (find-climbs df
                     #:descents (descents? #f)
                     #:min-height (mh 20)
                     #:filter-width (fw 50))

  ;; teleports are the timestamps where the recording was stopped, than the
  ;; user traveled a significant distance and re-started the recording.  They
  ;; are used intensively when recording ski runs; for other activities, this
  ;; should hopefully be empty.  We don't consider climbs that cross teleport
  ;; points.
  (define teleports
    (sort (filter (lambda (p) (is-teleport? df p))
                  (or (send df get-property 'stop-points) '()))
          <))

  ;; Return #t if GRADE is on a climb (or descent if descents is #t)
  (define (on-climb grade) (if descents? (<= grade 0) (>= grade 0)))

  ;; Update END-IDX and END-DST in the climb C and calculate the HDIFF field.
  ;; Also set the hard-end? field to the value of the HARD-END?
  ;; argument. Returns an updated climb structure, does not update C itself.
  (define (finalize c last-index (hard-end? #f))
    (define last-dst
      (let ((idx (min last-index (sub1 (send df get-row-count)))))
        (send df ref idx "dst")))
    (match-define (cons ascent descent)
      (total-cascent-cdescent df (climb-start-idx c) last-index))
    (struct-copy climb c
                 (end-idx last-index)
                 (end-dst last-dst)
                 (hdiff (if descents? descent ascent))
                 (hard-end? hard-end?)))

  ;; Implement a simple low-pass filtering for grade values, to allow passing
  ;; over small downhill portions of a climb.
  (define filter-width fw)
  (define filtered-grade #f)
  (define previous-distance #f)
  (define (filter-grade grade distance)
    (if filtered-grade
        (let* ((dt (- distance previous-distance))
               (alpha (/ dt (+ dt filter-width)))
               (nstate (+ (* alpha grade) (* (- 1 alpha) filtered-grade))))
          (set! filtered-grade nstate)
          (set! previous-distance distance)
          nstate)
        (begin
          (set! filtered-grade grade)
          (set! previous-distance distance)
          grade)))

  (define climbs '())
  (define current-climb #f)
  (define last-climb-idx 0)

  ;; Call finalize on the current climb, add it to the list of climbs and
  ;; reset the filter and the search.
  (define (complete-current-climb (hard-end? #f))
    (when current-climb
      ;; include last-climb-idx in the range our ranges are open at the end
      (let ((uclimb (finalize current-climb (add1 last-climb-idx) hard-end?)))
        (when (> (climb-hdiff uclimb) mh)
          (set! climbs (cons uclimb climbs)))))
    (set! filtered-grade #f)          ; reset filter
    (set! current-climb #f))

  (for (((val index) (in-indexed (send df select* "timestamp" "grade" "dst")))
        #:when (and (vector-ref val 0) (vector-ref val 1) (vector-ref val 2)))
    (match-define (vector t g d) val)
    (unless (or (null? teleports) (< t (car teleports)))
      (complete-current-climb #t)  ; reached a teleport point, climbs end here
      (set! teleports (cdr teleports)))
    (when (on-climb g)
      ;; This is an actual (unfiltered) climb point.  Climbs can only start
      ;; and end at unfiltered climb grades, but are extended past small
      ;; downhill portions by filtering the grade.
      (unless current-climb
        (set! current-climb (climb index index d d #f #f))
        (set! filtered-grade #f))
      (set! last-climb-idx index))
    (unless (on-climb (filter-grade g d))
      (complete-current-climb)))
  (complete-current-climb)              ; don't forget the last one

  (reverse climbs))

;; Return intervals based on the climb (or descent sections in the data frame
;; DF.  See FIND-CLIMBS for the meaning of the DESCENTS?, MH and FW arguments.
(define (make-climb-intervals df
                              #:descents (descents? #f)
                              #:min-height (mh 20)
                              #:filter-width (fw 20))
  (let ((climbs (find-climbs df #:min-height mh #:filter-width fw #:descents descents?)))
    (for/list ((c (in-list climbs)))
      (make-interval-summary df (climb-start-idx c) (climb-end-idx c)))))


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
;; (e.g. the fastest 1km in a 5 km run)
(define (make-best-pace-intervals df)
  (if (send df contains? "spd" "dst")
      (let ((bavg (df-best-avg df "spd" #:weight-column "dst" #:durations best-pace-distances)))
        (for/list ((item (in-list bavg)))
          (match-define (vector duration value position) item)
          (match-define (list start end) (send df get-index* "dst" position (+ position duration)))
          (make-interval-summary df start end)))
      '()))


;;............................................ make-best-power-intervals ....

(define best-power-durations
  '(10 30 60 180 300 600 900 1200 1800 3600 5400 7200 10800))

;; Return intervals based on the best power maintained for a set of predefined
;; durations (e.g. the best 20 minute power)
(define (make-best-power-intervals df)
  (if (send df contains? "pwr" "elapsed")
      (let ((bavg (df-best-avg df "pwr" #:weight-column "elapsed" #:durations best-power-durations)))
        (for/list ((item (in-list bavg)))
          (match-define (vector duration value position) item)
          (match-define (list start end) (send df get-index* "elapsed" position (+ position duration)))
          (make-interval-summary df start end)))
      '()))
