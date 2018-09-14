#lang racket/base
;; intervals.rkt -- find various types of intervals in session data frame
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

(require racket/match
         racket/dict
         math/statistics
         "data-frame/statistics.rkt"
         "data-frame/meanmax.rkt"
         "data-frame/df.rkt"
         "session-df/session-df.rkt")

(provide
 make-split-intervals
 make-climb-intervals
 make-best-pace-intervals
 make-best-power-intervals)

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
  (let ((base (df-ref df 0 series)))
    (define limit (df-row-count df))
    (let loop ((split 0)
               (positions '()))
      (let ((pos (df-index-of df series (+ base (* split amount)))))
        (if (< pos limit)
            (loop (add1 split) (cons pos positions))
            (if (or (null? positions)
                    (equal? (car positions) (df-row-count df)))
                (reverse positions)
                (reverse (cons (df-row-count df) positions))))))))

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

;; Calculate the total ascent and descent in the data frame DF between
;; START-INDEX and END-INDEX using the "alt" series.  Returns a (cons ASCENT
;; DESCENT)
(define (total-ascent-descent df start-index end-index)
  (df-fold df "alt" '(0 . 0)
           (lambda (accum prev next)
             (if prev
                 (match-let (((list palt) prev)
                             ((list nalt) next))
                   (if (and palt nalt)
                       (let ((diff (- nalt palt)))
                         (if (> diff 0)
                             (cons (+ (car accum) diff) (cdr accum))
                             (cons (car accum) (+ (cdr accum) (- diff)))))
                       accum))
                 accum))
           #:start start-index #:stop end-index))

;; Calculate the total corrected ascent and descent in the data frame DF
;; between START-INDEX and END-INDEX using the "alt" series.  Returns a (cons
;; ASCENT DESCENT)
(define (total-cascent-cdescent df start-index end-index)
  (df-fold df "calt" '(0 . 0)
           (lambda (accum prev next)
             (if prev
                 (match-let (((list palt) prev)
                             ((list nalt) next))
                   (if (and palt nalt)
                       (let ((diff (- nalt palt)))
                         (if (> diff 0)
                             (cons (+ (car accum) diff) (cdr accum))
                             (cons (car accum) (+ (cdr accum) (- diff)))))
                       accum))
                 accum))
           #:start start-index #:stop end-index))

;; Construct ascent and descent lap information from the data frame DF between
;; START-INDEX and END-INDEX
(define (make-ascent-descent df start-index end-index)
  (define ad
    (if (df-contains? df "alt")
        (total-ascent-descent df start-index end-index)
        (cons #f #f)))
  (define cad
    (if (df-contains? df "calt")
        (total-cascent-cdescent df start-index end-index)
        (cons #f #f)))
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
        (total-elapsed-time (dict-ref base 'total-elapsed-time #f)))
    (when (and total-distance total-elapsed-time (> total-elapsed-time 0))
      (set! base (cons (cons 'avg-speed (/ total-distance total-elapsed-time)) base))))

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
(define (make-split-intervals df series amount)
  (if (df-contains? df series)
      (let ((positions (split-by df series amount)))
        (for/list ([start positions]
                   [end (cdr positions)])
          (make-interval-summary df start end)))
      '()))


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

(define (climb-grade c)
  (let ((dd (- (climb-end-dst c) (climb-start-dst c))))
    (if (> dd 0)
        (* 100.0 (/ (climb-hdiff c) dd))
        0)))

(define (climb-distance c)
  (- (climb-end-dst c) (climb-start-dst c)))

;; Return the size of the gap between climbs C1 and C2
(define (climb-gap c1 c2)
  (- (climb-start-dst c2) (climb-end-dst c1)))

;; Return #t if there are teleport points between START and END.  TELEPORTS is
;; a list of teleport indexes (*not* timestamps!) and START, END are indexes
;; too...
(define (have-teleports-between? teleports start end)
  (for/first ([t (in-list teleports)] #:when (and (>= t start) (<= t end)))
    #t))

;; Return #t if climbs C1 and C2 should be joined.  They should be joined if
;; the gap between them is small and there are no teleport points in the gap
;; between them.
(define (should-join? c1 c2 teleports-idx)
  (let ((d1 (climb-distance c1))
        (d2 (climb-distance c2))
        (gap (climb-gap c1 c2)))
    (define result
      (and (<= (/ gap (+ d1 d2)) 0.15)
           (not (have-teleports-between? teleports-idx
                                         (climb-end-idx c1)
                                         (climb-start-idx c2)))))
    result))

;; Join adjacent CLIMBS if they are closer than MJD.  Discard resulting climbs
;; with a grade less than MG or a length less than ML
(define (merge-adjacent-climbs climbs
                               #:min-grade [mg 1.0]
                               #:min-length [ml 50]
                               #:teleports-idx [tli '()])

  (define (merge c1 c2)
    (climb
     (climb-start-idx c1)
     (climb-end-idx c2)
     (climb-start-dst c1)
     (climb-end-dst c2)
     (+ (climb-hdiff c1) (climb-hdiff c2))
     (climb-hard-end? c2)))

  (define (good? c)
    (and (>= (climb-distance c) ml)
         (>= (climb-grade c) mg)))

  (define (kons c l) (if (good? c) (cons c l) l))

  (if (null? climbs)
      '()
      (let loop ((candidate (car climbs))
                 (remaining (cdr climbs))
                 (result '()))
        (cond ((null? remaining)
               (reverse (kons candidate result)))
              ((climb-hard-end? candidate)
               (loop (car remaining) (cdr remaining) (kons candidate result)))
              (#t
               (let ((next (car remaining)))
                 (if (should-join? candidate next tli)
                     (loop (merge candidate next) (cdr remaining) result)
                     (loop (car remaining) (cdr remaining) (kons candidate result)))))))))

;; Find climb sections (or descents if the DESCENTS? arg is #t) in the data
;; frame DF.  Return a list of CLIMB structures, one for each climb.  MJD, MG
;; and ML are passed on to MERGE-ADJACENT-CLIMBS
(define (find-climbs df
                     #:descents (descents? #f)
                     #:min-grade [mg 1.0]
                     #:min-length (ml 50))

  ;; teleports are the timestamps where the recording was stopped, than the
  ;; user traveled a significant distance and re-started the recording.  They
  ;; are used intensively when recording ski runs; for other activities, this
  ;; should hopefully be empty.  We don't consider climbs that cross teleport
  ;; points.
  (define teleports
    (sort (filter (lambda (p) (is-teleport? df p))
                  (or (df-get-property df 'stop-points) '()))
          <))

  ;; This is the position of the teleport points in the data frame
  (define teleports-idx
    (apply df-index-of* df "timestamp" teleports))

  ;; Return #t if GRADE is on a climb (or descent if descents is #t)
  (define (on-climb grade) (if descents? (< grade 0) (> grade 0)))

  ;; Update END-IDX and END-DST in the climb C and calculate the HDIFF field.
  ;; Also set the hard-end? field to the value of the HARD-END?
  ;; argument. Returns an updated climb structure, does not update C itself.
  (define (finalize c last-index hard-end?)
    (define last-dst
      (let ((idx (min last-index (sub1 (df-row-count df)))))
        (df-ref df idx "dst")))
    (match-define (cons ascent descent)
      (cond ((df-contains? df "calt")
             (total-cascent-cdescent df (climb-start-idx c) last-index))
            ((df-contains? df "alt")
             (total-ascent-descent df (climb-start-idx c) last-index))
            (#t
             (cons 0 0))))
    (struct-copy climb c
                 (end-idx last-index)
                 (end-dst last-dst)
                 (hdiff (if descents? (- descent ascent) (- ascent descent)))
                 (hard-end? hard-end?)))

  (define climbs '())
  (define current-climb #f)
  (define last-climb-idx 0)

  ;; Call finalize on the current climb, add it to the list of climbs and
  ;; reset the search.
  (define (complete-current-climb hard-end?)
    (when current-climb
      (let ((uclimb (finalize current-climb last-climb-idx hard-end?)))
        (set! climbs (cons uclimb climbs))))
    (set! current-climb #f))

  (for (([t g d] (in-data-frame df "timestamp" "grade" "dst"))
        (index (in-range (df-row-count df)))
        #:when (and t g d))
    (unless (or (null? teleports) (<= t (car teleports)))
      (complete-current-climb #t)  ; reached a teleport point, climbs end here
      (set! teleports (cdr teleports)))

    (if current-climb
        ;; Current climb in progress, keep it on until the filtered grade is 0
        (if (on-climb g)
            (set! last-climb-idx index)
            (complete-current-climb #f))
        ;; Start a climb, but only if the grade is positive
        (when (on-climb g)
          (set! last-climb-idx index)
          (set! current-climb (climb index index d d #f #f)))))

  (complete-current-climb #f)           ; don't forget the last one
  (merge-adjacent-climbs (reverse climbs) #:min-grade mg #:min-length ml #:teleports-idx teleports-idx))


;; Return intervals based on the climb (or descent sections in the data frame
;; DF.  See FIND-CLIMBS and MERGE-ADJACENT-CLIMBS for the meaning of the
;; DESCENTS?, MJD, MH and FW arguments.
(define (make-climb-intervals df
                              #:descents (descents? #f)
                              #:min-grade (mg 0.5)
                              #:min-length (ml 100))
  (if (df-contains? df "timestamp" "dst" "grade")
      (let ((climbs (find-climbs df #:min-grade mg #:min-length ml #:descents descents?)))
        (for/list ((c (in-list climbs)))
          (make-interval-summary df (climb-start-idx c) (climb-end-idx c))))
      '()))


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
(define (make-best-pace-intervals df)
  (if (df-contains? df "spd" "dst")
      (let ((bavg (df-mean-max df "spd" #:weight-series "dst" #:durations best-pace-distances)))
        (for/list ((item (in-list bavg)))
          (match-define (vector duration value position) item)
          (match-define (list start end) (df-index-of* df "dst" position (+ position duration)))
          (make-interval-summary df start end)))
      '()))


;;............................................ make-best-power-intervals ....

(define best-power-durations
  '(10 30 60 180 300 600 900 1200 1800 3600 5400 7200 10800))

;; Return intervals based on the best power maintained for a set of predefined
;; durations (e.g. the best 20 minute power)
(define (make-best-power-intervals df)
  (if (df-contains? df "pwr" "elapsed")
      (let ((bavg (df-mean-max df "pwr" #:weight-series "elapsed" #:durations best-power-durations)))
        (for/list ((item (in-list bavg)))
          (match-define (vector duration value position) item)
          (match-define (list start end) (df-index-of* df "elapsed" position (+ position duration)))
          (make-interval-summary df start end)))
      '()))
