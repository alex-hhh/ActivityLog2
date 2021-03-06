#lang racket/base

;; fthr.rkt -- Functional Threshold Analysis
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2020, 2021 Alex Harsányi <AlexHarsanyi@gmail.com>
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
;;
;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary

;; This file contains utilities for analyzing sessions to determine a
;; threshold value (power, pace or heart rate) and setup sport zones based on
;; this data.  The best known example is determining the FTP: take the best 20
;; minute power of a test session and "threshold" or FTP is 95% of that value.
;; Power zones can be set as a percentage of this "threshold" value.  Similar
;; idea works for pace or heart rate.
;;
;; The `fthr-defs.json` file defines the data input for each sport + series
;; (HR, Pace or power).  It defines the duration for which a best value
;; searched, what percentage of this duration represents the threshold
;; (e.g. FTP is 95% of the best 20 minute power), and zone definitions, as
;; percentages of the threshold.
;;
;; This file contains functions for querying the data from `fthr-defs.json` as
;; well functions determining the best segments form a data frame plus
;; generating sport zones.

(require racket/runtime-path
         racket/contract
         json
         racket/class
         racket/draw
         data-frame
         racket/match
         math/statistics
         plot/utils
         racket/format
         racket/math
         pict
         "../fmt-util.rkt"
         "../utilities.rkt"
         "../models/sport-zone.rkt")


;;..................................................... Data Definitions ....

;; Threshold durations for sport and data series (HR, Power or Pace) as well
;; as zone definition percentages are stored in an external file.  There is a
;; global one defined below and the user can also place their own data
;; definitions in their user data directory.  Use `fthr-definitions` to obtain
;; the definitions, instead of reading the file directly.

(define-runtime-path fthr-defs-file "../../sql/fthr-defs.json")

;; JSON object of the loaded FTHR definitions.  Do not use directly, call
;; `fthr-definitions` instead.
(define the-fthr-definitions #f)

;; Return the valid FTHR definitions, and load them first if needed.
(define (fthr-definitions)
  (unless the-fthr-definitions
    (define user-file (build-path (data-directory) "fthr-defs.json"))
    (define user (if (file-exists? user-file)
                     (with-handlers
                       (((lambda (e) #t)
                         (lambda (e)
                           (dbglog "failed to read ~a, will use global FTHR definitions" user-file)
                           (dbglog-exception user-file e)
                           '())))
                       (call-with-input-file user-file
                         (lambda (in) (read-json in #:null #f))))
                     '()))
    (set! the-fthr-definitions
          (if (and (list? user) (not (null? user)))
              ;; TODO: need better validation that the file is well formed
              user
              (call-with-input-file fthr-defs-file
                (lambda (in) (read-json in #:null #f))))))
  the-fthr-definitions)

;; Return the threshold duration for a sport + metric combination. Returns #f
;; if the duration cannot be determined (e.g. because the sport or series do
;; not exist in the FTHR definitions file)
(define (fthr-duration sport metric)
  (for/first ([entry (in-list (fthr-definitions))]
              #:when (and (equal? sport (hash-ref entry 'sport #f))
                          (equal? metric (hash-ref entry 'metric #f))))
    (hash-ref entry 'duration #f)))

;; Return the threshold percentage for a sport + metric combination. Returns
;; #f if the threshold cannot be determined (e.g. because the sport or series
;; do not exist in the FTHR definitions file).  This value can be multiplied
;; with the best value to obtain the actual threshold.
(define (fthr-threshold sport metric)
  (for/first ([entry (in-list (fthr-definitions))]
              #:when (and (equal? sport (hash-ref entry 'sport #f))
                          (equal? metric (hash-ref entry 'metric #f))))
    (hash-ref entry 'threshold #f)))

;; Return the zone definitions for a sport + metric combination.  Returns #f
;; if the zone definitions cannot be determined.  The zone definitions are an
;; ordered list, of 3-element items. Each element has the name of the zone,
;; the calculation type (absolute or a percentage of the threshold value) and
;; the value.
(define (fthr-zone-definitions sport metric)
  (with-handlers
    ((symbol? (lambda (e) #f)))
    (for/first ([entry (in-list (fthr-definitions))]
                #:when (and (equal? sport (hash-ref entry 'sport #f))
                            (equal? metric (hash-ref entry 'metric #f))))
      (let ([zdef (hash-ref entry 'zone_definition #f)])
        (and zdef (list? zdef)
             (for/list ([item (in-list zdef)])
               (let ([name (hash-ref item 'name #f)]
                     [absolute (hash-ref item 'absolute #f)]
                     [percent (hash-ref item 'percent #f)])
                 (if (and (string? name) (or (number? percent) (number? absolute)))
                     (list name
                           (if absolute 'absolute 'percent)
                           (or absolute percent))
                     (raise 'bad-data #f)))))))))


;;........................................ Determining the Best Segments ....

;; Determine the best segment of the data SERIES for the specified DURATION.
;; Returns a hash table with data about the best segment (see below).  If BASE
;; is specified, it should be an immutable hash table to which the data is
;; added.  BASE can contain other keys relevant to the segment being computed.
;;
;; Will report an error if SERIES and the "elapsed" series is missing from the
;; data frame and will return #f if the data frame is shorter than DURATION.
;; NOTE that, if SERIES is "pace", the series "spd" must also exist.  This is
;; a hack, see #17,
;;
;; The following keys will be added / be present in the returned hash table:
;;
;; 'series -- the name of the series used to compute the best value, same as
;; the SERIES option
;;
;; 'duration -- the duration, in seconds, of the best segment, same as the
;; DURATION option.
;;
;; 'best -- the best average value for SERIES for the specified DURATION
;;
;; 'first-half-avg -- the average of the first half of the segment for SERIES
;;
;; 'second-half-avg -- the average of the second half of the segment for
;; SERIES
;;
;; 'start-position, 'end-position -- the start and end elapsed times, values
;; in the "elapsed" series, where the best segment is located.
;;
;; 'start-index, 'end-index -- the start and end index in the data frame for
;; where the best segment is located (0 is the first element in the data
;; series).  These are useful for `df-ref` and `df-ref*` calls.
;;
;; 'min-value, 'max-value are the minimum and maximum values for the data
;; series inside the segment.
;;
(define (best-segment df series duration #:extend (base (hash)))
  ;; Hack: if we are asked for the "pace" series, calculate the best avg for
  ;; "spd" and convert it.  This gives a better "best average" value.  See #17
  (define actual-series
    (if (equal? series "pace") "spd" series))

  (unless (df-contains? df actual-series "elapsed")
    (error "best-segment: missing required data series"))

  ;; DF-MEAN-MAX will return an empty list if the data frame is shorter than
  ;; DURATION.  We return #f in that case.
  (let ([mm (df-mean-max df actual-series #:durations (list duration))])
    (if (null? mm)
        #f
        (match-let ([(list (vector _ best position)) mm])
          (match-define (list start stop)
            (df-index-of* df "elapsed" position (+ position duration)))
          (define stats (df-statistics df series #:weight-series "elapsed" #:start start #:stop stop))
          (define-values
            (first-half-avg second-half-avg)
            (let ([mid (df-index-of df "elapsed" (+ position (/ duration 2)))])
              (define first-half (df-statistics df actual-series #:start start #:stop mid))
              (define second-half (df-statistics df actual-series #:start mid #:stop stop))
              (values (statistics-mean first-half) (statistics-mean second-half))))

          (define threshold-percent (hash-ref base 'threshold-percent 1.0))

          (hash-set*
           base
           'series series
           'duration duration
           'best (if (equal? series "pace") (convert-m/s->pace best) best)
           'threshold (let ([threshold (* best threshold-percent)])
                        (if (equal? series "pace")
                            (convert-m/s->pace threshold)
                            threshold))
           'first-half-average (if (equal? series "pace")
                                   (convert-m/s->pace first-half-avg)
                                   first-half-avg)
           'second-half-average (if (equal? series "pace")
                                    (convert-m/s->pace second-half-avg)
                                    second-half-avg)
           'start-index start
           'end-index stop
           'start-position position
           'end-position (+ position duration)
           'min-value (statistics-min stats)
           'max-value (statistics-max stats))))))

;; Determine the best pace segment for the data frame DF, as returned by
;; `best-segment` and augmented with other usefull properties for plotting and
;; data formatting.
(define (best-pace-segment df)

  (match-define (vector sport sub-sport)
    (df-get-property df 'sport
                     (lambda () (error "best-pace-segment: no sport in dataframe"))))


  (define sport-name
    (case sport
      ((1) "running")
      (else (error "best-pace-segment: sport must be running"))))

  (define duration (fthr-duration sport-name "speed"))

  (define base
    (hash
     'color (make-object color% 25 101 176)
     'name "Pace"
     'title "Best Pace"
     'unit (if (eq? 'metric (al-pref-measurement-system))
               "min/km"
               "min/mile")
     'format-value duration->string
     'plot-ticks (time-ticks #:formats '("~M:~f"))
     'plot-label (if (eq? 'metric (al-pref-measurement-system))
                     "Pace (min/km)"
                     "Pace (min/mile)")
     'zone-metric 'pace
     'threshold-percent (fthr-threshold sport-name "speed")
     'zone-definitions (fthr-zone-definitions sport-name "speed")
     'sport sport
     'sub-sport sub-sport))

  (and (df-contains? df "spd" "pace")
       (best-segment df "pace" duration #:extend base)))

;; Determine the best heart rate segment for the data frame DF, as returned by
;; `best-segment` and augmented with other usefull properties for plotting and
;; data formatting.
(define (best-heart-rate-segment df)

  (match-define (vector sport sub-sport)
    (df-get-property df 'sport
                     (lambda () (error "best-heart-rate-segment: no sport in dataframe"))))

  (define sport-name
    (case sport
      ((1) "running")
      ((2) "cycling")
      (else (error "best-heart-rate-segment: sport must be running or cycling"))))

  (define duration (fthr-duration sport-name "heart rate"))

  (define base
    (hash
     'color (make-object color% 220 5 12 1.0)
     'name "Heart Rate"
     'title "Highest Heart Rate"
     'unit "bpm"
     'format-value (lambda (v) (~a (exact-round v)))
     'plot-ticks (linear-ticks)
     'plot-label "Heart Rate (bpm)"
     'zone-metric 'heart-rate
     'threshold-percent (fthr-threshold sport-name "heart rate")
     'zone-definitions (fthr-zone-definitions sport-name "heart rate")
     'sport sport
     'sub-sport sub-sport))

    (and (df-contains? df "hr")
         (best-segment df "hr" duration #:extend base)))

;; Determine the best heart rate segment for the data frame DF, as returned by
;; `best-segment` and augmented with other usefull properties for plotting and
;; data formatting.
(define (best-power-segment df)

  (match-define (vector sport sub-sport)
    (df-get-property df 'sport
                     (lambda () (error "best-power-segment: no sport in dataframe"))))

  (define sport-name
    (case sport
      ((2) "cycling")
      (else (error "best-power-segment: sport must be cycling"))))

  (define duration (fthr-duration sport-name "power"))

  (define base
    (hash
     'color (make-object color% 136 46 114)
     'name "Power"
     'title "Best Power"
     'unit "watts"
     'format-value (lambda (v) (~a (exact-round v)))
     'plot-ticks (linear-ticks)
     'plot-label "Power (watts)"
     'zone-metric 'power
     'threshold-percent (fthr-threshold sport-name "power")
     'zone-definitions (fthr-zone-definitions sport-name "power")
     'sport sport
     'sub-sport sub-sport))

  (and (df-contains? df "pwr")
       (best-segment df "pwr" duration #:extend base)))

;; Construct sport zones from a best segment (as obtained by `best-segment')
;; and a zone definition, as obtained by `fthr-zone-definitions`).  Returns a
;; list if elements, each element is a list of the zone name and the start
;; value for that zone (the end value ends where the next zone starts)
(define (make-sport-zones segment [valid-from (current-seconds)])
  (define series (hash-ref segment 'series))
  ;; HACK: if the series is "pace", we really compute the zones for speed
  ;; (meters/second)
  (define threshold
    (let ([threshold (hash-ref segment 'threshold)])
      (if (equal? series "pace")
          (convert-pace->m/s threshold)
          threshold)))
  (sport-zones-from-threshold
   (hash-ref segment 'sport)
   #f ;; NOTE: we don't support creating sport zones for sub-sports
   (hash-ref segment 'zone-metric)
   threshold
   (hash-ref segment 'zone-definitions)
   #:valid-from valid-from))


;;...................................................... pretty-printing ....

(define (make-table-pict items)

  (define b-item-color (make-object color% #x2f #x4f #x4f))
  (define b-label-color (make-object color% #x77 #x88 #x99))

  (define b-item-font (send the-font-list find-or-create-font 14 'default 'normal 'normal))
  (define b-label-font (send the-font-list find-or-create-font 12 'default 'italic 'light))
  (define b-mini-label-font (send the-font-list find-or-create-font 8 'default 'italic 'light))

  (define b-item-face (cons b-item-color b-item-font))
  (define b-label-face (cons b-label-color b-label-font))
  (define b-mini-label-face (cons b-label-color b-mini-label-font))

  (define column-count
    (for/fold ([cc 0]) ([item (in-list items)]) (max cc (length item))))
  (define picts '())
  (for ([item (in-list (reverse items))])
    (let* ([key (car item)]
           [vals (reverse (cdr item))]
           [face (if key b-item-face b-mini-label-face)])
      (for ([dummy (in-range (- column-count (add1 (length vals))))])
        (set! picts (cons (text "" face) picts)))
      (for ([val (in-list vals)])
        (set! picts (cons (text val face) picts)))
      (set! picts (cons (text (or key "") b-label-face) picts))))
  (table column-count picts lc-superimpose cc-superimpose 15 3))

(define (pp-segment segment)
  (define format-value (hash-ref segment 'format-value))
  (define best (hash-ref segment 'best))
  (define 1st-half (hash-ref segment 'first-half-average))
  (define 2nd-half (hash-ref segment 'second-half-average))
  (define unit (hash-ref segment 'unit))
  (define duration (hash-ref segment 'duration))
  (define title (hash-ref segment 'title))
  (define start (hash-ref segment 'start-position))
  (define threshold (hash-ref segment 'threshold))
  (define threshold-percent (hash-ref segment 'threshold-percent))
  (printf "*** ~a~%~%" title)
  (printf "Threshold: ~a ~a;                 ~a (of best)~%"
          (if threshold (format-value threshold) "N/A")
          unit
          (if threshold-percent (format "~a %" (exact-round (* threshold-percent 100))) ""))
  (printf "Best:      ~a ~a;                 @~a~%"
          (format-value best)
          unit
          (duration->string start))
  (printf "Duration:  ~a minutes~%" (~r (/ duration 60) #:precision 1))
  (printf "Split:     ~a/~a (1st/2nd half);  ~a% change~%"
          (format-value 1st-half)
          (format-value 2nd-half)
          (~r (* (- (/ 2nd-half 1st-half) 1.0) 100.0) #:precision 1)))

(define (pp-segment/pict segment)

  (define b-title-color (make-object color% #x2f #x4f #x4f))
  (define b-title-font (send the-font-list find-or-create-font 16 'default 'normal 'normal))
  (define b-title-face (cons b-title-color b-title-font))

  (define format-value (hash-ref segment 'format-value))
  (define best (hash-ref segment 'best))
  (define 1st-half (hash-ref segment 'first-half-average))
  (define 2nd-half (hash-ref segment 'second-half-average))
  (define unit (hash-ref segment 'unit))
  (define duration (hash-ref segment 'duration))
  (define title (hash-ref segment 'title))
  (define start (hash-ref segment 'start-position))
  (define threshold (hash-ref segment 'threshold))
  (define threshold-percent (hash-ref segment 'threshold-percent))
  (define pict0
    (make-table-pict
     (list (list "Threshold" (if threshold (format-value threshold) "N/A")
                 (if threshold-percent (format "~a %" (exact-round (* threshold-percent 100))) ""))
           (list #f unit (if threshold-percent "... of best" ""))
           (list "Best" (format-value best) (format "@~a" (duration->string start)))
           (list #f unit "timer")
           (list "Duration" (format "~a minutes" (~r (/ duration 60) #:precision 1)))
           (list "Split"
                 (format "~a / ~a" (format-value 1st-half) (format-value 2nd-half))
                 (format "~a%" (~r (* (- (/ 2nd-half 1st-half) 1.0) 100.0) #:precision 1)))
           (list #f "1st/2nd half" "change"))))
  (let ((title (text title b-title-face)))
    (vc-append 15 title pict0)))


;;............................................................. provides ....

(define sport/c (or/c "running" "cycling"))
(define metric/c (or/c "speed" "heart rate" "power"))
(define segment/c hash?)

(provide/contract

 (fthr-duration (-> sport/c metric/c (or/c #f (and/c real? positive?))))
 (fthr-zone-definitions (-> sport/c metric/c
                            (or/c #f (listof (list/c string? (or/c 'percent 'absolute) real?)))))
 (best-segment (->* (data-frame? string? (and/c real? positive?))
                    (#:extend hash?)
                    (or/c segment/c #f)))

 (best-pace-segment (-> data-frame? (or/c #f segment/c)))
 (best-heart-rate-segment (-> data-frame? (or/c #f segment/c)))
 (best-power-segment (-> data-frame? (or/c #f segment/c)))

 (pp-segment (-> segment/c any/c))
 (pp-segment/pict (-> segment/c pict?)))

(provide make-sport-zones)
