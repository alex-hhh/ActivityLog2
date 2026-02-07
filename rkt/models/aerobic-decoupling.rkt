#lang racket/base

;; aerobic-decoupling.rkt -- Pw:HR and Pa:HR metrics and other related metrics
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2021, 2023, 2024, 2026 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require data-frame
         math/statistics
         racket/class
         racket/contract
         racket/match
         racket/math
         "../session-df/series-metadata.rkt"
         "critical-power.rkt"
         "sport-zone.rkt")

;; Calculate the decoupling between the ratio of two series S1 and S2 in the
;; data frame between the START and STOP indexes.  The decoupling is a
;; percentage indicating how much the ratio of the two values changes between
;; the first and second halves of the segment between START and STOP.
(define (decoupling df s1 s2 #:start (start 0) #:stop (stop (df-row-count df)))
  (let ((half-point (exact-truncate (/ (+ start stop) 2))))
    (let ((stat-s1-1 (df-statistics df s1 #:start start #:stop half-point))
          (stat-s1-2 (df-statistics df s1 #:start half-point #:stop stop))
          (stat-s2-1 (df-statistics df s2 #:start start #:stop half-point))
          (stat-s2-2 (df-statistics df s2 #:start half-point #:stop stop)))
      (and stat-s1-1 stat-s1-2 stat-s2-1 stat-s2-2
           (let ((r1 (/ (statistics-mean stat-s1-1)
                        (statistics-mean stat-s2-1)))
                 (r2 (/ (statistics-mean stat-s1-2)
                        (statistics-mean stat-s2-2))))
             (* 100.0 (/ (- r1 r2) r1)))))))

;; Calculate the decoupling between series S1 and S2 for every lap in the data
;; frame DF.  The lap start timestamps are taken from the 'laps property on
;; the data frame.  The resulting list will contain #f if the lap is too sort
;; to produce a meaningful decoupling value (currently 60 seconds).
(define (decoupling/laps df s1 s2)
  (let* ((laps (df-get-property df 'laps))
         (limit (vector-length laps)))
    (for/list ([idx (in-range 0 (vector-length laps))])
      (define start
        (df-index-of df "timestamp" (vector-ref laps idx)))
      (define stop
        (if (< idx (sub1 limit))
            (df-index-of df "timestamp" (vector-ref laps (+ idx 1)))
            (df-row-count df)))
      ;; don't compute decoupling for short intervals
      (if (and start stop (> (- stop start) 60))
          (decoupling df s1 s2 #:start start #:stop stop)
          #f))))

;; Calculate the aerobic decoupling in the data frame DF between the START and
;; STOP indexes.  For bike sessions, the power to heart rate ratio is used,
;; for running the speed to heart rate.  Returns #f for all other sports, or
;; if there are no data series (e.g. a bike activity has no power data
;; series).
(define (aerobic-decoupling df #:start (start 0) #:stop (stop (df-row-count df)))
  (define sport (df-get-property df 'sport))
  (unless sport
    (error "aerobic-decoupling: no sport property in data frame"))
  (cond ((and (equal? (vector-ref sport 0) 2) ; bike
              (df-contains? df "pwr" "hr"))
         (decoupling df "pwr" "hr" #:start start #:stop stop))
        ((and (equal? (vector-ref sport 0) 1) ; run
              (df-contains? df "spd" "hr"))
         (decoupling df "spd" "hr" #:start start #:stop stop))
        (#t #f)))

;; Same as `aerobic-decoupling` but return a value for each lap in the data
;; frame DF.
(define (aerobic-decoupling/laps df)
  (define sport (df-get-property df 'sport))
  (unless sport
    (error "aerobic-decoupling: no sport property in data frame"))
  (cond ((and (equal? (vector-ref sport 0) 2) ; bike
              (df-contains? df "pwr" "hr"))
         (decoupling/laps df "pwr" "hr"))
        ((and (equal? (vector-ref sport 0) 1) ; run
              (df-contains? df "spd" "hr"))
         (decoupling/laps df "spd" "hr"))
        (#t '())))


;; A moving average calculator -- seems to be a popular way to average a data
;; series.  This one takes into account that samples might not be spaced 1
;; second apart and can handle it correctly.
(define moving-average-calculator%
  (class object%
    (init-field
     ;; "width" in seconds of the filter, we average the last FILTER-WIDTH
     ;; seconds worth of samples.
     [filter-width 60]
     ;; max width of a sample (which does not need to be 1 second apart).  If
     ;; a sample arrives after more than MAX-STEP-WIDTH seconds, the filter is
     ;; reset.
     [max-step-width 10])
    (super-new)

    (define samples (make-vector filter-width 0.0))
    (define index 0)
    (define buffer-full? #f)
    (define last-sample #f)
    (define last-sample-timestap #f)

    (define/private (reset-calculator)
      (set! index 0)
      (set! buffer-full? #f)
      (set! last-sample #f)
      (set! last-sample-timestap #f))

    (define/private (put-sample s)
      (vector-set! samples index s)
      (let ([new-index (add1 index)])
        (if (>= new-index filter-width)
            (begin
              (set! buffer-full? #t)
              (set! index 0))
            (set! index new-index))))

    (define/public (add-sample! sample timestamp)
      (if last-sample
          (let ([dt (exact-floor (- timestamp last-sample-timestap))])
            (if (> dt max-step-width)
                (begin
                  (reset-calculator)
                  (put-sample sample))
                (let ([dy (/ (- sample last-sample) dt)])
                  (for ([i (in-range dt)])
                    (put-sample (+ last-sample (* (add1 i) dy)))))))
          (put-sample sample))
      (set! last-sample sample)
      (set! last-sample-timestap timestamp))

    (define/public (get-moving-average)
      (if buffer-full?
          (/ (for/sum ([i (in-vector samples)]) i) filter-width)
          (if (> index 0)
              (/ (for/sum ([i (in-vector samples 0 index)]) i) index)
              0)))

    ))

;; Add a "hr-reserve" data series to the data frame DF, if a "hr" data series
;; is already present and heart rate zones are defined for the corresponding
;; session.  HR Reserve is defined as the percentage of the heart rate between
;; the MIN-HR and MAX-HR, as defined by the sport zones.  Additionally, the
;; hr-reserve series is filtered using a moving average filter.
(define (maybe-add-hr-reserve! df szs)
  (when (df-contains? df "hr")
    (define sid (df-get-property df 'session-id))
    (when sid
      (define zones (send szs sport-zones-for-session sid 'heart-rate))
      (when zones
        (define-values (min-hr max-hr)
          (let ([b (sz-boundaries zones)])
            (values
             (vector-ref b 0)
             (vector-ref b (sub1 (vector-length b))))))
        (define hr-range (- max-hr min-hr))

        (define mavg (new moving-average-calculator%))

        (when (> hr-range 0)
          (df-add-lazy!
           df
           "hr-reserve"
           '("hr" "timestamp")
           (lambda (val)
             (match-define (list hr ts) val)
             (if (and hr ts)
                 (begin
                   (send mavg add-sample! hr ts)
                   (let ([hr60s (send mavg get-moving-average)])
                     (* 100 (/ (max 0 (- hr60s min-hr)) hr-range))))
                 #f))))))))

;; Add a "pwr-reserve" data series to the data-frame DF, if a "pwr" data
;; series is present in the data frame and critical power is defined for the
;; session.  Power reserve is defined as the ratio between the moving-average
;; power and the critical power (that is percent of the critical power).  Note
;; that this can go over 100%, since critical power is not the maximum power.
(define (maybe-add-pwr-reserve! df)
  (when (df-contains? df "pwr")
    (define cp (df-get-property df 'critical-power))
    (when cp
      (define critical-power
        (cond ((cp2? cp) (cp2-cp cp))
              ((cp3? cp) (cp3-cp cp))
              (else (error "unknown critical-power struct type"))))

      (define mavg (new moving-average-calculator%))

      (df-add-lazy!
       df
       "pwr-reserve"
       '("pwr" "timestamp")
       (lambda (val)
         (match-define (list pwr ts) val)
         (if (and pwr ts)
             (begin
               (send mavg add-sample! pwr ts)
               (let ([p (send mavg get-moving-average)])
                 (* 100 (/ p critical-power))))
             #f))))))

;; Add a "spd-reserve" data series to the data-frame DF, if a "spd" or "gaspd"
;; (grade adjusted speed) data series are present in the data frame and
;; critical power (critical speed, in this case) is defined for the session.
;; Speed reserve is defined as the ratio between the moving-average speed
;; (grade adjusted, if possible) and the critical speed (that is percent of
;; the critical speed).  Note that this can go over 100%, since critical speed
;; is not the maximum speed.
(define (maybe-add-spd-reserve! df)
  (define speed-series
    (cond ((df-contains? df "gaspd") "gaspd")
          ((df-contains? df "spd") "spd")
          (#t #f)))
  (when speed-series
    (define cp (df-get-property df 'critical-power))
    (when cp
      (define critical-power
        (cond ((cp2? cp) (cp2-cp cp))
              ((cp3? cp) (cp3-cp cp))
              (else (error "unknown critical-power struct type"))))

      (define mavg (new moving-average-calculator%))

      (df-add-lazy!
       df
       "spd-reserve"
       (list speed-series "timestamp")
       (lambda (val)
         (match-define (list pwr ts) val)
         (if (and pwr ts)
             (begin
               (send mavg add-sample! pwr ts)
               (let ([p (send mavg get-moving-average)])
                 (* 100 (/ p critical-power))))
             #f))))))

;; Add an "adecl", "hr-reserve", "pwr-reserve" and "spd-reserve" series (if
;; possible).  Aerobic decoupling, "adecl", is defined as the ratio between
;; hr-reserve and power or speed reserve.
(define (maybe-add-adecl! df szs)
  (unless (df-contains? df "hr-reserve")
    (maybe-add-hr-reserve! df szs))
  (unless (df-contains? df "pwr-reserve")
    (maybe-add-pwr-reserve! df))
  (unless (df-contains? df "spd-reserve")
    (maybe-add-spd-reserve! df))
  (cond
    ((df-contains? df "hr-reserve" "pwr-reserve")
     (df-add-lazy!
      df
      "adecl"
      '("hr-reserve" "pwr-reserve")
      (lambda (val)
        (match-define (list hr pwr) val)
        (and hr pwr (> hr 0) (* 100 (/ pwr hr))))))
    ((df-contains? df "hr-reserve" "spd-reserve")
     (df-add-lazy!
      df
      "adecl"
      '("hr-reserve" "spd-reserve")
      (lambda (val)
        (match-define (list hr spd) val)
        (and hr spd (> hr 0) (* 100 (/ spd hr))))))))


;;....................................................... axis definitions ....

;; Provide series meta-data definitions (used for plots) for the hr-reserve,
;; pwr-reserve, spd-reserve and adecl data frame series.

(define axis-hr-reserve
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label) "Heart Rate Reserve (%)")
         (define/override (should-filter?) #f)
         (define/override (series-name) "hr-reserve")
         (define/override (name) "Heart Rate Reserve")
         (define/override (fractional-digits) 1)
         (define/override (missing-value) #f)
         ;; We use 150% even though HR reserve can go up to 100 only.  This
         ;; graph will be lined up with the power and speed reserves and those
         ;; can go above 100% since they are relative to threshold power and
         ;; speed, rather than maximum ones.  This is a bit of a hack...
         (define/override (y-range) (cons 0 150))
         )))

(define axis-pwr-reserve
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label) "Power Reserve (%)")
         (define/override (should-filter?) #f)
         (define/override (series-name) "pwr-reserve")
         (define/override (name) "Power Reserve")
         (define/override (fractional-digits) 1)
         (define/override (missing-value) #f)
         (define/override (y-range) (cons 0 150))
         )))

(define axis-spd-reserve
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label) "Speed/Pace Reserve (%)")
         (define/override (should-filter?) #f)
         (define/override (series-name) "spd-reserve")
         (define/override (name) "Speed/Pace Reserve")
         (define/override (fractional-digits) 1)
         (define/override (missing-value) #f)
         (define/override (y-range) (cons 0 150))
         )))

(define axis-adecl
  (new (class series-metadata% (init) (super-new)
         (define/override (axis-label) "Aerobic Decoupling (%)")
         (define/override (should-filter?) #f)
         (define/override (series-name) "adecl")
         (define/override (name) "Percent Power/HR")
         (define/override (fractional-digits) 1)
         (define/override (missing-value) #f)
         (define/override (y-range) (cons 0 150))
         )))

;; Register series metadata

(begin
  (register-series-metadata axis-hr-reserve)
  (register-series-metadata axis-pwr-reserve)
  (register-series-metadata axis-spd-reserve)
  (register-series-metadata axis-adecl))


;;............................................................. provides ....

(provide/contract
 (aerobic-decoupling (->* (data-frame?)
                          (#:start exact-integer?
                           #:stop exact-nonnegative-integer?)
                          (or/c real? #f)))
 (aerobic-decoupling/laps (-> data-frame? (listof (or/c real? #f))))
 (maybe-add-adecl! (-> data-frame? (is-a?/c sport-zones%) any/c)))

(provide axis-hr-reserve
         axis-pwr-reserve
         axis-spd-reserve
         axis-adecl)
