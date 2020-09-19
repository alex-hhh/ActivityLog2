#lang racket/base

;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2018, 2019, 2020 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require al2-test-runner
         racket/math
         racket/match
         racket/date
         racket/format
         racket/class
         racket/runtime-path
         rackunit
         "../rkt/bavg-util.rkt"
         "../rkt/dbapp.rkt"
         "../rkt/metrics.rkt"
         "../rkt/models/critical-power.rkt"
         "../rkt/session-df/native-series.rkt"
         "../rkt/utilities.rkt")

;;(require rackunit/gui)
(set-dbglog-to-standard-output #t)     ; send dbglog calls to stdout, so we can see them!

(define (check-mmax sport sub-sport year axis)
  ;; At this time we only check that the code runs and generates something
  ;; without error...  It would be good to somehow verify that the result is
  ;; correct as well.
  (define ystart (date->seconds (date 0 0 0 1 1 year 0 0 #f 0)))
  (define yend (date->seconds (date 0 0 0 1 1 (add1 year) 0 0 #f 0)))
  (define candidates (fetch-candidate-sessions
                      (current-database) sport sub-sport ystart yend))
  (check > (length candidates) 0)
  (define ammax (get-aggregate-mmax candidates axis (lambda (msg) (void))))
  (check > (length ammax) 0)
  (when (send axis have-cp-estimate?)
    (define cp2params
      (hash
       'verify #t
       'model 'cp2
       'an-start 120
       'an-end 300
       'ae-start 720
       'ae-end 1200))
    (define mmax-fn (aggregate-mmax->spline-fn ammax))
    (define cp-estimate (send axis cp-estimate mmax-fn cp2params))
    (match-define (cp2 cp wprime) cp-estimate)
    (check > cp 0)
    (check > wprime 0)
    (printf "~a: [[ CP ~a; W' ~a ]]~%"
            (send axis series-name)
            (~r cp #:precision 2)
            (~r wprime #:precision 2)))
  (define aheat (get-aggregate-mmax-heat-map candidates ammax 0.90 axis))
  (check = (length aheat) (length ammax)))

(define (check-hist sport sub-sport year axis)
  ;; At this time we only check that the code runs and generates something
  ;; without error...  It would be good to somehow verify that the result is
  ;; correct as well.
  (define ystart (date->seconds (date 0 0 0 1 1 year 0 0 #f 0)))
  (define yend (date->seconds (date 0 0 0 1 1 (add1 year) 0 0 #f 0)))
  (define candidates (fetch-candidate-sessions
                      (current-database) sport sub-sport ystart yend))
  (check > (length candidates) 0)
  (define hist (aggregate-hist candidates
                               (send axis series-name)
                               #:progress-callback (lambda (msg) (void))))
  (check > (length hist) 0)
  (define total (for/sum ([item (in-list hist)])
                  (match-define (list key rank) item)
                  rank))
  (check > total 0))

;; This is downloaded by download-test-db.sh, this needs to be a runtime path
;; because we use a `place` to open the sqlite database.
(define-runtime-path test-database "./test-db/al2-v29.db")

(define test-axis-run
  (list
   axis-speed
   axis-pace
   ;; the GAP series cannot be tested as we are missing the grade series, so
   ;; it cannot be calculated.

   ;; axis-gap
   axis-speed-zone
   ;; Don't bother testing the grade series -- the data base has all altitude
   ;; data stripped out, so these will be 0

   ;; axis-grade
   ;; axis-grade-inverted
   axis-hr-bpm
   axis-hr-pct
   axis-hr-zone
   axis-cadence
   axis-stride
   axis-vratio
   axis-vertical-oscillation
   axis-stance-time
   axis-stance-time-percent))

(define test-axis-bike
  (list
   axis-speed
   axis-pace
   ;; Don't bother testing the grade series -- the data base has all altitude
   ;; data stripped out, so these will be 0

   ;; axis-grade
   ;; axis-grade-inverted
   axis-hr-bpm
   axis-hr-pct
   axis-hr-zone
   axis-cadence
   axis-stride
   axis-power
   axis-torque
   axis-power-zone
   axis-left-right-balance
   axis-left-torque-effectiveness
   axis-right-torque-effectiveness
   axis-left-pedal-smoothness
   axis-right-pedal-smoothness
   axis-left-platform-centre-offset
   axis-right-platform-centre-offset
   axis-left-power-phase-start
   axis-left-power-phase-end
   axis-left-power-phase-angle
   axis-right-power-phase-start
   axis-right-power-phase-end
   axis-right-power-phase-angle
   axis-left-peak-power-phase-start
   axis-left-peak-power-phase-end
   axis-left-peak-power-phase-angle
   axis-right-peak-power-phase-start
   axis-right-peak-power-phase-end
   axis-right-peak-power-phase-angle))

(define test-axis-swim
  (list axis-swim-pace
        axis-swim-swolf
        axis-swim-stroke-count
        axis-swim-stroke-length
        axis-swim-avg-cadence))

(define year 2017)

(define aggregate-test-suite
  (test-suite
   "Aggregate Functions"
   (let ([db (if (file-exists? test-database)
                 (open-activity-log test-database)
                 (begin0 #f
                   (printf "Could not find ~a, skipping aggregate tests~%" test-database)))])
     (when db
       (set-current-database db))
     ;; NOTE: we structure the tests to all check for the presence of the
     ;; database, so they are skipped, rather than omitted when the database
     ;; is not present.
     (for ((axis (in-list test-axis-run)))
       (test-case
           (format "Aggregate MMAX sport = 1/#f, series = ~a"
                   (send axis series-name))
         (if db (check-mmax 1 #f year axis) (skip-test)))
       (test-case
           (format "Aggregate HIST sport = 1/#f, series = ~a"
                   (send axis series-name))
         (if db (check-hist 1 #f year axis) (skip-test))))
     (for ((axis (in-list test-axis-bike)))
       (test-case
           (format "Aggregate MMAX sport = 2/#f, series = ~a"
                   (send axis series-name))
         (if db (check-mmax 2 #f year axis) (skip-test)))
       (test-case
           (format "Aggregate HIST sport = 2/#f, series = ~a"
                   (send axis series-name))
         (if db (check-hist 2 #f year axis) (skip-test))))
     (for ((axis (in-list test-axis-swim)))
       (test-case
           (format "Aggregate MMAX sport = 5/17, series = ~a"
                   (send axis series-name))
         (if db (check-mmax 5 17 year axis) (skip-test)))
       (test-case
           (format "Aggregate HIST sport = 5/17, series = ~a"
                   (send axis series-name))
         (if db (check-hist 5 17 year axis) (skip-test)))))))

(module+ test
  (run-tests #:package "aggregate-test"
             #:results-file "test-results/aggregate-test.xml"
             aggregate-test-suite))
