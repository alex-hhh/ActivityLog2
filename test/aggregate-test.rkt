#lang racket/base

;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2018 Alex Harsanyi <AlexHarsanyi@gmail.com>
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

(require racket/math
         racket/match
         racket/date
         racket/format
         racket/class
         rackunit
         "../rkt/bavg-util.rkt"
         "../rkt/dbapp.rkt"
         "../rkt/metrics.rkt"
         "../rkt/pdmodel.rkt"
         "../rkt/series-meta.rkt")

;;(require rackunit/gui)

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
    (define cp2params (cp2search 120 300 720 1200))
    (define mmax-fn (aggregate-mmax->spline-fn ammax))
    (define cp-estimate (send axis cp-estimate mmax-fn cp2params))
    (match-define (cp2 cp wprime fn t1 t2 cost) cp-estimate)
    (check > cp 0)
    (check > wprime 0)
    (printf " [[ CP ~a; W' ~a; t1 ~a; t2 ~a; cost ~a ]] "
            (~r cp #:precision 2)
            (~r wprime #:precision 2)
            (exact-round t1)
            (exact-round t2)
            (~r cost #:precision 2)))
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

;; This is downloaded by download-test-db.sh
(define test-database "./test-db/al2-v29.db")

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

(module+ test
  (if (file-exists? test-database)
      (let ((db (open-activity-log test-database)))
        (when db
          (set-current-database db)
          (for ((axis (in-list test-axis-run)))
            (check-not-exn
             (lambda ()
               (printf "Aggregate MMAX sport = 1/#f, series = ~a..."
                       (send axis series-name))
               (flush-output)
               (check-mmax 1 #f year axis)
               (printf "done.~%")
               (flush-output)
               (printf "Aggregate HIST sport = 1/#f, series = ~a..."
                       (send axis series-name))
               (flush-output)
               (check-hist 1 #f year axis)
               (printf "done.~%")
               (flush-output))))
          (for ((axis (in-list test-axis-bike)))
            (check-not-exn
             (lambda ()
               (printf "Aggregate MMAX sport = 2/#f, series = ~a..."
                       (send axis series-name))
               (flush-output)
               (check-mmax 2 #f year axis)
               (printf "done.~%")
               (flush-output)
               (printf "Aggregate HIST sport = 2/#f, series = ~a..."
                       (send axis series-name))
               (flush-output)
               (check-hist 2 #f year axis)
               (printf "done.~%")
               (flush-output))))
          (for ((axis (in-list test-axis-swim)))
            (check-not-exn
             (lambda ()
               (printf "Aggregate MMAX sport = 5/17, series = ~a..."
                       (send axis series-name))
               (flush-output)
               (check-mmax 5 17 year axis)
               (printf "done.~%")
               (flush-output)
               (printf "Aggregate HIST sport = 5/17, series = ~a..."
                       (send axis series-name))
               (flush-output)
               (check-hist 5 17 year axis)
               (printf "done.~%")
               (flush-output))))))
      (printf "Could not find ~a, skipping aggregate tests~%" test-database)))
