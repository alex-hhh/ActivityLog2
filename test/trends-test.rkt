#lang racket

;; trends-test.rkt -- test the trend charts
;; 
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2018 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require racket/gui/base
         rackunit
         "../rkt/plot-util.rkt"
         "../rkt/utilities.rkt"
         "../rkt/data-frame/csv.rkt"
         "../rkt/data-frame/df.rkt"
         "../rkt/trend-charts/trends-ae.rkt"
         "../rkt/trend-charts/trends-bw.rkt"
         "../rkt/trend-charts/trends-pmc.rkt"
         "../rkt/trend-charts/trends-tiz.rkt"
         "../rkt/trend-charts/trends-trivol.rkt"
         "../rkt/trend-charts/trends-tt.rkt"
         "../rkt/trend-charts/trends-vol.rkt"
         "../rkt/trend-charts/trends-bavg.rkt"
         "../rkt/trend-charts/trends-hist.rkt"
         "../rkt/trend-charts/trends-scatter.rkt"
         "test-util.rkt")

;; This file tests the trend charts, at least that the code can run without
;; any exceptions and it produces some output.  A few notes:
;;
;; For each trend chart, two things are tested:
;;
;; 1) running the trend chart on an empty database (I had too many errors,
;; since I never test this case during developent, as my database is full of
;; activities)
;;
;; 2) running the trend chart on a complete database.
;;
;; a single "settings" hash is used for each chart, but new ones can be added
;; if bugs are found and this code would exercise them.  Also, if charts need
;; to be updated in the future, this file could provide a basis for testing
;; the upgrade code.

(set-dbglog-to-standard-output #t)     ; send dbglog calls to stdout, so we can see them!
(set-worker-thread-count 1)            ; use 1 worker thread, so we can
                                       ; determine when tasks finish (See
                                       ; `do-tc-check`)


(define ae-settings
  (hash
   'name  "Aerobic Efficiency"
   'title  "Aerobic Efficiency"
   'date-range '(all-days "Tri 2012 / HIM" empty empty)
   ;; NOTE: timestamps is dynamically computed anyway...
   'timestamps '(0 . 1535990400)
   'sport  '(1 . #f)
   'equipment  '()
   'labels  '()
   'trendline 'poly-3))

(define bw-settings
  (hash
   'name "BodyWeight"
   'title "Body Weight"
   'date-range '(all-days "Tri 2012 / HIM" empty empty)
   'timestamps '(0 . 1535990400)
   'trendline 'poly-3))

(define pmc-settings
  (hash
   'name "PMC"
   'title "Performance"
   'date-range '(custom-dates "Tri 2012 / HIM" 1483200000 1514736000)
   'timestamps '(1483200000 . 1514822400)
   'show-fatigue? #t
   'show-fitness? #t
   'show-form? #t
   'show-tss? #t))

(define tiz-settings
  (hash
   'name "TIZ"
   'title "Time in Zone"
   'date-range '(all-days #f empty empty)
   'timestamps '(0 . 1535990400)
   'group-by 0
   'sport 1
   'zone-metric 1))

(define tt-settings
  (hash
   'name "Training Times"
   'title "TrainingTimes"
   'date-range '(all-days #f empty empty)
   'timestamps '(0 . 1535990400)
   'sport '(#f . #f)
   'tri?  #f))

(define vol-settings
  (hash
   'name "Vol"
   'title "Training Volume"
   'date-range '(all-days #f empty empty)
   'timestamps '(0 . 1535990400)
   'group-by 0
   'metric 0
   'sport '(#f . #f)))

(define trivol-settings
  (hash
   'name "TriVol"
   'title "Multisport Training Volume"
   'date-range '(all-days #f empty empty)
   'timestamps '(0 . 1535990400)
   'group-by 0
   'metric 0))

(define bavg-settings
  (hash
   'name "BestAvg"
   'title "Best Avg"
   'date-range '(custom-dates "Tri 2012 / HIM" 1483200000 1514736000)
   'timestamps '(1483200000 . 1514822400)
   'sport '(#f . #f)
   'labels '()
   'equipment '()
   'series "cad"   
   'estimate-cp?  #f
   'ae-start 720
   'ae-end 1200
   'an-start 120
   'an-end 300
   'heat-percent 'empty
   'show-heat? #t
   'zero-base? #f))

(define hist-settings
  (hash
   'name "Histogram"
   'title "Histogram"
   'date-range '(custom-dates "Tri 2012 / HIM" 1483200000 1514736000)
   'timestamps '(1483200000 . 1514822400)
   'sport '(#f . #f)
   'labels '()
   'equipment '()
   'series "lrbal"
   'bucket-width 'empty
   'color-by-zone? #f
   'include-zeroes? #f
   'outlier-trim 'empty
   'show-as-pct? #f))

(define scatter-settings
  (hash
   'name "Scatter"
   'title "Scatter Plot"
   'date-range '(custom-dates "Tri 2012 / HIM" 1483200000 1514736000)
   'timestamps '(1483200000 . 1514822400)
   'sport '(#f . #f)
   'equipment '()
   'labels '()
   'series1 "lrbal"
   'series2 "lrbal"
   'ohandling 'mark
   'opct #f))

(define (make-test-snip-canvas snip-canvas-class)
  (define f (new frame% [label "test-frame"] [width 150] [height 150]))
  (define c (new snip-canvas-class [parent f]))
  ;; reflow the frame container, so the geometries are calculated without
  ;; showing the frame
  (send f reflow-container)
  c)

(define test-snip-canvas% 
  (class snip-canvas% (init parent) (super-new [parent parent])
    (define num-set-background-message-calls 0)
    (define num-snips-set 0)
    (define/override (set-snip s)
      (when s
        (check is-a? s snip%)
        (set! num-snips-set (add1 num-snips-set))))
    (define/override (set-floating-snip s) (raise "unexpected-call to set-floating-snip"))
    (define/override (export-image-to-file fn (w #f) (h #f)) (void))
    (define/override (set-background-message msg)
      (check-pred string? msg)
      ;; (printf "set-background-message: ~a~%" msg)(flush-output)
      (set! num-set-background-message-calls (add1 num-set-background-message-calls)))
    (define/public (get-num-set-background-message-calls)
      num-set-background-message-calls)
    (define/public (get-num-snips-set) num-snips-set)))

(define test-image-file (make-temporary-file "al2-~a.png"))
(define test-data-file (make-temporary-file "al2-~a.csv"))

;; This is downloaded by download-test-db.sh
(define test-database "./test-db/al2-v29.db")

(define (do-tc-check db chart-class settings snip-canvas-class)
  (when (file-exists? test-image-file)
    (delete-file test-image-file))
  (when (file-exists? test-data-file)
    (delete-file test-data-file))
  (define c (new chart-class [database db]))
  (send c put-chart-settings settings)
  (define canvas (make-test-snip-canvas snip-canvas-class))
  (check-not-exn
   (lambda () (send c put-plot-snip canvas)))
  ;; Queue a 'done event on the worker thread -- this will be executed when
  ;; the put-plot-snip thread has finished working (since we only use one
  ;; thread), so the plot was completed.
  (define ch (make-channel))
  (queue-task "do-tc-check" (lambda () (channel-put ch 'done)))
  ;; Wait up to 10 minutes to complete the plot snip, some trend charts do
  ;; take a long time to complete (e.g bavg charts that need to compute the
  ;; BAVG for lots of activities).
  (unless (sync/timeout 600 ch) (error "put-plot-snip never finished"))
  ;; The chart might try to put the snip using a `queue-callback`, so we yield
  ;; to the event handler thread to let it execute this callback.
  (yield (current-eventspace))
  (check-not-exn
   (lambda () (send c save-plot-image test-image-file 1000 1000)))
  (check-not-exn
   (lambda () (send c export-data-to-file test-data-file #f)))
  canvas)

;; Check that the CSV file is valid (contains some data)
(define (check-csv-file file)
  (define df (df-read/csv file))
  (check > (df-row-count df) 0))

(define trend-charts-test-suite
  (test-suite
   "Trend Charts Test Suite"
   (test-case "BW trends / empty database"
     (define start (current-milliseconds))
     (printf "BW trends / empty database...")(flush-output)
     (with-fresh-database
       (lambda (db)
         (define result
           (do-tc-check db bw-trends-chart% bw-settings test-snip-canvas%))
         (check = 1 (send result get-num-set-background-message-calls))
         (check = 0 (send result get-num-snips-set))
         ;; With no data, these files should not be created
         (check-false (file-exists? test-image-file))
         (check-false (file-exists? test-data-file))))
     (define elapsed (/ (- (current-milliseconds) start) 1000.0))
     (printf " done in ~a seconds ~%" (~r elapsed #:precision 2))(flush-output))
   (test-case "BW trends / non-empty database"
     (define start (current-milliseconds))
     (printf "BW trends / non-empty database...")(flush-output)
     (if (file-exists? test-database)
         (with-database
           test-database
           (lambda (db)
             (define result
               (do-tc-check db bw-trends-chart% bw-settings test-snip-canvas%))
             (check = 0 (send result get-num-set-background-message-calls))
             (check = 1 (send result get-num-snips-set))
             ;; With no data, these files should not be created
             (check-true (file-exists? test-image-file))
             (check-true (file-exists? test-data-file))
             (check-csv-file test-data-file)
             (define elapsed (/ (- (current-milliseconds) start) 1000.0))
             (printf " done in ~a seconds ~%" (~r elapsed #:precision 2))))
         (printf "skipping (missing database file).~%"))
     (flush-output))
   (test-case "AE trends / empty database"
     (define start (current-milliseconds))
     (printf "AE trends / empty database...")(flush-output)
     (with-fresh-database
       (lambda (db)
         (define result
           (do-tc-check db ae-trends-chart% bw-settings test-snip-canvas%))
         (check = 1 (send result get-num-set-background-message-calls))
         (check = 0 (send result get-num-snips-set))
         ;; With no data, these files should not be created
         (check-false (file-exists? test-image-file))
         (check-false (file-exists? test-data-file))))
     (define elapsed (/ (- (current-milliseconds) start) 1000.0))
     (printf " done in ~a seconds ~%" (~r elapsed #:precision 2))(flush-output))
   (test-case "AE trends / non-empty database"
     (define start (current-milliseconds))
     (printf "AE trends / non-empty database...")(flush-output)
     (if (file-exists? test-database)
         (with-database
           test-database
           (lambda (db)
             (define result
               (do-tc-check db ae-trends-chart% bw-settings test-snip-canvas%))
             (check = 0 (send result get-num-set-background-message-calls))
             (check = 1 (send result get-num-snips-set))
             ;; With no data, these files should not be created
             (check-true (file-exists? test-image-file))
             (check-true (file-exists? test-data-file))
             (check-csv-file test-data-file)
             (define elapsed (/ (- (current-milliseconds) start) 1000.0))
             (printf " done in ~a seconds ~%" (~r elapsed #:precision 2))))
         (printf "skipping (missing database file).~%"))
     (flush-output))
   (test-case "TIZ trends / empty database"
     (define start (current-milliseconds))
     (printf "TIZ trends / empty database...")(flush-output)
     (with-fresh-database
       (lambda (db)
         (define result
           (do-tc-check db tiz-trends-chart% tiz-settings test-snip-canvas%))
         (check = 1 (send result get-num-set-background-message-calls))
         (check = 0 (send result get-num-snips-set))
         ;; With no data, these files should not be created
         (check-false (file-exists? test-image-file))
         (check-false (file-exists? test-data-file))))
     (define elapsed (/ (- (current-milliseconds) start) 1000.0))
     (printf " done in ~a seconds ~%" (~r elapsed #:precision 2))(flush-output))
   (test-case "TIZ trends / non-empty database"
     (define start (current-milliseconds))
     (printf "TIZ trends / non-empty database...")(flush-output)
     (if (file-exists? test-database)
         (with-database
           test-database
           (lambda (db)
             (define result
               (do-tc-check db tiz-trends-chart% tiz-settings test-snip-canvas%))
             (check = 0 (send result get-num-set-background-message-calls))
             (check = 1 (send result get-num-snips-set))
             ;; With no data, these files should not be created
             (check-true (file-exists? test-image-file))
             (check-true (file-exists? test-data-file))
             (check-csv-file test-data-file)
             (define elapsed (/ (- (current-milliseconds) start) 1000.0))
             (printf " done in ~a seconds ~%" (~r elapsed #:precision 2))))
         (printf "skipping (missing database file).~%"))
     (flush-output))
   (test-case "TRIVOL trends / empty database"
     (define start (current-milliseconds))
     (printf "TRIVOL trends / empty database...")(flush-output)
     (with-fresh-database
       (lambda (db)
         (define result
           (do-tc-check db trivol-trends-chart% trivol-settings test-snip-canvas%))
         (check = 1 (send result get-num-set-background-message-calls))
         (check = 0 (send result get-num-snips-set))
         ;; With no data, these files should not be created
         (check-false (file-exists? test-image-file))
         (check-false (file-exists? test-data-file))))
     (define elapsed (/ (- (current-milliseconds) start) 1000.0))
     (printf " done in ~a seconds ~%" (~r elapsed #:precision 2))(flush-output))
   (test-case "TRIVOL trends / non-empty database"
     (define start (current-milliseconds))
     (printf "TRIVOL trends / non-empty database...")(flush-output)
     (if (file-exists? test-database)
         (with-database
           test-database
           (lambda (db)
             (define result
               (do-tc-check db trivol-trends-chart% trivol-settings test-snip-canvas%))
             (check = 0 (send result get-num-set-background-message-calls))
             (check = 1 (send result get-num-snips-set))
             ;; With no data, these files should not be created
             (check-true (file-exists? test-image-file))
             (check-true (file-exists? test-data-file))
             (check-csv-file test-data-file)
             (define elapsed (/ (- (current-milliseconds) start) 1000.0))
             (printf " done in ~a seconds ~%" (~r elapsed #:precision 2))))
         (printf "skipping (missing database file).~%"))
     (flush-output))
   (test-case "VOL trends / empty database"
     (define start (current-milliseconds))
     (printf "VOL trends / empty database...")(flush-output)
     (with-fresh-database
       (lambda (db)
         (define result
           (do-tc-check db vol-trends-chart% vol-settings test-snip-canvas%))
         (check = 1 (send result get-num-set-background-message-calls))
         (check = 0 (send result get-num-snips-set))
         ;; With no data, these files should not be created
         (check-false (file-exists? test-image-file))
         (check-false (file-exists? test-data-file))))
     (define elapsed (/ (- (current-milliseconds) start) 1000.0))
     (printf " done in ~a seconds ~%" (~r elapsed #:precision 2))(flush-output))
   (test-case "VOL trends / non-empty database"
     (define start (current-milliseconds))
     (printf "VOL trends / non-empty database...")(flush-output)
     (if (file-exists? test-database)
         (with-database
           test-database
           (lambda (db)
             (define result
               (do-tc-check db vol-trends-chart% vol-settings test-snip-canvas%))
             (check = 0 (send result get-num-set-background-message-calls))
             (check = 1 (send result get-num-snips-set))
             ;; With no data, these files should not be created
             (check-true (file-exists? test-image-file))
             (check-true (file-exists? test-data-file))
             (check-csv-file test-data-file)
             (define elapsed (/ (- (current-milliseconds) start) 1000.0))
             (printf " done in ~a seconds ~%" (~r elapsed #:precision 2))))
         (printf "skipping (missing database file).~%"))
     (flush-output))
   (test-case "TT trends / empty database"
     (define start (current-milliseconds))
     (printf "TT trends / empty database...")(flush-output)
     (with-fresh-database
       (lambda (db)
         (define result
           (do-tc-check db tt-trends-chart% tt-settings test-snip-canvas%))
         (check = 1 (send result get-num-set-background-message-calls))
         (check = 0 (send result get-num-snips-set))
         ;; With no data, these files should not be created
         (check-false (file-exists? test-image-file))
         (check-false (file-exists? test-data-file))))
     (define elapsed (/ (- (current-milliseconds) start) 1000.0))
     (printf " done in ~a seconds ~%" (~r elapsed #:precision 2))(flush-output))
   (test-case "TT trends / non-empty database"
     (define start (current-milliseconds))
     (printf "TT trends / non-empty database...")(flush-output)
     (if (file-exists? test-database)
         (with-database
           test-database
           (lambda (db)
             (define result
               (do-tc-check db tt-trends-chart% tt-settings test-snip-canvas%))
             (check = 0 (send result get-num-set-background-message-calls))
             (check = 1 (send result get-num-snips-set))
             ;; With no data, these files should not be created
             (check-true (file-exists? test-image-file))
             (check-true (file-exists? test-data-file))
             (check-csv-file test-data-file)
             (define elapsed (/ (- (current-milliseconds) start) 1000.0))
             (printf " done in ~a seconds ~%" (~r elapsed #:precision 2))))
         (printf "skipping (missing database file).~%"))
     (flush-output))
   (test-case "PMC trends / empty database"
     (define start (current-milliseconds))
     (printf "PMC trends / empty database...")(flush-output)
     (with-fresh-database
       (lambda (db)
         (define result
           (do-tc-check db pmc-trends-chart% pmc-settings test-snip-canvas%))
         (check = 0 (send result get-num-set-background-message-calls))
         (check = 1 (send result get-num-snips-set))
         ;; NOTE PMC data is still exported on an empty database
         (check-true (file-exists? test-image-file))
         (check-true (file-exists? test-data-file))))
     (define elapsed (/ (- (current-milliseconds) start) 1000.0))
     (printf " done in ~a seconds ~%" (~r elapsed #:precision 2))(flush-output))
   (test-case "PMC trends / non-empty database"
     (define start (current-milliseconds))
     (printf "PMC trends / non-empty database...")(flush-output)
     (if (file-exists? test-database)
         (with-database
           test-database
           (lambda (db)
             (define result
               (do-tc-check db pmc-trends-chart% pmc-settings test-snip-canvas%))
             (check = 0 (send result get-num-set-background-message-calls))
             (check = 1 (send result get-num-snips-set))
             ;; NOTE PMC data is still exported on an empty database
             (check-true (file-exists? test-image-file))
             (check-true (file-exists? test-data-file))
             (check-csv-file test-data-file)
             (define elapsed (/ (- (current-milliseconds) start) 1000.0))
             (printf " done in ~a seconds ~%" (~r elapsed #:precision 2))))
         (printf "skipping (missing database file).~%"))
     (flush-output))
   (test-case "BAVG trends / empty database"
     (define start (current-milliseconds))
     (printf "BAVG trends / empty database...")(flush-output)
     (with-fresh-database
       (lambda (db)
         (define result
           (do-tc-check db mmax-trends-chart% bavg-settings test-snip-canvas%))
         (check = 2 (send result get-num-set-background-message-calls))
         (check = 0 (send result get-num-snips-set))
         (check-false (file-exists? test-image-file))
         (check-true (file-exists? test-data-file))))
     (define elapsed (/ (- (current-milliseconds) start) 1000.0))
     (printf " done in ~a seconds ~%" (~r elapsed #:precision 2))(flush-output))
   (test-case "BAVG trends / non-empty database"
     (define start (current-milliseconds))
     (printf "BAVG trends / non-empty database...")(flush-output)
     (if (file-exists? test-database)
         (with-database
           test-database
           (lambda (db)
             (define result
               (do-tc-check db mmax-trends-chart% bavg-settings test-snip-canvas%))
             (check > (send result get-num-set-background-message-calls) 0)
             (check = 1 (send result get-num-snips-set))
             (check-true (file-exists? test-image-file))
             (check-true (file-exists? test-data-file))
             (check-csv-file test-data-file)
             (define elapsed (/ (- (current-milliseconds) start) 1000.0))
             (printf " done in ~a seconds ~%" (~r elapsed #:precision 2))))
         (printf "skipping (missing database file).~%"))
     (flush-output))
   (test-case "HIST trends / empty database"
     (define start (current-milliseconds))
     (printf "HIST trends / empty database...")(flush-output)
     (with-fresh-database
       (lambda (db)
         (define result
           (do-tc-check db hist-trends-chart% hist-settings test-snip-canvas%))
         (check = 2 (send result get-num-set-background-message-calls))
         (check = 0 (send result get-num-snips-set))
         ;; NOTE PMC data is still exported on an empty database
         (check-false (file-exists? test-image-file))
         (check-false (file-exists? test-data-file))))
     (define elapsed (/ (- (current-milliseconds) start) 1000.0))
     (printf " done in ~a seconds ~%" (~r elapsed #:precision 2))(flush-output))
   (test-case "HIST trends / non-empty database"
     (define start (current-milliseconds))
     (printf "HIST trends / non-empty database...")(flush-output)
     (if (file-exists? test-database)
         (with-database
           test-database
           (lambda (db)
             (define result
               (do-tc-check db hist-trends-chart% hist-settings test-snip-canvas%))
             (check > (send result get-num-set-background-message-calls) 0)
             (check = 1 (send result get-num-snips-set))
             ;; NOTE PMC data is still exported on an empty database
             (check-true (file-exists? test-image-file))
             (check-true (file-exists? test-data-file))
             (check-csv-file test-data-file)
             (define elapsed (/ (- (current-milliseconds) start) 1000.0))
             (printf " done in ~a seconds ~%" (~r elapsed #:precision 2))))
         (printf "skipping (missing database file).~%"))
     (flush-output))
   (test-case "SCATTER trends / empty database"
     (define start (current-milliseconds))
     (printf "SCATTER trends / empty database...")(flush-output)
     (with-fresh-database
       (lambda (db)
         (define result
           (do-tc-check db scatter-trends-chart% scatter-settings test-snip-canvas%))
         (check = 2 (send result get-num-set-background-message-calls))
         (check = 0 (send result get-num-snips-set))
         ;; NOTE PMC data is still exported on an empty database
         (check-false (file-exists? test-image-file))
         (check-false (file-exists? test-data-file))))
     (define elapsed (/ (- (current-milliseconds) start) 1000.0))
     (printf " done in ~a seconds ~%" (~r elapsed #:precision 2))(flush-output))
   (test-case "SCATTER trends / non-empty database"
     (define start (current-milliseconds))
     (printf "SCATTER trends / non-empty database...")(flush-output)
     (if (file-exists? test-database)
         (with-database
           test-database
           (lambda (db)
             (define result
               (do-tc-check db scatter-trends-chart% scatter-settings test-snip-canvas%))
             (check > (send result get-num-set-background-message-calls) 0)
             (check = 1 (send result get-num-snips-set))
             (check-true (file-exists? test-image-file))
             ;; NOTE: a data file will be created, but it will be empty.
             (check-true (file-exists? test-data-file))
             (define elapsed (/ (- (current-milliseconds) start) 1000.0))
             (printf " done in ~a seconds ~%" (~r elapsed #:precision 2))))
         (printf "skipping (missing database file).~%"))
     (flush-output))

   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests trend-charts-test-suite 'verbose))

