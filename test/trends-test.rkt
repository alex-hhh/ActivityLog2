#lang racket
;; trends-test.rkt -- test the trend charts
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2018, 2019, 2020 Alex Harsányi <AlexHarsanyi@gmail.com>
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


;;;; Commentary

;; This file tests the trend charts, at least that the code can run without
;; any exceptions and it produces some output. For each trend chart, it tests
;; that it can construct a plot snip, it can export the plot as an image and
;; it can export the plot data as a CSV file.  The tests run in two
;; environments:
;;
;; 1) An empty database (I had too many errors, since I never test this case
;; during developent, as my database is full of activities)
;;
;; 2) A database containing actual data.
;;
;; a single "settings" hash is used for each chart, but new ones can be added
;; if bugs are found and this code would exercise them.  Also, if charts need
;; to be updated in the future, this file could provide a basis for testing
;; the upgrade code.

(require al2-test-runner
         data-frame
         map-widget
         plot-container
         racket/gui/base
         rackunit
         racket/runtime-path
         "../rkt/trend-charts/trends-ae.rkt"
         "../rkt/trend-charts/trends-bavg.rkt"
         "../rkt/trend-charts/trends-bw.rkt"
         "../rkt/trend-charts/trends-heatmap.rkt"
         "../rkt/trend-charts/trends-hist.rkt"
         "../rkt/trend-charts/trends-irisk.rkt"
         "../rkt/trend-charts/trends-pmc.rkt"
         "../rkt/trend-charts/trends-scatter.rkt"
         "../rkt/trend-charts/trends-tiz.rkt"
         "../rkt/trend-charts/trends-trivol.rkt"
         "../rkt/trend-charts/trends-tt.rkt"
         "../rkt/trend-charts/trends-vol.rkt"
         "../rkt/utilities.rkt"
         "test-util.rkt")

(set-dbglog-to-standard-output #t)     ; send dbglog calls to stdout, so we can see them!
;; Use 1 worker thread, so we can determine when tasks finish (See
;; `do-tc-check`)
(set-worker-thread-count 1)
(set-allow-tile-download #f)

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
   'model 'cp2
   'estimate-cp?  #f
   'ae-start 720
   'ae-end 1200
   'an-start 120
   'an-end 300
   'heat-percent 0.95
   'show-heat? #t
   'zero-base? #f))

(define hist-settings
  (hash
   'name "Histogram"
   'title "Histogram"
   'date-range '(custom-dates "Tri 2015 / HIM" 1483200000 1514736000)
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

(define dual-hist-settings
  (hash
   'name "Histogram"
   'title "Histogram"
   'date-range '(custom-dates "Tri 2015 / HIM" 1483200000 1514736000)
   'timestamps '(1483200000 . 1514822400)
   'sport '(#f . #f)
   'labels '()
   'equipment '()
   'series "lteff+rteff"
   'bucket-width 'empty
   'color-by-zone? #f
   'include-zeroes? #f
   'outlier-trim 'empty
   'show-as-pct? #f))

(define scatter-settings
  (hash
   'name "Scatter"
   'title "Scatter Plot"
   'date-range '(custom-dates "Tri 2015 / HIM" 1483200000 1514736000)
   'timestamps '(1483200000 . 1514822400)
   'sport '(#f . #f)
   'equipment '()
   'labels '()
   'series1 "lrbal"
   'series2 "lrbal"
   'ohandling 'mark
   'opct #f))

(define dual-scatter-settings
  (hash
   'name "Scatter"
   'title "Scatter Plot"
   'date-range '(custom-dates "Tri 2015 / HIM" 1483200000 1514736000)
   'timestamps '(1483200000 . 1514822400)
   'sport '(#f . #f)
   'equipment '()
   'labels '()
   'series1 "lrbal"
   'series2 "lteff+rteff"
   'ohandling 'mark
   'opct #f))

(define heatmap-settings
  (hash
   'name "Heatmap"
   'title "Heatmap"
   'date-range '(custom-dates "Tri 2015 / HIM" 1483200000 1514736000)
   'timestamps '(1483200000 . 1514822400)
   'sport '(#f . #f)
   'equipment '()
   'labels '()))

(define irisk-settings
  (hash
   'name "IRisk"
   'title "Injury Risk"
   'before 15
   'after 5
   'load 35))

(define (make-test-snip-canvas snip-canvas-class)
  (define f (new frame% [label "test-frame"] [width 150] [height 150]))
  (define c (new snip-canvas-class [parent f]))
  ;; reflow the frame container, so the geometries are calculated without
  ;; showing the frame
  (send f reflow-container)
  c)

(define test-snip-canvas%
  (class plot-container% (init parent) (super-new [parent parent])
    (define num-set-background-message-calls 0)
    (define num-snips-set 0)
    (define num-floating-snips-set 0)
    (define/override (set-snip s)
      (when s
        (check is-a? s snip%)
        (set! num-snips-set (add1 num-snips-set))))
    (define/override (set-snips/layout g)
      (check-true (plot-container-group? g))
      ;; NOTE: would be nice to count the number of snips set in the group,
      ;; but the grouping structures are not exported by `plot-container`
      (set! num-snips-set (add1 num-snips-set)))
    (define/override (set-floating-snip s x y)
      (when s
        (check is-a? s snip%)
        (set! num-floating-snips-set (add1 num-floating-snips-set))))
    (define/override (export-image-to-file fn (w #f) (h #f)) (void))
    (define/override (set-background-message msg)
      (check-pred string? msg)
      ;; (printf "set-background-message: ~a~%" msg)(flush-output)
      (set! num-set-background-message-calls (add1 num-set-background-message-calls)))
    (define/public (get-num-set-background-message-calls)
      num-set-background-message-calls)
    (define/public (get-num-snips-set) num-snips-set)
    (define/public (get-num-floating-snips-set) num-floating-snips-set)))

(define test-image-file (make-temporary-file "al2-~a.png"))
(define test-data-file (make-temporary-file "al2-~a.csv"))

;; This is downloaded by download-test-db.sh, needs to be a runtime path,
;; since we use a separate `place` to open the database
(define-runtime-path test-database "./test-db/al2-v29.db")

(define (do-tc-check db chart-class settings snip-canvas-class)
  (when (file-exists? test-image-file)
    (delete-file test-image-file))
  (when (file-exists? test-data-file)
    (delete-file test-data-file))
  (define c (new chart-class [database db]))
  (send c put-chart-settings settings)
  (define canvas (make-test-snip-canvas snip-canvas-class))
  (send c put-plot-snip canvas)
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
  ;; to the event handler thread to let it execute this callback.  If this
  ;; does not finish, maybe there are also some timers running?
  (yield (current-eventspace))
  (send c save-plot-image test-image-file 1000 1000)
  (send c export-data-to-file test-data-file #f)
  canvas)

;; Check that the CSV file is valid (contains some data)
(define (check-csv-file file)
  (define df (df-read/csv file))
  (let ((nrows (df-row-count df)))
    (check > nrows 0)                   ; file is not empty
    ;; Check that there is no runaway time period in the chart TRIVOL, VOL and
    ;; TIZ plots had bugs which bought in data from the "beginning of time"
    ;; (1970), when "all days" was selected, with all zeroes of course.
    (check < nrows 10000)))

(define trend-charts-test-suite
  (test-suite
   "Trend Charts"
   (test-case "BW trends / empty database"
     (with-fresh-database
       (lambda (db)
         (define result
           (do-tc-check db bw-trends-chart% bw-settings test-snip-canvas%))
         (check = 1 (send result get-num-set-background-message-calls))
         (check = 0 (send result get-num-snips-set))
         (check = 0 (send result get-num-floating-snips-set))
         ;; With no data, these files should not be created
         (check-false (file-exists? test-image-file))
         (check-false (file-exists? test-data-file)))))
   (test-case "BW trends / non-empty database"
     (unless (file-exists? test-database)
       (skip-test))
     (with-database
       test-database
       (lambda (db)
         (define result
           (do-tc-check db bw-trends-chart% bw-settings test-snip-canvas%))
         (check = 0 (send result get-num-set-background-message-calls))
         (check = 1 (send result get-num-snips-set))
         (check = 0 (send result get-num-floating-snips-set))
         ;; With no data, these files should not be created
         (check-true (file-exists? test-image-file))
         (check-true (file-exists? test-data-file))
         (check-csv-file test-data-file))))
   (test-case "AE trends / empty database"
     (with-fresh-database
       (lambda (db)
         (define result
           (do-tc-check db ae-trends-chart% bw-settings test-snip-canvas%))
         (check = 1 (send result get-num-set-background-message-calls))
         (check = 0 (send result get-num-snips-set))
         (check = 0 (send result get-num-floating-snips-set))
         ;; With no data, these files should not be created
         (check-false (file-exists? test-image-file))
         (check-false (file-exists? test-data-file)))))
   (test-case "AE trends / non-empty database"
     (unless (file-exists? test-database)
       (skip-test))
     (with-database
       test-database
       (lambda (db)
         (define result
           (do-tc-check db ae-trends-chart% bw-settings test-snip-canvas%))
         (check = 0 (send result get-num-set-background-message-calls))
         (check = 1 (send result get-num-snips-set))
         (check = 0 (send result get-num-floating-snips-set))
         ;; With no data, these files should not be created
         (check-true (file-exists? test-image-file))
         (check-true (file-exists? test-data-file))
         (check-csv-file test-data-file))))
   (test-case "TIZ trends / empty database"
     (with-fresh-database
       (lambda (db)
         (define result
           (do-tc-check db tiz-trends-chart% tiz-settings test-snip-canvas%))
         (check = 1 (send result get-num-set-background-message-calls))
         (check = 0 (send result get-num-snips-set))
         (check = 0 (send result get-num-floating-snips-set))
         ;; With no data, these files should not be created
         (check-false (file-exists? test-image-file))
         (check-false (file-exists? test-data-file)))))
   (test-case "TIZ trends / non-empty database"
     (unless (file-exists? test-database)
       (skip-test))
     (with-database
       test-database
       (lambda (db)
         (define result
           (do-tc-check db tiz-trends-chart% tiz-settings test-snip-canvas%))
         (check = 0 (send result get-num-set-background-message-calls))
         (check = 1 (send result get-num-snips-set))
         (check = 0 (send result get-num-floating-snips-set))
         ;; With no data, these files should not be created
         (check-true (file-exists? test-image-file))
         (check-true (file-exists? test-data-file))
         (check-csv-file test-data-file))))
   (test-case "TRIVOL trends / empty database"
     (with-fresh-database
       (lambda (db)
         (define result
           (do-tc-check db trivol-trends-chart% trivol-settings test-snip-canvas%))
         (check = 1 (send result get-num-set-background-message-calls))
         (check = 0 (send result get-num-snips-set))
         (check = 0 (send result get-num-floating-snips-set))
         ;; With no data, these files should not be created
         (check-false (file-exists? test-image-file))
         (check-false (file-exists? test-data-file)))))
   (test-case "TRIVOL trends / non-empty database"
     (unless (file-exists? test-database)
       (skip-test))
     (with-database
       test-database
       (lambda (db)
         (define result
           (do-tc-check db trivol-trends-chart% trivol-settings test-snip-canvas%))
         (check = 0 (send result get-num-set-background-message-calls))
         (check = 1 (send result get-num-snips-set))
         (check = 0 (send result get-num-floating-snips-set))
         ;; With no data, these files should not be created
         (check-true (file-exists? test-image-file))
         (check-true (file-exists? test-data-file))
         (check-csv-file test-data-file))))
   (test-case "VOL trends / empty database"
     (with-fresh-database
       (lambda (db)
         (define result
           (do-tc-check db vol-trends-chart% vol-settings test-snip-canvas%))
         (check = 1 (send result get-num-set-background-message-calls))
         (check = 0 (send result get-num-snips-set))
         (check = 0 (send result get-num-floating-snips-set))
         ;; With no data, these files should not be created
         (check-false (file-exists? test-image-file))
         (check-false (file-exists? test-data-file)))))
   (test-case "VOL trends / non-empty database"
     (unless (file-exists? test-database)
       (skip-test))
     (with-database
       test-database
       (lambda (db)
         (define result
           (do-tc-check db vol-trends-chart% vol-settings test-snip-canvas%))
         (check = 0 (send result get-num-set-background-message-calls))
         (check = 1 (send result get-num-snips-set))
         (check = 0 (send result get-num-floating-snips-set))
         ;; With no data, these files should not be created
         (check-true (file-exists? test-image-file))
         (check-true (file-exists? test-data-file))
         (check-csv-file test-data-file))))
   (test-case "TT trends / empty database"
     (with-fresh-database
       (lambda (db)
         (define result
           (do-tc-check db tt-trends-chart% tt-settings test-snip-canvas%))
         (check = 1 (send result get-num-set-background-message-calls))
         (check = 0 (send result get-num-snips-set))
         (check = 0 (send result get-num-floating-snips-set))
         ;; With no data, these files should not be created
         (check-false (file-exists? test-image-file))
         (check-false (file-exists? test-data-file)))))
   (test-case "TT trends / non-empty database"
     (unless (file-exists? test-database)
       (skip-test))
     (with-database
       test-database
       (lambda (db)
         (define result
           (do-tc-check db tt-trends-chart% tt-settings test-snip-canvas%))
         (check = 0 (send result get-num-set-background-message-calls))
         (check = 1 (send result get-num-snips-set))
         (check = 0 (send result get-num-floating-snips-set))
         ;; With no data, these files should not be created
         (check-true (file-exists? test-image-file))
         (check-true (file-exists? test-data-file))
         (check-csv-file test-data-file))))
   (test-case "PMC trends / empty database"
     (with-fresh-database
       (lambda (db)
         (define result
           (do-tc-check db pmc-trends-chart% pmc-settings test-snip-canvas%))
         (check = 0 (send result get-num-set-background-message-calls))
         (check = 1 (send result get-num-snips-set))
         (check = 0 (send result get-num-floating-snips-set))
         ;; NOTE PMC data is still exported on an empty database
         (check-true (file-exists? test-image-file))
         (check-true (file-exists? test-data-file)))))
   (test-case "PMC trends / non-empty database"
     (unless (file-exists? test-database)
       (skip-test))
     (with-database
       test-database
       (lambda (db)
         (define result
           (do-tc-check db pmc-trends-chart% pmc-settings test-snip-canvas%))
         (check = 0 (send result get-num-set-background-message-calls))
         (check = 1 (send result get-num-snips-set))
         (check = 0 (send result get-num-floating-snips-set))
         ;; NOTE PMC data is still exported on an empty database
         (check-true (file-exists? test-image-file))
         (check-true (file-exists? test-data-file))
         (check-csv-file test-data-file))))
   (test-case "BAVG trends / empty database"
     (with-fresh-database
       (lambda (db)
         (define result
           (do-tc-check db mmax-trends-chart% bavg-settings test-snip-canvas%))
         (check = 2 (send result get-num-set-background-message-calls))
         (check = 0 (send result get-num-snips-set))
         (check = 0 (send result get-num-floating-snips-set))
         (check-false (file-exists? test-image-file))
         (check-true (file-exists? test-data-file)))))
   (test-case "BAVG trends / non-empty database"
     (unless (file-exists? test-database)
       (skip-test))
     (with-database
       test-database
       (lambda (db)
         (define result
           (do-tc-check db mmax-trends-chart% bavg-settings test-snip-canvas%))
         (check > (send result get-num-set-background-message-calls) 0)
         (check = 1 (send result get-num-snips-set))
         (check = 0 (send result get-num-floating-snips-set))
         (check-true (file-exists? test-image-file))
         (check-true (file-exists? test-data-file))
         (check-csv-file test-data-file))))
   (test-case "HIST trends / empty database"
     (with-fresh-database
       (lambda (db)
         (define result
           (do-tc-check db hist-trends-chart% hist-settings test-snip-canvas%))
         (check = 2 (send result get-num-set-background-message-calls))
         (check = 0 (send result get-num-snips-set))
         (check = 0 (send result get-num-floating-snips-set))
         ;; NOTE PMC data is still exported on an empty database
         (check-false (file-exists? test-image-file))
         (check-false (file-exists? test-data-file)))))
   (test-case "DUAL HIST trends / empty database"
     (with-fresh-database
       (lambda (db)
         (define result
           (do-tc-check db hist-trends-chart% dual-hist-settings test-snip-canvas%))
         (check = 2 (send result get-num-set-background-message-calls))
         (check = 0 (send result get-num-snips-set))
         (check = 0 (send result get-num-floating-snips-set))
         ;; NOTE PMC data is still exported on an empty database
         (check-false (file-exists? test-image-file))
         (check-false (file-exists? test-data-file)))))
   (test-case "HIST trends / non-empty database"
     (unless (file-exists? test-database)
       (skip-test))
     (with-database
       test-database
       (lambda (db)
         (define result
           (do-tc-check db hist-trends-chart% hist-settings test-snip-canvas%))
         (check > (send result get-num-set-background-message-calls) 0)
         (check = 1 (send result get-num-snips-set))
         (check = 0 (send result get-num-floating-snips-set))
         ;; NOTE PMC data is still exported on an empty database
         (check-true (file-exists? test-image-file))
         (check-true (file-exists? test-data-file))
         (check-csv-file test-data-file))))
   (test-case "DUAL HIST trends / non-empty database"
     (unless (file-exists? test-database)
       (skip-test))
     (with-database
       test-database
       (lambda (db)
         (define result
           (do-tc-check db hist-trends-chart% dual-hist-settings test-snip-canvas%))
         (check > (send result get-num-set-background-message-calls) 0)
         (check = 1 (send result get-num-snips-set))
         (check = 0 (send result get-num-floating-snips-set))
         ;; NOTE PMC data is still exported on an empty database
         (check-true (file-exists? test-image-file))
         (check-true (file-exists? test-data-file))
         (check-csv-file test-data-file))))
   (test-case "SCATTER trends / empty database"
     (with-fresh-database
       (lambda (db)
         (define result
           (do-tc-check db scatter-trends-chart% scatter-settings test-snip-canvas%))
         (check = 2 (send result get-num-set-background-message-calls))
         (check = 0 (send result get-num-snips-set))
         (check = 0 (send result get-num-floating-snips-set))
         ;; NOTE PMC data is still exported on an empty database
         (check-false (file-exists? test-image-file))
         (check-false (file-exists? test-data-file)))))
   (test-case "DUAL SCATTER trends / empty database"
     (with-fresh-database
       (lambda (db)
         (define result
           (do-tc-check db scatter-trends-chart% dual-scatter-settings test-snip-canvas%))
         (check = 2 (send result get-num-set-background-message-calls))
         (check = 0 (send result get-num-snips-set))
         (check = 0 (send result get-num-floating-snips-set))
         ;; NOTE PMC data is still exported on an empty database
         (check-false (file-exists? test-image-file))
         (check-false (file-exists? test-data-file)))))
   (test-case "SCATTER trends / non-empty database"
     (unless (file-exists? test-database)
       (skip-test))
     (with-database
       test-database
       (lambda (db)
         (define result
           (do-tc-check db scatter-trends-chart% scatter-settings test-snip-canvas%))
         (check > (send result get-num-set-background-message-calls) 0)
         (check = 1 (send result get-num-snips-set))
         (check = 0 (send result get-num-floating-snips-set))
         (check-true (file-exists? test-image-file))
         (check-false (file-exists? test-data-file)))))
   (test-case "DUAL SCATTER trends / non-empty database"
     (unless (file-exists? test-database)
       (skip-test))
     (with-database
       test-database
       (lambda (db)
         (define result
           (do-tc-check db scatter-trends-chart% dual-scatter-settings test-snip-canvas%))
         (check > (send result get-num-set-background-message-calls) 0)
         (check = 1 (send result get-num-snips-set))
         (check = 0 (send result get-num-floating-snips-set))
         (check-true (file-exists? test-image-file))
         ;; NOTE: a data file will be created, but it will be empty.
         (check-false (file-exists? test-data-file)))))
   (test-case "HEATMAP trends / empty database"
     (with-fresh-database
       (lambda (db)
         (define result
           (do-tc-check db heatmap-chart% heatmap-settings test-snip-canvas%))
         (check = 0 (send result get-num-set-background-message-calls))
         ;; The map and map control snips are always set, whether there are
         ;; any activities or not.
         (check = 1 (send result get-num-snips-set))
         (check = 1 (send result get-num-floating-snips-set))
         ;; NOTE heatmap data is still exported on an empty database
         (check-true (file-exists? test-image-file))
         (check-false (file-exists? test-data-file)))))
   (test-case "HEATMAP trends / non-empty database"
     ;; NOTE: the test database contains activities, but does not contain any
     ;; GPS data (since it is stripped out from the test databases) xso this is
     ;; an incomplete test, but it is better than nothing...
     (unless (file-exists? test-database)
       (skip-test))
     (with-database
       test-database
       (lambda (db)
         (define result
           (do-tc-check db heatmap-chart% heatmap-settings test-snip-canvas%))
         (check = 0 (send result get-num-set-background-message-calls))
         ;; The map and map control snips are always set, whether there
         ;; are any activities or not.
         (check = 1 (send result get-num-snips-set))
         (check = 1 (send result get-num-floating-snips-set))
         (check-true (file-exists? test-image-file))
         ;; NOTE: we don't export any data from this chart
         (check-false (file-exists? test-data-file)))))

   (test-case "IRISK trends / empty database"
     (with-fresh-database
       (lambda (db)
         (define result
           (do-tc-check db irisk-trends-chart% irisk-settings test-snip-canvas%))
         (check < 0 (send result get-num-set-background-message-calls))
         (check = 1 (send result get-num-snips-set))
         (check = 0 (send result get-num-floating-snips-set))
         (check-false (file-exists? test-image-file))
         (check-true (file-exists? test-data-file)))))
   (test-case "IRISK trends / non-empty database"
     ;; NOTE: the test database contains activities, but does not contain any
     ;; GPS data (since it is stripped out from the test databases) xso this is
     ;; an incomplete test, but it is better than nothing...
     (unless (file-exists? test-database)
       (skip-test))
     (with-database
       test-database
       (lambda (db)
         (define result
           (do-tc-check db irisk-trends-chart% irisk-settings test-snip-canvas%))
         (check < 0 (send result get-num-set-background-message-calls))
         (check = 1 (send result get-num-snips-set))
         (check = 0 (send result get-num-floating-snips-set))
         (check-false (file-exists? test-image-file))
         (check-true (file-exists? test-data-file)))))
   ))

(module+ test
  (run-tests #:package "trends-test"
             #:results-file "test-results/trends-test.xml"
             trend-charts-test-suite))

