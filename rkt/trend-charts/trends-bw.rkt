#lang racket/base
;; trends-bw.rkt -- bodyweight trend chart
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2016 Alex Harsanyi (AlexHarsanyi@gmail.com)
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

(require db/base
         plot/no-gui
         racket/class
         racket/gui/base
         racket/match
         "../database.rkt"
         "../fmt-util.rkt"
         "../plot-hack.rkt"
         "../plot-util.rkt"
         "../widgets/main.rkt"
         "trends-chart.rkt")

(provide bw-trends-chart%)

(struct bw-params tc-params (start-date end-date group-by))

(define bw-chart-settings%
  (class edit-dialog-base%
    (init-field database
                [default-name "Trends"]
                [default-title "Trends Chart"])

    (super-new [title "Chart Settings"]
               [icon (edit-icon)]
               [min-height 10])

    (define name-gb (make-group-box-panel (send this get-client-pane)))
    (define name-field (new text-field% [parent name-gb] [label "Name "]))
    (send name-field set-value default-name)
    (define title-field (new text-field% [parent name-gb] [label "Title "]))
    (send title-field set-value default-title)

    (define time-gb (make-group-box-panel (send this get-client-pane)))
    (define date-range-selector (new date-range-selector% [parent time-gb]))

    (define grouping-gb (make-group-box-panel (send this get-client-pane)))
    (define group-by-choice
      (new choice% [parent grouping-gb] [label "Group By "]
           [choices '("Week" "Month" "Year")]))

    (define/public (get-restore-data)
      (list
       (send name-field get-value)
       (send title-field get-value)
       (send date-range-selector get-restore-data)
       (send group-by-choice get-selection)))

    (define/public (restore-from data)
      (when database
        (send date-range-selector set-seasons (db-get-seasons database)))
      (match-define (list d0 d1 d2 d3) data)
      (send name-field set-value d0)
      (send title-field set-value d1)
      (send date-range-selector restore-from d2)
      (send group-by-choice set-selection d3))

    (define/public (show-dialog parent)
      (when database
        (send date-range-selector set-seasons (db-get-seasons database)))
      (if (send this do-edit parent)
          (get-settings)
          #f))

    (define/public (get-settings)
      (let ((dr (send date-range-selector get-selection)))
        (if dr
            (let ((start-date (car dr))
                  (end-date (cdr dr)))
              (when (eqv? start-date 0)
                (set! start-date (get-true-min-start-date database)))
              (bw-params
               (send name-field get-value)
               (send title-field get-value)
               start-date
               end-date
               (send group-by-choice get-selection)))
            #f)))
    ))

(define (get-data db sql-query start-date end-date group-by)
  (let* ((filter-width (* 24 60 60 (case group-by ((0) 7) ((1) 30) ((2) 365))))
         (filter (make-low-pass-filter filter-width #f)))
    (for/list (([timestamp bw] (in-query db sql-query start-date end-date)))
      (filter (vector timestamp bw)))))

(define (get-entry-for-day bw-data timestamp)
  (for/or ((today bw-data)
           (tomorrow (cdr bw-data)))
    ;; (printf "today ~a; tomorrow ~a; timestamp ~a~%" today tomorrow timestamp)
    (and (< (vector-ref today 0) timestamp (vector-ref tomorrow 0))
         (cons today tomorrow))))

(define *sea-green* '(#x2e #x8b #x57))

(define (make-renderer-tree bw-data)
  (list (tick-grid) (lines bw-data #:color *sea-green* #:width 3.0)))

(define (generate-plot output-fn renderer-tree)
  (parameterize ([plot-x-ticks (pmc-date-ticks)]
                 [plot-x-label #f]
                 [plot-y-label "Bodyweight"])
    (output-fn renderer-tree)))

(define (insert-plot-snip canvas renderer-tree)
  (generate-plot
   (lambda (renderer-tree)
     (plot-snip/hack canvas renderer-tree))
   renderer-tree))

(define (save-plot-to-file file-name width height renderer-tree)
  (generate-plot
   (lambda (renderer-tree)
     (plot-file renderer-tree file-name #:width width #:height height))
   renderer-tree))

(define bw-trends-chart%
  (class trends-chart%
    (init-field database)
    (super-new)

    (define data-valid? #f)

    (define bw-query
      (virtual-statement
       (lambda (dbsys)
         "select timestamp, body_weight as bw
            from ATHLETE_METRICS
           where timestamp between ? and ?")))

    (define bw-data #f)                 ; fetched from the database

    (define/override (make-settings-dialog)
      (new bw-chart-settings%
           [default-name "BodyWeight"]
           [default-title "Body Weight"]
           [database database]))

    (define/override (invalidate-data)
      (set! data-valid? #f))

    (define/override (is-invalidated-by-events? events)
      (or (hash-ref events 'athlete-metrics-deleted #f)
          (hash-ref events 'athlete-metrics-updated #f)
          (hash-ref events 'athlete-metrics-created #f)))

    (define/override (export-data-to-file file formatted?)
      (when bw-data
        (call-with-output-file file export-data-as-csv
          #:mode 'text #:exists 'truncate)))

    (define (export-data-as-csv out)
      ;; NOTE: we write out the filtered bodyweight, as plotted on the graph.
      (write-string "Timestamp, Bodyweight (filtered)" out)
      (newline out)
      (for ((datum bw-data))
        (match-define (vector timestamp bw) datum)
        (write-string (format "~a, ~a~%" timestamp bw) out)))

    (define (plot-hover-callback snip event x y)
      (define info '())
      (define (add-info tag val) (set! info (cons (list tag val) info)))
      (define renderers '())
      (define (add-renderer r) (set! renderers (cons r renderers)))
      
      (when (good-hover? x y event)
        (let ((entry (get-entry-for-day bw-data x)))
          (when entry
            (add-renderer (pu-vrule x))
            (match-define (vector today bw1) (car entry))
            (match-define (vector tomorrow bw2) (cdr entry))
            (define bw (+ bw1 (* (- bw2 bw1) (/ (- x today) (- today tomorrow)))))
            (add-info "Date" (calendar-date->string today))
            (add-info "Bodyweight" (weight->string bw #t))
            (unless (null? info)
              (add-renderer (pu-label x y (make-hover-badge info)))))))

      (set-overlay-renderers snip renderers))

    (define/override (put-plot-snip canvas)
      (maybe-fetch-data)
      (if data-valid?
          (let ((snip (insert-plot-snip
                       canvas
                       (make-renderer-tree bw-data))))
            (set-mouse-event-callback snip plot-hover-callback))
          (begin
            (send canvas set-snip #f)
            (send canvas set-background-message "No data to plot"))))

    (define/override (save-plot-image file-name width height)
      ;; We assume the data is ready, and don't do anything if it is not.
      (when data-valid?
        (save-plot-to-file file-name width height
                           (make-renderer-tree bw-data))))

    (define (maybe-fetch-data)
      (unless data-valid?
        (let ((params (send this get-params)))
          (when params
            (let ((start (bw-params-start-date params))
                  (end (bw-params-end-date params))
                  (group-by (bw-params-group-by params)))
              (set! bw-data (get-data database bw-query start end group-by))
              (set! data-valid? (> (length bw-data) 0)))))))

    ))
