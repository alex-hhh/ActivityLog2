#lang racket/base

;; trends-tt.rkt -- "Training Time chart, a punch card style chart
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2016, 2018 Alex Harsányi <AlexHarsanyi@gmail.com>
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


(require
 db/base
 plot/no-gui
 racket/class
 racket/match
 racket/gui/base
 racket/math
 racket/format
 "../database.rkt"
 "trends-chart.rkt"
 "../plot-util.rkt"
 "../widgets/main.rkt"
 "../sport-charms.rkt"
 "../data-frame/df.rkt"
 "../data-frame/sql.rkt"
 "../data-frame/csv.rkt"
 "../al-widgets.rkt"
 "../plot-util.rkt")

(provide tt-trends-chart%)

(define tt-chart-settings%
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
    (define sport-hp (make-horizontal-pane time-gb))
    (define sport-selector (new sport-selector% [parent sport-hp] [sports-in-use-only? #t]))
    (define tri-checkbox
      (new check-box% [parent sport-hp] [label "Tri Activities"]
           [value #f]
           [callback (lambda (c e) (on-triathlon-activties (send c get-value)))]))
    (define date-range-selector (new date-range-selector% [parent time-gb]))

    (define (on-triathlon-activties flag)
      (send sport-selector enable (not flag)))

    (define/public (get-chart-settings)
      (hash
       'name (send name-field get-value)
       'title (send title-field get-value)
       'date-range (send date-range-selector get-restore-data)
       'timestamps (send date-range-selector get-selection)
       'sport (send sport-selector get-selection)
       'tri? (send tri-checkbox get-value)))

    (define/public (put-chart-settings data)
      (when database
        (send date-range-selector set-seasons (db-get-seasons database)))
      (send name-field set-value (hash-ref data 'name))
      (send title-field set-value (hash-ref data 'title))
      (send date-range-selector restore-from (hash-ref data 'date-range))
      (let ((sp (hash-ref data 'sport #f)))
        (when sp
          (send sport-selector set-selected-sport (car sp) (cdr sp))))
      (send tri-checkbox set-value (hash-ref data 'tri?))
      (on-triathlon-activties (hash-ref data 'tri?)))

    (define/public (show-dialog parent)
      (when database
        (send date-range-selector set-seasons (db-get-seasons database)))
      (and (send this do-edit parent) (get-chart-settings)))

    ))

(define tt-tri-stmt
  (virtual-statement
   (lambda (dbsys)
  "select round(strftime('%w', S.start_time, 'unixepoch', 'localtime'), 0) as dow,
       round ((strftime('%H', S.start_time, 'unixepoch', 'localtime') * 3600
               + strftime('%M', S.start_time, 'unixepoch', 'localtime') * 60
               + strftime('%S', S.start_time, 'unixepoch', 'localtime') * 60) / 3600.0, 1) as time,
       count(S.id) as ntotal,
       sum(case S.sport_id when 2 then 1 else 0 end) as ncycle,
       sum(case S.sport_id when 1 then 1 else 0 end) as nrun,
       sum(case S.sport_id when 5 then 1 else 0 end) as nswim,
       sum(case when S.sport_id = 4 and S.sub_sport_id = 20 then 1 else 0 end) as nstrength
  from A_SESSION S
 where S.start_time between ? and ?
 group by dow, time
 order by dow, time")))

(define tt-sport-stmt-1                 ; both sport-id and sub-sport-id
  (virtual-statement
   (lambda (dbsys)
  "select round(strftime('%w', S.start_time, 'unixepoch', 'localtime'), 0) as dow,
       round ((strftime('%H', S.start_time, 'unixepoch', 'localtime') * 3600
               + strftime('%M', S.start_time, 'unixepoch', 'localtime') * 60
               + strftime('%S', S.start_time, 'unixepoch', 'localtime')) / 3600.0, 1) as time,
       count(S.id) as ntotal
  from A_SESSION S
 where S.start_time between ? and ?
   and S.sport_id = ? and S.sub_sport_id = ?
 group by dow, time
 order by dow, time")))

(define tt-sport-stmt-2                 ; only sport-id
  (virtual-statement
   (lambda (dbsys)
  "select round(strftime('%w', S.start_time, 'unixepoch', 'localtime'), 0) as dow,
       round ((strftime('%H', S.start_time, 'unixepoch', 'localtime') * 3600
               + strftime('%M', S.start_time, 'unixepoch', 'localtime') * 60
               + strftime('%S', S.start_time, 'unixepoch', 'localtime')) / 3600.0, 1) as time,
       count(S.id) as ntotal
  from A_SESSION S
 where S.start_time between ? and ?
   and S.sport_id = ?
 group by dow, time
 order by dow, time")))

(define tt-sport-stmt-3                 ; all sports, grouped by sport
  (virtual-statement
   (lambda (dbsys)
     "
select round(strftime('%w', S.start_time, 'unixepoch', 'localtime'), 0) as dow,
       round ((strftime('%H', S.start_time, 'unixepoch', 'localtime') * 3600
               + strftime('%M', S.start_time, 'unixepoch', 'localtime') * 60
               + strftime('%S', S.start_time, 'unixepoch', 'localtime')) / 3600.0, 1) as time,
       sport_id,
       sub_sport_id,
       count(S.id) as ntotal
  from A_SESSION S
 where S.start_time between ? and ?
 group by dow, time, sport_id, sub_sport_id
 order by dow, time, sport_id, sub_sport_id")))

(define days-of-week #("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))

(define (days-of-week-layout start end)
  (for/list ((ts (in-range (exact-floor start) (exact-ceiling end))))
    (pre-tick ts #t)))

(define (days-of-week-format start end pre-ticks)
  (for/list ((tick pre-ticks))
    (let ((val (pre-tick-value tick)))
      (if (and (exact-integer? val) (>= val 0) (< val (vector-length days-of-week)))
          (vector-ref days-of-week val)
          ""))))

(define (day-of-week-ticks)
  (ticks days-of-week-layout days-of-week-format))

(define (hours-of-day-layout start end)
  (for/list ((ts (in-range (exact-truncate start) (exact-ceiling end) 0.5)))
    (pre-tick ts (integer? ts))))

(define (hours-of-day-format start end pre-ticks)
  (for/list ((tick pre-ticks))
    (let ((val (pre-tick-value tick)))
      (if (and (>= val 0) (<= val 24))
          (let* ((hrs (exact-truncate val))
                 (mins (exact-truncate (* 60.0 (- val hrs)))))
            (string-append
             (~a hrs #:width 2 #:align 'right #:pad-string "0")
             ":"
             (~a mins #:width 2 #:align 'right #:pad-string "0")))
          ""))))

(define (hours-of-day-ticks)
  (ticks hours-of-day-layout hours-of-day-format))

(define (make-group df count-series)
  (define group (make-hash))
  (df-for-each
   df
   (list "dow" "time" count-series)
   (lambda (val)
     (match-define (list dow time count) val)
     (when (> count 0)
       (hash-update! group count (lambda (prev) (cons (vector dow time) prev)) '()))))
  group)

(define (make-data df count-series)
  (define group '())
  (df-for-each
   df
   (list "dow" "time" count-series)
   (lambda (val)
     (match-define (list dow time count) val)
     (when (> count 0)
       (let ((item (vector dow time)))
         (for ([x (in-range count)])
           (set! group (cons item group)))))))
  group)

(define (make-data/multisport df)
  (define result (make-hash))
  (df-for-each
   df
   '("dow" "time" "sport_id" "sub_sport_id" "ntotal")
   (lambda (val)
     (match-define (list dow time sport sub-sport count) val)
     (when (> count 0)
            (let ((group (hash-ref result (cons sport sub-sport) '()))
                  (item (vector dow time)))
              (for ([x (in-range count)])
                (set! group (cons item group)))
              (hash-set! result (cons sport sub-sport) group)))))
  result)

(define (make-renderer data
                       #:color color #:label label
                       #:size size #:alpha [alpha 1.0])
  (points data
          #:sym 'fullcircle
          #:color color
          #:fill-color color
          #:label label
          #:size (* (point-size) size)
          #:alpha alpha
          #:x-jitter 0.2))

(define (make-renderer-tree triathlon? tt-data sport)
  (cond (triathlon?
         (list
          (tick-grid)
          (make-renderer
           (make-data tt-data "ncycle")
           #:color (get-sport-color 2 #f #t)
           #:label (get-sport-name 2 #f)
           #:alpha 0.8
           #:size 1.5)
          (make-renderer
           (make-data tt-data "nrun")
           #:color (get-sport-color 1 #f #t)
           #:label (get-sport-name 1 #f)
           #:alpha 0.8
           #:size 1.5)
          (make-renderer
           (make-data tt-data "nswim")
           #:color (get-sport-color 5 #f #t)
           #:label (get-sport-name 5 #f)
           #:alpha 0.8
            #:size 1.5)
          (make-renderer
           (make-data tt-data "nstrength")
           #:color (get-sport-color 4 20 #t)
           #:label (get-sport-name 4 20)
           #:alpha 0.8
           #:size 1.5)))
        ((and (eq? (car sport) #f) (eq? (cdr sport) #f)) ; all sports
         (let* ((data (make-data/multisport tt-data))
                (keys (sort (hash-keys data) string<?
                            #:key (lambda (k) (get-sport-name (car k) (cdr k))))))
           (append
            (list (tick-grid))
            (for/list ([k keys])
              (make-renderer
               (hash-ref data k '())
               #:color (get-sport-color (car k) (cdr k) #t)
               #:label (get-sport-name (car k) (cdr k))
               #:size 1.5
               #:alpha 0.8)))))
        (#t
         (list
          (tick-grid)
          (make-renderer
           (make-data tt-data "ntotal")
           #:color (get-sport-color (car sport) (cdr sport) #t)
           #:label (get-sport-name (car sport) (cdr sport))
           #:size 1.5)))))

(define (generate-plot output-fn renderer-tree)
  (parameterize ([plot-x-ticks (day-of-week-ticks)]
                 [plot-x-label #f]
                 [plot-y-ticks (hours-of-day-ticks)]
                 [plot-y-label #f])
    (output-fn renderer-tree)))

(define (save-plot-to-file file-name width height renderer-tree)
  (when renderer-tree
    (generate-plot
     (lambda (rt)
       (plot-file rt file-name
                  #:x-min -1 #:x-max 7 #:y-min -1 #:y-max 25
                  #:width width #:height height))
    renderer-tree)))

(define (insert-plot-snip canvas renderer-tree)
  (if renderer-tree
      (generate-plot
       (lambda (rt)
         (plot-to-canvas
          rt canvas #:x-min -1 #:x-max 7 #:y-min -1 #:y-max 25))
       renderer-tree)
    (begin
      (send canvas set-snip #f)
      (send canvas set-background-message "No data to plot"))))

(define tt-trends-chart%
  (class trends-chart%
    (init-field database) (super-new)

    (define data-valid? #f)
    (define tt-data #f)                 ; a data-frame%

    (define/override (make-settings-dialog)
      (new tt-chart-settings%
           [default-name "Training Times"]
           [default-title "TrainingTimes"]
           [database database]))

    (define/override (invalidate-data)
      (set! data-valid? #f))

    (define/override (is-invalidated-by-events? events)
      (or (hash-ref events 'session-deleted #f)
          (hash-ref events 'session-created #f)
          (hash-ref events 'session-updated #f)))

    (define/override (export-data-to-file file formatted?)
      (when (and tt-data (> (df-row-count tt-data) 0))
        (call-with-output-file file
          (lambda (out) (export-data-as-csv out formatted?))
          #:mode 'text #:exists 'truncate)))

    (define (export-data-as-csv out formatted?)
      (define all-series '("dow" "time" "ntotal" "nstrength" "nswim"
                           "ncycle" "nrun" "sport_id" "sub_sport_id"))
      (define actual-series
        (for/list ([series all-series] #:when (df-contains? tt-data series))
          series))
      (apply df-write/csv tt-data out actual-series))

    (define/override (put-plot-snip canvas)
      (maybe-fetch-data)
      (if data-valid?
          (let* ((params (send this get-chart-settings))
                 (sport (hash-ref params 'sport))
                 (tri? (hash-ref params 'tri?))
                 (rt (make-renderer-tree tri? tt-data sport)))
            (insert-plot-snip canvas rt))
          (begin
            (send canvas set-snip #f)
            (send canvas set-background-message "No data to plot"))))

    (define/override (save-plot-image file-name width height)
      (when data-valid?
        (define params (send this get-chart-settings))
        (define sport (hash-ref params 'sport))
        (define tri? (hash-ref params 'tri?))
        (define rt (make-renderer-tree tri? tt-data sport))
        (save-plot-to-file file-name width height rt)))

    (define (maybe-fetch-data)
      (unless data-valid?
        (let ((params (send this get-chart-settings)))
          (when params
            (match-define (cons start end) (hash-ref params 'timestamps (cons 0 0)))
            (match-define (cons sport-id sub-sport-id)
              (hash-ref params 'sport))
            (define tri? (hash-ref params 'tri?))
            (set! tt-data
                  (if tri?
                      (df-read/sql database tt-tri-stmt start end)
                      (cond ((and sport-id sub-sport-id)
                             (df-read/sql database tt-sport-stmt-1
                                          start end sport-id sub-sport-id))
                            (sport-id
                             (df-read/sql database tt-sport-stmt-2
                                          start end sport-id))
                            (#t
                             (df-read/sql database tt-sport-stmt-3 start end)))))
            (set! data-valid? (> (df-row-count tt-data) 0))))))

    ))
