#lang racket/base
;; SPDX-License-Identifier: GPL-3.0-or-later
;; inspect-traffic.rkt -- show traffic data from MyBikeTraffic
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2024 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require racket/class
         racket/format
         racket/math
         "../widgets/qresults-list.rkt"
         "../fmt-util.rkt"
         "../fmt-util-ut.rkt"
         "../session-df/traffic.rkt")

(provide traffic-panel%)

(define column-defs
  (list
   (let ([fn (lambda (r) (hash-ref r 'total-vehicles #f))])
     (qcolumn
      "Total Vehicles"
      (lambda (r)
        (let ([v (fn r)])
          (if v (~a (exact-truncate v)) "")))
      fn))
   (let ([fn (lambda (r) (hash-ref r 'timestamp #f))])
     (qcolumn
      "Time Of Day"
      (lambda (r)
        (let ([v (fn r)])
          (if v (time-of-day->string v) "")))
      fn))
   (let ([fn (lambda (r) (hash-ref r 'elapsed #f))])
     (qcolumn
      "Elapsed Time"
      (lambda (r)
        (let ([v (fn r)])
          (if v (duration->string v) "")))
      fn))
   (let ([fn (lambda (r) (hash-ref r 'distance #f))])
     (qcolumn
      "Distance"
      (lambda (r)
        (let ([v (fn r)])
          (if v (distance->string v) "")))
      fn))
   (let ([fn (lambda (r) (hash-ref r 'vehicle-count #f))])
     (qcolumn
      "Vehicles"
      (lambda (r)
        (let ([v (fn r)])
          (if v (~a (exact-truncate v)) "")))
      fn))
   (let ([fn (lambda (r) (hash-ref r 'vehicle-speed #f))])
     (qcolumn
      "Vehicle Speed"
      (lambda (r)
        (let ([v (fn r)])
          (if v (speed->string v) "")))
      fn))
   (let ([fn (lambda (r) (hash-ref r 'max-vehicle-speed #f))])
     (qcolumn
      "Max Vehicle Speed"
      (lambda (r)
        (let ([v (fn r)])
          (if v (speed->string v) "")))
      fn))
   (let ([fn (lambda (r) (hash-ref r 'rider-speed #f))])
     (qcolumn
      "Rider Speed"
      (lambda (r)
        (let ([v (fn r)])
          (if v (speed->string v) "")))
      fn))
   (let ([fn (lambda (r) (hash-ref r 'overtaking-speed #f))])
     (qcolumn
      "Overtaking Speed"
      (lambda (r)
        (let ([v (fn r)])
          (if v (speed->string v) "")))
      fn))
   (let ([fn (lambda (r) (hash-ref r 'overtaking-distance #f))])
     (qcolumn
      "Overtaking Distance"
      (lambda (r)
        (let ([v (fn r)])
          (if v (short-distance->string v) "")))
      fn))
   (let ([fn (lambda (r) (hash-ref r 'road-slope #f))])
     (qcolumn
      "Road Slope"
      (lambda (r)
        (let ([v (fn r)])
          (if v (pct->string v) "")))
      fn))
   (let ([fn (lambda (r) (hash-ref r 'overtaking-duration #f))])
     (qcolumn
      "Overtaking Duration"
      (lambda (r)
        (let ([v (fn r)])
          (if v (duration->string v) "")))
      fn))))

(define traffic-panel%
  (class object%
    (init parent)
    (super-new)

    (define view
      (new qresults-list%
           [parent parent]
           [pref-tag 'activity-log:traffic-view]))

    (send view setup-column-defs column-defs)

    (define/public (save-visual-layout)
      (send view save-visual-layout))

    (define/public (set-session session df)
      (send view set-data (maybe-get-traffic-data! df)))

    ))
