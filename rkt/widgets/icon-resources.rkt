#lang racket/base
;; icon-resources.rkt -- hold all (most) icon bitmap objects used by the
;; application, avoids re-loading them in each module that needs them.
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015, 2021 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require racket/draw
         racket/runtime-path)

(define-runtime-path weather-icon-file "../../img/partly_cloudy_day-64.png")
(define the-weather-icon #f)
(define (weather-icon)
  (unless the-weather-icon
    (set! the-weather-icon (read-bitmap weather-icon-file)))
  the-weather-icon)
(provide weather-icon)

(define-runtime-path sql-export-icon-file "../../img/data_configuration-64.png")
(define the-sql-export-icon #f)
(define (sql-export-icon)
  (unless the-sql-export-icon
    (set! the-sql-export-icon (read-bitmap sql-export-icon-file)))
  the-sql-export-icon)
(provide sql-export-icon)

(define-runtime-path wscale-icon-file "../../img/scale-64.png")
(define the-wscale-icon #f)
(define (wscale-icon)
  (unless the-wscale-icon
    (set! the-wscale-icon (read-bitmap wscale-icon-file)))
  the-wscale-icon)
(provide wscale-icon)

(define-runtime-path edit-icon-file "../../img/edit-64.png")
(define the-edit-icon #f)
(define (edit-icon)
  (unless the-edit-icon
    (set! the-edit-icon (read-bitmap edit-icon-file)))
  the-edit-icon)
(provide edit-icon)

(define-runtime-path import-icon-file "../../img/import-64.png")
(define the-import-icon #f)
(define (import-icon)
  (unless the-import-icon
    (set! the-import-icon (read-bitmap import-icon-file)))
  the-import-icon)
(provide import-icon)

(define-runtime-path stopwatch-icon-file "../../img/stopwatch-64.png")
(define the-stopwatch-icon #f)
(define (stopwatch-icon)
  (unless the-stopwatch-icon
    (set! the-stopwatch-icon (read-bitmap stopwatch-icon-file)))
  the-stopwatch-icon)
(provide stopwatch-icon)

(define-runtime-path planner-icon-file "../../img/planner-64.png")
(define the-planner-icon #f)
(define (planner-icon)
  (unless the-planner-icon
    (set! the-planner-icon (read-bitmap planner-icon-file)))
  the-planner-icon)
(provide planner-icon)

(define-runtime-path equipment-icon-file "../../img/robot-64.png")
(define the-equipment-icon #f)
(define (equipment-icon)
  (unless the-equipment-icon
    (set! the-equipment-icon (read-bitmap equipment-icon-file)))
  the-equipment-icon)
(provide equipment-icon)

(define-runtime-path pmc-icon-file "../../img/statistics-64.png")
(define the-pmc-icon #f)
(define (pmc-icon)
  (unless the-pmc-icon
    (set! the-pmc-icon (read-bitmap pmc-icon-file)))
  the-pmc-icon)
(provide pmc-icon)

(define-runtime-path waypoints-icon-file "../../img/waypoint_map-64.png")
(define the-waypoints-icon #f)
(define (waypoints-icon)
  (unless the-waypoints-icon
    (set! the-waypoints-icon (read-bitmap waypoints-icon-file)))
  the-waypoints-icon)
(provide waypoints-icon)

(define (reports-icon) (pmc-icon))
(provide reports-icon)
