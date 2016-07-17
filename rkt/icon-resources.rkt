#lang racket/base
;; icon-resources.rkt -- hold all (most) icon bitmap objects used by the
;; application, avoids re-loading them in each module that needs them.
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015 Alex Harsanyi (AlexHarsanyi@gmail.com)
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

(define-runtime-path weather-icon-file "../img/partly_cloudy_day-64.png")
(define weather-icon (read-bitmap weather-icon-file))
(provide weather-icon)

(define-runtime-path sql-export-icon-file "../img/data_configuration-64.png")
(define sql-export-icon (read-bitmap sql-export-icon-file))
(provide sql-export-icon)

(define-runtime-path wscale-icon-file "../img/scale-64.png")
(define wscale-icon (read-bitmap wscale-icon-file))
(provide wscale-icon)

(define-runtime-path edit-icon-file "../img/edit-64.png")
(define edit-icon (read-bitmap edit-icon-file))
(provide edit-icon)

(define-runtime-path import-icon-file "../img/import-64.png")
(define import-icon (read-bitmap import-icon-file))
(provide import-icon)

(define-runtime-path stopwatch-icon-file "../img/stopwatch-64.png")
(define stopwatch-icon (read-bitmap stopwatch-icon-file))
(provide stopwatch-icon)

(define-runtime-path planner-icon-file "../img/planner-64.png")
(define planner-icon (read-bitmap planner-icon-file))
(provide planner-icon)

(define-runtime-path equipment-icon-file "../img/robot-64.png")
(define equipment-icon (read-bitmap equipment-icon-file))
(provide equipment-icon)

(define-runtime-path pmc-icon-file "../img/statistics-64.png")
(define pmc-icon (read-bitmap pmc-icon-file))
(provide pmc-icon)

(define reports-icon pmc-icon)
(provide reports-icon)
