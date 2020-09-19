#lang info
;; info.rkt -- package listing all dependencies of ActivityLog2
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2020 Alex Harsányi <AlexHarsanyi@gmail.com>
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

;; When packages from this folder are indexed (see ../docs/README.md), all
;; ActivityLog2 dependencies can be installed with a single:
;;
;;     raco pkg install al2-metapkg
;;
;; NOTE: this package is not published in the racket package catalog, as such
;; if AL2 dependencies are to be installed from the package catalog, they need
;; to be installed separately.

(define collection "al2-dependencies")
(define deps '("base"

               "the-application"
               "tzinfo"
               "tzgeolookup"
               "data-frame"
               "plot-container"
               "gui-widget-mixins"
               "map-widget"
               "geoid"))
(define build-deps '("al2-test-runner"))
(define scribblings '())
(define pkg-desc "Meta package to install all ActivityLog2 dependencies")
(define version "0.0")
(define pkg-authors '(aharsanyi))
