#lang racket/base

;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; humidex.rkt -- "feels-like" temperature
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2022 Alex Harsányi <AlexHarsanyi@gmail.com>
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

;; Compute a "Feels like" temperature based on TEMPERATURE and DEW-POINT, this
;; is done using the formula from https://en.wikipedia.org/wiki/Humidex

(require racket/contract)

(define (humidex air-temperature dew-point)
  (let* ([dewpk (+ dew-point 273.15)]
         [e (* 6.11 (exp (* 5417.7530 (- (/ 1 273.16) (/ 1 dewpk)))))]
         [h (* 5/9 (- e 10.0))])
    (+ air-temperature h)))

(provide/contract
 [humidex (-> rational? rational? rational?)])
