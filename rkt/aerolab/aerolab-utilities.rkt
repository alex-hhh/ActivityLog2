#lang racket/base

;; aerolab-utilities.rkt
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2023 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require racket/format
         "../fmt-util.rkt")

(provide
 min-text-field-width
 min-cda
 max-cda
 min-crr
 max-crr
 min-wind-speed
 max-wind-speed
 min-wind-direction
 max-wind-direction
 slider-clicks

 validate-positive-integer
 validate-positive-rational
 validate-non-negative-rational
 validate-rational
 validate-rational-between
 wind-direction->string)

(define min-text-field-width 120)
(define min-cda 0.1)
(define max-cda 0.9)
(define min-crr 0.002)
(define max-crr 0.01)
(define min-wind-speed 0)
(define max-wind-speed 50)              ; km/h
(define min-wind-direction -180)
(define max-wind-direction 180)
(define slider-clicks 10000)

(define (validate-positive-integer v)
  (define n (string->number v))
  (and n (exact-integer? n) (> n 0) n))

(define (validate-positive-rational v)
  (define n (string->number v))
  (and n (rational? n) (> n 0) n))

(define (validate-non-negative-rational v)
  (define n (string->number v))
  (and n (rational? n) (>= n 0) n))

(define (validate-rational v)
  (define n (string->number v))
  (and n (rational? n) n))

(define ((validate-rational-between minimum maximum) v)
  (define n (string->number v))
  (and n (rational? n) (>= n minimum) (<= n maximum) n))

(define (wind-direction->string n)
  (if (and n (rational? n) (>= n -180) (<= n 180))
      (format "~a° (~a)" (~r n #:precision 1) (degrees->wind-rose n))
      (~a n)))
