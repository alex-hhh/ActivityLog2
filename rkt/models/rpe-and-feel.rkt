#lang racket/base

;; SPDX-License-Identifier: GPL-3.0-or-later
;; rpe-and-feel.rkt -- utilities to deal with RPE and "athlete feel" metrics
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2025 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require racket/math)

(provide rpe->string
         feel->string
         index->feel
         feel->index
         feel-string-values)

;; Convert a RPE value into the corresponding string.  RPE values are from 1
;; (rest) to 10 (maximal effort), with 0 representing "not specified", however
;; we are lenient on what is passed in as RPE and we output "Unknwon" for
;; unknown values.
(define (rpe->string rpe)
  (if (and (rational? rpe) (>= rpe 0))
      (case (exact-floor rpe)
        ((0) "Not Specified (0)")
        ((1) "Rest (1)")
        ((2) "Very Easy (2)")
        ((3) "Easy (3)")
        ((4) "Confortable (4)")
        ((5) "Somewhat Difficult (5)")
        ((6) "Difficult (6)")
        ((7) "Hard (7)")
        ((8) "Very Hard (8)")
        ((9) "Extremely Hard (9)")
        ((10) "Maximal (10)")
        (else (format "Unknwon (~a)" rpe)))
      (format "Unknwon (~a)" rpe)))

;; "Feel" values are defined by Garmin and appear to be from 0 (very weak) to
;; 10 (very strong), but there is only 5 of them with "5.0" representing
;; "normal".  Here we break up the 0..10 range into 5 sections and we also use
;; #f for "Not specified", with any other value being "Unknown"
(define (feel->string feel)
  (cond
    ((eq? feel #f) "Not Specified")
    ((and (rational? feel) (>= feel 0))
     (cond
       ((< feel 1.25) "Very Weak")     ; I think this is stored as 0
       ((< feel 3.75) "Weak")          ; stored as 2.5
       ((< feel 6.25) "Normal")        ; stored as 5.0
       ((< feel 8.75) "Strong")        ; stored as 7.5
       (else "Very Strong")))
    (#t (format "Unknwon (~a)" feel))))

(define feel-index-to-value #(#f 0 2.5 5.0 7.5 10.0))

;; Convert an index (from a choice% widget) into a "feel" value to be stored
;; in the database.
(define (index->feel index)
  (vector-ref feel-index-to-value index))

;; Convert a feel value into an index of a choice% widget array.
(define (feel->index feel)
  (cond
    ((eq? feel #f) 0)
    ((and (rational? feel) (>= feel 0))
     (cond
       ((< feel 1.25) 1)     ; I think this is stored as 0
       ((< feel 3.75) 2)     ; stored as 2.5
       ((< feel 6.25) 3)     ; stored as 5.0
       ((< feel 8.75) 4)     ; stored as 7.5
       (else 5)))
    (#t 0)))

(define (feel-string-values)
  (for/list ([f (in-vector feel-index-to-value)])
    (feel->string f)))
