#lang racket/base

;; SPDX-License-Identifier: GPL-3.0-or-later
;; fiets-score.rkt -- calculate the FIETS score for a segment
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


(require data-frame
         racket/math
         racket/contract)

;; Calculate the FIETS score, which indicates the difficulty of a climb.  See
;; "docs/climbs.md" for a discussion on how it is calculated and why.  Return
;; #f if the score cannot be calculated.
(define (fiets-score df #:start (start 0) #:stop (stop (df-row-count df)))
  (and (df-contains? df "dst" "grade")
       (for/fold ([last-dst #f]
                  [fiets-score 0]
                  #:result fiets-score)
                 ([(dst grade) (in-data-frame df "dst" "grade" #:start start #:stop stop)])
         (cond
           ((and last-dst dst grade (> grade 0.0))
            (define Δ-dst (- dst last-dst))
            (define chunk (* grade grade Δ-dst 1e-5))
            (values dst (+ fiets-score chunk)))
           ((and last-dst (not grade))
            (values last-dst fiets-score))
           (#t
            (values dst fiets-score))))))

;; Return a string representing the climb category based on the FIETS score.
;; See "docs/climbs.md" for the table.  This is intended for display purposes.
(define (fiets-score->climb-category fs)
  (cond ((> fs 6.5) "HC")
        ((> fs 5.0) "CAT 1")
        ((> fs 3.5) "CAT 2")
        ((> fs 2.0) "CAT 3")
        ((> fs 0.5) "CAT 4")
        ((> fs 0.25) "CAT 5")
        (#t "")))

(provide/contract
 (fiets-score (->* (data-frame?)
                   (#:start (or/c zero? positive-integer?)
                    #:stop (or/c zero? positive-integer?))
                   (or/c zero? positive? #f)))
 (fiets-score->climb-category (-> (or/c zero? positive?) string?)))

