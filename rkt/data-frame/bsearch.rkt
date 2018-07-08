#lang racket/base
;; bsearch.rkt -- binary search in a sorted vector
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2018 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require racket/contract
         racket/math)

;; Search a sorted vector, VEC for a value VAL.  The vector is assumed to
;; contain sorted values, as defined by CMP-FN.  KEY, if present, selects the
;; value to compare (useful if the vector contains structures and we want to
;; search on a structure slot).  START and END define the sub-range of the
;; vector to search.
;;
;; bsearch will return an index identifying the position where VAL could be
;; inserted to keep the range sorted.  That is:
;;
;; * if VAl exists in the vector, its location is returned
;;
;; * if VAL is smaller than the first value in the range, START is returned
;;
;; * if VAL is greater than the last value in the range, END is returned (this
;; is considered out of range for the vector)
;;
;; * otherwise, an index is returned representing the location of VAL in the
;; vector (or the "best" location, if val is not found).
;;
;; NOTE: this works like the std::lower_bound() function in C++.
(define (bsearch vec val
                 #:cmp (cmp-fn <)
                 #:key (key-fn values)
                 #:start (start 0)
                 #:stop (end (vector-length vec)))

  (define (do-search start end)
    (if (= start end)
        start
        (let* ((mid (exact-truncate (/ (+ start end) 2)))
               (mid-val (key-fn (vector-ref vec mid))))
          (if (cmp-fn val mid-val)
              (do-search start mid)
              (if (cmp-fn mid-val val)
                  (do-search (+ mid 1) end)
                  mid)))))

  (let ((vlen (vector-length vec)))
    (cond ((or (< start 0) (> start vlen))
           (raise-range-error 'bsearch "vector" "starting " start vec 0 vlen))
          ((or (< end 0) (> end vlen))
           (raise-range-error 'bsearch "vector" "ending " end vec 0 vlen))
          ((> start end)
           (raise-range-error
            'bsearch "vector" "ending " end vec start vlen 0))))

  (do-search start end))


;;............................................................. provides ....

(provide/contract
 (bsearch (->* ((vectorof any/c) any/c)
               (#:cmp (-> any/c any/c boolean?)
                #:key (-> any/c any/c)
                #:start integer?
                #:stop integer?)
               integer?)))
