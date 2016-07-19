#lang racket
;; df-test.rkt -- tests for data-frame.rkt
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2016 Alex Harsanyi (AlexHarsanyi@gmail.com)
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


(require rackunit)
(require rackunit/gui)
(require rackunit/text-ui)

(require "../rkt/data-frame.rkt")

(define bsearch-tests
  (test-case "BSEARCH test cases"
    (let ([data (for/vector ([x (in-range 1 20)]) x)])
      (check equal? (bsearch data 5) 4)

      ;; Out of range
      (check equal? (bsearch data -1) 0)
      (check equal? (bsearch data 25) 19)

      ;; Edge of range
      (check equal? (bsearch data 1) 0)
      (check equal? (bsearch data 20) 19)

      ;; Inexact values
      (check equal? (bsearch data 0.9) 0)
      (check equal? (bsearch data 1.1) 1)
      (check equal? (bsearch data 18.9) 18)

      ;; Sub-range searching, basics
      (check equal? (bsearch data 5 #:start 0 #:end 3) 3)
      (check equal? (bsearch data 5 #:start 3 #:end 7) 4)
      (check equal? (bsearch data 5 #:end 3) 3)
      (check equal? (bsearch data 5 #:start 7) 7)

      ;; 1 and 0 length ranges
      (check equal? (bsearch data 5 #:start 4 #:end 5) 4)
      (check equal? (bsearch data 5 #:start 3 #:end 3) 3)

      ;; Off the grid searches
      (check equal? (bsearch data 5 #:start -100 #:end 100) 4)
      (check equal? (bsearch data 5 #:start 100 #:end 200) 19)
      (check equal? (bsearch data 1 #:start -200 #:end -100) 0)

      ;; Reversed ranges
      (check equal? (bsearch data 5 #:start 3 #:end 0) 3)
      (check equal? (bsearch data 5 #:start 7 #:end 3) 4)
      (check equal? (bsearch data 5 #:start 100 #:end -100) 4)
      (check equal? (bsearch data 5 #:start 200 #:end 100) 19)

      )

    ;; Custom comparison function
    (let ([rdata (for/vector ([x (in-range 20 1 -1)]) x)])
      (check equal? (bsearch rdata 5 #:cmp >=) 15))

    ))


(define data-series-tests
  (test-case "DATA-SERIES% test cases"

    (let* ([data (for/vector ([x (in-range 0 20)]) x)]
           [ds (new data-series% [name "ds1"] [data data])])
      (check equal? (send ds get-name) "ds1")
      (check equal? (send ds get-data) data)
      (check equal? (send ds get-count) 20)
      (check equal? (send ds count-invalid-values) 0)
      (check eq? (send ds has-valid-values) #t)
      (send ds set-sorted #t)
      (check equal? (send ds get-index 5) 5)
      (check equal? (send ds get-index 4.5) 5))

    (let* ([data #("alice" "bob" "carol" "david" "ethel" "frank" "jasmin")]
           (ds (new data-series% [name "ds2"] [data data]
                    [sorted? #t] [cmp-fn string<=?])))

      (check equal? (send ds get-index "david") 3)
      (check equal? (send ds get-index "george") 6))

      ))

(define data-frame-tests
  (test-case "DATA-FRAME% test cases"
    (let* ([data1 (for/vector ([x (in-range 1 20)]) x)]
           [ds1 (new data-series% [name "ds1"] [data data1])]
           [data2 (for/vector ([x (in-range 20 39)]) x)]
           [ds2 (new data-series% [name "ds2"] [data data2])]
           [df (new data-frame% [series (list ds1 ds2)])])
      (check equal? (send df contains? "ds1") #t)
      (check equal? (send df contains? "ds1" "ds2") #t)
      (check equal? (send df contains? "ds1" "ds-non-exist") #f)
      (check equal? (send df contains/any? "ds1" "ds-non-exist") #t)

      ;; Adding a derived series
      (send df add-derived-series
            "ds3"
            '("ds1" "ds2")
            (lambda (val)
              (match-define (vector v1 v2) val)
              (+ v1 v2)))

      (check equal? (send df contains? "ds3") #t)

      ;; check that it contains the right values.  note that select* packs
      ;; individual values in a vector.
      (for ([d (send df select* "ds1" "ds2" "ds3")])
        (match-define (vector d1 d2 d3) d)
        (check equal? (+ d1 d2) d3))

      ;; Add a derived series using deltas.  The lambda receives two values:
      ;; the current one and the previous one.  In the first call, the
      ;; previous value is #f.
      (send df add-derived-series
            "ds4"
            '("ds1")
            (lambda (prev-val val)
              ;; First value has prev-val #f
              (if prev-val
                  (let ()
                    (match-define (vector pv) prev-val)
                    (match-define (vector v) val)
                    (- v pv))
                  (let ()
                    (match-define (vector v) val)
                    v))))
      
      (check equal? (send df contains? "ds4") #t)

      ;; check that it contains the right values, note that select returns
      ;; individual values, not vectors.
      (for ([d (send df select "ds4")])
        (check equal? d 1))

      )))

(define ds-tests
  (test-suite
   "all data frame tests"
   bsearch-tests
   data-series-tests
   data-frame-tests
   ))

(module+ test
  (run-tests ds-tests))

;; TODO
;;; * df-statistics for series with #f in them

