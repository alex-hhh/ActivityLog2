#lang racket/base
;; df-test.rkt -- tests for data-frame.rkt
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2016, 2018 Alex Harsányi <AlexHarsanyi@gmail.com>
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


(require rackunit
         racket/math
         racket/list
         racket/base
         racket/match
         racket/port
         racket/vector
         math/statistics
         db)

(require "../rkt/data-frame/bsearch.rkt"
         "../rkt/data-frame/series.rkt"
         "../rkt/data-frame/exn.rkt"
         "../rkt/data-frame/spline.rkt"
         "../rkt/data-frame/df.rkt"
         "../rkt/data-frame/sql.rkt"
         "../rkt/data-frame/csv.rkt"
         "../rkt/data-frame/gpx.rkt"
         "../rkt/data-frame/statistics.rkt"
         "../rkt/data-frame/meanmax.rkt"
         "../rkt/data-frame/histogram.rkt"
         "../rkt/data-frame/rdp-simplify.rkt"
         "../rkt/data-frame/least-squares-fit.rkt"
         "../rkt/data-frame/scatter.rkt")


;;.............................................................. bsearch ....

(define bsearch-tests
  (test-suite
   "`bsearch` test suite"

   (test-case "`bsearch` test cases"
     (define empty (make-vector 0))
     (define data (for/vector ([x (in-range 1 21)]) x)) ; 20 element vector

     ;; Simple search
     (check equal? (bsearch empty 5) 0)
     (check equal? (bsearch data 5) 4)
     ;; Search for out of range values
     (check equal? (bsearch data -1) 0)
     (check equal? (bsearch data 25) 20)
     ;; Search for values that are at the end of the range
     (check equal? (bsearch data 1) 0)
     (check equal? (bsearch data 20) 19)
     ;; Search for inexact values -- the best place will be found
     (check equal? (bsearch data 0.9) 0)
     (check equal? (bsearch data 1.1) 1)
     (check equal? (bsearch data 18.9) 18)

     ;; Sub-range searching, basics
     (check equal? (bsearch data 5 #:start 0 #:stop 3) 3)
     (check equal? (bsearch data 5 #:start 3 #:stop 7) 4)
     (check equal? (bsearch data 5 #:stop 3) 3)
     (check equal? (bsearch data 5 #:start 7) 7)

     ;; Searches in ranges of 1 and 0 length ranges
     (check equal? (bsearch data 5 #:start 4 #:stop 5) 4)
     (check equal? (bsearch data 5 #:start 3 #:stop 3) 3)

     ;; ;; Off the grid searches
     (check-exn
      exn:fail:contract?
      (lambda ()
        ;; start is out of range, should raise exception
        (bsearch data 5 #:start -100 #:stop 5)))

     (check-exn
      exn:fail:contract?
      (lambda ()
        ;; end is out of range, should raise exception
        (bsearch data 5 #:start 0 #:stop 200)))

     (check-exn
      exn:fail:contract?
      (lambda ()
        ;; start is after end
        (bsearch data 5 #:start 5 #:stop 1)))

     (define rdata (for/vector ([x (in-range 20 1 -1)]) x))
     (check equal? (bsearch rdata 5 #:cmp >) 15)

     )))


;;................................................................ series ....

(define series-tests
  (test-suite
   "`series` test suite"

   (test-case "`series` test cases"

     ;; Wrong sort order
     (check-exn
      exn:fail:data-frame?
      (lambda () (make-series "col1" #:data #(1 2 3) #:cmpfn >)))

     ;; Wrong contract
     (check-exn
      exn:fail:data-frame?
      (lambda () (make-series "col1" #:data #(1 2 3) #:contract string?)))

     (define c1 (make-series "col1" #:capacity 10))
     (check = (series-size c1) 0)
     (check = (series-free-space c1) 10)
     (check-true (series-empty? c1))
     (series-reserve-space c1 100)
     (check = (series-free-space c1) 100)

     ;; NOTE: c1 is empty
     (check-false (series-has-non-na? c1))
     (check-false (series-has-na? c1))

     (define c2 (make-series "col2" #:data #(1 2 3) #:contract integer? #:cmpfn <))
     (check = (series-size c2) 3)
     (check = (series-free-space c2) 0)
     (check = (series-ref c2 1) 2)
     (check-exn
      exn:fail:contract?
      (lambda ()
        (series-ref c2 10)))
     (series-push-back c2 5)
     (check = (series-size c2) 4)
     (check = (series-ref c2 3) 5)
     (check-exn
      exn:fail:contract?
      ;; cannot add a non sorted element
      (lambda () (series-push-back c2 1)))

     (check-false (series-has-na? c2))
     (check-true (series-has-non-na? c2))

     (check equal? (for/list ((x (in-series c1))) x) '())
     (check equal? (for/list ((x (in-series c2))) x) '(1 2 3 5))

     (check equal? (series-index-of c2 3) 2)

     (check-not-exn
      (lambda ()
        (series-bless-sorted c2 <)))

     (check-exn
      exn:fail:data-frame?
      (lambda ()
        (series-bless-contract c2 string?)))

     (check-exn
      exn:fail:contract?
      (lambda ()
        ;; c2 contains only integers
        (series-push-back c2 "abc")))

     (check-exn
      exn:fail:contract?
      (lambda ()
        ;; c2 contains only integers
        (series-push-back c2 6.5)))

     (check-not-exn
      (lambda ()
        (series-bless-contract c2 real?)))

     (check-not-exn
      (lambda ()
        ;; c2 now contains reals
        (series-push-back c2 6.5)))

     (check-exn
      exn:fail:data-frame?
      (lambda ()
        (series-bless-sorted c2 >)))

     (check equal? (series-na-count c2) 0)    ; no NA values in C2
     (check equal? (series-index-of c2 3) 2)

     (check-exn
      exn:fail:contract?
      (lambda ()
        (series-push-back c2 4)))
     (check = (series-size c2) 5)
     (check-not-exn
      (lambda ()
        (series-push-back c2 7)))

     (check-exn
      exn:fail:contract?
      (lambda ()
        ;; c2 is marked sorted, as such we cannot set a NA value
        (series-set! c2 3 #f)))

     (check-not-exn
      (lambda ()
        ;; remove sort constrain on c2
        (series-bless-sorted c2 #f)))

     (check-not-exn
      (lambda ()
        (series-set! c2 3 #f)))

     (check equal? (series-ref c2 3) #f)
     (check equal? (series-na-count c2) 1) ; C2 has one NA value

     )))


;;............................................................... spline ....



(define spline-tests
  (test-suite
   "`spline` test suite"

   (test-case "`spline` test cases"
     (define data-points '(#(-1 0.5) #(0 0) #(3 3)))
     (define fn (spline data-points))
     (check equal? (fn 0) 0)
     (check-pred real? (fn 0.5))
     (check-pred real? (fn 1.0))
     (check-pred real? (fn -2))         ; outside the points range
     (check-pred real? (fn 10))         ; outside the points range

     (define data-points2 #(#(-1 0.5) #(0 0) #(3 3)))
     (define fn2 (spline data-points))
     (check equal? (fn2 0) 0)
     (check-pred real? (fn2 0.5))
     (check-pred real? (fn2 1.0))
     (check-pred real? (fn2 -2))        ; outside the points range
     (check-pred real? (fn2 10))        ; outside the points range

     ;;
     ;; This will plot the spline and the know points, useful for
     ;; visualization
     ;;
     ;; (require plot)
     ;; (send (plot-frame (list (function fn)
     ;;                    (points data-points))
     ;;              #:x-min -5 #:x-max 5) show #t)

     )))


;;........................................................... data-frame ....

;; These functions are not tested...

;;  (df-put-property (-> data-frame? symbol? any/c any/c))
;;  (df-get-property (->* (data-frame? symbol?) ((-> any/c)) any/c))
;;  (df-del-property (-> data-frame? symbol? any/c))
;;  (df-set-default-weight-series (-> data-frame? string? any/c))
;;  (df-get-default-weight-series (-> data-frame? (or/c #f string?)))
;;  (valid-only (-> any/c boolean?))

(define df-tests
  (test-suite
   "`df` test suite"

   (test-case "`df` test cases"

     ;; Basic construction and item access
     (define df (make-data-frame))
     ;; This data frame is empty
     (check equal? (df-series-names df) '())
     (check equal? (df-property-names df) '())
     (check equal? (df-row-count df) 0)

     (define c1 (make-series "col1" #:data #(1 2 3 4) #:contract integer? #:cmpfn <))
     (check-not-exn
      (lambda ()
        (df-add-series df c1)))
     (check equal? (df-count-na df "col1") 0)
     (define c2 (make-series "col2" #:data #(3 2 1 0) #:contract integer? #:cmpfn >))
     (check-not-exn
      (lambda ()
        (df-add-series df c2)))
     (check-not-exn
      (lambda ()
        (df-add-derived
         df "col3" '("col1" "col2")
         (lambda (v)
           (match-define (list c1 c2) v)
           (+ c1 c2)))))
     (check-exn
      exn:fail:data-frame?
      (lambda ()
        ;; This will fail as col0 has 2 rows
        (define c0 (make-series "col0" #:data #(1 2)))
        (df-add-series df c0)))
     (check = (df-row-count df) 4)
     (check-true (df-contains? df "col1" "col2"))
     ;; NOTE: col4 does not exist
     (check-false (df-contains? df "col1" "col4"))
     (check-true (df-contains/any? df "col1" "col4"))
     (check-true (df-contains? df "col3"))
     (check-not-exn
      (lambda ()
        (df-del-series df "col3")))
     (check-false (df-contains? df "col3"))
     ;; Deleting a non-existent series should be OK
     (check-not-exn
      (lambda ()
        (df-del-series df "col3")))

     (check equal? (df-select df "col1") #(1 2 3 4))
     (check equal? (df-select df "col1" #:filter odd?) #(1 3))
     (check equal? (df-select df "col1" #:start 1 #:stop 3) #(2 3))
     (check equal? (df-select* df "col1" "col2") #(#(1 3) #(2 2) #(3 1) #(4 0)))
     (check equal? (df-select* df "col1" "col2" #:start 1 #:stop 3) #(#(2 2) #(3 1)))

     (define result (df-select* df "col1" "col2" #:start 1 #:stop 2
                                #:filter (lambda (val)
                                           (check-pred vector? val)
                                           (check = (vector-length val) 2)
                                           (match-define (vector c1 c2) val)
                                           (check = c1 2)
                                           (check = c2 2)
                                           #f)))
     (check-true (and (vector? result) (= (vector-length result) 0)))

     (let ()                            ; (test-case "`df-map` test cases"
       (check equal?
              (df-map df
                      '("col1" "col2")
                      (lambda (prev current)
                        (if prev
                            ;; A delta series on col1, col2
                            (match-let (((list pc1 pc2) prev)
                                        ((list c1 c2) current))
                              (list (- c1 pc1) (- c2 pc2)))
                            'none)))
              #(none (1 -1) (1 -1) (1 -1)))

       (check equal?
              (df-map df
                      '("col1" "col2")
                      (lambda (current)
                        (match-let (((list c1 c2) current))
                          (+ c1 c2))))
              #(4 4 4 4))

       (check equal?
              (df-map df
                      '("col1" "col2")
                      (lambda (prev current)
                        (if prev
                            ;; A delta series on col1, col2
                            (match-let (((list pc1 pc2) prev)
                                        ((list c1 c2) current))
                              (list (- c1 pc1) (- c2 pc2)))
                            'none))
                      #:start 1 #:stop 3)
              #(none (1 -1))))

     (let ()                            ; test-case "`df-for-each` test cases"
       (define result1 '())
       (df-for-each df
                    '("col1" "col2")
                    (lambda (prev current)
                      (define v (if prev
                                    ;; A delta series on col1, col2
                                    (match-let (((list pc1 pc2) prev)
                                                ((list c1 c2) current))
                                      (list (- c1 pc1) (- c2 pc2)))
                                    'none))
                      (set! result1 (cons v result1)))
                    #:start 1 #:stop 3)
       (check equal? result1 '((1 -1) none))

       (define result2 '())
       (df-for-each df
                    '("col1" "col2")
                    (lambda (prev current)
                      (define v (if prev
                                    ;; A delta series on col1, col2
                                    (match-let (((list pc1 pc2) prev)
                                                ((list c1 c2) current))
                                      (list (- c1 pc1) (- c2 pc2)))
                                    'none))
                      (set! result2 (cons v result2))))
       (check equal? result2 '((1 -1) (1 -1) (1 -1) none))

       (define result3 '())
       (df-for-each df
                    '("col1" "col2")
                    (lambda (current)
                      (define v (match-let (((list c1 c2) current))
                                  (+ c1 c2)))
                      (set! result3 (cons v result3))))
       (check equal? result3 '(4 4 4 4)))

     (let ()                            ; test-case "`df-fold` test cases"
       (check equal? (df-fold df
                              '("col1" "col2") ; series
                              0                ; initial value
                              (lambda (accumulator v)
                                (match-define (list c1 c2) v)
                                (+ accumulator c1 c2)))
              16)
       (check equal? (df-fold df
                              '("col1" "col2") ; series
                              0                ; initial value
                              (lambda (accumulator prev current)
                                (if prev
                                    (match-let (((list c1 c2) prev)
                                                ((list c3 c4) current))
                                      (+ accumulator c1 c2 c3 c4))
                                    accumulator)))
              24)
       (check equal? (df-fold df
                              '("col1" "col2") ; series
                              0                ; initial value
                              (lambda (accumulator v)
                                (match-define (list c1 c2) v)
                                (+ accumulator c1 c2))
                              #:start 1 #:stop 4)
              12)
       (check equal? (df-fold df
                              '("col1" "col2") ; series
                              0                ; initial value
                              (lambda (accumulator prev current)
                                (if prev
                                    (match-let (((list c1 c2) prev)
                                                ((list c3 c4) current))
                                      (+ accumulator c1 c2 c3 c4))
                                    accumulator))
                              #:start 1 #:stop 4)
              16))

     (let ()                     ; test-case "`in-data-frame/list` test cases"
       (define result1 '())
       (for ((item (in-data-frame/list df "col1" "col2" #:start 1 #:stop 3)))
         (set! result1 (cons item result1)))
       (check equal? result1 '((3 1) (2 2)))
       (define result2 '())
       (for ((item (in-data-frame/list df "col1" "col2")))
         (set! result2 (cons item result2)))
       (check equal? result2 '((4 0) (3 1) (2 2) (1 3)))
       (define result3 '())
       ;; See if we can select backwards
       (for ((item (in-data-frame/list df "col1" "col2" #:start 2 #:stop -1)))
         (set! result3 (cons item result3)))
       (check equal? result3 '((1 3) (2 2) (3 1))))

     (let ()                          ; test-case "`in-data-frame` test-cases"
       (define sum1 (for/sum (((c1 c2) (in-data-frame df "col1" "col2" #:start 1 #:stop 3)))
                      (+ c1 c2)))
       (check = sum1 8)
       (define sum2 (for/sum (((c1 c2) (in-data-frame df "col1" "col2")))
                      (+ c1 c2)))
       (check = sum2 16))

     (check = (df-index-of df "col1" 2) 1)
     (check = (df-index-of df "col1" -1) 0)
     (check = (df-index-of df "col1" 100) 4)
     (check equal? (df-index-of* df "col1" 2 -1 100) '(1 0 4))

     ;; col1: 1 2 3 4; col2: 3 2 1 0
     (check equal? (df-lookup df "col1" "col2" 3) 1)
     (check equal? (df-lookup df "col1" '("col1" "col2") 3) #(3 1))
     (check equal? (df-lookup* df "col1" "col2" 1 4) '(3 0))
     (check equal? (df-lookup* df "col1" '("col1" "col2") 1 4) '(#(1 3) #(4 0)))

     (check equal? (df-lookup/interpolated df "col1" "col2" 2.5) 1.5)

     (check = (df-ref df 1 "col1") 2)
     (check equal? (df-ref* df 1 "col1" "col2") #(2 2))

     (check-exn
      exn:fail:contract?
      (lambda ()
        ;; only integer? values can be set (there is a #:contract on this
        ;; column
        (df-set! df 0 1.5 "col1")))
     (check-exn
      exn:fail:contract?
      (lambda ()
        ;; will break sort order
        (df-set! df 0 100 "col1")))
     (check-not-exn
      (lambda ()
        (df-set! df 0 -1 "col1")))
     ;; Check that it was indeed set!
     (check = (df-ref df 0 "col1") -1)

     (check-not-exn
      (lambda ()
        (df-add-lazy
         df "col5" '("col1" "col2")
         (lambda (v)
           (match-define (list c1 c2) v)
           (+ c1 c2)))))
     (check-true (df-contains? df "col5"))
     ;; Col5 will be materialized now
     (check equal? (df-select df "col5") #(2 4 4 4))

     (define c6 (make-series "col6" #:data #(1 2 3 4) #:contract integer?))
     (df-add-series df c6)
     (check-not-exn
      (lambda ()
        (df-set-sorted df "col6" <)))
     (check-exn
      exn:fail:data-frame?
      (lambda ()
        ;; wrong sort order
        (df-set-sorted df "col6" >)))

     (check-not-exn
      (lambda ()
        ;; note: this probably needs more tests...
        (df-shallow-copy df)))

     )))

(define (with-database thunk)
  (let ((db (sqlite3-connect #:database 'memory #:mode 'create)))
    (dynamic-wind
      (lambda () (void))
      ;; NOTE: cannot really catch errors as error trace will loose context
      (lambda () (thunk db))
      (lambda () (disconnect db)))))

(define dfdb-tests
  (test-suite
   "`dfdb` test suite"

   (test-case "`dfdb` test cases"
     (with-database
       (lambda (db)
         (query-exec db "create table T(a real, b real, c text)")
         (query-exec db "insert into T(a, b, c) values (?, ?, ?)" 1 2 "alpha")
         (query-exec db "insert into T(a, b, c) values (?, ?, ?)" 2 4 "beta")
         (query-exec db "insert into T(a, b, c) values (?, ?, ?)" 4 8 "gamma")
         (query-exec db "insert into T(a, b, c) values (?, ?, ?)" 8 16 "delta")

         (define df (df-read/sql db "select a, b, c from T order by a"))
         (check-true (df-contains? df "a" "b" "c"))
         (check equal? (df-select df "a") #(1.0 2.0 4.0 8.0))
         (check equal? (df-select df "b") #(2.0 4.0 8.0 16.0))
         (check equal? (df-select df "c") #("alpha" "beta" "gamma" "delta")))))))

(define csv-test-file "./csv-tests-t1.csv")

(define csv-tests
  (test-suite
   "`csv` test suite"
   #:after (lambda ()
             (with-handlers (((lambda (e) #t) (lambda (e) #t)))
               (delete-file csv-test-file)))
   (test-case "`csv` test cases"
     (define df (make-data-frame))
     (define s1 (make-series "s,1" #:data #(1 1/2 3 #f 5) #:na #f))
     (define s2 (make-series "s,2" #:data #("one" "two" "th\"ree" #f "") #:na ""))
     (df-add-series df s1)
     (df-add-series df s2)

     (define text (call-with-output-string
                   (lambda (out) (df-write/csv df out))))
     (check equal? text "\"s,1\",\"s,2\"\n1,\"one\"\n0.5,\"two\"\n3,\"th\"\"ree\"\n,\"#f\"\n5,\n")

     (define text2 (call-with-output-string
                    (lambda (out) (df-write/csv df out "s,1" #:start 1 #:stop 3))))
     (check equal? text2 "\"s,1\"\n0.5\n3\n")

     (define df1 (df-read/csv "./test-data/sample.csv"))
     (check equal? (sort (df-series-names df1) string<?) '("four" "one" "three" "two"))
     (check = (df-row-count df1) 5)
     (check equal? (df-select* df1 "one" "two" "three" "four")
            #(#(1 2 3 4)
              #(4 5 6 "abc")
              #(7 8 9 "def,gh")
              #(10 11 12 "a,bc\" 123 \"d\"ef")
              #(14 15 #f #f)))

     ;; Try writing it out to file and reading from an input port, this is a
     ;; slightly different code path than writing to an output port.
     (check-not-exn
      (lambda ()
        (df-write/csv df csv-test-file)))
     (check-not-exn
      (lambda ()
        (call-with-input-string text (lambda (in) (df-read/csv in)))))

     ;; This CSV file contains "-" in the "empty" cells, strip them out when
     ;; reading them.
     (let ((df (df-read/csv "./test-data/sample2.csv" #:na "-")))
       (check > (df-count-na df "two") 0))

     )))

(define gpx-test-file "./gpx-tests-t1.gpx")

(define gpx-tests
  (test-suite
   "`gpx` test suite"
   #:after (lambda ()
             (with-handlers (((lambda (e) #t) (lambda (e) #t)))
               (delete-file gpx-test-file)))
   (test-case "`gpx` test cases"
     (define df (df-read/gpx "./test-data/sample.gpx"))
     (check-true (df-contains? df "lat" "lon" "alt" "dst" "timestamp"))
     (check > (df-row-count df) 0)

     (define df-1136 (df-read/csv "./test-data/track-data-1136.csv"))
     (check-true (df-contains? df-1136 "lat" "lon" "alt" "timestamp"))
     (check > (df-row-count df-1136) 0)
     (df-write/gpx df-1136 gpx-test-file)
     (define df-1136-2 (df-read/gpx gpx-test-file))

     ;; Check that we read back what we wrote out...
     (for (([lat1 lon1 alt1 ts1] (in-data-frame df-1136 "lat" "lon" "calt" "timestamp"))
           ([lat2 lon2 alt2 ts2] (in-data-frame df-1136-2 "lat" "lon" "alt" "timestamp")))
       (check < (abs (- lat1 lat2)) 1e-5)
       (check < (abs (- lon1 lon2)) 1e-5)
       (check < (abs (- alt1 alt2)) 1e-5)
       (check < (abs (- ts1 ts2)) 1e-5))

     ;; Try writing and reading from an output port, this is a slightly
     ;; different code path from reading and writing to file...
     (define str #f)
     (check-not-exn
      (lambda ()
        (set! str (call-with-output-string
                   (lambda (out) (df-write/gpx df-1136 out))))))
     (check-not-exn
      (lambda ()
        (call-with-input-string str (lambda (in) (df-read/gpx in)))))

     )))

(define stats+mmax-tests
  (test-suite
   "`statistics + meanmax` test suite"
   (test-case "`statistics + meanmax` test cases"

     (define df (df-read/csv "./test-data/track-data-1136.csv"))
     (check-not-exn
      (lambda ()
        (df-set-default-weight-series df #f)))

     (define s (df-statistics df "spd"))
     (check < (abs (- (statistics-mean s) 0.88)) 1e-2)

     ;; This session data contains data that is sampled at irregular
     ;; intervals.  A simple average (where the weight series is #f) will not
     ;; be correct, since each sample "counts" a variable amount to the total.
     ;; Using the "timer" series as the weight is the correct way to calculate
     ;; average speed, using the "dst" series as the weight is the correct way
     ;; to calculate average pace, see also
     ;; https://github.com/alex-hhh/ActivityLog2/issues/17
     ;;
     ;; If data would have been sampled at regular intervals, a simple
     ;; unweighted average would do.

     (check-not-exn
      (lambda ()
        (df-set-default-weight-series df "timer")))

     (define w (df-statistics df "spd"))
     (check < (abs (- (statistics-mean w) 0.81)) 1e-2)

     ;; TODO: need better tests for the quantile.  This really tests that we
     ;; can run the function in its basic form

     (define q (df-quantile df "spd"))
     (check equal? (length q) 5)

     (check-not-exn
      (lambda()
        (define mmax (df-mean-max df "spd"))
        (check > (length mmax) 0)       ; need a better test
        ))


     )))

(define histogram-tests
  (test-suite
   "`df-histogram` test suite"
   (test-case "`df-histogram` test cases"

     (define df-1136 (df-read/csv "./test-data/track-data-1136.csv"))

     (let ((h (df-histogram df-1136 "spd" #:bucket-width 1)))
       (check = (vector-length h) 3)
       (let ((n (for/sum ([item (in-vector h)]) (vector-ref item 1))))
         (check = n (df-row-count df-1136))))

     (let ((h (df-histogram df-1136 "spd" #:bucket-width 0.01)))
       (check > (vector-length h) 1)
       (let ((n (for/sum ([item (in-vector h)]) (vector-ref item 1))))
         (check = n (df-row-count df-1136))))

     ;; Add a string series classifying the heart rate.  We get a histogram of
     ;; this classification and check that too...
     (df-add-derived df-1136 "spd-tag" '("spd")
                     (lambda (val)
                       (match-define (list spd) val)
                       (cond ((< spd 1.0) "low")
                             ((> spd 1.1) "high")
                             (#t "med"))))
     (let ((h (df-histogram df-1136 "spd-tag")))
       (check = (vector-length h) 3)      ; the three tags
       (let ((n (for/sum ([item (in-vector h)]) (vector-ref item 1))))
         (check = n (df-row-count df-1136))))

     )))


(define rdp-simplify-tests
  (test-suite
   "`rdp-simplify` test suite"
   (test-case "`rdp-simplify` test cases"

     ;; Degenerate cases for 0, 1, 2 vector sizes.  In these cases, the
     ;; function should just return the input vector

     (define vzero (vector))
     (check-not-exn
      (lambda ()
        (define result (rdp-simplify vzero))
        (check = (vector-length result) 0)))

     (define vone (vector (vector 0 1)))
     (check-not-exn
      (lambda ()
        (define result (rdp-simplify vone))
        (check = (vector-length result) 1)
        (check equal? (vector-ref result 0) (vector 0 1))))

     (define vtwo (vector (vector 0 1) (vector 0 2)))
     (check-not-exn
      (lambda ()
        (define result (rdp-simplify vtwo))
        (check = (vector-length result) 2)
        (check equal? (vector-ref result 0) (vector 0 1))
        (check equal? (vector-ref result 1) (vector 0 2))))

     ;; NOTE: it would be nice if we could test that rdp-simplify actually did
     ;; a sane simplification, rather than just reduce the number of points...

     (define df-1136 (df-read/csv "./test-data/track-data-1136.csv"))
     (define data (df-select* df-1136 "timer" "spd"))

     (define data-1 (rdp-simplify data #:epsilon 0.01))
     (define data-2 (rdp-simplify data #:epsilon 0.02))
     (define data-3 (rdp-simplify data #:epsilon 0.03))

     (check < (vector-length data-2) (vector-length data-1))
     (check < (vector-length data-3) (vector-length data-2))

     (define nitems (vector-length data))
     (define test-point (exact-truncate (/ nitems 2))) ; midway point
     (define data-5 (rdp-simplify data #:epsilon 0.03
                                  #:keep-positions
                                  (list test-point
                                        (sub1 nitems) ; last one
                                        (+ nitems 5) ; out of range
                                        )))
     ;; The test-point and the one that follows were kept in the output set...
     (check-pred exact-nonnegative-integer?
                 (vector-memq (vector-ref data test-point) data-5))
     (check-pred exact-nonnegative-integer?
                 (vector-memq (vector-ref data (add1 test-point)) data-5))

     (define data-4 (rdp-simplify data #:epsilon 0.04 #:destroy-original? #t))
     (check < (vector-length data-4) (vector-length data-3))
     ;; data now contains #f values, as it was destroyed
     (check > (for/sum ([d (in-vector data)] #:when (eq? #f d)) 1) 0)

     )))

(define scatter-tests
  (test-suite
   "`scatter` test suite"
   (test-case "`time-delay-series` test case"

     (define timestamp 1000000)
     (define nitems 100)
     (define shift-amount 20)

     (define tdata
       (for/vector #:length nitems
           ([i (in-range nitems)])
         (vector i i (+ timestamp i))))

     ;; We expect less items in the delayed series, as items will be dropped
     ;; from the end or the start...
     (define expected-length (- nitems shift-amount))

     (define delay-positive (time-delay-series tdata shift-amount))
     (check = (vector-length delay-positive) expected-length)
     (for ((item (in-vector delay-positive)))
       (match-define (vector x y ts) item)
       ;; NOTE: the x y and timestamp values were chosen carefully so we can
       ;; check if they shifted correctly with simple arithmetic!
       (check = (- ts x) timestamp)
       (check = (- y x) shift-amount))

     (define delay-negative (time-delay-series tdata (- shift-amount)))
     (check = (vector-length delay-negative) expected-length)
     (for ((item (in-vector delay-negative)))
       (match-define (vector x y ts) item)
       ;; NOTE: the x y and timestamp values were chosen carefully so we can
       ;; check if they shifted correctly with simple arithmetic!
       (check = (- ts x) timestamp)
       (check = (- y x) (- shift-amount)))

     )))

;;.................................................... least-squares-fit ....

;; Check that the fit returned by `df-least-squares-fit` is indeed a best fit
;; -- we test that by modifying the coefficients one by one and see if the
;; residual from the modified fit function is bigger than the original. If it
;; is smaller, we just found a better fit than `df-least-squares-fit`
;; returned!
(define (check-modified-residuals fit df xseries yseries)

  (define (make-polynomial coefficients)
    (lambda (x)
      (define-values (y p)
        (for/fold ([y 0] [p 1])
                  ([c (in-list coefficients)])
          (values (+ y (* p c)) (* p x))))
      y))

  (define (make-exponential coefficients)
    (match-define (list a b c) coefficients)
    (lambda (x) (+ (* a (exp (* b x))) c)))

  (define (make-logarithmic coefficients)
    (match-define (list a b) coefficients)
    (lambda (x) (+ a (* b (log x)))))

  (define (make-power-law coefficients)
    (match-define (list a b) coefficients)
    (lambda (x) (* a (expt x b))))

  (define (calculate-residual fn)
    (for/sum (([x y] (in-data-frame df xseries yseries)))
      (define d (- y (fn x)))
      (* d d)))

  (define (scale-coeffient coefficients n factor)
    (let ((item (list-ref coefficients n))
          (head (take coefficients n))
          (tail (drop coefficients (add1 n))))
      (append head (list (* item factor)) tail)))

  (match-define (least-squares-fit type coefficients residual fn) fit)
  (for ([n (in-range (length coefficients))])
    (for ([factor (in-list '(0.95 1.05))])
      (define ncoeff (scale-coeffient coefficients n factor))
      (define nfn
        (case type
          ((linear polynomial) (make-polynomial ncoeff))
          ((exponential) (make-exponential ncoeff))
          ((logarithmic) (make-logarithmic ncoeff))
          ((power) (make-power-law ncoeff))
          (else (lambda (x) 1))))
      (define nresidual (calculate-residual nfn))
      (check > nresidual residual))))

(define least-squares-fit-tests
  (test-suite
   "`df-least-squares-fit` test suite"
   (test-case "`df-least-squares-fit` test cases"

     (define df (df-read/csv "./test-data/lsq-test.csv"))

     (define fit-linear (df-least-squares-fit
                         df "base" "linear"
                         #:mode 'linear
                         #:residual? #t))
     (check-modified-residuals fit-linear df "base" "linear")

     (define fit-first (df-least-squares-fit
                        df "base" "linear"
                        #:mode 'polynomial
                        #:polynomial-degree 1
                        #:residual? #t))
     (check-modified-residuals fit-first df "base" "linear")

     (define fit-second (df-least-squares-fit
                         df "base" "second"
                         #:mode 'polynomial
                         #:polynomial-degree 2
                         #:residual? #t))
     (check-modified-residuals fit-second df "base" "second")

     (define fit-third (df-least-squares-fit
                        df "base" "third"
                        #:mode 'polynomial
                        #:polynomial-degree 3
                        #:residual? #t))
     (check-modified-residuals fit-third df "base" "third")

     ;; NOTE: exponential fit does not seem to generate minimum residuals, not
     ;; a mathematician, so I don't know why, see explanation for
     ;; `df-least-squares-fit`.

     ;; (define fit-exp (df-least-squares-fit
     ;;                  df "base" "exp"
     ;;                  #:mode 'exponential
     ;;                  #:residual? #t
     ;;                  #:annealing? #t
     ;;                  #:annealing-iterations 1000))
     ;; (check-modified-residuals fit-exp df "base" "exp")

     (define fit-log (df-least-squares-fit
                      df "base2" "log"
                      #:mode 'logarithmic
                      #:residual? #t))
     (check-modified-residuals fit-log df "base2" "log")

     ;; NOTE: power fit does not seem to generate minimum residuals, not a
     ;; mathematician, so I don't know why, see explanation for
     ;; `df-least-squares-fit`.
     ;;
     ;; (define fit-pow (df-least-squares-fit
     ;;                  df "base2" "pow"
     ;;                  #:mode 'power
     ;;                  #:residual? #t
     ;;                  #:annealing? #t
     ;;                  #:annealing-iterations 1000))
     ;; (check-modified-residuals fit-pow df "base2" "pow")

     )))

;;................................................................. rest ....

(define data-frame-tests
  (test-suite
   "data frame tests"
   bsearch-tests
   series-tests
   spline-tests
   df-tests
   dfdb-tests
   csv-tests
   gpx-tests
   stats+mmax-tests
   histogram-tests
   rdp-simplify-tests
   scatter-tests
   least-squares-fit-tests))

(module+ test
  (require rackunit/text-ui)
  (run-tests data-frame-tests))
