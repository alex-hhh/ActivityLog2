#lang racket/base
;; df.rkt -- data frame implementation and basic routines
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
         racket/match
         "exn.rkt"
         "series.rkt")


;;........................................................... data-frame ....

;; A data frame holds together some data series in an unspecified order.  All
;; series in the data series have the same number of elements.  There are
;; methods for selecting a subset of the data, looking up values adding new
;; series, plus other things.
;;
;; A data series can also contain "properties" which are key,value pairs.
(struct data-frame
  (semaphore                 ; controls access to materializing delayed series
   locking-thread            ; the thread owning the semaphore
   series                    ; a hash table of series?
   delayed                   ; a hash table of functions which can create
   ; series (see `df-add-lazy`)
   properties)               ; a hash table containing the series properties.
  #:mutable)

;; Construct an empty data frame
(define (make-data-frame)
  (data-frame
   (make-semaphore 1)
   #f
   (make-hash)
   (make-hash)
   (make-hash)))

;; Create a copy of the data frame DF.  The returned copy will reference the
;; same data series objects as the original (and the properties), but any
;; add/delete operations, for both series and properties, will only affect the
;; copy.
;;
;; Before copying the data frame, all lazy series will be materialized.
(define (df-shallow-copy df)
  (match-define (data-frame _ _ series delayed properties) df)
  ;; Materialize all lazy series
  (for ([c (hash-keys delayed)])
    (df-get-series c))
  (data-frame
   (make-semaphore 1)
   #f
   (hash-copy series)
   (make-hash)
   (hash-copy properties)))

;; Return the series names in the data frame DF, as a list of strings.  The
;; names are returned in an unspecified order.
(define (df-series-names df)
  (match-define (data-frame _ _ series delayed _) df)
  (append (hash-keys series) (hash-keys delayed)))

;; Return the property names in the data frame DF, as a list of symbols.  The
;; names are returned in an unspecified order.
(define (df-property-names df)
  (match-define (data-frame _ _ _ _ properties) df)
  (hash-keys properties))

;; Return #t if the data frame DF contains ALL the series specified in
;; SERIES-NAMES
(define (df-contains? df . series-names)
  (match-define (data-frame _ _ series delayed _) df)
  (for/and ([n (in-list series-names)])
    (and (or (hash-ref series n #f)
             (hash-ref delayed n #f))
         #t)))

;; Return #t if the data frame DF contains ANY of the series specified in
;; SERIES-NAMES.
(define (df-contains/any? df . series-names)
  (match-define (data-frame _ _ series delayed _) df)
  (for/or ([n (in-list series-names)])
    (and (or (hash-ref series n #f)
             (hash-ref delayed n #f))
         #t)))

;; Return the series named NAME in the data frame DF.  The series is
;; materialized first, if needed -- i.e. if the series was added using
;; `df-add-lazy`, the thunk will be invoked to create the series.  An error is
;; raised if the series does not exist.
(define (df-get-series df name)
  (match-define (data-frame semaphore locking-thread series delayed _) df)
  (define should-unlock? #f)

  ;; Only lock the semaphore if it wasn't already locked it in the current
  ;; thread.  This is so 'get-series' can be recursively called in the current
  ;; thread and the delayed column can be materialized (as they usually depend
  ;; on other column.)
  (unless (eq? (current-thread) locking-thread)
    (semaphore-wait semaphore)
    (set! should-unlock? #t)
    (set-data-frame-locking-thread! df (current-thread)))

  (define (unlock)
    (when should-unlock?
      (set-data-frame-locking-thread! df #f)
      (set! should-unlock? #f)
      (semaphore-post semaphore)))

  (with-handlers
    (((lambda (e) #t)
      (lambda (e) (unlock) (raise e))))

    ;; Check if we have to materialize a delayed column first.
    (let ((thunk (hash-ref delayed name #f)))
      (when thunk
        (thunk)                         ; create the column now
        (hash-remove! delayed name)))

    (begin0
        (or (hash-ref series name #f)
            (df-raise "df-get-series (\"~a\"): not found" name))
      (unlock))))

;; Put the property (KEY, VALUE) inside the data frame DF, possibly replacing
;; an existing one for KEY.
(define (df-put-property df key value)
  (match-define (data-frame _ _ _ _ properties) df)
  (hash-set! properties key value))

;; Return the value for property KEY inside the data frame DF.  If there is no
;; property named KEY, the DEFAULT-VALUE-FN is invoked to produce a value (the
;; default function just returns #f)
(define (df-get-property df key [default-value-fn (lambda () #f)])
  (match-define (data-frame _ _ _ _ properties) df)
  (hash-ref properties key default-value-fn))

;; Delete the property KEY in the data frame DF.
(define (df-del-property df key)
  (match-define (data-frame _ _ _ _ properties) df)
  (hash-remove! properties key))

;; Returns the number of rows in the data frame DF.  All series inside a data
;; frame have the same number of rows.
(define (df-row-count df)
  (match-define (data-frame _ _ series _ _) df)
  (or (for/first ([v (in-hash-values series)])
        (series-size v))
      0))

;; Returns a vector with the values in the series NAME from the data frame DF.
;; START and STOP indicate the first and one-before-last row to be
;; selected. FILTER-FN, when present, will filter values selected, only values
;; for which FILTER-FN returns #t will be added to the resulting vector.
;;
;; If there is no FILTER-FN specified, the resulting vector will have (- STOP
;; START) elements.  If there is a filter, the number of elements depends on
;; how many are filtered out by this function.
(define (df-select df name
                   #:filter (filter-fn #f)
                   #:start (start 0)
                   #:stop (stop (df-row-count df)))
  (define col (df-get-series df name))
  (if filter-fn
      (for/vector ([d (in-series col start stop)] #:when (filter-fn d)) d)
      (for/vector #:length (- stop start) ([d (in-series col start stop)]) d)))

;; Return a sequence that produces values from a list of SERIES between START
;; and STOP rows.  Each value in the sequence is a list of the values
;; corresponding to the series names.  NOTE: this is intended to be used in
;; `for` and related constructs to iterate over elements in the data frame.
;;
;; Example:
;;
;; (for ((coord (in-data-frame/list df "lat" "lon")))
;;    (match-define (list lat lon) coord)
;;    (printf "lat = ~a, lon = ~a~%" lat lon))
;;
(define (in-data-frame/list df
                            #:start (start 0)
                            #:stop (stop (df-row-count df))
                            . series)
  (define generators
    (for/list ([n (in-list series)])
      (let ((c (df-get-series df n)))
        (in-series c start stop (if (<= start stop) 1 -1)))))
  ;; When there are no series, the `(apply in-parallel '())` call will result
  ;; in an infinite loop.
  (if (null? generators)
      (in-values-sequence (in-parallel '()))
      (in-values-sequence (apply in-parallel generators))))

;; Return a sequence that produces values from a list of SERIES between START
;; and STOP rows.  The sequence produces values, each one corresponding to the
;; series names.  NOTE: this is intended to be used in `for` and related
;; constructs to iterate over elements in the data frame.
;;
;; Example:
;;
;; (for (([lat lon] (in-data-frame df "lat" "lon")))
;;    (printf "lat = ~a, lon = ~a~%" lat lon))
;;
(define (in-data-frame df
                       #:start (start 0)
                       #:stop (stop (df-row-count df))
                       . series)
  (define generators
    (for/list ([n (in-list series)])
      (let ((c (df-get-series df n)))
        (in-series c start stop (if (<= start stop) 1 -1)))))
  ;; When there are no series, the `(apply in-parallel '())` call will result
  ;; in an infinite loop.
  (if (null? generators)
      (in-parallel '())
      (apply in-parallel generators)))

;; Return a vector where each element is a vector containing values from
;; multiple SERIES between the START and STOP rows of the data frame.
;; FILTER-FN, when present, will filter values selected, only values for which
;; FILTER-FN returns #t will be added to the resulting vector.
;;
;; If there is no FILTER-FN specified, the resulting vector will have (- STOP
;; START) elements.  If there is a filter, the number of elements depends on
;; how many are filtered out by this function.
(define (df-select* df
                    #:filter (filter-fn #f)
                    #:start (start 0)
                    #:stop (stop (df-row-count df)) . series)
  (define sequence (keyword-apply in-data-frame/list null null df series #:start start #:stop stop))
  (if filter-fn
      (for*/vector ([item sequence]
                    [val (in-value (list->vector item))]
                    #:when (filter-fn val))
        val)
      (for/vector #:length (- stop start) ([item sequence]) (list->vector item))))

;; Return the index (position) of VALUE in the data frame SERIES.  If SERIES
;; is not sorted, this will raise an error.  VALUE might not be present in the
;; series, in that case, the returned index is the position of the first
;; element which is greater than VALUE (i.e. the position where VALUE could be
;; inserted and still keep the series sorted).  A value of 0 is returned if
;; VALUE is less or equal than the first value of the series and a value of
;; (df-row-count df) is returned if the value is greater than all the values
;; in SERIES.
(define (df-index-of df series value)
  (let ([s (df-get-series df series)])
    (series-index-of s value)))

;; Return a list of indexes corresponding to each element in VALUES for
;; SERIES.  This is the same as calling `df-index-of` for each individual
;; value, but it is more efficient.
(define (df-index-of* df series . values)
  (let ([s (df-get-series df series)])
    (for/list ([v (in-list values)]) (series-index-of s v))))

;; Return the value at INDEX for SERIES.
(define (df-ref df index series)
  (let ([s (df-get-series df series)])
    (series-ref s index)))

;; Set the VALUE at INDEX in SERIES.  This will first validate that the series
;; is still sorted (if SERIES was sorted) and that VALUE satisfies the SERIES
;; contract (if any).  The update will fail if these constraints are not
;; satisfied.
(define (df-set! df index value series)
  (let ([s (df-get-series df series)])
    (series-set! s index value)))

;; Return a vector with values at INDEX for each element in the SERIES list.
(define (df-ref* df index . series)
  (for/vector ([series (in-list series)])
    (let ([s (df-get-series df series)])
      (series-ref s index))))

;; Return the NA value for SERIES.
(define (df-na-value df series)
  (define s (df-get-series df series))
  (series-na s))

;; Perform an indexed lookup: the index of VALUE is found in BASE-SERIES (a
;; series name) and the value at the same index is returned from SERIES, which
;; is either a single series or a list of series.  IF SERIES is a single
;; string, a single value is returned, if it is a list of names, a list of
;; values is returned.
;;
;; This is a combination of `df-index-of` and `df-ref`, but more efficient.
(define (df-lookup df base-series series value)
  (let ((index (df-index-of df base-series value)))
    (if (< index (df-row-count df))
        (if (list? series)
            (apply df-ref* df index series)
            (df-ref df index series))
        (if (list? series)
            (for/vector ([s (in-list series)])
              (df-na-value df s))
            (df-na-value df series)))))

;; Perform an indexed lookup on multiple values.  Same as calling `df-lookup`
;; on each value in VALUES and returning the result as a list.
(define (df-lookup* df base-series series . values)
  (define slist
    (if (list? series)
        (for/list ([s series]) (df-get-series df s))
        (df-get-series df series)))
  (define max (df-row-count df))
  (define nitems (if (list? series) (length series) 1))
  (for/list ([index (apply df-index-of* df base-series values)])
    (if (< index max)
        (if (list? slist)
            (for/vector #:length nitems ([s (in-list slist)])
              (series-ref s index))
            (series-ref slist index))
        (if (list? slist)
            (for/vector #:length nitems ([s (in-list slist)])
              (series-na s))
            (series-na slist)))))

;; Perform an interpolated lookup: same as `df-lookup`, but if VALUE is not
;; found exactly in BASE-SERIES, it's relative position is determined and it
;; is used to interpolate values from the corresponding SERIES.  An
;; interpolation function can be specified, if the default one is not
;; sufficient.  This function is called once for each resulting SERIES.
(define (df-lookup/interpolated
         df base-series series value
         #:interpolate (interpolate (lambda (t v1 v2)
                                      (+ (* t v1) (* (- 1 t) v2)))))
  (define index (df-index-of df base-series value))
  (define slist (if (list? series) series (list series)))
  (cond ((<= index 0)
         (apply df-ref* df 0 slist))
        ((>= index (df-row-count df))
         (df-ref* df (sub1 (df-row-count df)) ))
        (#t
         (let* ((pval (df-ref df (sub1 index) base-series))
                (aval (df-ref df index base-series))
                (t (/ (- value pval) (- aval pval)))
                (prev (apply df-ref* df (sub1 index) slist))
                (next (apply df-ref* df index slist))
                (result (for/vector #:length (length slist)
                            ([p (in-vector prev)]
                             [n (in-vector next)])
                          (interpolate t p n))))
           (if (list? series) result (vector-ref result 0))))))

;; Map the function FN over a list of SERIES between START and STOP.  Returns
;; a vector with the values that FN returns.  FN is a function of ether one or
;; two arguments.  If FN is a function with one argument, it is called with
;; the values from all SERIES as a single vector.  If FN is a function of two
;; arguments, it is called with the current and previous set of values, as
;; vectors (this allows calculating "delta" values).  I.e. FN is invoked as
;; (FN PREV CURRENT).  If FN accepts two arguments, it will be invoked as (FN
;; #f CURRENT) for the first element of the iteration.
(define (df-map df series fn #:start (start 0) #:stop (stop (df-row-count df)))
  (define sequence (keyword-apply in-data-frame/list null null df
                                  (if (string? series) (list series) series)
                                  #:start start #:stop stop))
  (define need-prev-val? (eq? (procedure-arity fn) 2))
  (if need-prev-val?
      (let ([prev-val #f])
        (for/vector #:length (- stop start) ([val sequence])
          (begin0 (fn prev-val val)
            (set! prev-val val))))
      (for/vector #:length (- stop start) ([val sequence])
        (fn val))))

;; Iterate the function FN over a list of SERIES between START and STOP,
;; discarding the results of FN.  FN is invoked as for `df-map`, which see.
(define (df-for-each df series fn #:start (start 0) #:stop (stop (df-row-count df)))
  (define sequence (keyword-apply in-data-frame/list null null df
                                  (if (string? series) (list series) series)
                                  #:start start #:stop stop))
  (define need-prev-val? (eq? (procedure-arity fn) 2))
  (if need-prev-val?
      (let ([prev-val #f])
        (for ([val sequence])
          (fn prev-val val)
          (set! prev-val val)))
      (for ([val sequence])
        (fn val))))

;; Fold a function FN over a list of SERIES between START and STOP.  INIT-VAL
;; is the initial value for the fold operation.  The last value returned by FN
;; is returned by this function.  FN is a function of ether two or three
;; arguments.  If FN is a function with two arguments, it is called with the
;; fold value plus the values from all SERIES is passed in as a single vector.
;; If FN is a function of three arguments, it is called with the fold value
;; plus the current and previous set of values, as vectors (this allows
;; calculating "delta" values).  I.e. FN is invoked as (FN VAL PREV CURRENT).
;; If FN accepts two arguments, it will be invoked as (FN INIT-VAL #f CURRENT)
;; for the first element of the iteration.
(define (df-fold df series init-val fn #:start (start 0) #:stop (stop (df-row-count df)))
  (define sequence (keyword-apply in-data-frame/list null null df
                                  (if (string? series) (list series) series)
                                  #:start start #:stop stop))
  (define need-prev-val? (eq? (procedure-arity fn) 3))
  (define accumulator init-val)
  (if need-prev-val?
      (let ([prev-val #f])
        (for ([val sequence])
          (set! accumulator (fn accumulator prev-val val))
          (set! prev-val val)))
      (for ([val sequence])
        (set! accumulator (fn accumulator val))))
  accumulator)

;; Add SERIES to the data frame DF.  The new series must have the same number
;; of rows as existing series in the data frame, unless the data frame is
;; empty.
(define (df-add-series df s)
  (match-define (data-frame _ _ series delayed _) df)
  ;; Check if the series has the same number of rows as the rest of the
  ;; series (if there are any other series in the data frame)
  (let ((nrows (df-row-count df))
        (name (series-name s))
        (size (series-size s)))
    (unless (or (hash-empty? series) (equal? nrows size))
      (df-raise "df-add-series (\"~a\"): bad length ~a, expecting ~a" name size nrows))
    (hash-set! series name s)))

;; Remove series NAME from the data frame, does nothing if the series does not
;; exist.
(define (df-del-series df name)
  (match-define (data-frame _ _ series delayed _) df)
  (hash-remove! series name)
  (hash-remove! delayed name))

;; Add a new series to the data frame whose values are computed from values of
;; existing series.  The data for the series is created using `df-map` by
;; applying VALUE-FN on BASE-SERIES and the new data is added to the data
;; frame.  See `df-map` for notes on the VALUE-FN.
(define (df-add-derived df name base-series value-fn)
  (define data (df-map df base-series value-fn))
  (define col (make-series name #:data data))
  (df-add-series df col))

;; Add a new series to the data frame, but delay creating it until it is
;; referenced.  This function allows adding many series to a data frame, with
;; the expectation that the cost to create those series is paid when (and if)
;; they are used.
(define (df-add-lazy df name base-series value-fn)
  (match-define (data-frame _ _ _ delayed _) df)
  (hash-set! delayed name
             (lambda () (df-add-derived df name base-series value-fn))))

;; Mark the SERIES as sorted according to CMPFN.  This does not actually sort
;; the data series, it just tells the data frame that the series can be used
;; for index lookup.  An error is raised if the series is not actually sorted
;; or if it contains NA values.
(define (df-set-sorted df series cmpfn)
  (define col (df-get-series df series))
  (series-bless-sorted col cmpfn))

;; Set the contract for values in the data SERIES to CONTRACTFN.  An exception
;; is thrown if not all values in SERIES match contractfn or are NA (the
;; contractfn need not return #t for the NA value)
(define (df-set-contract df series contractfn)
  (define col (df-get-series df series))
  (series-bless-contract col contractfn))

;; Count the number of NA (not available) values in SERIES.
(define (df-count-na df series)
  (define col (df-get-series df series))
  (series-na-count col))

;; Return true if SERIES has any NA (not available) values.  This is
;; equivalent to (> (df-count-na DF SERIES) 0), but it is faster.
(define (df-has-na? df series)
  (define col (df-get-series df series))
  (series-has-na? col))

;; Return true if SERIES has any values except NA (not available) values.
;; This is equivalent to (< (df-count-na DF SERIES) (df-row-count DF)), but
;; faster.
(define (df-has-non-na? df series)
  (define col (df-get-series df series))
  (series-has-non-na? col))

;; Return #t if VAL is the NA (not available) value for SERIES.
(define (df-is-na? df series val)
  (define col (df-get-series df series))
  (series-is-na? col val))

;; Helper function to select only entries with valie values.  Usefull as a
;; parameter for the #:filter parameter of the select and select* methods of
;; data-frame%
(define (valid-only vec)
  (cond ((vector? vec)
         (and (for/and ([v (in-vector vec)]) v) #t))
        ((list? vec)
         (and (for/and ([v (in-list vec)]) v) #t))
        (#t
         (not (not vec)))))


;;............................................................. provides ....

(define index/c (or/c -1 exact-nonnegative-integer?))
(define mapfn/c (or/c (-> any/c any/c) (-> any/c any/c any/c)))
(define foldfn/c (or/c (-> any/c any/c any/c) (-> any/c any/c any/c any/c)))

(provide/contract
 (make-data-frame (-> data-frame?))
 (df-series-names (-> data-frame? (listof string?)))
 (df-property-names (-> data-frame? (listof symbol?)))
 (df-contains? (->* (data-frame?) () #:rest (listof string?) boolean?))
 (df-contains/any? (->* (data-frame?) () #:rest (listof string?) boolean?))
 (df-put-property (-> data-frame? symbol? any/c any/c))
 (df-get-property (->* (data-frame? symbol?) (any/c) any/c))
 (df-del-property (-> data-frame? symbol? any/c))
 (df-row-count (-> data-frame? exact-nonnegative-integer?))
 (df-select (->* (data-frame? string?) (#:filter (or/c #f (-> any/c any/c))
                                        #:start index/c #:stop index/c)
                 vector?))
 (df-select* (->* (data-frame?) (#:filter (or/c #f (-> any/c any/c))
                                 #:start index/c #:stop index/c)
                  #:rest (listof string?) vector?))
 (in-data-frame/list (->* (data-frame?)
                          (#:start index/c #:stop index/c)
                          #:rest (listof string?)
                          sequence?))
 (in-data-frame (->* (data-frame?)
                     (#:start index/c #:stop index/c)
                     #:rest (listof string?)
                     sequence?))
 (df-index-of (-> data-frame? string? any/c index/c))
 (df-index-of* (->* (data-frame? string?) #:rest list? (listof index/c)))
 (df-ref (-> data-frame? index/c string? any/c))
 (df-set! (-> data-frame? index/c any/c string? any/c))
 (df-ref* (->* (data-frame? index/c) #:rest (listof string?) vector?))
 (df-lookup (-> data-frame? string? (or/c string? (listof string?)) any/c any/c))
 (df-lookup* (->* (data-frame? string? (or/c string? (listof string?))) () #:rest list? list?))
 (df-lookup/interpolated (->* (data-frame? string? (or/c string? (listof string?)) any/c)
                              (#:interpolate (-> real? any/c any/c any/c))
                              any/c))
 (df-map (->* (data-frame? (or/c string? (listof string?)) mapfn/c)
              (#:start index/c #:stop index/c)
              vector?))
 (df-for-each (->* (data-frame? (or/c string? (listof string?)) mapfn/c)
                   (#:start index/c #:stop index/c)
                   any/c))
 (df-fold (->* (data-frame? (or/c string? (listof string?)) any/c foldfn/c)
               (#:start index/c #:stop index/c)
               any/c))
 (df-add-series (-> data-frame? series? any/c))
 (df-del-series (-> data-frame? string? any/c))
 (df-add-derived (-> data-frame? string? (listof string?) mapfn/c any/c))
 (df-add-lazy (-> data-frame? string? (listof string?) mapfn/c any/c))
 (df-set-sorted (-> data-frame? string? (or/c #f (-> any/c any/c boolean?)) any/c))
 (df-set-contract (-> data-frame? string? (or/c #f (-> any/c boolean?)) any/c))
 (df-count-na (-> data-frame? string? exact-nonnegative-integer?))
 (df-shallow-copy (-> data-frame? data-frame?))
 (df-is-na? (-> data-frame? string? any/c boolean?))
 (df-has-na? (-> data-frame? string? boolean?))
 (df-has-non-na? (-> data-frame? string? boolean?))
 (data-frame? (-> any/c boolean?))
 (valid-only (-> any/c boolean?)))
