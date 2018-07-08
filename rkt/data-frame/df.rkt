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

(struct data-frame
  (semaphore
   locking-thread
   series
   delayed
   properties)
  #:mutable)

(define (make-data-frame)
  (data-frame
   (make-semaphore 1)
   #f
   (make-hash)
   (make-hash)
   (make-hash)))

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

(define (df-series-names df)
  (match-define (data-frame _ _ series delayed _) df)
  (append (hash-keys series) (hash-keys delayed)))

(define (df-property-names df)
  (match-define (data-frame _ _ _ _ properties) df)
  (hash-keys properties))

(define (df-contains? df . column-names)
  (match-define (data-frame _ _ series delayed _) df)
  (for/and ([n column-names])
    (and (or (hash-ref series n #f)
             (hash-ref delayed n #f))
         #t)))

(define (df-contains/any? df . column-names)
  (match-define (data-frame _ _ series delayed _) df)
  (for/or ([n column-names])
    (and (or (hash-ref series n #f)
             (hash-ref delayed n #f))
         #t)))

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

(define (df-put-property df key value)
  (match-define (data-frame _ _ _ _ properties) df)
  (hash-set! properties key value))

(define (df-get-property df key [default-value-fn (lambda () #f)])
  (match-define (data-frame _ _ _ _ properties) df)
  (hash-ref properties key default-value-fn))

(define (df-del-property df key)
  (match-define (data-frame _ _ _ _ properties) df)
  (hash-remove! properties key))

(define (df-row-count df)
  (match-define (data-frame _ _ series _ _) df)
  (or (for/first ([v (in-hash-values series)])
        (series-size v))
      0))

(define (df-select df name
                   #:filter (filter-fn #f)
                   #:start (start 0)
                   #:stop (stop (df-row-count df)))
  (define col (df-get-series df name))
  (if filter-fn
      (for/vector ([d (in-series col start stop)] #:when (filter-fn d)) d)
      (for/vector #:length (- stop start) ([d (in-series col start stop)]) d)))

(define (in-data-frame/list df
                            #:start (start 0)
                            #:stop (stop (df-row-count df))
                            . series)
  (define generators
    (for/list ([n (in-list series)])
      (let ((c (df-get-series df n)))
        (in-series c start stop))))
  (in-values-sequence (apply in-parallel generators)))

(define (in-data-frame df
                       #:start (start 0)
                       #:stop (stop (df-row-count df))
                       . series)
  (define generators
    (for/list ([n (in-list series)])
      (let ((c (df-get-series df n)))
        (in-series c start stop))))
  (apply in-parallel generators))

(define (df-select* df
                    #:filter (filter-fn #f)
                    #:start (start 0)
                    #:stop (stop (df-row-count df)) . names)
  (define sequence (keyword-apply in-data-frame/list null null df names #:start start #:stop stop))
  (if filter-fn
      (for*/vector ([item sequence]
                    [val (in-value (list->vector item))]
                    #:when (filter-fn val))
        val)
      (for/vector #:length (- stop start) ([item sequence]) (list->vector item))))

(define (df-index-of df column value)
  (let ([s (df-get-series df column)])
    (series-index-of s value)))

(define (df-index-of* df column . values)
  (let ([s (df-get-series df column)])
    (for/list ([v (in-list values)]) (series-index-of s v))))

(define (df-ref df index column)
  (let ([s (df-get-series df column)])
    (series-ref s index)))

(define (df-set! df index value column)
  (let ([s (df-get-series df column)])
    (series-set! s index value)))

(define (df-ref* df index . series)
  (for/vector ([column (in-list series)])
    (let ([s (df-get-series df column)])
      (series-ref s index))))

(define (df-na-value df series)
  (define s (df-get-series df series))
  (series-na s))

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

(define (df-add-series df column)
  (match-define (data-frame _ _ series delayed _) df)
  ;; Check if the column has the same number of rows as the rest of the
  ;; column.  Take special care for the first column (row-count)
  ;; returns #f.
  (let ((row-count (df-row-count df))
        (srow-count (series-size column)))
    (unless (or (hash-empty? series) (equal? row-count srow-count))
      (df-raise "data-frame%/add-series (\"~a\"): bad length ~a, expecting ~a"
                (series-name column) srow-count row-count)))
  (let ([name (series-name column)])
    (hash-set! series name column)))

(define (df-del-series df name)
  (match-define (data-frame _ _ series delayed _) df)
  (hash-remove! series name)
  (hash-remove! delayed name))

(define (df-add-derived df name base-seriess value-fn)
  (define data (df-map df base-seriess value-fn))
  (define col (make-series name #:data data))
  (df-add-series df col))

(define (df-add-lazy df name base-series value-fn)
  (match-define (data-frame _ _ _ delayed _) df)
  (hash-set! delayed name
             (lambda () (df-add-derived df name base-series value-fn))))

(define (df-set-sorted df column cmpfn)
  (define col (df-get-series df column))
  (series-bless-sorted col cmpfn))

(define (df-set-contract df column contractfn)
  (define col (df-get-series df column))
  (series-bless-contract col contractfn))

(define (df-count-na df column)
  (define col (df-get-series df column))
  (series-na-count col))

(define (df-is-na? df column val)
  (define col (df-get-series df column))
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

(define index/c exact-nonnegative-integer?)
(define mapfn/c (or/c (-> any/c any/c) (-> any/c any/c any/c)))
(define foldfn/c (or/c (-> any/c any/c any/c) (-> any/c any/c any/c any/c)))

(provide/contract
 (make-data-frame (-> data-frame?))
 (df-series-names (-> data-frame? (listof string?)))
 (df-property-names (-> data-frame? (listof string?)))
 (df-contains? (->* (data-frame?) () #:rest (listof string?) boolean?))
 (df-contains/any? (->* (data-frame?) () #:rest (listof string?) boolean?))
 (df-put-property (-> data-frame? symbol? any/c any/c))
 (df-get-property (->* (data-frame? symbol?) ((-> any/c)) any/c))
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
 (data-frame? (-> any/c boolean?))
 (valid-only (-> any/c boolean?)))
