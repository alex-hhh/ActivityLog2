#lang racket/base
(require racket/match
         racket/math
         racket/contract
         racket/vector
         "bsearch.rkt"
         "exn.rkt")

;; NOTE: provides are at the end of the file

(struct series
  (name                          ; name of this column
   data                          ; a vector containing column data
   beg                           ; start index in the vector where data begins
   end                           ; end index in the vector where data ends
   cmpfn                         ; a compare function, when present, all
                                 ; elements in this column must be sorted
                                 ; according to this function
   na                            ; value for the Not Available slots
   contractfn                    ; when not #f, specified contract for elements
   )
  #:mutable)


;; Check that the data frame column C is sorted according to CMPFN, and raise
;; an error if it is not.
(define (check-valid-sort c cmpfn)
  (match-define (series name data beg end _ na _) c)
  (for ((index (in-range beg end)))
    (define v (vector-ref data index))
    (define vprev (if (> index beg) (vector-ref data (sub1 index)) #f))
    (cond ((equal? v na)
           (df-raise "check-valid-sort: ~a contains NA / values @~a" name index))
          ((and (> index beg) (not (cmpfn vprev v)))
           (df-raise "check-valid-sort:: ~a not really sorted @~a (~a vs ~a)"
                     name index vprev v)))))

;; Check that the values in the data frame column C match the contract
;; function CONTRACTFN
(define (check-valid-contract c contractfn)
  (match-define (series name data beg end _ na _) c)
  (for ((index (in-range beg end)))
    (define v (vector-ref data index))
    (unless (or (equal? v na) (contractfn v))
      (df-raise "check-valid-contract: ~a value ~a fails contract @~a" name v index))))

;; Check that inserting VAL at INDEX is valid.  If there is a contract
;; specified, we check that the contract is valid, if the data frame column is
;; sorted, we check that the sort order is preserved.
(define (check-valid-insert c index val)
  (match-define (series _ data beg end cmpfn na contractfn) c)
  (when (or (< index beg) (> index end))
    (raise-range-error
     'check-valid-insert "vector" "" index data 0 (- end beg)))
  ;; Check for contract match
  (when (and contractfn (not (equal? val na)) (not (contractfn val)))
    (raise-argument-error 'check-valid-insert
                          "val"
                          "contract"
                          val))
  (when cmpfn
    (when (equal? na val)
      (raise-argument-error
       'check-valid-insert
       "value cannot be NA in a sorted column"
       val))
    (let ((prev-index (if (> index beg) (sub1 index) #f)))
      (when prev-index
        (unless (cmpfn (vector-ref data prev-index) val)
          (raise-argument-error
           'check-valid-insert
           "not sorted w.r.t. previous element"
           val))))
    (let ((next-index (if (< index (sub1 end)) (add1 index) #f)))
      (when next-index
        (unless (cmpfn val (vector-ref data next-index))
          (raise-argument-error
           'check-valid-insert
           "not sorted w.r.t. next element"
           val))))))

;; Make sure there is enough SPACE in the data frame column C.  A new storage
;; vector will be allocated if necessary, but the number of actual elements
;; will not change.
(define (series-reserve-space c space)
  (when (< (series-free-space c) space)
    (match-define (series name data beg end _ na _) c)
    (define ncap (max (exact-truncate (* (vector-length data) 1.5))
                      (+ end space)))
    (define ndata (make-vector ncap na))
    (for (([v idx] (in-indexed (in-vector data beg end))))
      (vector-set! ndata idx v))
    (set-series-data! c ndata)
    (set-series-beg! c 0)
    (set-series-end! c (- end beg))))

(define (make-series name
                    #:data (data #f)
                    #:capacity (capacity 10)
                    #:cmpfn (cmpfn #f)
                    #:na (na #f)
                    #:contract (contractfn #f))
  (define df
    (series
     name
     (or data (make-vector capacity na))
     0
     (if data (vector-length data) 0)
     cmpfn
     na
     contractfn))
  (when contractfn
    (check-valid-contract df contractfn))
  (when cmpfn
    (check-valid-sort df cmpfn))
  df)

(define (series-size c)
  (- (series-end c) (series-beg c)))

(define (series-free-space c)
  (- (vector-length (series-data c)) (series-end c)))

(define (series-empty? c)
  (= (series-size c) 0))

(define (series-ref c index)
  (match-define (series name data beg end _ _ _) c)
  (when (or (< index beg) (>= index end))
    (raise-range-error
     'series-ref (format "vector(~a)" name) "" index data 0 (- end beg)))
  (vector-ref data (- index beg)))

(define (series-set! c index val)
  (match-define (series name data beg end _ _ contractfn) c)
  (when (or (< index beg) (>= index end))
    (raise-range-error
     'series (format "vector(~a)" name) "" index data 0 (- end beg)))
  (check-valid-insert c index val)
  ;; When the series is passed a #:data argument, we might get an immutable
  ;; vector, make a mutable one, so we can set a value in it (we delay doing
  ;; this, so we avoid unnecessary copying if the data never changes.
  (when (immutable? data)
    (set! data (vector-copy data)))
  (set-series-data! c data)
  (vector-set! data (- index beg) val))

(define (series-bless-sorted c cmpfn)
  (when cmpfn
    (check-valid-sort c cmpfn))
  (set-series-cmpfn! c cmpfn))

(define (series-bless-contract c contractfn)
  (when contractfn
    (check-valid-contract c contractfn))
  (set-series-contractfn! c contractfn))
  
(define (series-push-back c val)
  (check-valid-insert c (series-end c) val)
  (series-reserve-space c 1)
  (vector-set! (series-data c) (series-end c) val)
  (set-series-end! c (add1 (series-end c))))

(define (in-series c [start 0] [stop (series-size c)] [step 1])
  (match-define (series _ data beg end _ _ _) c)
  (define s (+ start beg))
  (define e (+ stop beg))
  (when (or (< s beg) (> s end))
    (raise-range-error
     'in-series "vector" "" start data 0 (- end beg)))
  (when (or (< e beg) (> e end))
    (raise-range-error
     'in-series "vector" "" stop data 0 (- end beg)))
  (in-vector (series-data c) s e step))

(define (series-index-of c value)
  (match-define (series name data beg end cmpfn _ _) c)
  (if cmpfn
      (bsearch data value #:cmp cmpfn #:start beg #:stop end)
      (df-raise (format "series-index-of: ~a is not sorted" name))
      #;(for/first ([(x index) (in-indexed (in-vector data beg end))]
                  #:when (equal? x value))
        index)))

(define (series-na-count c)
  (match-define (series _ data beg end _ na _) c)
  (or (for/sum ([x (in-vector data beg end)] #:when (equal? x na)) 1) 0))

(define (series-is-na? c val)
  (equal? (series-na c) val))


;;............................................................. provides ....

(provide/contract
 (make-series (->* (string?)
                  (#:data (or/c vector? #f)
                   #:capacity exact-nonnegative-integer?
                   #:cmpfn (or/c #f (-> any/c any/c boolean?))
                   #:na any/c
                   #:contract (-> any/c boolean?))
                  series?))
 (series? (-> any/c boolean?))
 (series-na (-> series? any/c))
 (series-name (-> series? string?))
 (series-size (-> series? exact-nonnegative-integer?))
 (series-empty? (-> series? boolean?))
 (series-is-na? (-> series? any/c boolean?))
 (series-free-space (-> series? exact-nonnegative-integer?))
 (series-reserve-space (-> series? exact-nonnegative-integer? any/c))
 (series-ref (-> series? exact-nonnegative-integer? any/c))
 (series-set! (-> series? exact-nonnegative-integer? any/c any/c))
 (series-push-back (-> series? any/c any/c))
 (in-series (->* (series?) (exact-nonnegative-integer?
                          exact-nonnegative-integer?
                          exact-nonnegative-integer?)
                sequence?))
 (series-index-of (-> series? any/c (or/c #f exact-nonnegative-integer?)))
 (series-bless-sorted (-> series? (or/c #f (-> any/c any/c boolean?)) any/c))
 (series-bless-contract (-> series? (or/c #f (-> any/c boolean?)) any/c))
 (series-na-count (-> series? exact-nonnegative-integer?)))

