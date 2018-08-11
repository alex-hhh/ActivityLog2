#lang racket/base
;; scatter.rkt -- utilities for scatter plots
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

(require plot/no-gui
         plot/utils
         racket/class                   ; ->pen-color
         racket/contract
         racket/draw
         racket/list
         racket/match
         racket/math
         "bsearch.rkt"
         "color-util.rkt")

;; Delay the data in DATA-SERIES by AMOUNT.  Data-series is a sequence of
;; 3-element vectors: #(X Y TIMESTAMP).  For each item, this function replaces
;; the Y value with the Y value at TIMESTAMP + AMOUNT.
(define (time-delay-series data-series amount)

  (define (key-fn item) (vector-ref item 2))
  (define (delayed-value start-index)
    (define lookup-val
      (+ (key-fn (vector-ref data-series start-index))
         amount))
    (define index (bsearch data-series lookup-val #:key key-fn))
    ;; NOTE: `bsearch` will return 0 if `lookup-val` is smaller than the first
    ;; item in the vector, so we need one extra check in that case, so we drop
    ;; values from the start of the series if the shift is beyond the start of
    ;; the series.
    (if (and (< index (vector-length data-series))
             (or (> index 0)
                 (>= lookup-val (key-fn (vector-ref data-series 0)))))
        (vector-ref (vector-ref data-series index) 1)
        #f))

  (for*/vector ([index (in-range (vector-length data-series))]
                [val (in-value (delayed-value index))]
                #:when val)
    (define item (vector-ref data-series index))
    (vector (vector-ref item 0) val (vector-ref item 2))))

;; Group samples in DATA-SERIES in groups of identical elements.  DATA-SERIES
;;is a sequence of at least 2 elements #(X Y ...).  The X values are rounded
;;to FRAC-DIGITS1 and Y values to FRAC-DIGITS2.  Identical (X, Y) pairs are
;;grouped together.
;;
;; Return a hash, mapping "RANK" (the number of pairs) to a list of pairs with
;; that rank.
(define (group-samples data-series (frac-digits1 0) (frac-digits2 0))

  (define result (make-hash))

  (define mult1 (expt 10 frac-digits1))
  (define inv1 (expt 10 (- frac-digits1)))
  (define mult2 (expt 10 frac-digits2))
  (define inv2 (expt 10 (- frac-digits2)))

  (for ([d data-series])
    (define s1 (vector-ref d 0))
    (define s2 (vector-ref d 1))
    (define cell (cons
                  (exact-round (* s1 mult1))
                  (exact-round (* s2 mult2))))
    (hash-set! result
               cell
               (+ 1 (hash-ref result cell 0))))

  (define result-1 (make-hash))

  (for ([k (in-hash-keys result)])
    (match-define (cons s1 s2) k)
    (define rank (hash-ref result k))
    (hash-set! result-1
               rank
               (cons (vector (* s1 inv1) (* s2 inv2))
                     (hash-ref result-1 rank '()))))
  result-1)

;; Apply FACTOR-FN to each item in DATA-SERIES and group items according to
;; the result of the function.  If KEY is specified, it is applied to the
;; items first and its result is passed to FACTOR-FN, e.g (FACTOR-FN (KEY
;; item))
;;
;; Returns a hash mapping a factor value to the list of items with that value.
(define (group-samples/factor data-series factor-fn #:key (key #f))
  (define result (make-hash))
  (for ([item data-series])
    (let ((factor (factor-fn (if key (key item) item))))
      (hash-set! result factor (cons item (hash-ref result factor '())))))
  result)

;; Make a scatter plot renderer for items in DATA-SERIES.
;;
;; COLOR is the color of the dots
;;
;; SIZE is the size of the dots, as a multiplier of (point-size)
;;
;; LABEL is the label to use for the data
;;
;; ALPHA specifies the transparency for the dots.
;;
(define (scatter-renderer data-series #:color color #:size size #:label label #:alpha [alpha 1.0])
  (let ((kwd '()) (val '()))
    (define (add-arg k v) (set! kwd (cons k kwd)) (set! val (cons v val)))
    (add-arg '#:sym 'fullcircle)
    (add-arg '#:size (* (point-size) size))
    ;; (add-arg '#:line-width 2)
    (when label
      (add-arg '#:label label))
    (when color
      (add-arg '#:fill-color color)
      (add-arg '#:color color))
    (when alpha
      (add-arg '#:alpha alpha))
    (keyword-apply points kwd val data-series '())))

;; Compute colors for the keys of a scatter group renderer.  KEYS is a sorted
;; list of numbers (the groups ranks for the group renderer).  The BASE-COLOR
;; is used to compute a range of colors from lightest for the smallest rank to
;; darkest for the highest rank.  Returns a hash map mapping each key to a
;; color value.
(define (make-key-colors keys base-color)
  ;; NOTE: keys are sorted and should not contain duplicates

  (define range                         ; make sure range is never 0
    (if (< (length keys) 2)
        1
        (- (last keys) (first keys))))
  (match-define (list h s l) (apply rgb->hsl/255 (->pen-color base-color)))
  (define min-l 0.3)
  (define max-l 0.8)
  (define l-range (- max-l min-l))
  (for/hash ((key (in-list keys)))
    (let* ((pct (/ (- key (first keys)) range))
           (new-l (+ min-l (* (- 1 pct) l-range))))
      (match-define (list r g b) (hsl->rgb/255 h s new-l))
      (values
       key
       (make-object color% r g b)))))

;; Make a scatter plot renderer for GROUP, as returned by `group-samples`. The
;; color of the dots will vary depending on the number of items at that point.
;; This allows creating simplified plots which use color to show areas with
;; lots of points, instead of rendering the points themselves.
;;
;; COLOR determines the base color for the plot.  The actual color for a dot
;; will be lighter or darker depending on the number of items at that point.
;;
;; LABEL determines the label of the plot
;;
;; SIZE is the size of the dots, as a multiplier of (point-size)
;;
(define (scatter-group-renderer group #:color color #:label [label #f] #:size [size 1.5])
  (define keys (sort (hash-keys group) <))
  (define color-map (make-key-colors keys color))
  (define first-time? #t)
  (for/list ([key (in-list keys)])
    (define data (hash-ref group key))
    (begin0
        (scatter-renderer
         data
         #:color (hash-ref color-map key color)
         #:size size
         #:label (if first-time? label #f))
      (set! first-time? #f))))


;;................................................. contract definitions ....

;; Time series items are X, Y and a timestamp, elapsed or distance value
(define ts-item/c (vector/c (or/c #f real?) (or/c #f real?) real?))
(define ts-data/c (or/c
                   (vectorof ts-item/c)
                   (listof ts-item/c)))

;; Pairs used in scatter plots
(define xy-item/c (vector/c (or/c #f real?) (or/c #f real?)))
(define xy-data/c (or/c
                   (vectorof xy-item/c)
                   (listof xy-item/c)))

;; Data that can be used as input for scatter functions (group-samples, etc)
(define scatter-data/c (or/c ts-data/c xy-data/c))

(define group-data/c (hash/c integer? xy-data/c))
(define factor-data/c (hash/c any/c (or/c xy-data/c ts-data/c)))


;;............................................................. provides ....

(provide ts-item/c ts-data/c factor-data/c)

(provide/contract
 (time-delay-series (-> ts-data/c real? ts-data/c))
 (group-samples (-> scatter-data/c integer? integer? group-data/c))
 (group-samples/factor (->* (scatter-data/c (-> any/c any/c))
                            (#:key (or/c #f (-> (or/c xy-item/c ts-item/c) any/c)))
                            factor-data/c))
 (scatter-renderer (->* (scatter-data/c #:color any/c #:size positive? #:label (or/c #f string?))
                        (#:alpha (between/c 0 1))
                        renderer2d?))
 (scatter-group-renderer (->* (group-data/c #:color any/c)
                              (#:label (or/c #f string?) #:size positive?)
                              (treeof renderer2d?))))
