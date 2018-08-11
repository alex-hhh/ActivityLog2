#lang racket/base
;; histogram.rkt -- histograms and histogram plots for data frames
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
         racket/contract
         racket/list
         racket/match
         racket/math
         "df.rkt"
         "exn.rkt"
         "statistics.rkt")

;; Return a hash table mapping each sample in the data-frame% DF SERIES to the
;; number of times it appears in the series.  If WEIGHT is not #f, this is
;; used as the weight of the samples (instead of 1). INITIAL-BUCKETS
;; determines the hash table that is updated, BUCKET-WIDTH allows grouping the
;; samples into intervals (can be less than 1).  INCLUDE-ZEROES? if #f will
;; cause values that are equal to 0 to be discarded.
(define (samples->buckets df series
                          #:weight-series (weight #f)
                          #:initial-buckets [initial-buckets (make-hash)]
                          #:bucket-width [bucket-width 1]
                          #:include-zeroes? [include-zeroes? #t])

  ;; NOTE: using `exact-truncate' instead of `exact-round' works more
  ;; correctly for distributing values into buckets for zones.  The bucket
  ;; value is the start of the interval (as opposed to the middle of the
  ;; interval if `exact-round` would be used.
  (define (val->bucket v)
    ;; NOTE: has to work for non real values (e.g. strings!)
    (if (real? v) (exact-truncate (/ v bucket-width)) v))

  (define (weighted-binning buckets prev-val val)
    (when prev-val
      (match-define (list pws pv) prev-val)
      (match-define (list ws v) val)
      (when (and pws pv ws v)
        (let* ([dx (- ws pws)]
               [dy (/ (+ v pv) 2)]
               [bucket (val->bucket dy)])
          (when (or (not (number? bucket))
                    (not (zero? bucket))
                    include-zeroes?)
            (let ([pval (hash-ref buckets bucket 0)])
              (hash-set! buckets bucket (+ dx pval)))))))
    buckets)

  (define (unweighted-binning buckets val)
    (match-define (list v) val)
    (when v
      (let ([bucket (val->bucket v)])
        (when (or (not (number? bucket))
                  (not (zero? bucket))
                  include-zeroes?)
          (let ([pval (hash-ref buckets bucket 0)])
            (hash-set! buckets bucket (+ 1 pval))))))
    buckets)

  (df-fold
   df
   (if weight (list weight series) (list series))
   initial-buckets
   (if weight weighted-binning unweighted-binning)))

;; Create a histogram from BUCKETS (a hash table mapping sample value to its
;; rank), as produced by `samples->buckets`.  A histogram is a vector where
;; each value is a vector of sample and rank.  Entries will be created for
;; missing sample value (with 0 rank), so the vector contains all possible
;; sample values.  BUCKET-WIDTH is the width of the sample slot (should be the
;; same value as passed to `samples->buckets`.  When AS-PERCENTAGE? is #t, the
;; ranks are converted to a percentage of the total.
(define (buckets->histogram buckets
                            #:bucket-width (bucket-width 1)
                            #:as-percentage? (as-percentage? #f))

  (define total (for/sum ([v (in-hash-values buckets)]) v))
  (define unsorted (hash-keys buckets))
  (define real-keys? #f)
  ;; Try to sort the keys, if we can (we know how to sort reals and strings
  ;; only)
  (define keys
    (cond ((for/and ([k (in-list unsorted)]) (real? k))
           (set! real-keys? #t)
           (sort unsorted <))
          ((for/and ([k (in-list unsorted)]) (string? k))
           (sort unsorted string<?))
          (#t
           unsorted)))

  (define (bucket->val b)
    ;; don't touch b, if the bucket-width is 1, this allows working with
    ;; non-number bucket keys.
    (if (eqv? bucket-width 1) b (* b bucket-width)))

  (if (> (length keys) 0)
      ;; If the keys are all real numbers, we create the histogram with empty
      ;; slots in it as well (this looks nicer when plotted.
      (if real-keys?
          (let ([min (first keys)]
                [max (last keys)])
            (for/vector #:length (add1 (- max min))
                        ([bucket (in-range min (add1 max))])
              (vector (bucket->val bucket)
                      (let ((val (hash-ref buckets bucket 0)))
                        (if (and as-percentage? (> total 0))
                            (* 100 (/ val total))
                            val)))))
          (for/vector #:length (length keys)
                      ([bucket (in-list keys)])
            (vector bucket
                    (let ((val (hash-ref buckets bucket 0)))
                      (if (and as-percentage? (> total 0))
                          (* 100 (/ val total))
                          val)))))
      #f))

;; Drop buckets from boths ends of HISTOGRAM which have elements less than
;; PERCENT of the total.  We stop at the first bucket which has more than
;; PERCENT elements.  Note that empty buckets in the middle are still kept.
;; This is used to make the histogram look nicer on a graph.
(define (trim-histogram-outliers histogram [percent 0.001])
  (define total (for/sum ([b histogram]) (vector-ref b 1)))
  (define min (for/first ([b histogram]
                          [index (vector-length histogram)]
                          #:when (> (/ (vector-ref b 1) total) percent))
                index))
  (define max (for/last ([b histogram]
                         [index (vector-length histogram)]
                         #:when (> (/ (vector-ref b 1) total) percent))
                index))
  (if (and min max)
      (for/vector ([index (in-range min (add1 max))])
        (vector-ref histogram index))
      histogram))

;; Create a histogram of the data frame DF SERIES.  A histogram is a vector of
;; values, each value is a (Vectorof SAMPLE-SLOT RANK).
;;
;; #:weight-series specifies the series to be used for weighting the samples
;; (by default it it uses the weight property stored in the data-frame).  Use
;; #f for no weighting (each sample will have a weight of 1 in that case).
;;
;; #:bucket-width specifies the width of each histogram slot.  Samples are
;; grouped into slots (can be less than 0.1)
;;
;; #:trim-outliers specifies to remove slots from both ends of the histogram
;; that contain less than the specified percentage of values.
;;
;; #:include-zeroes? specifies whether samples with a slot of 0 are included
;; in the histogram or not.
;;
;; #:as-percentage? determines if the data in the histogram represents a
;; percentage (adding up to 100) or it is the rank of each slot.
;;
;; In the resulting histogram, samples that are numbers or strings will be
;; sorted.  In addition, if the samples are numbers, empty slots will be
;; created so that the buckets are also consecutive.
;;
(define (df-histogram df series
                      #:weight-series [weight (df-get-default-weight-series df)]
                      #:bucket-width [bwidth 1]
                      #:trim-outliers [trim #f]
                      #:include-zeroes? [zeroes? #t]
                      #:as-percentage? [as-pct? #f])
  (if (and (df-contains? df series)
           (or (not weight) (df-contains? df weight)))
      (let ()
        (define buckets
          (samples->buckets df series
                            #:weight-series weight
                            #:bucket-width bwidth
                            #:include-zeroes? zeroes?))
        (define histogram (buckets->histogram buckets
                                              #:bucket-width bwidth
                                              #:as-percentage? as-pct?))
        (if (and trim histogram)
            (trim-histogram-outliers histogram trim)
            histogram))
      #f))

;; Put an empty label every NTH item in DATA (a histogram/c) (which is a
;; histogram data to be plotted).  The function tries to find a suitable
;; anchor, so that the labels look nice (for example, if every second label is
;; to be dropped, the labels with even values will be kept).
(define (blank-nth data nth)
  (define anchor
    (or (for/or (((item index) (in-indexed (in-vector data))))
          (let ((label (vector-ref item 0)))
            (and (integer? label) (zero? (remainder label nth)) index)))
        (for/or (((item index) (in-indexed (in-vector data))))
          (let ((label (vector-ref item 0)))
            (and (integer? label) index)))
        0))
  (for/vector #:length (vector-length data)
              (((data index) (in-indexed (in-vector data))))
    (match-define (vector label value) data)
    (vector (if (= 0 (remainder (- index anchor) nth)) label "") value)))

;; Empty some labels in DATA (a histogram/c), which is to be plotted as a
;; histogram.  The number of blanked labels depends on how many items are in
;; the data set.
(define (blank-some-labels data)
  (define nitems (vector-length data))
  (cond ((< nitems 25) data)            ; no simplification needed
        ((< nitems 50) (blank-nth data 2))
        ((< nitems 100) (blank-nth data 5))
        ((< nitems 200) (blank-nth data 10))
        ((< nitems 400) (blank-nth data 20))
        ((< nitems 800) (blank-nth data 50))
        (#t (blank-nth data 100))))

;; Format the bucket values by calling FMT function on them.  This is used for
;; example to convert a pace value like 300 sec/min into 5:00 (min/km)
(define (format-values fmt data)
  (for/vector #:length (vector-length data)
              ((item (in-vector data)))
    (match-define (vector label value) item)
    (vector (if (number? label) (fmt label) label) value)))

;; Create a histogram plot renderer from DATA (a sequence of [BUCKET
;; NUM-SAMPLES]), as received from `df-histogram` (which see).
;;
;; #:color determines the color of the plot.
;;
;; #:skip and #:x-min are used to plot dual histograms,
;;
;; #:label prints the label of the plot.
;;
;; All the above arguments are sent directly to the `discrete-histogram' call,
;; which see
;;
;; #:blank-some-labels, controls if some of the labels are blanked out if the
;; plot contains too many values (see `blank-some-labels`)
;;
;; #:x-value-formatter which controls how the histogram values are displayed
;; (see `format-values`)
(define (histogram-renderer data
                            #:color [color #f]
                            #:skip [skip (discrete-histogram-skip)]
                            #:x-min [x-min 0]
                            #:label [label #f]
                            #:x-value-formatter [xfmt #f]
                            #:blank-some-labels [blank? #t])
  (let ((kwd '())
        (val '()))
    (define (add-arg k v) (set! kwd (cons k kwd)) (set! val (cons v val)))
    (let ((max-val #f))
      ;; Determine the max value in the plot
      (for ((d (in-vector data)))
        (let ((v (vector-ref d 1)))
          (when (or (not max-val) (> v max-val))
            (set! max-val v))))
      ;; Make the max value of the plot larger, so the top value does not
      ;; reach the top of the plot area.
      (add-arg '#:y-max (* max-val 1.1)))
    (add-arg '#:x-min x-min)
    (add-arg '#:skip skip)
    (add-arg '#:line-width 1.5)
    (when color
      (add-arg '#:line-color color))
    (add-arg '#:label label)
    (add-arg '#:gap 0.15)
    (when color
      (add-arg '#:color color)
      (add-arg '#:alpha 0.8))
    ;; Blank some of the labels, and format the remaining ones.
    (define bdata (if blank? (blank-some-labels data) data))
    (define fdata (if xfmt (format-values xfmt bdata) bdata))
    (keyword-apply discrete-histogram kwd val fdata '())))

;; Return a list of the buckets in a histogram (as made by `df-histogram`).
(define (get-histogram-buckets h)
  (for/list ([e (in-vector h)])
    (vector-ref e 0)))

;; Merge two sorted lists.
(define (merge-lists l1 l2)
  (let loop ((l1 l1)
             (l2 l2)
             (result '()))
    (cond ((null? l1) (append (reverse result) l2))
          ((null? l2) (append (reverse result) l1))
          ((= (car l1) (car l2)) (loop (cdr l1) (cdr l2) (cons (car l1) result)))
          ((< (car l1) (car l2)) (loop (cdr l1) l2 (cons (car l1) result)))
          (#t (loop l1 (cdr l2) (cons (car l2) result))))))

;; Ensure that HISTOGRAM has all buckets in BUCKETS (a sorted list).  This is
;; done by adding buckets with 0 elements if needed.  This is used when
;; histograms for two data series need to be displayed on a single plot.
(define (normalize-histogram histogram buckets)
  (for/vector #:length (length buckets) ([b (in-list buckets)])
    (or (for/first ([h (in-vector histogram)] #:when (eqv? b (vector-ref h 0))) h)
        (vector b 0))))

;; Combine two histograms H1, H2 into a single one.  The combined histogram is
;; in the format (vectorof (vector slot value1 value2))
(define (combine-histograms h1 h2)
  (let* ((nbuckets (merge-lists (get-histogram-buckets h1) (get-histogram-buckets h2)))
         (n1 (normalize-histogram h1 nbuckets))
         (n2 (normalize-histogram h2 nbuckets)))
    (unless (= (vector-length n1) (vector-length n2))
      (df-raise "combine-histograms: bad length"))
    (for/vector #:length (vector-length n1)
                ([e1 (in-vector n1)]
                 [e2 (in-vector n2)])
      (unless (equal? (vector-ref e1 0) (vector-ref e2 0))
        (df-raise "combine-histograms: bad value"))
      (vector (vector-ref e1 0) (vector-ref e1 1) (vector-ref e2 1)))))

;; Create a plot renderer with two histograms.
(define (histogram-renderer/dual combined-histogram
                                 label1 label2
                                 #:x-value-formatter [xfmt #f]
                                 #:color1 [color1 #f]
                                 #:color2 [color2 #f])
  (define data1 (make-vector (vector-length combined-histogram) #f))
  (define data2 (make-vector (vector-length combined-histogram) #f))
  (for ([(e index) (in-indexed (in-vector combined-histogram))])
    (vector-set! data1 index (vector (vector-ref e 0) (vector-ref e 1)))
    (vector-set! data2 index (vector (vector-ref e 0) (vector-ref e 2))))
  (list
   (histogram-renderer
    data1 #:color color1 #:skip 2.5 #:x-min 0 #:label label1 #:x-value-formatter xfmt)
   (histogram-renderer
    data2 #:color color2 #:skip 2.5 #:x-min 1 #:label label2 #:x-value-formatter xfmt)))

;; Split the histogram HIST into sub-histograms using FACTOR-FN (which maps
;; the histogram value to a symbol).  Returns a list of (cons TAG SUB-HIST).
;; The items in sub-histograms are kept in order with only adjacent values
;; being collapsed together under the same tag, so the same tag can appear
;; multiple times in the list (for an example of this, see splitting a
;; left-right-balance histogram)
;;
;; WARNING: `blank-some-labels' will also be called on the HIST data
(define (factor-histogram hist factor-fn)
  (define result '())
  (define tag #f)
  (define batch '())
  ;; We need to find out which labels are to be blanked out before we split
  ;; them.  Also we cannot use the blanked data itself, as we would not be
  ;; able to classify items that have been blanked out.
  (define blanked (blank-some-labels hist))
  (for ((item hist) (blanked-item blanked))
    (match-define (vector val rank) item)
    (let ((factor (factor-fn val)))
      (unless (eq? tag factor)
        (when tag
          (set! result (cons (cons tag (list->vector (reverse batch))) result)))
        (set! tag factor)
        (set! batch '())))
    (set! batch (cons blanked-item batch)))
  (when tag                           ; last one
    (set! result (cons (cons tag (list->vector (reverse batch))) result)))
  (reverse result))

;; Create a plot rendered where DATA (a histogram) is split into sections by
;; FACTOR-FN and each section is colored according to FACTOR-COLORS
(define (histogram-renderer/factors data factor-fn factor-colors
                                    #:x-value-formatter [xfmt #f])
  (define factored-data (factor-histogram data factor-fn))
  (define x 0)
  (for/list ((factor (in-list factored-data)))
    (match-define (cons ftag fdata) factor)
    (define color (cdr (assq ftag factor-colors)))
    (begin0
        (histogram-renderer fdata
                            #:color color
                            #:x-min x
                            #:x-value-formatter xfmt
                            #:blank-some-labels #f ; we already blanked them
                            )
      (set! x (+ x (vector-length fdata))))))


;;................................................. contract definitions ....

(define histogram/c (vectorof (vector/c (or/c real? string?) real?)))
(define combined-histogram/c (vectorof (vector/c (or/c real? string?) real? real?)))


;;............................................................. provides ....

(provide histogram/c combined-histogram/c)

(provide/contract
 (df-histogram (->* (data-frame? string?)
                    (#:weight-series (or/c #f string?)
                     #:bucket-width real?
                     #:trim-outliers (or/c #f (between/c 0 1))
                     #:include-zeroes? boolean?
                     #:as-percentage? boolean?)
                    (or/c #f histogram/c)))
 (trim-histogram-outliers (->* (histogram/c) (real?) histogram/c))
 (combine-histograms (-> histogram/c histogram/c combined-histogram/c))
 (histogram-renderer (->* (histogram/c)
                          (#:color any/c
                           #:skip real?
                           #:x-min real?
                           #:label (or/c #f string?)
                           #:blank-some-labels boolean?
                           #:x-value-formatter (or/c #f (-> number? string?)))
                          (treeof renderer2d?)))
 (histogram-renderer/dual (->* (combined-histogram/c string?  string?)
                               (#:color1 any/c
                                #:color2 any/c
                                #:x-value-formatter (or/c #f (-> number? string?)))
                               (treeof renderer2d?)))
 (histogram-renderer/factors (->* (histogram/c
                                   (-> real? symbol?) ; factor function
                                   (listof (cons/c symbol? color/c)))
                                  (#:x-value-formatter (or/c #f (-> number? string?)))
                                  (treeof renderer2d?))))
