#lang racket

(require "../rkt/al-prefs.rkt")
(require "../rkt/database.rkt")
(require "../rkt/dbapp.rkt")
(require "../rkt/data-frame.rkt")
(require "../rkt/session-df.rkt")
;; (require "../rkt/plot-builder.rkt")
(require "../rkt/plot-axis-def.rkt")
(require db)
(require math/statistics)
(require plot)
(require (rename-in srfi/48 (format format-48)))

;;(define session-id 1610) ;; run activity
;;(define session-id 1672)
;;(define session-id 30) ; swim
;;(define session-id 1668)
(define session-id 1672)

;; Open the default database
(define *db*
  (let ((db-file (al-get-pref 'activity-log:database-file (lambda () #f))))
    (unless db-file
      (error "No default database"))
    (open-activity-log db-file)))
(when *db* (current-database *db*))


;.....................................................................  ....


(define df (time (make-session-data-frame *db* session-id)))

(define (classify-cadence cad)
  (define cad1 (* 2 cad))
  (cond ((> cad1 185) 'purple)
        ((> cad1 174) 'blue)
        ((> cad1 163) 'green)
        ((> cad1 151) 'orange)
        (#t 'red)))

(define (add-cadence-classifier-series data-frame)
  (send data-frame add-derived-series
        "cad/class"
        '("cad")
        (lambda (val)
          (match-define (vector cad) val)
          (classify-cadence cad))))

(define (make-mask series pred)
  (for/vector ([elt series]) (pred elt)))

(define (mask-series series mask)
  (for/vector ([tst mask] [val series] #:when tst)
    val))

(define classifier-table
  (list
   (list 'red '(220 20 60))
   (list 'orange '(255 127 80))
   (list 'green '(34 139 34))
   (list 'blue '(30 144 255))
   (list 'purple '(139 0 139))))

(define (cad-nice-plot df)
  (define data (send df select-columns "timer" "cad"))
  (for/list ([elt classifier-table])
    (match-define (list classifier color) elt)
    (define plot-data
      (for/vector ([elt data]
                   #:when
                   (let ()
                     (match-define (vector timer cad) elt)
                     (and cad (eq? (classify-cadence cad) classifier))))
        elt))
    (points plot-data #:color color)))

(define (calculate-correlation data-frame col1 col2)
  (define data1 (send data-frame select-column col1))
  (define data2 (send data-frame select-column col2))
  (define sel-mask
    (for/vector ([d1 data1] [d2 data2]) (and d1 d2)))
  (define filt-data1 (for/vector ([idx (in-range (vector-length sel-mask))]
                                  #:when (vector-ref sel-mask idx))
                       (vector-ref data1 idx)))
  (define filt-data2 (for/vector ([idx (in-range (vector-length sel-mask))]
                                  #:when (vector-ref sel-mask idx))
                       (vector-ref data2 idx)))
  (if (> (vector-length filt-data1) 0)
      (correlation filt-data1 filt-data2)
      #f))

(define (print-correlation-matrix data-frame . columns)

  (define cwidth (max 6 (apply max (map string-length columns))))

  (define (fmt-hdr name)
    (~a name #:width cwidth #:align 'right #:pad-string " "))

  (define (fmt-num num)
    (fmt-hdr (~r num #:precision 2)))

  (printf (fmt-hdr ""))
  (for ([c columns])
    (printf " ")
    (printf (fmt-hdr c)))
  (printf "~%")
  (for ([r columns])
    (printf (fmt-hdr r))
    (for ([c columns])
      (let ((r (calculate-correlation df r c)))
        (printf " ")
        (if r
            (printf (fmt-num r))
            (printf (fmt-hdr "")))))
    (printf "~%")))


(define (print-stats title s)
  (printf
   (format-48 "~a mean: ~1,2F, stddev: ~1,2F, skewness: ~1,2F, kurtosis: ~1,2F~%"
              title
              (statistics-mean s)
              (statistics-stddev s)
              (statistics-skewness s)
              (statistics-kurtosis s))))

(define (split-series data-series)
  (define s1 '())
  (define s2 '())
  (for ([d data-series])
    (match-define (vector e1 e2 _) d)
    (when (> e1 0)
      (set! s1 (cons e1 s1))
      (set! s2 (cons e2 s2))))
  (values (reverse s1) (reverse s2)))

(define (correlation1 data-series)
  (let-values ([(s1 s2) (split-series data-series)])
    (correlation s1 s2)))

(define (my-round val (fractional-digits 0))
  (define mult (expt 10 fractional-digits))
  (define inv (expt 10 (- fractional-digits)))
  (* (round (* val mult)) inv))

(define (pretty-print-scatter-group group)
  (define keys (sort (hash-keys group) <))
  (printf "rank count: ~a~%" (length keys))
  (for ((key keys))
    (let ((nitems (length (hash-ref group key))))
      (printf "rank: ~a, items ~a~%" key nitems))))

(define (find-ranges series)
  (if (= (sequence-length series) 0)
      '()
      (let ((start 0)
            (item (sequence-ref series 0)))
        ;; NOTE: index will start at 0, but we already removed the first
        ;; element
        (for/list ([(val index)
                    (in-indexed
                     (in-sequences (sequence-tail series 1) (list (gensym))))]
                   #:unless (equal? item val))
          (begin0
              (vector start (+ 1 index) item)
            (set! start (+ 1 index))
            (set! item val))))))

(define (decoupling df s1 s2 #:start (start 0) #:end (end (send df get-row-count)))
  (let ((half-point (exact-truncate (/ (+ start end) 2))))
    (let ((stat-s1-1 (df-statistics df s1 #:start start #:end half-point))
          (stat-s1-2 (df-statistics df s1 #:start half-point #:end end))
          (stat-s2-1 (df-statistics df s2 #:start start #:end half-point))
          (stat-s2-2 (df-statistics df s2 #:start half-point #:end end)))
      (let ((r1 (/ (statistics-mean stat-s1-1)
                   (statistics-mean stat-s2-1)))
            (r2 (/ (statistics-mean stat-s1-2)
                   (statistics-mean stat-s2-2))))
        (* 100.0 (/ (- r1 r2) r1))))))

(define (decoupling/laps df s1 s2)
  (let ((laps (send df get-property 'laps)))
    (for/list ([idx (in-range 1 (vector-length laps))])
      (match-define (list start end)
        (send df get-index*
              "timestamp"
              (vector-ref laps (- idx 1))
              (vector-ref laps idx)))
      (decoupling df s1 s2 #:start start #:end end))))
