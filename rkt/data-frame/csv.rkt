#lang racket/base

;; Read and write CSV files in and out of data frames

(require racket/contract
         racket/string
         racket/format
         racket/list
         "df.rkt"
         "series.rkt")


;;............................................................ write-csv ....

(define (quote-string str)
  (string-append "\"" (string-replace str "\"" "\"\"") "\""))

(define (write-csv df outp series #:start start #:stop stop)
  (define first? #t)
  (define columns (if (null? series) (df-series-names df) series))
  (for ([header (in-list columns)])
    (if first?
        (set! first? #f)
        (write-string "," outp))
    (write-string (quote-string header) outp))
  (newline outp)
  (df-for-each
   df
   columns
   (lambda (val)
     (define first? #t)
     (for ([col (in-list columns)]
           [item (in-list val)])
       (if first?
           (set! first? #f)
           (write-string "," outp))
       (define oitem
         (cond
           ((df-is-na? df col item) "") ; this is not very fast...
           ((string? item) (quote-string item))
           ((real? item)
            (~a
             (if (exact-integer? item)
                 item
                 (exact->inexact item))))
           ;; otherwise we write in a way that we might be able to read it
           ;; back... this would work for transparent structs...
           (#t (quote-string (~s item)))))
       (write-string oitem outp))
     (newline outp))
   #:start start #:stop stop))

;; Write to OUTP a data frame DF. Optionally, the series to be written out can
;; be specified as the SERIES list. If SERIES is empty, all series are written
;; out as columns in an unspecified order.
(define (df-write/csv df outp #:start (start 0) #:stop (stop (df-row-count df)) . series)
  (if (path-string? outp)
      (call-with-output-file outp
        #:mode 'text #:exists 'truncate/replace
        (lambda (o)
          (write-csv df o series #:start start #:stop stop)))
      (write-csv df outp series #:start start #:stop stop)))



;;............................................................. read-csv ....

(define (parse-line line)
  (let ((in (open-input-string line)))
    (let loop ((c (read-char in))
               (current "")
               (row '())
               (in-string? #f))
      (cond ((eof-object? c)
             (reverse (cons current row)))
            ((and (eqv? c #\,) (not in-string?))
             (loop (read-char in) "" (cons current row) #f))
            ((and in-string? (eqv? c #\") (eqv? (peek-char in) #\"))
             (read-char in)             ; consume the next char
             (loop (read-char in) (string-append current (string c)) row in-string?))
            ((eqv? c #\")
             (loop (read-char in) (string-append current (string c)) row (not in-string?)))
            (#t
             (loop (read-char in) (string-append current (string c)) row in-string?))))))

(define (decode-value val)
  (and val
       ;; string->number decodes #x #b and #o as hex, binary or octal. We
       ;; could also recognize 0x as hex, but we don't for now.
      (let* ((trimmed (string-trim val))
             (radix 10)
             (n (string-length trimmed)))
        (cond ((= n 0) #f)
              ((and
                (>= n 2)
                (eqv? #\" (string-ref trimmed 0))
                (eqv? #\" (string-ref trimmed (sub1 n))))
               (substring trimmed 1 (sub1 n)))
              (#t
               (or (string->number trimmed radix) trimmed))))))

(define (read-csv input headers?)
  (define df (make-data-frame))
  (define series #f)
  (let loop ((line (read-line input)))
    (cond ((eof-object? line)
           (for ((s (in-vector series)))
             (df-add-series df s))
           df)
          (#t
           (let ((slots (parse-line line)))
             (if series
                 ;; Normally, a CSV file should have the same number of slots
                 ;; in each line, if there are more slots than series, we
                 ;; discard extra ones, if there is a shortfall, we add #f to
                 ;; the remaining series.
                 (let ((shortfall (max 0 (- (vector-length series) (length slots)))))
                   (for ([s (in-vector series)] [v (in-list (append slots (make-list shortfall #f)))])
                     (series-push-back s (decode-value v))))
                 ;; First row
                 (if headers?
                     (let ((index 1))
                       (set! series
                             (for/vector ([h slots])
                               ;; Gracefully handle series with empty header names
                               (let ((name (decode-value h)))
                                 (unless name
                                   (set! name (~a "col" index))
                                   (set! index (add1 index)))
                                 (make-series name)))))
                     (begin
                       (set! series (for/vector ([idx (in-range (length slots))])
                                      (make-series (format "col~a" idx))))
                       (for ([s (in-vector series)] [v (in-list slots)])
                         (series-push-back s (decode-value v)))))))
           (loop (read-line input))))))

(define (df-read/csv inp #:headers? (headers? #t))
  (if (path-string? inp)
      (call-with-input-file inp #:mode 'text
        (lambda (i) (read-csv i headers?)))
      (read-csv inp headers?)))


;;............................................................. provides ....

(provide/contract
 (df-write/csv (->* (data-frame? (or/c path-string? output-port?))
                    (#:start exact-nonnegative-integer? #:stop exact-nonnegative-integer?)
                    #:rest (listof string?)
                    any/c))
 (df-read/csv (->* ((or/c path-string? input-port?))
                   (#:headers? boolean?)
                   data-frame?)))
