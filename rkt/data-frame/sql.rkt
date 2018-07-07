#lang racket/base
(require racket/contract
         db
         "df.rkt"
         "series.rkt")

;; Create a data-frame% from the result of running SQL-QUERY.  Each column
;; from the result will be a series in the data frame, sql-null values will be
;; converted to #f.
(define (df-read/sql db sql-query . params)
  (define result (apply query db sql-query params))
  (define headers
    (for/list ([hdr (rows-result-headers result)])
      (cond ((assq 'name hdr) => cdr)
            (#t "unnamed"))))
  (define rows (rows-result-rows result))
  (define num-rows (length rows))
  (define df (make-data-frame))

  ;; If query returned 0 rows, don't add any series to the data frame
  (when (> num-rows 0)
    (define data-series
      (for/list ((x (in-range (length headers))))
        (make-vector num-rows #f)))
    (for ([row rows]
          [x (in-range num-rows)])
      (for ([series data-series]
            [y (in-range (length data-series))])
        (let ((val (vector-ref row y)))
          (vector-set! series x (if (sql-null? val) #f val)))))
    
    (for ([h (in-list headers)]
          [s (in-list data-series)])
      (let ((col (make-series h #:data s)))
        (df-add-series df col))))

  df)

(provide/contract
 (df-read/sql (->* (connection? (or/c string? virtual-statement?))
                   ()
                   #:rest (listof any/c)
                   data-frame?)))
