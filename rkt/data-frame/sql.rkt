#lang racket/base
;; sql.rkt -- read data frames from SQL queries
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

(require db/base
         racket/contract
         "df.rkt"
         "series.rkt")

;; Create a data frame from the result of running SQL-QUERY on the database DB
;; with the supplied PARAMS.  SQL-QUERY can be either a string or a
;; virtual-query object.  Each column from the result set will become a series
;; in the data frame, sql-null values will be converted to #f.
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


;;............................................................. provides ....

(provide/contract
 (df-read/sql (->* (connection? (or/c string? virtual-statement?))
                   ()
                   #:rest (listof any/c)
                   data-frame?)))
