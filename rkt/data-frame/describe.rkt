#lang racket/base
;; describe.rkt -- print out summary contents of the data frame
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
         racket/format
         math/statistics
         "df.rkt"
         "statistics.rkt")

;; Print to the standard output port a nice description of DF, This is useful
;; in interactive mode.
(define (df-describe df)

  (define (ppval val)
    (let ((v (if (rational? val)
                 (~r val #:precision 2)
                 (~a val))))
      (~a v #:min-width 13 #:align 'right)))
  
  (printf "data-frame: ~a columns, ~a rows~%"
          (length (df-series-names df))
          (df-row-count df))
  (printf "properties:~%")
  (let ((prop-names (df-property-names df))
        (maxw 0))
    (for ([pn prop-names])
      (set! maxw (max (string-length (~a pn)) maxw)))
    (for ([pn prop-names])
      (display "  ")
      (display (~a pn #:min-width maxw))
      (display " ")
      (display (~a (df-get-property df pn) #:max-width (max (- 75 maxw) 10)))
      (newline)))
  (printf "series:~%")
  (let ((series-names (sort (df-series-names df) string<?))
        (maxw 0))
    (for ([sn series-names])
      (set! maxw (max (string-length (~a sn)) maxw)))
    (display "  ")
    (display (~a " " #:min-width maxw))
    (printf "   NAs           min           max          mean        stddev~%")
    (for ([sn series-names])
      (display "  ")
      (display (~a sn #:min-width maxw))
      (let ((inv (df-count-na df sn)))
        (display " ")
        (display (~r inv #:min-width 5)))
      (let ([stats (df-statistics df sn)])
        (if stats
            (begin
              (display " ")
              (display (ppval (statistics-min stats)))
              (display " ")
              (display (ppval (statistics-max stats)))
              (display " ")
              (display (ppval (statistics-mean stats)))
              (display " ")
              (display (ppval (statistics-stddev stats)))
              (newline))
            (newline))))))


;;............................................................. provides ....

(provide/contract
 (df-describe (-> data-frame? any/c)))
