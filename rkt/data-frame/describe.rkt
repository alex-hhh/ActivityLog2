#lang racket/base
(require racket/contract)

;; Print to the standard output port a nice description of DF, This is useful
;; in interactive mode.
(define (df-describe df)

  (define (ppval val)
    (let ((v (if (rational? val)
                 (~r val #:precision 2)
                 (~a val))))
      (~a v #:min-width 13 #:align 'right)))
  
  (printf "data-frame: ~a columns, ~a rows~%"
          (length (df-column-names df))
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
  (let ((series-names (sort (df-column-names df) string<?))
        (maxw 0))
    (for ([sn series-names])
      (set! maxw (max (string-length (~a sn)) maxw)))
    (display "  ")
    (display (~a " " #:min-width maxw))
    (printf "   NAs           min           max          mean        stddev~%")
    (for ([sn series-names])
      (display "  ")
      (display (~a sn #:min-width maxw))
      (let ((inv (dfcol-na-count (df-get-column df sn))))
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

(provide/contract
 (df-describe (-> data-frame? any/c)))
