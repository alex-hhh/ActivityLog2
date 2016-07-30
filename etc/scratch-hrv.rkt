#lang racket

(require db
         math/statistics
         plot
         "../rkt/data-frame.rkt"
         "al-interactive.rkt"
         "../rkt/hrv.rkt")


;;................................................................. rest ....

(define (df-density-renderer df series)
  (match-define (list q01 q25 q50 q75 q99) (df-quantile df series 0 0.25 0.5 0.75 0.99))
  (define stats (df-statistics df series))
  (list
   (density (send df select series) #:x-min q01 #:x-max q99 #:width 2 #:color "red")
   (tick-grid)
   (vrule q25 #:label "25%" #:color "blue" #:style 'long-dash)
   (vrule q50 #:label "50%" #:color "green" #:style 'long-dash)
   (vrule (statistics-mean stats) #:label "mean" #:color "black" #:style 'long-dash)
   (vrule q75 #:label "75%" #:color "blue" #:style 'long-dash)))

(define session-id 1717)                ; skiing
;;(define session-id 1709)                ; running

