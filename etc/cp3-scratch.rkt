#lang racket
(require "al-interactive.rkt")
(require "../rkt/models/critical-power.rkt")
(require "../rkt/metrics.rkt")
(require plot)
(require al2-profiler)
(require future-visualizer)

(define candidates
  (query-list (current-database)
              "select S.id from A_SESSION s where S.sport_id = 2 and S.start_time > strftime('%s', 'now', '-42 days')"))

(define ammax (aggregate-mmax candidates "pwr"))
(define bavgfn (aggregate-mmax->spline-fn ammax))

(define search-params-2
  (cp2search 120 300
             720 1200))

(define search-params-3
  (cp3search 10 45
             120 300
             720 1200))

#;(define cp2-result (search-best-cp2/exhausive
                    bavgfn search-params-2
                    #:cp-precision 1
                    #:wprime-precision -2))

#;(define cp3-result (search-best-cp3/exhausive
                    bavgfn search-params-3
                    #:cp-precision 1
                    #:wprime-precision -2
                    #:k-precision 1))

#;(define (do-plot)
  (parameterize ([plot-x-transform log-transform])
    (send (plot-frame (list (function bavgfn #:color 1 #:label "Data")
                            (function (cp2-fn cp2-result) #:color 2 #:label "CP2")
                            (function (cp3-fn cp3-result) #:color 3 #:label "CP3")) #:x-min 10 #:x-max 3600)
                       show #t)))

#;(visualize-futures (search-best-cp3/exhausive
                    bavgfn search-params-3
                    #:cp-precision 1
                    #:wprime-precision -2
                    #:k-precision 1))
