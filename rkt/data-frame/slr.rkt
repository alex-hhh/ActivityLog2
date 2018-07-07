#lang racket/base

(require racket/contract
         racket/format
         math/statistics
         plot/utils
         plot)

;; Simple linear regression parameters Y = alpha + beta * X.  r is the
;; correlation coefficient.
(struct slr (alpha beta r))

;; Compute linear regression parameters for the list of samples XS, YS,
;; optionally weighted by WS.
;;
;; https://en.wikipedia.org/wiki/Simple_linear_regression
(define (make-slr xs ys (ws #f))
  (let ((x-stats (update-statistics* empty-statistics xs ws))
        (y-stats (update-statistics* empty-statistics ys ws))
        (r (correlation xs ys)))
    (let* ((beta (* r (/ (statistics-stddev y-stats) (statistics-stddev x-stats))))
           (alpha (- (statistics-mean y-stats) (* beta (statistics-mean x-stats)))))
      (slr alpha beta r))))

;; Return a function renderer for the linear regression defined by SLR
(define (slr-renderer slr)
  (function
   (lambda (x) (+ (slr-alpha slr) (* (slr-beta slr) x)))
   #:color '(#x2f #x4f #x4f)
   #:width 2
   #:label (format "r = ~a" (~r (slr-r slr) #:precision 2))))

(provide/contract
 (slr? (-> any/c boolean?))
 (make-slr (->* ((listof number?) (listof number?))
                ((listof number?))
                slr?))
 (slr-renderer (-> slr? (treeof renderer2d?))))
