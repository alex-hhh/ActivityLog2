#lang racket/base
(require racket/match
         racket/vector)

;; Implement the Ramer–Douglas–Peucker line simplification algorithm, as
;; described here:
;; https://en.wikipedia.org/wiki/Ramer%E2%80%93Douglas%E2%80%93Peucker_algorithm

(provide rdp-simplify)

;; Return the distance from the point P0 to the line defined by P1 - P2.  A
;; point is a (vector X Y)
(define (perpendicular-distance p0 p1 p2)
  (match-define (vector x0 y0) p0)
  (match-define (vector x1 y1) p1)
  (match-define (vector x2 y2) p2)
  (let* ((yd (- y2 y1))
         (xd (- x2 x1))
         (denom (sqrt (+ (* yd yd) (* xd xd))))
         (nom (abs (- (+ (- (* yd x0) (* xd y0)) (* x2 y1)) (* y2 x1)))))
    (/ nom denom)))


;; Run the simplification algorithm on POINTS, a (vectorof (vector X Y)),
;; return a new vector contained the simplified line.
(define (rdp-simplify points eps)

  ;; Run the simplification algorithm on POINTS a (vectorof (vector X Y))
  ;; between the START and END indexes.  END is inclusive.  The algorithm
  ;; updates POINTS in place, replacing points that should be removed with #f.
  (define (rdp-simplify-1 points (start 0) (end (sub1 (vector-length points))))
    (when (> (- end start) 1)
      (let ((p1 (vector-ref points start))
            (p2 (vector-ref points end)))
        (define epsilons
          (for/vector ((index (in-range (add1 start) end)))
            (perpendicular-distance (vector-ref points index) p1 p2)))
        (let ((max-epsilon 0)
              (max-epsilon-index -1))
        (for (((epsilon index) (in-indexed (in-vector epsilons))))
          (when (> epsilon max-epsilon)
            (set! max-epsilon epsilon)
            (set! max-epsilon-index index)))
        (if (> max-epsilon eps)
            (let ((mid (add1 (+ start max-epsilon-index))))
              (rdp-simplify-1 points start mid)
              (rdp-simplify-1 points mid end))
            (for ([index (in-range (add1 start) end)])
              (vector-set! points index #f)))))))
  
  (define working-data (vector-copy points))
  (rdp-simplify-1 working-data)
  (for/vector ((p (in-vector working-data)) #:when p)
    p))
