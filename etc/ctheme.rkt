#lang racket
(require racket/draw)
(require pict)
(require "../rkt/color-theme.rkt")

;; Pretty print a pict with the colors for a theme color set, to allow us to
;; visualize them.
(define (pp-colors colors)

  (define pad 30)

  (apply
   hc-append
   (for/list ([item colors])
     (match-define (cons label color) item)
     (define txpict (text (~a label)))
     (cc-superimpose
      (filled-rectangle (+ pad (pict-width txpict))
                        (+ pad (pict-height txpict))
                        #:draw-border? #f
                        #:color color)
      txpict))))

;; Same as pp-colors but for (sport-colors) and (sport-colors-dark)
(define (pp-colors/sport colors)

  (define pad 30)

  (apply
   hc-append
   (for/list ([(sport color) (in-hash colors)])
     (define txpict (text (~a sport)))
     (cc-superimpose
      (filled-rectangle (+ pad (pict-width txpict))
                        (+ pad (pict-height txpict))
                        #:draw-border? #f
                        #:color color)
      txpict))))
