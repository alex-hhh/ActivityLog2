#lang racket/base

(require racket/draw racket/class pict racket/format
         "wkstep.rkt")

(provide make-step-pict
         make-repeat-pict-top
         make-repeat-pict-bottom
         total-width)

(define bg-color (make-object color% #xff #xf8 #xdc 0.8))

(define repeat-bg-color (make-object color% #xdc #xdc #xdc))

(define item-color (make-object color% #x2f #x4f #x4f))
(define label-color (make-object color% #x77 #x88 #x99))
(define title-font (send the-font-list find-or-create-font 12 'default 'normal 'normal))
(define item-font (send the-font-list find-or-create-font 12 'default 'normal 'normal))
(define label-font (send the-font-list find-or-create-font 10 'default 'normal 'normal))
(define title-face (cons item-color title-font))
(define item-face (cons item-color item-font))
(define label-face (cons label-color label-font))

(define repeat-bg-colors
  (list
   (make-object color% #x36 #x64 #x8b)  ; steel blue4
   (make-object color% #x6c #x7b #x8b)  ; slate gray4
   ))

(define repeat-item-color (make-object color% #xff #xfa #xfa 0.8)) ; snow
(define repeat-label-color (make-object color% #xdc #xdc #xdc))

(define repeat-item-font (send the-font-list find-or-create-font 14 'default 'normal 'normal))

(define repeat-item-face (cons repeat-item-color repeat-item-font))
(define repeat-label-face (cons repeat-label-color label-font))

(define total-width 600)
(define total-height 40)
(define item-margin 20)
(define item-spacing 15)

(define (make-step-pict step [nesting-level 0])
  (define width (- total-width item-margin item-spacing item-spacing item-spacing
                   (* nesting-level 2 item-spacing)))
  (define section-width (/ width 12))

  (define (adjust p section-width)
    (define radj (- section-width (pict-width p)))
    (inset p 0 0 (max 0 radj) 0))

  (define t-pict (adjust (text (wkstep-type->string step) title-face)
                         (* 2 section-width)))
  (define d-pict (adjust (hc-append
                          10
                          (text (wkstep-dtype->string step) label-face)
                          (text (wkstep-dvalue->string step) item-face))
                         (* 3 section-width)))
  (define i-pict (adjust (hc-append
                          10
                          (text (wkstep-ttype->string step) label-face)
                          (text (wkstep-tvalue->string step) item-face))
                         (* 6 section-width)))
  (define s-pict (adjust (text "") section-width)) ; spacer
  (define p1 (hc-append item-spacing t-pict d-pict i-pict s-pict))

  (define height-adjust (- total-height (pict-height p1)))

  (define bg-color (wkstep-color step))
  (define p2
    (cc-superimpose
     (filled-rounded-rectangle (+ (pict-width p1) item-margin)
                               (+ (pict-height p1) height-adjust) -0.2
                               #:draw-border? #f
                               #:color bg-color)
     p1))

  (if (> nesting-level 0)
      (inset p2 (* 2 nesting-level item-spacing) 0 0 0)
      p2))

(define (make-repeat-pict-top times [nesting-level 0])
  (define width (- total-width item-margin (* nesting-level 2 item-spacing)))
  (define section-width width)

  (define (adjust p)
    (define radj (- section-width (pict-width p)))
    (inset p 0 0 (max 0 radj) 0))

  (define r-pict (adjust (hc-append
                          10
                          (text "Repeat" repeat-label-face)
                          (text (~a times) repeat-item-face)
                          (text "times" repeat-label-face))))
  (define p1 r-pict)
  (define height-adjust (- total-height (pict-height p1)))
  (define repeat-bg-color
    (list-ref repeat-bg-colors
              (remainder nesting-level (length repeat-bg-colors))))

  (define p2
    (cc-superimpose
     (filled-rounded-rectangle (+ (pict-width p1) item-margin)
                               (+ (pict-height p1) height-adjust) -0.2
                               #:draw-border? #f
                               #:color repeat-bg-color)
     p1))
  (if (> nesting-level 0)
      (inset p2 (* 2 nesting-level item-spacing) 0 0 0)
      p2))

(define (make-repeat-pict-bottom [nesting-level 0])
  (define width (- total-width (* nesting-level 2 item-spacing)))
  (define repeat-bg-color
    (list-ref repeat-bg-colors
              (remainder nesting-level (length repeat-bg-colors))))
  (define p2
    (filled-rounded-rectangle width (/ total-height 3) -0.2
                              #:draw-border? #f
                              #:color repeat-bg-color))
  (if (> nesting-level 0)
      (inset p2 (* 2 nesting-level item-spacing) 0 0 0)
      p2))
