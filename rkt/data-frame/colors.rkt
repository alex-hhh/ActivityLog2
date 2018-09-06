#lang racket/base
;; colors.rkt -- color manipulation utilitite
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

(require pict
         racket/class
         racket/contract
         racket/draw
         racket/flonum
         racket/list
         racket/match
         racket/math
         plot/utils)

;; harmonious-mix, mix-neutral, contrast-of and desaturate-neutral, are
;; inspired by https://tallys.github.io/color-theory/
;;
;; Converting between RGB and HSL code is based on the Wikipedia article at:
;; https://en.wikipedia.org/wiki/HSL_and_HSV

;; Racket does not seem to have this built in?
(define (mod x y)
  (let ((r (flfloor (fl/ x y))))
    (fl- x (fl* r y))))

;; Convert a set of RGB values (in the range of 0..1) into HSL values.
(define (rgb->hsl red green blue)
  (let* ((r (if (flonum? red) red (->fl red)))
         (g (if (flonum? green) green (->fl green)))
         (b (if (flonum? blue) blue (->fl blue)))

         (M (flmax r (flmax g b)))
         (m (flmin r (flmin g b)))
         (C (fl- M m))
         (HPRIME (cond ((zero? C) 0.0)
                       ((fl= M r)
                        (fl/ (fl- g b) C))
                       ((fl= M g)
                        (fl+ (fl/ (fl- b r) C) 2.0))
                       ((fl= M b)
                        (fl+ (fl/ (fl- r g) C) 4.0))))
         (H (fl* (mod HPRIME 6.0) 60.0))
         (L (fl/ (fl+ M m) 2.0))
         (S-HSL (if (fl> L 0.999) 0 (fl/ C (fl- 1.0 (flabs (fl- (fl* 2.0 L) 1.0)))))))
    (list H S-HSL L)))

;; Convert a set of RGB values in the range 0 .. 255 into HSL values.
(define (rgb->hsl/255 red green blue)
  (rgb->hsl (/ red 255.0) (/ green 255.0) (/ blue 255.0)))

(define (color->hsl c)
  (rgb->hsl (/ (send c red) 255.0)
            (/ (send c green) 255.0)
            (/ (send c blue) 255.0)))

(define (hsl->rgb hue saturation luminance)
  (let* ((h (if (flonum? hue) hue (->fl hue)))
         (s (if (flonum? saturation) saturation (->fl saturation)))
         (l (if (flonum? luminance) luminance (->fl luminance)))
         (C (fl* (fl- 1.0 (flabs (fl- (fl* 2.0 l) 1.0))) s))
         (HPRIME (fl/ h 60.0))
         (X (fl* C (fl- 1.0 (flabs (fl- (mod HPRIME 2.0) 1.0)))))
         (m (flmax 0.0 (fl- l (fl* 0.5 C)))))
    (match-define (list r1 g1 b1)
      (cond ((fl< HPRIME 0.0) (list 0.0 0.0 0.0))
            ((fl<= HPRIME 1.0) (list C X 0.0))
            ((fl<= HPRIME 2.0) (list X C 0.0))
            ((fl<= HPRIME 3.0) (list 0.0 C X))
            ((fl<= HPRIME 4.0) (list 0.0 X C))
            ((fl<= HPRIME 5.0) (list X 0.0 C))
            ((fl<= HPRIME 6.0) (list C 0.0 X))
            (#t (list 0.0 0.0 0.0))))
    (list (fl+ r1 m) (fl+ g1 m) (fl+ b1 m))))

(define (hsl->rgb/255 hue saturation value)
  (match-define (list r g b) (hsl->rgb hue saturation value))
  (list
   (exact-round (fl* r 255.0))
   (exact-round (fl* g 255.0))
   (exact-round (fl* b 255.0))))

(define (hsl->color h s l)
  (match-define (list r g b) (hsl->rgb/255 h s l))
  (make-object color% r g b))

;; -------------------------------------------------------------

;; Combine PERCENT amount from the CMIX color into CBASE.  If MIX-ALPHA? is
;; #t, the alpha of CBASE is also mixed by the same percent, otherwise it
;; remains unchanged.
(define (mix-colors cmix cbase percent #:mix-alpha? [mix-alpha? #f])
  (define percent^ (- 1 percent))
  (make-object color%
    (exact-round (+ (* (send cbase red) percent^) (* (send cmix red) percent)))
    (exact-round (+ (* (send cbase green) percent^) (* (send cmix green) percent)))
    (exact-round (+ (* (send cbase blue) percent^) (* (send cmix blue) percent)))
    (if mix-alpha?
        (+ (* (send cbase alpha) percent^) (* (send cmix alpha) percent))
        (send cbase alpha))))

;; Return the complement of the color C.  This is the color at the opposite
;; side (180 degrees) of the hue circle.
(define (complement c)
  (match-define (list h s l) (color->hsl c))
  (define ch (mod (+ h 180.0) 360.0))
  (hsl->color ch s l))

;; Return a new color by increasing the saturation of color C by AMOUNT.
(define (saturate c amount)
  (match-define (list h s l) (color->hsl c))
  (define cs (max 0 (min 1 (+ s amount))))
  (hsl->color h cs l))

;; Return a new color by decreasing the saturation of color C by ADJUST.
(define (desaturate c amount)
  (saturate c (- amount)))

;; Return a new color by increasing the lightness of the color C by AMOUNT.
(define (lighten c amount)
  (match-define (list h s l) (color->hsl c))
  (define cl (max 0 (min 1 (+ l amount))))
  (hsl->color h s cl))

;; Return a new color by decreasing the lightness of the color C by AMOUNT.
(define (darken c amount)
  (lighten c (- amount)))

(define (cool-color? c)
  (match-define (list h s l) (color->hsl c))
  (< 140 h 310))

(define (high-key-value? c)
  (match-define (list h s l) (color->hsl c))
  (< 20 h 190))

(define (highest-key-value? c)
  (match-define (list h s l) (color->hsl c))
  (< 30 h 90))

;; Determine a mixing percent between the CMIX and CBASE colors that produces
;; "nice" results.  Nice is subjective.
(define (hmix-percent-1 cmix cbase)
  (if (cool-color? cmix)
      (if (high-key-value? cbase) 0.11 0.16)
      (if (high-key-value? cbase) 0.13 0.23)))

;; Determine a mixing percent between the CMIX and CBASE colors that produces
;; "nice" results.  Nice is subjective, in fact this is the second version of
;; "nice".
(define (hmix-percent-2 cmix cbase)
  (cond ((highest-key-value? cbase)
         (if (high-key-value? cmix) 0.19 0.13))
        ((high-key-value? cbase)
         (if (high-key-value? cmix) 0.31 0.23))
        (else
         (cond ((highest-key-value? cmix) 0.31)
               ((high-key-value? cmix) 0.26)
               (else 0.32)))))

(define (harmonious-mix cmix cbase)
  (mix-colors cmix cbase (hmix-percent-1 cmix cbase)))

(define (mix-neutral cbase)
  (define cmix (complement cbase))
  (define pct (hmix-percent-2 cmix cbase))
  (mix-colors cmix cbase pct))

(define (desaturate-neutral cbase)
  (define cmix (complement cbase))
  (define pct (hmix-percent-2 cmix cbase))
  (desaturate cbase pct))

(define (contrast-of color)
  (match-define (list h s l) (color->hsl color))
  (if (high-key-value? color)
      (cond ((< l 0.3) (lighten (complement color) 0.86))
            ((> l 0.7) (darken (complement color) 0.68))
            (else (darken (complement color) 0.53)))
      (cond ((< l 0.3) (lighten (complement color) 0.86))
            ((> l 0.7) (darken (complement color) 0.68))
            (else (lighten (complement color) 0.53)))))

(define (make-palette start-hue num-colors)
  (define slice (/ 360.0 num-colors))
  (match-define (list h s l) (list (exact->inexact start-hue) 1.0 0.5))
  (for/list ([r (in-range num-colors)])
    (define ch (mod (+ h (* r slice)) 360.0))
    (hsl->color ch s l)))

;; Compute colors for the keys of a scatter group renderer.  KEYS is a sorted
;; list of numbers (the groups ranks for the group renderer).  The BASE-COLOR
;; is used to compute a range of colors from lightest for the smallest rank to
;; darkest for the highest rank.  Returns a hash map mapping each key to a
;; color value.
(define (make-key-colors keys base-color)
  ;; NOTE: keys are sorted and should not contain duplicates
  (define range                         ; make sure range is never 0
    (if (< (length keys) 2)
        1
        (- (last keys) (first keys))))
  (match-define (list h s l) (apply rgb->hsl/255 (->pen-color base-color)))
  (define min-l 0.3)
  (define max-l 0.8)
  (define l-range (- max-l min-l))
  (for/hash ((key (in-list keys)))
    (let* ((pct (/ (- key (first keys)) range))
           (new-l (+ min-l (* (- 1 pct) l-range))))
      (match-define (list r g b) (hsl->rgb/255 h s new-l))
      (values
       key
       (make-object color% r g b)))))

(provide/contract
 [mix-colors (->* ((is-a?/c color%) (is-a?/c color%) (between/c 0 1))
                  (#:mix-alpha? boolean?)
                  (is-a?/c color%))]
 [complement (-> (is-a?/c color%) (is-a?/c color%))]
 [saturate (-> (is-a?/c color%) real? (is-a?/c color%))]
 [desaturate (-> (is-a?/c color%) real? (is-a?/c color%))]
 [lighten (-> (is-a?/c color%) real? (is-a?/c color%))]
 [darken (-> (is-a?/c color%) real? (is-a?/c color%))]
 [harmonious-mix (-> (is-a?/c color%) (is-a?/c color%) (is-a?/c color%))]
 [mix-neutral (-> (is-a?/c color%) (is-a?/c color%))]
 [desaturate-neutral (-> (is-a?/c color%) (is-a?/c color%))]
 [contrast-of (-> (is-a?/c color%) (is-a?/c color%))]
 [make-palette (-> real? (and/c integer? positive?) (listof (is-a?/c color%)))]
 [make-key-colors (-> (listof real?) plot-color/c (hash/c real? (is-a?/c color%)))])
