#lang racket/base
;; color-util.rkt -- convert colors from RGB to HSV and back
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

(require racket/flonum
         racket/match
         racket/math)

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

(define (hsl->rgb hue saturation luminance)
  (let* ((h (if (flonum? hue) hue (->fl hue)))
         (s (if (flonum? saturation) saturation (->fl saturation)))
         (l (if (flonum? luminance) luminance (->fl luminance)))
         (C (fl* (fl- 1.0 (flabs (fl- (fl* 2.0 l) 1.0))) s))
         (HPRIME (fl/ h 60.0))
         (X (fl* C (fl- 1.0 (flabs (fl- (mod HPRIME 2.0) 1.0)))))
         (m (flmax 0.0 (fl- l (fl* 0.5 C)))))
    (match-define (list r1 g1 b1)
      (cond ((fl<= HPRIME 0.0) (list 0.0 0.0 0.0))
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


;;............................................................. provides ....

(provide
 rgb->hsl
 rgb->hsl/255
 hsl->rgb
 hsl->rgb/255)
