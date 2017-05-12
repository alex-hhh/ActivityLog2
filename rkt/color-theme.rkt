#lang racket/base
;; color-theme.rkt -- support for color themes
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2017 Alex Harsanyi (AlexHarsanyi@gmail.com)
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

(require racket/draw
         racket/runtime-path
         racket/class
         racket/match
         "utilities.rkt")

(define-runtime-path the-theme-file "./default-theme.rktd")
(define the-theme-data #f)
(define the-zone-colors #f)
(define the-factor-colors #f)
(define the-swim-stroke-colors #f)
(define the-series-colors #f)
(define the-sport-colors #f)            ; NOTE: this is a hash!
(define the-sport-dark-colors #f)       ; NOTE: this is a hash!

(provide zone-colors
         factor-colors
         swim-stroke-colors
         series-colors
         sport-colors
         sport-colors-dark)

;; Return the color theme database, if it is not loaded, it will also be
;; loaded from disk.
(define (theme-data)
  (unless the-theme-data
    (with-handlers
      (((lambda (e) #t)
        (lambda (e)
          (dbglog-exception "theme-data" e)
          #f)))
      (set! the-theme-data (call-with-input-file the-theme-file read))))
  the-theme-data)

;; Create a color map for the colors specified by TAG from DATA (as returned
;; by (theme-data).  Returns an ALIST mapping either a color name to the
;; corresponding color% object.
(define (make-color-map tag data)
  (with-handlers
    (((lambda (e) #f)
      (lambda (e)
        (dbglog-exception (format "make-color-map ( ~a )" tag) e)
        #f)))
    (let ((colors (assq tag data)))
      (if colors
          (for/list ((color (in-list (cdr colors))))
            (cons (car color) (apply make-object color% (cdr color))))
          #f))))

;; Create a color map for sport ID's for the colors specified by TAG from DATA
;; (as returned by (theme-data).  Returns a HASH mapping a sport a (Vector
;; sport sub-sport) to the color% object
(define (make-sport-color-map tag data)
  (with-handlers
    (((lambda (e) #f)
      (lambda (e)
        (dbglog-exception (format "make-color-map ( ~a )" tag) e)
        #f)))
    (let ((colors (assq tag data)))
      (if colors
          (for/hash ((color (in-list (cdr colors))))
            (match-define (list sport sub-sport red green blue) color)
            (values
             (vector sport sub-sport)
             (make-object color% red green blue)))
          #f))))

;; Return the colors to be used for sport zones, load them from disk if
;; needed.
(define (zone-colors)
  (unless the-zone-colors
    (let ((data (theme-data)))
      (when data
        (set! the-zone-colors
              (or (make-color-map 'zone-colors data)
                  ;; Provide a default set, if we fail to load the color data
                  (list
                   (cons 'z0 (make-object color% #xad #xd8 #xe6))
                   (cons 'z1 (make-object color% #x00 #xbf #xff))
                   (cons 'z2 (make-object color% #x22 #x8b #x22))
                   (cons 'z3 (make-object color% #xff #x7f #x50))
                   (cons 'z4 (make-object color% #xcd #x5c #x5c))
                   (cons 'z5 (make-object color% #xdc #x14 #x3c))
                   (cons 'z6 (make-object color% #x8b #x00 #x00))
                   (cons 'z7 (make-object color% #x99 #x32 #xcc))
                   (cons 'z8 (make-object color% #x00 #x00 #x8b))
                   (cons 'z9 (make-object color% #xff #x8c #x00))
                   (cons 'z10 (make-object color% #xda #xa5 #x20))))))))
  the-zone-colors)

;; Return the colors to be used for "factors" (these are currently the colors
;; used for the GCT, Cadence, etc. colorization). Load them from disk if
;; needed.
(define (factor-colors)
  (unless the-factor-colors
    (let ((data (theme-data)))
      (when data
        (set! the-factor-colors
              (or (make-color-map 'factor-colors data)
                  (list
                   (cons 'red (make-object color% 220 20 60))
                   (cons 'orange (make-object color% 255 127 80))
                   (cons 'green (make-object color% 34 139 34))
                   (cons 'blue (make-object color% 30 144 255))
                   (cons 'purple (make-object color% 139 0 139))))))))
  the-factor-colors)

;; Return the colors used to draw swim strokes.  Load them from disk if
;; needed.
(define (swim-stroke-colors)
  (unless the-swim-stroke-colors
    (let ((data (theme-data)))
      (when data
        (set! the-swim-stroke-colors
              (or (make-color-map 'swim-stroke-colors data)
                  (list
                   (cons 0 (make-object color% 65 105 225))      ; freestyle
                   (cons 1 (make-object color% 159 237 255))     ; backstroke
                   (cons 2 (make-object color% 160 255 178))     ; breaststroke
                   (cons 3 (make-object color% #xff #xd7 #x00))  ; butterfly
                   (cons 4 (make-object color% 255 191 0))       ; drill
                   (cons 5 (make-object color% #x00 #xbf #xff))  ; mixed
                   (cons 6 (make-object color% #xdc #x14 #x3c))  ; IM
                   (cons #f (make-object color% #x80 #x80 #x80)) ; unknown
                   ))))))
  the-swim-stroke-colors)

(define (series-colors)
  (unless the-series-colors
    (let ((data (theme-data)))
      (when data
        (set! the-series-colors (make-color-map 'series-colors data)))))
  the-series-colors)

(define (sport-colors)
  (unless the-sport-colors
    (let ((data (theme-data)))
      (set! the-sport-colors
            (or (make-sport-color-map 'sport-colors data)
                (hash
                 (vector 0 #f) (make-object color% 238 238 238))))))
  the-sport-colors)

(define (sport-colors-dark)
  (unless the-sport-dark-colors
    (let ((data (theme-data)))
      (set! the-sport-dark-colors
            (or (make-sport-color-map 'sport-colors-dark data)
                (hash
                 (vector 0 #f) (make-object color% 47 79 79))))))
  the-sport-dark-colors)
