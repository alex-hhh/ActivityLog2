#lang racket/base
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2018 Alex Harsanyi <AlexHarsanyi@gmail.com>
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

(require racket/class
         racket/draw
         "pict-util.rkt"
         "workout-step-snip-base.rkt"
         "../widgets/esc-controls.rkt")

(provide workout-repeat-snip%
         workout-repeat-end-snip%)

(define workout-repeat-snip-class (define-snip-class "workout-repeat-snip"))
(define workout-repeat-end-snip-class (define-snip-class "workout-repeat-end-snip"))

;; A snip representing a repeat.  Has buttons to increment/decrement the
;; number of times the repeat is executed.
;;
;; The end of the repeat is represented by a workout-repeat-end-snip% (defined
;; below), and all snips between such snip pairs are part of the repeat.  The
;; workout-pasteboard% ensures that workout-repeat-snip% and
;; workout-repeat-end-snip% are always inserted and deleted in pairs.
(define workout-repeat-snip%
  (class workout-step-snip-base%
    (init-field (times 5))
    (super-new
     [snip-class workout-repeat-snip-class]
     [pict (make-repeat-pict-top times)]
     [dark-background? #t])

    (define (get-editor)
      (let ((admin (send this get-admin)))
        (and admin (send admin get-editor))))

    (define (add-undo)
      (let ((editor (get-editor))
            (old-times times))
        (send editor x-add-undo
              (lambda ()
                (set! times old-times)
                (send this refresh-pict)))))

    (define (on-increment)
      (add-undo)
      (set! times (add1 times))
      (send this refresh-pict))

    (define (on-decrement)
      (add-undo)
      (set! times (max 1 (sub1 times)))
      (send this refresh-pict))

    (send this add-additional-button
          (new esc-button%
               [parent-snip this]
               [label 'plus]
               [width button-size]
               [height button-size]
               [callback on-increment]
               [color #f] [disabled-color #f] ; make the background transparent
               [text-color (make-object color% #xff #xfa #xcd)]
               [hover-color (make-object color% #xda #xa5 #x20)]
               [pushed-color (make-object color% #xee #xe8 #xaa)]))

    (send this add-additional-button
          (new esc-button%
               [parent-snip this]
               [label 'minus]
               [width button-size]
               [height button-size]
               [callback on-decrement]
               [color #f] [disabled-color #f] ; make the background transparent
               [text-color (make-object color% #xff #xfa #xcd)]
               [hover-color (make-object color% #xda #xa5 #x20)]
               [pushed-color (make-object color% #xee #xe8 #xaa)]))

    (define/override (make-pict nesting-level)
      (make-repeat-pict-top times nesting-level))

    ;; Return the number of times this repeat should run (note that the steps
    ;; inside the repeat are actually managed by the workout-pasteboard%, not
    ;; by this snip.
    (define/public (get-times)
      times)
    ))

;; Represents the end of a repeat block (see comments for
;; workout-repeat-snip%)
(define workout-repeat-end-snip%
  (class workout-step-snip-base%
    (init-field (times 5))
    (super-new
     [snip-class workout-repeat-end-snip-class]
     [pict (make-repeat-pict-bottom)]
     [dark-background? #t]
     [show-close-button? #f])

    (define/override (make-pict nesting-level)
      (make-repeat-pict-bottom nesting-level))
    ))
