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

(require racket/gui/base racket/class
         "number-input-field.rkt")
(provide number-range-selector%)

(define number-range-selector%
  (class object%
    (init parent label callback)
    (super-new)

    (define cb callback)
    (define low #f)
    (define high #f)

    (define pane (new horizontal-pane% [parent parent] [stretchable-height #f]
                      [spacing 0] [border 0] [alignment '(left center)]))

    (new message% [parent pane] [label label] [min-width 70])

    (define low-input
      (new number-input-field%
           [valid-value-cb (lambda (v) (set! low (if (eq? v 'empty) #f v)) (cb (cons low high)))]
           [parent pane] [label ""]
           [style '(single)] [stretchable-width #f] [min-width 1]))
    ;; (new message% [parent pane] [label "--"])
    (define high-input
      (new number-input-field%
           [parent pane] [label "--"]
           [valid-value-cb (lambda (v) (set! high (if (eq? v 'empty) #f v)) (cb (cons low high)))]
           [style '(single)] [stretchable-width #f] [min-width 1]))

    (define/public (get-number-range) (cons low high))
    (define/public (set-number-range l h)
      (if (number? l)
          (begin
            (send low-input set-numeric-value l)
            (set! low l))
          (begin
            (send low-input set-value "")
            (set! low #f)))
      (if (number? h)
          (begin
            (send high-input set-numeric-value h)
            (set! high h))
          (begin
            (send high-input set-value "")
            (set! high #f))))

    ))
