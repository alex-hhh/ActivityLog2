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
         racket/format
         racket/gui/base
         racket/math)

(provide make-horizontal-pane
         make-vertical-pane
         make-group-box-panel
         make-spacer
         with-busy-cursor
         w-duration->string)

;; Convenience function to make a horizontal-pane as required by the various
;; dialog boxes
(define (make-horizontal-pane parent (stretchable-height? #t))
  (new horizontal-pane% [parent parent]
       [border 0] [spacing 5]
       [stretchable-height stretchable-height?]))

(define (make-vertical-pane parent (stretchable-width? #t))
  (new vertical-pane% [parent parent]
       [border 0] [spacing 5]
       [stretchable-width stretchable-width?]))

(define (make-group-box-panel parent
                              (label "")
                              (stretchable-width? #t)
                              (stretchable-height? #t))
  (let ((gb (new group-box-panel%
                 [parent parent] [label label] [alignment '(left center)]
                 [stretchable-width stretchable-width?]
                 [stretchable-height stretchable-height?]
                 [spacing 10] [border 5])))
    ;; The group box panel seems to ignore the internal border, so we create
    ;; an inner vertical pane and return that.
    (new vertical-pane% [parent gb] [spacing 10]
         [alignment '(left center)]
         [stretchable-width stretchable-width?]
         [stretchable-height stretchable-height?])))

;; Create a spacer widget, with a fixed width.
(define (make-spacer parent (witdh 5) (stretchable? #f))
  (new message% [parent parent] [label ""] [min-width witdh] [stretchable-width stretchable?]))

(define (with-busy-cursor thunk)
  (begin-busy-cursor)
  (with-handlers
    (((lambda (x) #t)
      (lambda (x) (end-busy-cursor) (raise x))))
    (thunk))
  (end-busy-cursor))

(define (w-duration->string seconds)
  (let* ((h (exact-truncate (/ seconds 3600.0)))
         (m (exact-truncate (/ (- seconds (* h 3600.0)) 60.0)))
         (s (exact-truncate (- seconds (* h 3600.0) (* m 60.0)))))
    (string-append
     (if (> h 0)
         (string-append (~r h #:precision 0 #:min-width 2 #:pad-string "0") ":")
         "")
     (~r m #:precision 0 #:min-width 2 #:pad-string "0") ":"
     (~r s #:precision 0 #:min-width 2 #:pad-string "0"))))
