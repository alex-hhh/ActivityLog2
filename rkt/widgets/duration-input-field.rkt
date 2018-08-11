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

(require racket/class racket/string
         "validating-input-field.rkt"
         "widget-utilities.rkt")
(provide duration-input-field%)

;; TODO: check for valid ranges for hours, minutes, seconds
(define (convert-to-duration data)
  (let ((t (string-trim data)))
    (cond ((= (string-length t) 0) 'empty)
          ((regexp-match "^([0-9]+):([0-9]+):([0-9]+)$" t) =>
           (lambda (m)
             (let ((hours (string->number (list-ref m 1)))
                   (minutes (string->number (list-ref m 2)))
                   (seconds (string->number (list-ref m 3))))
               (if (and (< minutes 60) (< seconds 60))
                   (+ (* (+ (* hours 60) minutes) 60) seconds)
                   #f))))
          ((regexp-match "^([0-9]+):([0-9]+)$" t) =>
           (lambda (m)
             (let ((minutes (string->number (list-ref m 1)))
                   (seconds (string->number (list-ref m 2))))
               (if (and (< minutes 60) (< seconds 60))
                   (+ (* minutes 60) seconds)
                   #f))))
          (#t #f))))

(define duration-input-field%
  (class validating-input-field%
    (init)
    (super-new
     [cue-text "hh:mm:ss"]
     [validate-fn convert-to-duration]
     [convert-fn convert-to-duration])
    (inherit set-value)

    (define/public (set-duration-value duration)
      (set-value (w-duration->string duration)))

    ))

