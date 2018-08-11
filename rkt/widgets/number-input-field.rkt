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

(require racket/class racket/format racket/string
         "validating-input-field.rkt")
(provide number-input-field%)

(define (valid-number? data min max)
  (let ((t (string-trim data)))
    (or (= (string-length t) 0)
        (let ((v (string->number t)))
          (and v
               (or (not min) (>= v min))
               (or (not max) (<= v max)))))))

(define (convert-to-number data)
  (let ((t (string-trim data)))
    (if (= (string-length data) 0)
        'empty
        (string->number data))))

;; An input field for reading numbers, integer or floating
;; point. `valid-value-cb` needs to be specified to read the value.
;; `min-value' and `max-value`, if not #f, specify the minimum and maximum
;; values that are considered valid.
(define number-input-field%
  (class validating-input-field%
    (init [min-value #f] [max-value #f])
    (super-new [validate-fn (lambda (v) (valid-number? v min-value max-value))]
               [convert-fn convert-to-number])
    (inherit set-value)

    ;; Set the numeric value N as the contents of this field.
    (define/public (set-numeric-value n)
      (set-value
       (if (integer? n) (format "~a" n) (~r n #:precision 2))))

    ))
