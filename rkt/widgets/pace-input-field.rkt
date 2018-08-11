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
         "../fmt-util.rkt")

(provide pace-input-field% swim-pace-input-field%)

(define (validate-pace data)
  ;; run-pace-string->mps takes care of measurement system
  (let ((t (string-trim data)))
    (or (= (string-length t) 0) (run-pace-string->mps t))))

(define (convert-pace data)
  (let ((t (string-trim data)))
    (if (= (string-length t) 0)
        'empty
        (run-pace-string->mps t))))

(define pace-input-field%
  (class validating-input-field%
    (init)
    (super-new
     [cue-text (if (eq? (al-pref-measurement-system) 'metric) "mm:ss / km" "mm:ss / mi")]
     [validate-fn validate-pace]
     [convert-fn convert-pace])
    (inherit set-value)

    (define/public (set-pace-value value)
      (set-value (if value (pace->string value) "")))

    ))

(define (validate-swim-pace data)
  ;; run-pace-string->mps takes care of measurement system
  (let ((t (string-trim data)))
    (or (= (string-length t) 0) (swim-pace-string->mps t))))

(define (convert-swim-pace data)
  (let ((t (string-trim data)))
    (if (= (string-length t) 0)
        'empty
        (swim-pace-string->mps t))))

(define swim-pace-input-field%
  (class validating-input-field%
    (init)
    (super-new
     [cue-text (if (eq? (al-pref-measurement-system) 'metric) "mm:ss / 100m" "mm:ss / 100yd")]
     [validate-fn validate-swim-pace]
     [convert-fn convert-swim-pace])
    (inherit set-value)

    (define/public (set-pace-value value)
      (set-value (if value (swim-pace->string value) "")))

    ))
