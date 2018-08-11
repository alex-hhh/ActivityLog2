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

(require racket/class racket/string racket/format
         "validating-input-field.rkt")
(provide time-of-day-input-field%)

;; TODO: check for valid ranges for hours, minutes
(define (convert-to-time-of-day data)
  (let ((t (string-trim data)))
    (cond ((= (string-length t) 0) 'empty)
          ((regexp-match "^([0-9]+):([0-9]+):?([0-9]+)? *((?i:am|pm))?$" t) =>
           (lambda (m)
             (let ((hour (string->number (list-ref m 1)))
                   (minute (string->number (list-ref m 2)))
                   (seconds (let ((s (list-ref m 3)))
                              (if s (string->number s) 0)))
                   (am-pm? (list-ref m 4)))
               (when (string? am-pm?)
                 (set! am-pm? (string-upcase am-pm?)))
                (+ (* (+ (* hour 60) minute) 60) seconds
                   (if am-pm?
                       (cond ((and (string=? am-pm? "AM") (= hour 12))
                              (* -12 60 60))
                             ((and (string=? am-pm? "PM") (not (= hour 12)))
                              (* 12 60 60))
                             (#t 0))
                       0)))))
          (#t #f))))

(define time-of-day-input-field%
  (class validating-input-field%
    (init)
    (super-new
     [cue-text "hh:mm AM/PM"]
     [validate-fn convert-to-time-of-day]
     [convert-fn convert-to-time-of-day])
    (inherit set-value)

    (define/public (set-time-of-day-value seconds)
      (let ((d (seconds->date seconds #t)))
        (set-value
         (string-append
          (~r (date-hour d) #:precision 0 #:min-width 2 #:pad-string "0")
          ":"
          (~r (date-minute d) #:precision 0 #:min-width 2 #:pad-string "0")
          ":"
          (~r (date-second d) #:precision 0 #:min-width 2 #:pad-string "0")))))
    ))

