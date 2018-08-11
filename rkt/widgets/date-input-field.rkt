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

(require racket/class racket/string racket/format racket/date
         "validating-input-field.rkt")
(provide date-input-field%)

;; TODO: accept dates using 10-12-2014, plus other combinations
(define (convert-to-date data)
  (let ((t (string-trim data)))
    (if (= (string-length t) 0)
        'empty
        (let ((m (regexp-match "^([0-9]+)/([0-9]+)/([0-9]+)$" t)))
          (if m
              (let ((day (string->number (list-ref m 1)))
                    (month (string->number (list-ref m 2)))
                    (year (string->number (list-ref m 3))))
                ;; Limit the year to a reasonable range, as a big year range
                ;; causes performance problems in different places.  If this
                ;; app is still in use after 2100, it is a succesful one :-)
                (if (or (> year 2100) (< year 1900))
                    #f
                    (with-handlers (((lambda (e) #t) (lambda (e) #f)))
                                   (find-seconds 0 0 0 day month year))))
              #f)))))

(define date-input-field%
  (class validating-input-field%
    (init)
    (super-new
     [cue-text "dd/mm/yyyy"]
     [validate-fn convert-to-date]
     [convert-fn convert-to-date])
    (inherit set-value)

    (define/public (set-date-value seconds)
      (if (or (eq? seconds #f) (eq? seconds 'empty)) ; special case
          (set-value "")
          (let ((d (seconds->date seconds #t)))
            (set-value
             (string-append
              (~r (date-day d) #:precision 0 #:min-width 2 #:pad-string "0")
              "/"
              (~r (date-month d) #:precision 0 #:min-width 2 #:pad-string "0")
              "/"
              (~r (date-year d) #:precision 0 #:min-width 2 #:pad-string "0"))))))
    ))
