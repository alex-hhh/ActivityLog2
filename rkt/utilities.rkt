#lang racket/base
;; utilities.rkt -- various utilities
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015 Alex Harsanyi (AlexHarsanyi@gmail.com)
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


(require db)

(provide assq1)
(provide sql-column-ref)

(define (assq1 tag alist)
  (cond ((assq tag alist) => cdr)
        (#t #f)))

(define (sql-column-ref row col [if-null #f])
  (let ((v (vector-ref row col)))
    (if (sql-null? v) if-null v)))
