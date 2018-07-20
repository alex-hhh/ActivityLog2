#lang racket/base
;; exn.rkt -- exceptions thrown by data frame code
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2018 Alex Harsányi <AlexHarsanyi@gmail.com>
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


;;.................................................. exn:fail:data-frame ....

(struct exn:fail:data-frame exn:fail ()
  #:extra-constructor-name make-exn:fail:data-frame
  #:transparent)

;; Raise an exn:fail:data-frame exception first composing a message by
;; applying MESSAGE and ARGS to format, an embedding the stack
;; (current-continuation-marks)
(define (df-raise message . args)
  (raise
   (make-exn:fail:data-frame
    (apply format message args)
    (current-continuation-marks))))


;;............................................................. provides ....

(provide (struct-out exn:fail:data-frame)
         df-raise)

