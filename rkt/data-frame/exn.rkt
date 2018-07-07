#lang racket/base

(provide (struct-out exn:fail:data-frame)
         df-raise)


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

