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

(require json
         racket/contract
         "wkstep.rkt")

(provide/contract
 (workout->jsexpr (-> workout? jsexpr?))
 (jsexpr->workout (-> jsexpr? workout?)))

;; The functions below help convert a workout structure (and wkstep, wkrepeat
;; structures) to and from a jsexpr which can be serialized to JSON.  While a
;; workout expression can be written and read just fine, and uses less space
;; than the equivalent JSON expression, the JSON string has the advantage that
;; it can be easily read and written from other languages, and it can also
;; adapt to "upgrades" in the future, should the contents of the workout
;; structure change.

(define (wkstep->jsexpr step)
  (hash
   'kind "step"
   'intensity (symbol->string (wkstep-type step))
   'duration-type (symbol->string (wkstep-dtype step))
   'duration (let ((d (wkstep-dvalue step)))
               (and d (exact->inexact d)))
   'target-type (symbol->string (wkstep-ttype step))
   'target-low (let ((d (wkstep-tlow step)))
                 (and d (exact->inexact d)))
   'target-high (let ((d (wkstep-thigh step)))
                  (and d (exact->inexact d)))))

(define (wkrepeat->jsexpr repeat)
  (hash
   'kind "repeat"
   'times (wkrepeat-times repeat)
   'steps (map step->jsexpr (wkrepeat-steps repeat))))

(define (step->jsexpr step)
  (cond ((wkstep? step) (wkstep->jsexpr step))
        ((wkrepeat? step) (wkrepeat->jsexpr step))
        (#t (error step))))

(define (workout->jsexpr wk)
  (hash
   'name (workout-name wk)
   'description (workout-description wk)
   'sport (symbol->string (workout-sport wk))
   'serial (workout-serial wk)
   'timestamp (workout-timestamp wk)
   'steps (map step->jsexpr (workout-steps wk))))

(define (jsexpr->wkstep jsexpr)
  (wkstep
   (string->symbol (hash-ref jsexpr 'intensity))
   (string->symbol (hash-ref jsexpr 'duration-type))
   (hash-ref jsexpr 'duration)
   (string->symbol (hash-ref jsexpr 'target-type))
   (hash-ref jsexpr 'target-low)
   (hash-ref jsexpr 'target-high)
   #f                                   ; is-ramp?
   ))

(define (jsexpr->repeat jsexpr)
  (wkrepeat
   (hash-ref jsexpr 'times)
   (let ((steps (hash-ref jsexpr 'steps)))
     (map jsexpr->step steps))))

(define (jsexpr->step jsexpr)
  (let ((kind (hash-ref jsexpr 'kind)))
    (cond ((string=? kind "step")
           (jsexpr->wkstep jsexpr))
          ((string=? kind "repeat")
           (jsexpr->repeat jsexpr))
          (#t
           (error (format "bad step kind: ~a" kind))))))

(define (jsexpr->workout jsexpr)
  (workout
   (hash-ref jsexpr 'name)
   (hash-ref jsexpr 'description (lambda () #f))
   (let ((sport (hash-ref jsexpr 'sport)))
     (string->symbol sport))
   (hash-ref jsexpr 'serial (lambda () 1))
   (hash-ref jsexpr 'timestamp (lambda () (current-seconds)))
   (let ((steps (hash-ref jsexpr 'steps)))
     (map jsexpr->step steps))))
