#lang racket/base
;; wk-estimate.rkt -- estimate the duration of workoutsx
;;
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

(require pict
         racket/draw
         racket/match
         racket/class
         racket/math
         racket/contract
         "../fmt-util.rkt"
         "wkstep.rkt")

(provide/contract
 (estimate-workout (-> workout? (hash/c symbol? (list/c (or/c #f number?) symbol?))))
 (estimate-workout/pict (-> workout? (or/c pict? #f))))

;; Estimate the duration of a workout step.  Returns a list of two values: a
;; duration in seconds and the type of the estimate which can be one of the
;; symbols 'exact, 'open, 'approximate and 'unknown.
;;
;; Not all workout steps can be accurately estimated.  For example, we cannot
;; estimate the duration of a workout step like "1000 meters @ Heart Rate
;; between 150 and 180".
(define (estimate step)
  (match-define (wkstep type dtype dval ttype tlow thigh ramp?) step)
  (case dtype
    ((time) (list type dval 'exact))
    ((open) (list type 0 'open))
    ;; If the "duration" is distance based, and the target is "speed" we can
    ;; estimate the time duration of the step...
    ((distance)
     (if (eq? ttype 'speed)
         (list type (/ dval (* 0.5 (+ thigh tlow))) 'approximate)
         (list type 0 'unknown)))))

;; Merge two estimate types (e.g. when adding the estimates for an exact step
;; and an approximate step, the result will be an approximate step.
(define (merge e1 e2)
  (case e1
    ((exact) e2)
    ((open) e1)
    ((approximate) e1)
    ((unknown) e1)
    (else (error e1))))

;; Estimate the total duration of a list of workout STEPS.  Returns a hash
;; mapping the step kind (warmup, active, cooldown, etc) to a duration and
;; duration estimate, as returned by `estimate`.
(define (estimate-steps steps)
  (define result (make-hash))
  (for ([step (in-list steps)])
    (cond
      ((wkstep? step)
       (match-let* (((list type duration kind) (estimate step))
                    ((list d k) (hash-ref result type '(0 exact))))
         (hash-set! result type (list (+ d duration) (merge kind k)))))
      ((wkrepeat? step)
       (let ((times (wkrepeat-times step)))
         (for ([(key value) (in-hash (estimate-steps (wkrepeat-steps step)))])
           (match-define (list d k) (hash-ref result key '(0 exact)))
           (match-define (list duration kind) value)
           (hash-set! result key (list (+ d (* times duration)) (merge kind k))))))))
  result)

;; Estimate the total duration of the workout WK.  Returns the same value as
;; `estimate-steps`.
(define (estimate-workout wk)
  (estimate-steps (workout-steps wk)))

(define item-color (make-object color% #x2f #x4f #x4f))
(define label-color (make-object color% #x77 #x88 #x99))
(define title-font (send the-font-list find-or-create-font 14 'default 'normal 'normal))
(define item-font (send the-font-list find-or-create-font 12 'default 'normal 'normal))
(define label-font (send the-font-list find-or-create-font 10 'default 'normal 'normal))
(define title-face (cons item-color title-font))
(define item-face (cons item-color item-font))
(define label-face (cons label-color label-font))

;; Return a pict with the duration estimate for the workout WK.  This is
;; intended to be displayed in the GUI.
(define (estimate-workout/pict wk)
  (define estimates (estimate-steps (workout-steps wk)))

  (define total
    (for/sum ([v (in-hash-values estimates)])
      (match-define (list duration _) v)
      duration))

  (define (line-item name tag)

    (match-define (list duration kind)
      (hash-ref estimates tag (list 0 'unknown)))

    (if (zero? duration)
        null
        (list
         (filled-rounded-rectangle 20 20 -0.25
                                   #:color (wkstep-type->color tag)
                                   #:border-color item-color
                                   #:border-width 1.0)
         (text name item-face)
         (text (duration->string duration) item-face)
         (text (format "~a %" (exact-round (* 100 (/ duration total)))) item-face)
         (text (case kind
                 ((exact) "")
                 ((open) "... or more")
                 ((unknown approximate) "... approximate")
                 (else "")) label-face))))

  (define items
    (append
     (line-item "Warmup" 'warmup)
     (line-item "Active" 'active)
     (line-item "Recover" 'recover)
     (line-item "Rest" 'rest)
     (line-item "Cooldown" 'cooldown)))

  (and (> (length items) 0)
       (vl-append
        30
        (text (format "Total duration   ~a" (duration->string total)) title-face)
        (table 5 items lc-superimpose cc-superimpose 15 3))))
