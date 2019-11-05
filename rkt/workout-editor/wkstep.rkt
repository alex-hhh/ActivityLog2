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

(require racket/class
         racket/dict
         racket/draw
         racket/format
         racket/math
         "../fmt-util.rkt")

(provide
 (struct-out wkstep)
 (struct-out wkrepeat)
 (struct-out workout)
 wkstep-type->string
 wkstep-type->color
 wkstep-color
 wkstep-dtype->string
 wkstep-dvalue->string
 wkstep-ttype->string
 wkstep-tvalue->string

 wkstep-type-names
 wkstep-dtype-names
 wkstep-ttype-names

 wkstep-type->index
 wkstep-dtype->index
 wkstep-ttype->index

 )
 
;; A workout step, encapsulates a duration or distance and an intensity level
;; to be maintained.  An entire workout is a sequence of these steps, plus
;; repeats.
(struct wkstep
  (type                       ; 'warmup, 'active, 'recover, 'cooldown or 'rest
   dtype                      ; duration type: 'time, 'distance or 'open
   dvalue                     ; time in seconds or distance in meters
   ttype                      ; target intensity type: 'open, 'heart-rate,
                              ; 'speed, 'power, 'power-ftp-pct
   tlow                       ; low value for target (depends on tkind)
   thigh                      ; high value for target (depends on tkind)
   ramp?                      ; is this a ramp between tlow and thigh? (not
                              ; used for now)
   )
  #:transparent)

;; A workout repeat encapsulates a number of steps that has to be repeated a
;; certain number of times.
(struct wkrepeat
  (times
   steps)
  #:transparent)

;; A workout, encapsulates a name, a sport and a list of steps.
;;
;; On the SERIAL and TIMESTAMP fields: a FIT workout is identified by two
;; values: a serial number and a timestamp.  At least one of them need to be
;; different for the Garmin head unit to recognize the file as new.  We
;; generate a serial number when we create a workout and a new timestamp each
;; time it is saved.  Therefore new versions of the same workout will have the
;; same serial number but a different timestamp.
(struct workout
  (name                                 ; string?
   description                          ; (or/c string? #f)
   sport                                ; (or/c 'running 'cycling)
   serial                               ; number?
   timestamp                            ; number?
   steps)                               ; (listof (or/c wkstep? wkrepeat?))
  #:transparent)

(define wkstep-type-names
  '((warmup . "Warmup")
    (active . "Active")
    (recover . "Recover")
    (cooldown . "Cooldown")
    (rest . "Rest")))

(define wkstep-type-colors
  (list
   (cons 'warmup (make-object color% #xff #xa0 #x7a 0.8)) ; light salmon
   (cons 'active (make-object color% #x87 #xce #xfa 0.8)) ; light sky blue
   (cons 'recover (make-object color% #xff #xff #xe0 0.8)) ; light yellow
   (cons 'cooldown (make-object color% #x8f #xbc #x8f 0.8)) ;dark sea green
   (cons 'rest (make-object color% #xf5 #xf5 #xdc 0.8))))   ; beige

(define wkstep-dtype-names
  '((time . "Duration")
    (distance . "Distance")
    (open . "Lap Button Press")))

(define wkstep-ttype-names
  '((open . "No Target")
    (heart-rate . "Heartrate")
    (speed . "Pace")
    (power . "Power")
    (power-ftp-pct . "Power (% of FTP)")))

(define (wkstep-type->string step)
  (dict-ref wkstep-type-names (wkstep-type step)))

(define (wkstep-type->color type)
  (dict-ref wkstep-type-colors type))

(define (wkstep-color step)
  (dict-ref wkstep-type-colors (wkstep-type step)))

(define (wkstep-dtype->string step)
  (dict-ref wkstep-dtype-names (wkstep-dtype step)))

(define (wkstep-dvalue->string step)
  (define value (wkstep-dvalue step))
  (case (wkstep-dtype step)
    ((distance)
     (if (< value 2000)
         (short-distance->string value #t)
         (distance->string value #t)))
    ((time)
     (duration->string value))
    ((open) "")
    (else (raise-arguments-error
           'wkstep-dvalue->string "bad duration kind"
           "step" (wkstep-dtype step)))))

(define (wkstep-ttype->string step)
  (dict-ref wkstep-ttype-names (wkstep-ttype step)))

(define (wkstep-tvalue->string step)
  (define low (wkstep-tlow step))
  (define high (wkstep-thigh step))
  (case (wkstep-ttype step)
    ((open) "")
    ((heart-rate)
     (string-append (~a (exact-round low)) " to "
                    (~a (exact-round high) " bpm")))
    ((speed)
     (string-append
      (pace->string low #f) " to " (pace->string high #t)))
    ((power)
     (string-append (~a (exact-round low)) " to "
                    (~a (exact-round high)) " watts"))
    ((power-ftp-pct)
     (string-append (~a (exact-round low)) " to "
                    (~a (exact-round high)) " %"))
    (else (raise-arguments-error
           'wkstep-tvalue->string "bad target kind"
           "step" (wkstep-ttype step)))))

(define (wkstep-type->index type)
  (or (for/first ([(item index) (in-indexed wkstep-type-names)]
                  #:when (eq? (car item) type))
        index)
      (error type)))

(define (wkstep-dtype->index dtype)
  (or (for/first ([(item index) (in-indexed wkstep-dtype-names)]
                  #:when (eq? (car item) dtype))
        index)
      (error dtype)))

(define (wkstep-ttype->index ttype)
  (or (for/first ([(item index) (in-indexed wkstep-ttype-names)]
                  #:when (eq? (car item) ttype))
        index)
      (error ttype)))

