#lang racket/base
;; wk-fit.rkt -- read and write FIT workout files
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

(require
 racket/contract
 racket/class
 racket/match
 racket/dict
 racket/list
 racket/math
 "wkstep.rkt"
 "../fit-file/fit-file.rkt")

(provide/contract
 (fit->workout (-> (or/c bytes? path-string? input-port?) workout?))
 (workout->fit (-> workout? bytes?)))

;; A fit-event-dispatcher% for FIT workout files.  Collects the individual
;; messages and constructs a workout (as defined in wkstep.rkt) from the data.
(define workout-builder%
  (class fit-event-dispatcher%
    (init)
    (super-new)

    (define serial #f)
    (define time-created #f)
    (define name #f)
    (define sport #f)
    (define steps '())

    ;; Process a "file id" FIT message.  This contains the serial number and
    ;; the timestamp of the workout -- these are used by Garmin devices to
    ;; identify workouts (and not the workout name!)
    (define/override (on-file-id file-id)
      (let ((type (dict-ref file-id 'type)))
        (unless (eq? 'workout type)
          (error (format "FIT File type is not a workout, type is: ~a" type))))
      (set! serial (dict-ref file-id 'serial-number #f))
      (set! time-created (dict-ref file-id 'time-created #f)))

    ;; Process the "workout" FIT message.  This contains the name of the
    ;; workout and the sport it applies to.
    (define/override (on-workout workout)
      (set! name (dict-ref workout 'name #f))
      (set! sport (dict-ref workout 'sport #f)))

    ;; Decode the duration type (time, distance or open) and the duration
    ;; value from a workout step message.  We only decode the cases that are
    ;; supported by the wkstep structures.
    (define (duration-type-and-value workout-step)
      (define dtype (dict-ref workout-step 'duration-type))
      (define dvalue (dict-ref workout-step 'duration-value))
      (case dtype
        ((time) (values dtype (/ dvalue 1000)))
        ((distance) (values dtype (/ dvalue 100)))
        ((open) (values dtype #f))
        (else (error "unknown duration type"))))

    ;; Decode the target intensity and type from a workout step message.  We
    ;; only decode the cases that are supported by the wkstep structures.
    (define (target-type-and-value workout-step)
      (define ttype (dict-ref workout-step 'target-type))
      ;; NOTE: the FIT workout format supports other ways of specifying the
      ;; target intensity (e.g. by sport zones).  We don't support that for
      ;; now.  LOW and HIGH might be #f for 'open target types.
      (define low (dict-ref workout-step 'custom-target-value-low #f))
      (define high (dict-ref workout-step 'custom-target-value-high #f))
      (case ttype
        ((heart-rate)
         ;; HR values blow 100 indicate that the value is expressed as % of
         ;; max
         (cond ((or (eq? low #f) (eq? high #f))
                (error "Only custom target types are supported for workout steps"))
               ((and (> low 100) (> high 100))
                (values ttype (- low 100) (- high 100)))
               (#t
                (error "Heart rate pct of max target type not supported"))))
        ((speed)
         (cond ((or (eq? low #f) (eq? high #f))
                (error "Only custom target types are supported for workout steps"))
               (#t
                (values ttype (/ low 1000) (/ high 1000)))))
        ((open)
         (values ttype #f #f))
        ((power)
         ;; Power values below 1000 indicate they are % of FTP, above 1000
         ;; they are POWER - 1000
         (cond ((or (eq? low #f) (eq? high #f))
                (error "Only custom target types are supported for workout steps"))
               ((and (> low 1000) (> high 1000))
                (values 'power (- low 1000) (- high 1000)))
               (#t
                (values 'power-ftp-pct low high))))
        (else
         ;; The FIT workout format supports other target types (e.g cadence).
         ;; We don't support that for now.
         (error "Unsupported target type for workout step: ~a" ttype))))

    ;; Construct a wkstep from a FIT workout step message.
    (define (make-wkstep workout-step)
      (define-values (dtype dvalue) (duration-type-and-value workout-step))
      (define-values (ttype low high) (target-type-and-value workout-step))
      (define intensity (dict-ref workout-step 'intensity))
      (wkstep intensity dtype dvalue ttype low high #f))

    ;; Construct a wkrepeat from a FIT workout repeat message
    (define (make-wkrepeat workout-step)
      (define times (dict-ref workout-step 'target-value))
      (define mindex (dict-ref workout-step 'duration-value))
      (define-values (repeat-steps remaining-steps)
        (splitf-at steps (lambda (s) (>= (car s) mindex))))
      (set! steps remaining-steps)
      (wkrepeat times (reverse (map cdr repeat-steps))))

    ;; Process a FIT workout step message constructing the appropiate wkstep
    ;; or wkrepeat structure and storing it in the STEPS list.
    (define/override (on-workout-step workout-step)
      (define mindex (dict-ref workout-step 'message-index))
      (define dtype (dict-ref workout-step 'duration-type))
      (case dtype
        ((time distance open)
         (set! steps (cons (cons mindex (make-wkstep workout-step)) steps)))
        ((repeat-until-steps-cmplt)
         (set! steps (cons (cons mindex (make-wkrepeat workout-step)) steps)))
        (else
         ;; The FIT workout format supports other duration types (mostly
         ;; regarding how the repeats can be specified.  For now, we only
         ;; support repeating a certain number of times.
         (error (format "Unsupported duration type for step: ~a" dtype)))))

    ;; Construct a workout from the data processed from the FIT file.  This
    ;; should be called after the FIT file has been parsed using
    ;; `read-fit-records`
    (define/public (get-workout)
      (workout (if (bytes? name) (bytes->string/latin-1 name) name)
               #f                     ; no description present in FIT files...
               sport
               serial
               time-created
               (reverse (map cdr steps))))
    ))


;; Read a FIT workout from SOURCE (which can be a byte string, a file name or
;; an input port) and return a WORKOUT structure containing the workout data
;; and steps.
(define (fit->workout source)
  (let ((stream (make-fit-data-stream source))
        (consumer (new workout-builder%)))
    (read-fit-records stream consumer)
    (send consumer get-workout)))


;;............................................ fit-workout-file% helpers ....

;; The functions below construct ALIST structures used in building FIT
;; messages for workouts, they are intended to be passed to the
;; fit-workout-file% add-step method.

;; Duration type: 0 -- time, 1 -- distance, 2 -- hr less than, 3 -- hr greater
;; than, 4 -- calories, 5 -- open, 6 -- repeat until steps cmplt, 7 -- repeat
;; until time, 8 -- repeat until distance, 9 -- repeat until calories, 10 --
;; repeat until hr less than, 11 -- repeat until hr greater than, 12 -- repeat
;; until power less than, 13 -- repeat until power greater than, 14 -- power
;; less than, 15 -- power greater than 15, 28 -- repetition time.

(define (make-duration seconds)
  `((duration-type . 0)
    (duration-value . ,(exact-truncate (* seconds 1000)))))

(define (make-distance meters)
  `((duration-type . 1)
    (duration-value . ,(exact-truncate (* meters 100)))))

(define (make-open)
  `((duration-type . 5)
    (duration-value . 0)))

;; Target type: 0 -- speed, 1 -- heart rate, 2 - open, 3 -- cadence, 4 --
;; power, 5 --grade, 6 -- resistance

(define (make-hr/bpm low high)
  `((target-type . 1)
    (target-value . 0)
    (custom-target-value-low . ,(exact-truncate (+ low 100)))
    (custom-target-value-high . ,(exact-truncate (+ high 100)))))

(define (make-speed low high)
  `((target-type . 0)
    (target-value . 0)
    (custom-target-value-low . ,(exact-truncate (* 1000.0 low)))
    (custom-target-value-high . ,(exact-truncate (* 1000.0 high)))))

(define (make-power low high)
  `((target-type . 4)
    (target-value . 0)
    (custom-target-value-low . ,(exact-truncate (+ 1000.0 low)))
    (custom-target-value-high . ,(exact-truncate (+ 1000.0 high)))))

(define (make-power/pct low high)
  `((target-type . 4)
    (target-value . 0)
    (custom-target-value-low . ,(exact-truncate low))
    (custom-target-value-high . ,(exact-truncate high))))
  
(define (make-no-target)
  `((target-type . 2)
    (target-value . 0)))

;; Construct a workout step ALIST from the wkstep STEP and pass it to the
;; workout-builder (a fit-workout-file% instance).
(define (build-step workout-builder step)
  (match-define (wkstep type dtype dvalue ttype tlow thigh ramp?) step)
  (define duration
    (case dtype
      ((time) (make-duration dvalue))
      ((distance) (make-distance dvalue))
      ((open) (make-open))
      (else (error dtype))))
  (define target
    (case ttype
      ((heart-rate) (make-hr/bpm tlow thigh))
      ((speed) (make-speed tlow thigh))
      ((power) (make-power tlow thigh))
      ((power-ftp-pct) (make-power/pct tlow thigh))
      ((open) (make-no-target))
      (else (error ttype))))
  (define intensity
    (list
     (cons 'intensity (case type
                        ((active) 0)
                        ((rest) 1)
                        ((warmup) 2)
                        ((cooldown) 3)
                        ((recover) 4)
                        (else (error type))))))

  (send workout-builder add-step (append duration target intensity)))

;; Construct workout repeat ALIST (steps inside the repeat are added as well)
;; from the wkrepeat REPEAT and pass it to the WORKOUT-BUILDER (a
;; fit-workout-file% instance)
(define (build-repeat workout-builder repeat)
  (match-define (wkrepeat times steps) repeat)
  (define start-index (send workout-builder get-next-message-index))
  (for ([step steps])
    (build workout-builder step))
  (send workout-builder add-step
        `((duration-type . ,6)
          (duration-value . ,start-index)
          (target-type . 2)             ; open
          (target-value . ,times))))

;; Construct a workout step (either a wkstep or a wkrepeat) and pass it to
;; WORKOUT-BUILDER (a fit-workout-file% instance)
(define (build workout-builder step)
  (cond ((wkstep? step) (build-step workout-builder step))
        ((wkrepeat? step) (build-repeat workout-builder step))
        (#t (error step))))

;; Convert WK, a workout structure into a FIT data and return it as a byte
;; string.  This byte string can be written to file and sent to a Garmin
;; device directly.
(define (workout->fit wk)
  (match-define (workout name description sport serial timestamp steps) wk)
  (define workout-builder
    (new fit-workout-file%
         [name name]
         [sport (case sport
                  ((running) 1)
                  ((cycling) 2)
                  (else "unknown sport"))]
         [serial-number serial]
         [time-created timestamp]))
  (for ([step (in-list steps)])
    (build workout-builder step))
  (send workout-builder get-fit-data))
