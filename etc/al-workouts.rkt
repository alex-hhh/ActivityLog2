#lang racket
;; al-worktouts.rkt -- library functions for creating FIT workouts

;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015, 2018 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require "../rkt/fit-file/fit-file.rkt")
(require "../rkt/fit-file/fit-defs.rkt")

(provide (all-defined-out))
(provide (all-from-out "../rkt/fit-file/fit-file.rkt"))


;;....................................................... duration types ....

;; Duration type: 0 -- time, 1 -- distance, 2 -- hr less than, 3 -- hr greater
;; than, 4 -- calories, 5 -- open, 6 -- repeat until steps cmplt, 7 -- repeat
;; until time, 8 -- repeat until distance, 9 -- repeat until calories, 10 --
;; repeat until hr less than, 11 -- repeat until hr greater than, 12 -- repeat
;; until power less than, 13 -- repeat until power greater than, 14 -- power
;; less than, 15 -- power greater than 15, 28 -- repetition time.

(define (duration seconds)
  `((duration-type . 0)
    (duration-value . ,(exact-truncate (* seconds 1000)))))

(define (distance meters)
  `((duration-type . 1)
    (duration-value . ,(exact-truncate (* meters 100)))))

(define (lap-button)
  `((duration-type . 5)
    (duration-value . 0)))

;; derived

(define (minutes m) (duration (* m 60)))
(define (kms k) (distance (* k 1000)))
(define (miles m) (distance (* m 1600)))


;;......................................................... target types ....

;; Target type: 0 -- speed, 1 -- heart rate, 2 - open, 3 -- cadence, 4 --
;; power, 5 --grade, 6 -- resistance

(define (hr/bpm low high)
  `((target-type . 1)
    (target-value . 0)
    (custom-target-value-low . ,(exact-truncate (+ low 100)))
    (custom-target-value-high . ,(exact-truncate (+ high 100)))))

(define (pace min sec)
  (let ((t (+ (* min 60) sec)))
    (/ 1000.0 t)))

(define (speed low high)
  `((target-type . 0)
    (target-value . 0)
    (custom-target-value-low . ,(exact-truncate (* 1000.0 low)))
    (custom-target-value-high . ,(exact-truncate (* 1000.0 high)))))

(define (no-target)
  `((target-type . 2)
    (target-value . 0)))


;;...................................................... intensity types ....

(define active 0)
(define rest 1)
(define warmup 2)
(define cooldown 3)
(define recover 4)

(define (step duration target intensity)
  (let ((step (append duration target (list (cons 'intensity intensity)))))
    (lambda (builder)
      (send builder add-step step))))

(define (repeat times . steps)
  (lambda (builder)
    (let ((mindex (send builder get-next-message-index)))
      (for ([step (in-list steps)])
        (step builder))
      (send builder add-step
            `((duration-type . ,6)
              (duration-value . ,mindex)
              (target-value . ,times))))))

(define (workout . steps)
  (lambda (builder)
    (for ([step (in-list steps)])
      (step builder))))


;;.................................................. reading and writing ....

(define (wr file-name bstr)
  (call-with-output-file file-name
    (lambda (out) (write-bytes bstr out))
    #:mode 'binary
    #:exists 'replace))

(define workout-builder%
  (class fit-event-dispatcher%
    (init)
    (super-new)

    (define/override (on-file-id file-id) (printf "file-id: ~a~%" file-id))
    (define/override (on-file-creator creator) (printf "file-creator: ~a~%" creator))
    ;;(define/override (on-activity activity) #f)
    ;;(define/override (on-session session) (printf "session~%"))
    ;;(define/override (on-record record) #f)
    ;;(define/override (on-length length) #f)
    ;;(define/override (on-lap lap) #f)
    ;; (define/override (on-device-info device-info) #f)
    ;; (printf "device-info: ~a~%" device-info))
    ;;(define/override (on-location location) #f)
    (define/override (on-workout workout) (printf "workout: ~a~%" workout))
    (define/override (on-workout-step workout-step) (printf "workout-step: ~a~%" workout-step))
    (define/override (on-sport sport) (printf "sport: ~a~%" sport))

    ))

(define (rd file)
  (let ((stream (make-fit-data-stream file))
        (consumer (new workout-builder%)))
    (printf "made the stream~%")
    (read-fit-records stream consumer)))
