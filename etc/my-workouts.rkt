#lang racket
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

(define (duration seconds)
  `((duration-type . 0)
    (duration-value . ,(* seconds 1000))))

(define (distance meters)
  `((duration-type . 1)
    (duration-value . ,(* meters 100))))

(define (hr/bpm low high)
  `((target-type . 1)
    (target-value . 0)
    (custom-target-value-low . ,(+ low 100))
    (custom-target-value-high . ,(+ high 100))))

(define (pace min sec)
  (let ((t (+ (* min 60) sec)))
    (/ 1000.0 t)))

(define (speed low high)
  `((target-type . 0)
    (target-value . 0)
    (custom-target-value-low . ,(exact-truncate (* 1000.0 low)))
    (custom-target-value-high . ,(exact-truncate (* 1000.0 high)))))

(define (lap-button)
  `((duration-type . 5)
    (duration-value . 0)))

(define (no-target)
  `((target-type . 2)
    (target-value . 0)))

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



;
;                                                        ;            ;;
;     ;;;                  ;                             ;           ;
;    ;   ;                 ;                             ;           ;
;   ;      ;   ;   ;;;   ;;;;;   ;;;   ;;;;;          ;;;;   ;;;   ;;;;;   ;;;
;   ;      ;   ;  ;   ;    ;    ;   ;  ; ; ;         ;   ;  ;;  ;    ;    ;   ;
;   ;      ;   ;  ;        ;    ;   ;  ; ; ;         ;   ;  ;   ;    ;    ;
;   ;      ;   ;   ;;;     ;    ;   ;  ; ; ;         ;   ;  ;;;;;    ;     ;;;
;   ;      ;   ;      ;    ;    ;   ;  ; ; ;         ;   ;  ;        ;        ;
;    ;   ; ;   ;  ;   ;    ;    ;   ;  ; ; ;         ;   ;  ;   ;    ;    ;   ;
;     ;;;   ;;;;   ;;;     ;;;   ;;;   ; ; ;          ;;;;   ;;;     ;     ;;;
;
;
;

(define (hr/warmup) (hr/bpm 60 160))
(define (pace/tempo) (speed (pace 4 40) (pace 5 0)))

;
;                                               ;        ;
;   ;;;;;;  ;;;;          ;;;;    ;                      ;
;       ;; ;    ;        ;    ;   ;                      ;
;       ;       ;        ;      ;;;;;   ;;;;  ;;;     ;;;;   ;;;    ;;;
;      ;        ;        ;;       ;     ;;  ;   ;    ;   ;  ;;  ;  ;   ;
;     ;;       ;          ;;;;    ;     ;       ;    ;   ;  ;   ;  ;
;     ;       ;               ;   ;     ;       ;    ;   ;  ;;;;;   ;;;
;    ;       ;                ;   ;     ;       ;    ;   ;  ;          ;
;   ;;      ;            ;    ;   ;     ;       ;    ;   ;  ;   ;  ;   ;
;   ;;;;;; ;;;;;;         ;;;;    ;;;   ;     ;;;;;   ;;;;   ;;;    ;;;
;
;
;


(define (wk-z2-strides)
  (define wk (new fit-workout-file% [name "z2-strides"] [sport 1]))
  ((workout
    (step (duration (* 30 60)) (hr/bpm 90 160) 0)
    (step (duration (* 2 60)) (no-target) 1) ; rest
    (repeat
     6
     (step (duration 20) (no-target) 0)
     (step (duration 60) (no-target) 4))          ; recover

    ;; Cooldown
    (step (duration (* 10 60))
          (speed (pace 5 20) (pace 6 10))
          3) ; cooldown
    (step (lap-button) (no-target) 3)             ; cooldown
    )
   wk)
  (send wk get-fit-data))


;
;
;   ;     ;;;;;;; ;    ; ;;;;;        ;;;;;;;                 ;
;   ;        ;    ;    ; ;    ;          ;                    ;
;   ;        ;    ;    ; ;    ;          ;     ;;;    ;;;   ;;;;;
;   ;        ;    ;    ; ;    ;          ;    ;;  ;  ;   ;    ;
;   ;        ;    ;;;;;; ;;;;;           ;    ;   ;  ;        ;
;   ;        ;    ;    ; ;   ;           ;    ;;;;;   ;;;     ;
;   ;        ;    ;    ; ;    ;          ;    ;          ;    ;
;   ;        ;    ;    ; ;    ;          ;    ;   ;  ;   ;    ;
;   ;;;;;;   ;    ;    ; ;     ;         ;     ;;;    ;;;     ;;;
;
;
;


(define (wk-lthr-test)
  (define wk (new fit-workout-file% [name "run-fthr-test"] [sport 1]))
  ((workout
    (step (duration (* 10 60)) (hr/bpm 60 160) 2)
    (step (duration (* 10 60)) (no-target) 0)
    (step (duration (* 20 60)) (no-target) 0))
   wk)
  (send wk get-fit-data))


;
;                                      ;                             ;
;   ;    ;                             ;               ;      ;
;   ;;  ;;                             ;               ;      ;
;   ;;  ;;  ;;;   ; ;;    ;;;    ;;;;  ; ;;    ;;;   ;;;;;  ;;;;;  ;;;
;   ; ;; ; ;   ;  ;;  ;  ;;  ;  ;   ;  ;;  ;  ;;  ;    ;      ;      ;
;   ; ;; ; ;   ;  ;   ;  ;   ;  ;   ;  ;   ;  ;   ;    ;      ;      ;
;   ; ;; ; ;   ;  ;   ;  ;;;;;  ;   ;  ;   ;  ;;;;;    ;      ;      ;
;   ;    ; ;   ;  ;   ;  ;      ;   ;  ;   ;  ;        ;      ;      ;
;   ;    ; ;   ;  ;   ;  ;   ;  ;;  ;  ;   ;  ;   ;    ;      ;      ;
;   ;    ;  ;;;   ;   ;   ;;;    ;;;;  ;   ;   ;;;     ;;;    ;;;  ;;;;;
;                                   ;
;                                ;  ;
;                                 ;;


(define (wk-the-moneghetti)
  ;; http://blog.strava.com/wow-the-moneghetti-9402/
  ;; squeezed into a 40 minute workout (shortened the w/u and c/d)
  (define wk (new fit-workout-file% [name "moneghetti"] [sport 1]))
  ((workout
    (step (duration (* 15 60)) (hr/warmup) 2) ; warmup
    (repeat
     2
     (step (duration 90) (no-target) 0) ; active
     (step (duration 90) (no-target) 4) ; recover
     )
    (repeat
     4
     (step (duration 60) (no-target) 0) ; active
     (step (duration 60) (no-target) 4) ; recover
     )
    (repeat
     4
     (step (duration 30) (no-target) 0) ; active
     (step (duration 30) (no-target) 4) ; recover
     )
    (repeat
     4
     (step (duration 15) (no-target) 0) ; active
     (step (duration 15) (no-target) 4) ; recover
     )
    (step (duration (* 5 60)) (no-target) 3) ; cooldown
    (step (lap-button) (no-target) 3) ; cooldown
    )
   wk)
  (send wk get-fit-data))


;
;                        ;;;                      ;
;   ;;;;;                  ;                      ;        ;;;;;;;
;   ;    ;                 ;                      ;           ;
;   ;    ;  ;;;  ;     ;   ;     ;;;   ; ;;    ;;;;           ;     ;;;   ;;;;;  ;;;;    ;;;
;   ;    ; ;   ; ;     ;   ;    ;   ;  ;;  ;  ;   ;           ;    ;;  ;  ; ; ;  ;   ;  ;   ;
;   ;;;;;  ;   ;  ; ; ;    ;        ;  ;   ;  ;   ;           ;    ;   ;  ; ; ;  ;   ;  ;   ;
;   ;   ;  ;   ;  ; ; ;    ;     ;;;;  ;   ;  ;   ;           ;    ;;;;;  ; ; ;  ;   ;  ;   ;
;   ;    ; ;   ;  ;; ;;    ;    ;   ;  ;   ;  ;   ;           ;    ;      ; ; ;  ;   ;  ;   ;
;   ;    ; ;   ;   ; ;     ;    ;   ;  ;   ;  ;   ;           ;    ;   ;  ; ; ;  ;   ;  ;   ;
;   ;     ; ;;;    ; ;      ;;   ;;;;  ;   ;   ;;;;           ;     ;;;   ; ; ;  ;;;;    ;;;
;                                                                                ;
;                                                                                ;
;                                                                                ;


(define (wk-the-rowlandtempo)
  ;; http://blog.strava.com/wow-the-rowlandtempo-9552/
  ;; Workout squeezed to fit into 40 min
  (define wk (new fit-workout-file% [name "rowlandtempo"] [sport 1]))
  ((workout
    (step (duration (* 7 60)) (hr/warmup) 2) ; warmup
    (repeat
     3
     (step (duration (* 3 60))
           (speed (pace 4 50) (pace 5 00))
           0)
     (step (duration (* 3 60))
           (speed (pace 4 40) (pace 4 50))
           0)
     (step (duration (* 3 60))
           (speed (pace 4 30) (pace 4 40))
           0)
     (step (duration (* 2 60)) (no-target) 4) ; recover
     )
    (step (lap-button) (no-target) 3) ; cooldown
    )
   wk)
  (send wk get-fit-data))


;
;                   ;                  ;      ;      ;;;               ;
;    ;;;;                  ;           ;      ;        ;               ;
;   ;    ;                 ;           ;      ;        ;               ;
;   ;     ;     ; ;;;    ;;;;;   ;;;   ; ;;   ;;;;     ;     ;;;    ;;;;   ;;;
;   ;;    ;     ;   ;      ;    ;;  ;  ;;  ;  ;   ;    ;    ;   ;  ;   ;  ;;  ;
;    ;;;;  ; ; ;    ;      ;    ;      ;   ;  ;   ;    ;        ;  ;   ;  ;   ;
;        ; ; ; ;    ;      ;    ;      ;   ;  ;   ;    ;     ;;;;  ;   ;  ;;;;;
;        ; ;; ;;    ;      ;    ;      ;   ;  ;   ;    ;    ;   ;  ;   ;  ;
;   ;    ;  ; ;     ;      ;    ;;     ;   ;  ;   ;    ;    ;   ;  ;   ;  ;   ;
;    ;;;;   ; ;   ;;;;;    ;;;   ;;;;  ;   ;  ;;;;      ;;   ;;;;   ;;;;   ;;;
;
;
;

;; 3 x 2 mile tempo* with 15-20 second pace change between mile one and two, 2 min recovery.

(define (wk-the-switchblade)
  ;; http://blog.strava.com/wow-theswitchblade-9225/
  ;; Approx 1h 5 - 1h 10 min.
  (define wk (new fit-workout-file% [name "switchblade"] [sport 1]))
  ((workout
    (step (duration (* 10 60)) (hr/warmup) 2) ; warmup
    (repeat
     3
     (step (distance 1600) (speed (pace 4 50) (pace 5 0)) 0) ;; active
     (step (distance 1600) (speed (pace 4 30) (pace 4 40)) 0) ;; active
     (step (duration (* 2 60)) (no-target) 4)) ; recover
    (step (duration (* 5 60)) (no-target) 3) ;; cooldown
    (step (lap-button) (no-target) 3)
    )
   wk)
  (send wk get-fit-data))


;
;                                                                                           ;    ;                  ;
;   ;;;;;                                                                                   ;           ;           ;
;     ;                                                                                     ;           ;           ;
;     ;     ;;;    ;;;    ;;;    ;;;;   ;;;    ;;;   ;;;;;          ;;;    ;;;   ; ;;    ;;;;  ;;;    ;;;;;   ;;;   ; ;;
;     ;    ;;  ;  ;;  ;  ;;  ;   ;;  ; ;;  ;  ;   ;  ; ; ;         ;   ;  ;   ;  ;;  ;  ;   ;    ;      ;    ;;  ;  ;;  ;
;     ;    ;      ;   ;  ;       ;     ;   ;      ;  ; ; ;         ;          ;  ;   ;  ;   ;    ;      ;    ;      ;   ;
;     ;    ;      ;;;;;  ;       ;     ;;;;;   ;;;;  ; ; ;          ;;;    ;;;;  ;   ;  ;   ;    ;      ;    ;      ;   ;
;     ;    ;      ;      ;       ;     ;      ;   ;  ; ; ;             ;  ;   ;  ;   ;  ;   ;    ;      ;    ;      ;   ;
;     ;    ;;     ;   ;  ;;      ;     ;   ;  ;   ;  ; ; ;         ;   ;  ;   ;  ;   ;  ;   ;    ;      ;    ;;     ;   ;
;   ;;;;;   ;;;;   ;;;    ;;;;   ;      ;;;    ;;;;  ; ; ;          ;;;    ;;;;  ;   ;   ;;;;  ;;;;;    ;;;   ;;;;  ;   ;
;
;
;


(define (wk-icecream-sandwitch)
  ;; http://blog.strava.com/wow-the-icecreamsandwich-9584/
  ;; 1 hour
  (define wk (new fit-workout-file% [name "icecream"] [sport 1]))
  ((workout
    (step (duration (* 15 60)) (hr/warmup) 2) ; warmup
    (step (duration (* 10 60)) (speed (pace 4 40) (pace 5 0)) 0) ; active
    (step (duration (* 3 60)) (no-target) 4) ; recover
    (repeat
     6
     (step (duration 60) (no-target) 0) ; active
     (step (duration 60) (no-target) 4) ; recover
     )
    (step (duration (* 3 60)) (no-target) 4) ; recover
    (step (duration (* 10 60)) (speed (pace 4 40) (pace 5 0)) 0) ; active
    (step (duration (* 7 60)) (no-target) 3) ;; cooldown
    (step (lap-button) (no-target) 3)
    )
   wk)
  (send wk get-fit-data))


(define (wr file-name bstr)
  (call-with-output-file file-name
    (lambda (out) (write-bytes bstr out))
    #:mode 'binary
    #:exists 'replace))

(define (wr-all)
  ;; NOTE: the FR920 identifies the workouts by the timestamp they were
  ;; created (and possibly by the serial number, which we always put as 1).
  ;; We sleep between generating the workouts, otherwise, they would all be
  ;; identical as far as the watch is concerned.
  (wr "z2-strides.fit" (wk-z2-strides))
  (sleep 3)
  (wr "run-fthr-test.fit" (wk-lthr-test))
  (sleep 3)
  (wr "moneghetti.fit" (wk-the-moneghetti))
  (sleep 3)
  (wr "rowlandtempo.fit" (wk-the-rowlandtempo))
  (sleep 3)
  (wr "switchblade.fit" (wk-the-switchblade))
  (sleep 3)
  (wr "icecream.fit" (wk-icecream-sandwitch)))

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
