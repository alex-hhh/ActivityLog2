#lang racket
(require "al-workouts.rkt")

;; NOTE: to write a workout to a file, use
;; (wr "daniel.q1.fit" (wk-daniel-q1))

(define (hr/warmup) (hr/bpm 60 163))                 ; Z1 + Z2
(define (pace/hard) (speed (pace 4 29) (pace 4 0)))  ; Z5 + 
(define (pace/easy) (speed (pace 7 32) (pace 5 7))) ; Z1 and Z2 
(define (pace/tempo) (speed (pace 4 45) (pace 4 31))) ; Z4

(define (wk-daniel-q1)
  (define wk (new fit-workout-file% [name "daniel-q1"] [sport 1]))
  ((workout
    (step (minutes 20) (hr/warmup) warmup)
    (repeat 
     6
     (step (minutes 4) (pace/hard) active)
     (step (minutes 3) (no-target) recover))
    (step (minutes 8) (pace/easy) cooldown)
    (step (lap-button) (pace/easy) cooldown))
   wk)
  (send wk get-fit-data))

(define (wk-daniel-q2)
  (define wk (new fit-workout-file% [name "daniel-q2"] [sport 1]))
  ((workout
    (step (minutes 20) (hr/warmup) warmup)
    (step (minutes 40) (pace/tempo) active)
    (step (minutes 20) (pace/easy) cooldown)
    (step (lap-button) (pace/easy) cooldown))
   wk)
  (send wk get-fit-data))

(define (wk-daniel-q3)
  (define wk (new fit-workout-file% [name "daniel-q3"] [sport 1]))
  ((workout
    (step (minutes 20) (hr/warmup) warmup)
    (repeat 
     4
     (step (kms 1.2) (pace/hard) active)
     (step (minutes 4) (no-target) recover))
    (step (lap-button) (pace/easy) cooldown))
   wk)
  (send wk get-fit-data))

(define (wk-daniel-q4)
  (define wk (new fit-workout-file% [name "daniel-q4"] [sport 1]))
  ((workout
    (step (miles 2) (hr/warmup) warmup)
    (repeat 
     5
     (step (minutes 5) (pace/tempo) active)
     (step (minutes 1) (no-target) recover))
    (step (minutes 60) (pace/easy) active)
    (step (lap-button) (pace/easy) cooldown))
   wk)
  (send wk get-fit-data))

(define (wk-daniel-q5)
  (define wk (new fit-workout-file% [name "daniel-q5"] [sport 1]))
  ((workout
    (step (miles 2) (hr/warmup) warmup)
    (repeat
     2
     (step (minutes 12) (pace/tempo) active)
     (step (minutes 3) (no-target) recover))
    (step (minutes 60) (pace/easy) active)
    (step (lap-button) (pace/easy) cooldown))
   wk)
  (send wk get-fit-data))

;; 2 miles E Pace, sets of 5 min HARD with 3 to 5 min jogs to total of 10K or
;; 8% of week's mileage; whichever is less
(define (wk-daniel-q6)
  (define wk (new fit-workout-file% [name "daniel-q6"] [sport 1]))
  ((workout
    (step (miles 2) (hr/warmup) warmup)
    (repeat 
     6
     (step (minutes 5) (pace/hard) active)
     (step (minutes 4) (no-target) recover))
    (step (lap-button) (pace/easy) cooldown))
   wk)
  (send wk get-fit-data))

;; 2 miles E pace + 6 x (5 to 6 min at T pace with 1 min rests) + 2 miles E
;; pace
(define (wk-daniel-q7)
  (define wk (new fit-workout-file% [name "daniel-q7"] [sport 1]))
  ((workout
    (step (miles 2) (hr/warmup) warmup)
    (repeat 
     6
     (step (minutes 6) (pace/tempo) active)
     (step (minutes 1) (no-target) recover))
    (step (miles 2) (pace/easy) cooldown)
    (step (lap-button) (pace/easy) cooldown))
   wk)
  (send wk get-fit-data))

;; 2 miles E pace + 4 x (10 to 12 min T pace with 2 min rests) + 2 miles E
;; pace
(define (wk-daniel-q8)
  (define wk (new fit-workout-file% [name "daniel-q8"] [sport 1]))
  ((workout
    (step (miles 2) (hr/warmup) warmup)
    (repeat 
     4
     (step (minutes 12) (pace/tempo) active)
     (step (minutes 2) (no-target) recover))
    (step (miles 2) (pace/easy) cooldown)
    (step (lap-button) (pace/easy) cooldown))
   wk)
  (send wk get-fit-data))

;; 2 miles E pace + 4 x (5 to 6 min T pace with 1 min rests) + 1 hour E pace +
;; 15 to 20 min T Pace + 2 miles E Pace
(define (wk-daniel-q9)
  (define wk (new fit-workout-file% [name "daniel-q9"] [sport 1]))
  ((workout
    (step (miles 2) (hr/warmup) warmup)
    (repeat
     4
     (step (minutes 6) (pace/tempo) active)
     (step (minutes 1) (no-target) recover))
    (step (minutes 60) (pace/easy) active)
    (step (minutes 20) (pace/tempo) active)
    (step (miles 2) (pace/easy) cooldown)
    (step (lap-button) (pace/easy) cooldown))
   wk)
  (send wk get-fit-data))

;; 2 miles E Pace + 4 x (1 mile T pace with 1 min rest) + 5 min E Pace + 3 x (
;; 1 mile T pace with 1 min rest) + 2 miles E Pace)
(define (wk-daniel-q10)
  (define wk (new fit-workout-file% [name "daniel-q9"] [sport 1]))
  ((workout
    (step (miles 2) (hr/warmup) warmup)
    (repeat
     4
     (step (miles 1) (pace/tempo) active)
     (step (minutes 1) (no-target) recover))
    (step (minutes 5) (pace/easy) recover)
    (repeat
     3
     (step (miles 1) (pace/tempo) active)
     (step (minutes 1) (no-target) recover))
    (step (miles 2) (pace/easy) cooldown)
    (step (lap-button) (pace/easy) cooldown))
   wk)
  (send wk get-fit-data))


     
    
