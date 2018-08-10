#lang racket/base
;; annealing.rkt -- probabilistic find of an optimum goal.
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

(require racket/contract)

(define (default-temperature r)
  ; R is growing from almost 0 to 1, the temperature will drop from 1 to 0
  (- 1 r))

;; Default transition probability function as described in the simulated
;; annealing wikipedia page.
;;
(define (default-transition cost ncost temperature)
  (cond ((< ncost cost) 1.0)          ; always transition towards a lower cost
        ((> temperature 0)
         ;; cost and ncost vary between 0 and 2.  As such we use a 10.0
         ;; multiplier on them to reduce the probability of a transition when
         ;; ncost > cost
         (let* ((exponent (/ (* 10.0 (/ (- ncost cost) cost)) temperature))
                (probability (exp (- exponent))))
           probability))
        (#t 0.0)                        ; never transition if TEMPERATURE is 0
        ))

;; Implement the simulated annealing algorithm as described in:
;; https://en.wikipedia.org/wiki/Simulated_annealing
;;
;; NOTE: this is a probabilistic goal finding function.  Running it twice with
;; the same inputs will produce a different output!
;;
;; INITIAL-STATE is the initial state of the system
;;
;; NEIGHBOUR is a function (state, temperature) => state, given a state and
;; the temperature t, will return a new state
;;
;; GOAL is a function (STATE) => COST, that evaluates the cost of the state
;; (smaller is better)
;;
;; TEMPERATURE is a function (iteration-pct) => temperature, that gets passed
;; in the percentage of the completed iterations
;; (num-iterations/max-iterations) and produces a "temperature" value.
;;
;; TRANSITION is a function (cost, new-cost, temperature) => probability, that
;; computes the probability of transitioning to a new state given the cost of
;; the states and the temperature.
;;
;; ITERATIONS is the number of iterations to perform
;;
;; The function returns a goal state which minimizes the GOAL function.
;;  
(define (annealing
         #:initial initial-state
         #:neighbour neighbour
         #:goal goal
         #:temperature (temperature default-temperature)
         #:transition (transition default-transition)
         #:iterations (iterations 1000))

  (define-values
    (state cost)
    (for/fold ([state initial-state]
               [cost (goal initial-state)])
              ([k (in-range 1 (add1 iterations))])
      (let* ((t (temperature (exact->inexact (/ k iterations))))
             (nstate (neighbour state t))
             (ncost (goal nstate))
             (p (transition cost ncost t)))
        (if (>= p (random))
            (values nstate ncost)
            (values state cost)))))

  state)

(provide/contract
 (annealing
  (parametric->/c
   (state)
   (->* (#:initial state
         #:neighbour (-> state (between/c 0 1) state)
         #:goal (-> state real?))
        (#:temperature (-> (between/c 0 1) (between/c 0 1))
         #:transition (-> real? real? real? (between/c 0 1))
         #:iterations exact-nonnegative-integer?)
        state))))
