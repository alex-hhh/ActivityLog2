#lang racket/base

;; gap.rkt -- Grade Adjusted Pace calculations
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2017 Alex Harsanyi (AlexHarsanyi@gmail.com)
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

(require "data-frame/bsearch.rkt")
(require "fmt-util.rkt")                ; for convert-pace->m/s, convert-m/s->pace

;; Grade Adjusted Pace is calculated by obtaining a multiplier factor value
;; based on the grade.  The factor can be directly multiplied with speed to
;; obtain a grade adjusted speed -- this is the speed that would be achieved
;; on a flat ground for the same level of effort.  To apply it to a pace
;; value, see `adjust-pace-for-grade`.
;;
;; GAP applies to running only and the multiplier tables are based on the
;; Strava blog post:
;;
;; https://medium.com/strava-engineering/an-improved-gap-model-8b07ae8886c3


;; Array of grade value obtained by sampling the strava GAP adjustment plot.
;; Has the same number of elements as `v-multiplier` below.
(define v-grade
  (vector -32.24354244
          -27.72693727
          -23.81254613
          -20.3498155
          -17.28856089
          -14.62878229
          -12.3202952
          -10.31291513
          -8.556457565
          -7.050922509
          -5.746125461
          -4.591881919
          -3.638376384
          -2.785239852
          -2.082656827
          -1.480442804
          -0.978597786
          -0.5771217712
          -0.02509225092
          0
          0.5269372694
          0.9284132841
          1.430258303
          2.032472325
          2.735055351
          3.588191882
          4.541697417
          5.695940959
          7.000738007
          8.506273063
          10.26273063
          12.2701107
          14.62878229
          17.28856089
          20.299631
          23.76236162
          27.67675277
          32.14317343))

;; Multiplier values for each grade value in `v-grade`.
(define v-multiplier
  (vector 1.593137255
          1.382352941
          1.220588235
          1.088235294
          0.9901960784
          0.9264705882
          0.887254902
          0.8725490196
          0.8725490196
          0.8774509804
          0.8921568627
          0.9068627451
          0.9215686275
          0.9362745098
          0.9558823529
          0.9705882353
          0.9803921569
          0.9901960784
          0.9950980392
          1.0
          1.004901961
          1.014705882
          1.029411765
          1.053921569
          1.083333333
          1.112745098
          1.156862745
          1.210784314
          1.279411765
          1.362745098
          1.475490196
          1.62254902
          1.808823529
          2.044117647
          2.318627451
          2.617647059
          2.960784314
          3.328431373))

;; Return the multiplier corresponding to GRADE by looking them up in
;; `v-grade` and `v-multiplier`.  The returned value can be multiplied
;; directly with a speed value to obtain a grade adjusted speed, *BUT NOT*
;; with a pace value.  For pace values, use `adjust-pace-for-grade` instead.
(define (grade->multiplier grade)
  (let ((index (bsearch v-grade grade)))
    (cond ((= index 0)
           ;; NOTE: should extrapolate the slope of the first segment!
           (vector-ref v-multiplier 0))
          ((= index (vector-length v-grade))
           ;; NOTE: should extrapolate the slope of the last segment!
           (vector-ref v-multiplier (sub1 (vector-length v-grade))))
          (#t
           ;; Perform a linear interpolation between two adjacent grade
           ;; values.
           (let ((g1 (vector-ref v-grade (sub1 index)))
                 (g2 (vector-ref v-grade index))
                 (m1 (vector-ref v-multiplier (sub1 index)))
                 (m2 (vector-ref v-multiplier index)))
             (let ((alpha (/ (- grade g1) (- g2 g1))))
               (+ (* (- 1 alpha) m1) (* alpha m2))))))))

(define (adjust-pace-for-grade pace grade)
  (let ((mult (grade->multiplier grade))
        (speed (convert-pace->m/s pace)))
    (convert-m/s->pace (* mult speed))))

(provide adjust-pace-for-grade)
(provide grade->multiplier)
