#lang racket/base

;; air-density.rkt -- Air density calculations from weather data
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2022, 2023 Alex Harsányi <AlexHarsanyi@gmail.com>
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
;;
;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Air density calculations are based on formulas from:
;; https://wahiduddin.net/calc/density_altitude.htm

(require racket/contract)
(provide/contract
 [air-density/dew-point (-> rational? rational? rational? rational?)]
 [air-density/relative-humidity (-> rational? rational? rational? rational?)])

(define vapor-pressure-polynomial-cefficients
  '(0.99999683
    -0.90826951e-2
    0.78736169e-4
    -0.61117958e-6
    0.43884187e-8
    -0.29883885e-10
    0.21874425e-12
    -0.17892321e-14
    0.11112018e-16
    -0.30994571e-19))

(define (evaluate-polynomial x coefficients)
  (let loop ([term x]
             [result (car coefficients)]
             [coefficients (cdr coefficients)])
    (if (null? coefficients)
        result
        (loop
         (* term x)
         (+ result (* term (car coefficients)))
         (cdr coefficients)))))

;; vapor pressure at specified temperature (degrees Celsius)
(define (saturation-vapor-pressure t)
  (define p (evaluate-polynomial t vapor-pressure-polynomial-cefficients))
  (/ 6.1078 (expt p 8.0)))

;; Alternate implementation for the saturation vapor pressure, less precise
(define (saturation-vapor-pressure/alt t)
  (* 6.1078 (expt 10 (/ (* 7.5 t) (+ 237.3 t)))))

(define (vapor-pressure/dew-point dp)
  (saturation-vapor-pressure dp))

(define (vapor-pressure/relative-humidity rh t)
  (* (/ rh 100.0) (saturation-vapor-pressure t)))

;; Calculate air density based on PRESSURE, TEMPERATURE and DEW-POINT.  This
;; is the more accurate method.
;;
;; Returned AIR DENSITY is in kg/m^3
;;
;; Supplied PRESSURE must be in hectoPascals (hPa), which is the usual unit
;; shown in weater applications.  1 hPa = 1 mb (millibar), Pa = mb *
;; 100. NOTE: pressure must be station (actual) pressure, NOT equivalent
;; sea-level pressure, which most weather services report.
;;
;; TEMPERATURE and DEW-POINT are in degrees Celsius.
;; 
(define (air-density/dew-point pressure temperature dew-point)
  (define Rd 287.05)                    ; gas constant for dry air
  (define Rv 461.496)                   ; gas constant for vater vapor
  (define T (+ temperature 273.15))     ; temperature in Kelvin
  (define Pv (* (vapor-pressure/dew-point dew-point) 100.0))
  (define Pd (- (* pressure 100.0) Pv)) ; dry air pressure
  (+ (/ Pd (* Rd T)) (/ Pv (* Rv T))))

;; Calculate air density based in PRESSURE, TEMPERATURE and relative humidity
;; (RH).  This is the less accurate method
;;
;; Returned AIR DENSITY is in kg/m^3
;;
;; Supplied PRESSURE must be in hectoPascals (hPa), which is the usual unit
;; shown in weater applications.  1 hPa = 1 mb (millibar), Pa = mb *
;; 100. NOTE: pressure must be station (actual) pressure, NOT equivalent
;; sea-level pressure, which most weather services report.
;;
;; TEMPERATURE must be in degrees Celsius.
;;
;; Relative Humidity (RH) must beas a percentage (0 -- 100)
;;
(define (air-density/relative-humidity pressure temperature rh)
  (define Rd 287.05)                    ; gas constant for dry air
  (define Rv 461.496)                   ; gas constant for vater vapor
  (define T (+ temperature 273.15))     ; temperature in Kelvin
  (define Pv (* (vapor-pressure/relative-humidity rh temperature) 100.0))
  (define Pd (- (* pressure 100.0) Pv)) ; dry air pressure
  (+ (/ Pd (* Rd T)) (/ Pv (* Rv T))))
