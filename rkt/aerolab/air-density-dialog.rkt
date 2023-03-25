#lang racket/base

;; air-density-dialog.rkt -- calculate air density interactively
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2023 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require gui-widget-mixins
         racket/gui
         "../widgets/edit-dialog-base.rkt"
         "../widgets/grid-pane.rkt"
         "../widgets/icon-resources.rkt"
         "aerolab-utilities.rkt"
         "air-density.rkt")

;; A dialog that allows the user to get the air density by supplying pressure,
;; temperature, dew-point and relative humidity values.  This is an internal
;; class, the user function is `air-density-interactive`, defined below.
(define air-density-dialog%
  (class edit-dialog-base%
    (init)
    (super-new
     [icon (sql-export-icon)]
     [title "Air Density Calculator"])

    (define gui-controls (make-hash))

    (define/private (value-of control-name)
      (define control (hash-ref gui-controls control-name))
      (cond ((is-a? control text-field%)
             (send control get-value/validated))
            ((is-a? control check-box%)
             (send control get-value))
            ((is-a? control slider%)
             (send control get-value))
            ((is-a? control choice%)
             (send control get-selection))
            (#t
             (error "value-of: unknown control type"))))

    (define/private (put-value control-name value)
      (define control (hash-ref gui-controls control-name))
      (cond ((is-a? control text-field%)
             (send control set-value value))
            ((is-a? control choice%)
             (send control set-selection value))
            ((is-a? control message%)
             (send control set-label value))
            ((is-a? control check-box%)
             (send control set-value value))
            ((is-a? control slider%)
             (send control set-value value))
            (#t
             (error "put-value: unknown control type"))))

    (define/private (setup-from-state state)
      (put-value 'temperature (hash-ref state 'temperature (lambda () "")))
      (put-value 'dew-point (hash-ref state 'dew-point (lambda () "")))
      (put-value 'humidity (hash-ref state 'humidity (lambda () "")))
      (put-value 'pressure (hash-ref state 'pressure (lambda () "")))
      (define method
        (cond
          ;; Method can also be explicitly specified
          ((hash-ref state 'calculation-method (lambda () #f)) =>
           (lambda (m) (and (eq? m 'dew-point) 1 0)))
          ((hash-ref state 'dew-point (lambda () #f)) 0)
          ((hash-ref state 'humidity (lambda () #f)) 1)
          (#t 0)))
      (put-value 'calculation-method method)
      (on-calculation-method (hash-ref gui-controls 'calculation-method) #f))

    (define/private (calculate-air-density)
      (case (value-of 'calculation-method)
        ((0)
         (let ([t (value-of 'temperature)]
               [dp (value-of 'dew-point)]
               [p (value-of 'pressure)])
           (and (andmap rational? (list t dp p))
                (air-density/dew-point p t dp))))
        ((1)
         (let ([t (value-of 'temperature)]
               [rh (value-of 'humidity)]
               [p (value-of 'pressure)])
           (and (andmap rational? (list t rh p))
                (air-density/relative-humidity p t rh))))
        (else
         (error "calculate-air-density: unknown calculation method"))))

    (define/private (get-new-state old-state)
      ;; Start with the calculation method
      (define state0
        (let ([m (if (equal? (value-of 'calculation-method) 0)
                     'dew-point
                     'humidity)])
          (hash-set old-state 'calculation-method m)))
      ;; ... if air density can be calculated, add that to the state too.
      (define state1
        (let ([a (calculate-air-density)])
          (if a
              (hash-set state0 'air-density a)
              state0)))

      ;; Ad the rest of the keys to the state
      (define keys
        '(temperature dew-point pressure humidity))

      (let loop ([h state1]
                 [keys keys])
        (if (null? keys)
            h
            (let* ([k (car keys)]
                   [v (value-of k)])
              (if (rational? v)
                  (loop (hash-set h k v) (cdr keys))
                  h)))))

    (define/private (update-air-density)
      (define a (calculate-air-density))
      (put-value 'air-density-value
                 (if a
                     (string-append (~r a #:precision 3) " kg/m³")
                     "--- kg/m³")))

    (define (on-temperature _control _valid?)
      (update-air-density))

    (define (on-dew-point _control _valid?)
      (update-air-density))

    (define (on-humidity _control _valid?)
      (update-air-density))

    (define (on-pressure _control _valid?)
      (update-air-density))

    (define (on-calculation-method control event)
      (define dew-point-choices
        '(method-message
          calculation-method
          temperature-message
          temperature
          dew-point-message
          dew-point
          pressure-message
          pressure
          air-density-message
          air-density-value))
      (define humidity-choices
        '(method-message
          calculation-method
          temperature-message
          temperature
          humidity-message
          humidity
          pressure-message
          pressure
          air-density-message
          air-density-value))
      (send (hash-ref gui-controls 'grid-pane)
            change-children
            (lambda (_old)
              (for/list ([c (case (send control get-selection)
                              ((0) dew-point-choices)
                              ((1) humidity-choices))])
                (hash-ref gui-controls c))))
      (update-air-density))

    (define/override (has-valid-data?)
      (and (rational? (value-of 'temperature))
           (rational? (value-of 'pressure))
           (let* ([method (value-of 'calculation-method)]
                  [c (if (equal? method 0) 'dew-point 'humidity)])
             (rational? (value-of c)))))

    (define/public (run-dialog parent state)
      (setup-from-state state)
      (if (send this do-edit parent)
          (get-new-state state)
          #f))

    (let ([pane (send this get-client-pane)])
      (send pane border 15)
      (define g (new grid-pane%
                     [columns 2]
                     [spacing 10]
                     [alignment '(left center)]
                     [parent pane]))
      (hash-set! gui-controls 'grid-pane g)
      (hash-set!
       gui-controls
       'method-message
       (new message%
           [parent g]
           [label "Calculation Method: "]))
      (hash-set!
       gui-controls
       'calculation-method
       (new choice%
            [parent g]
            [label ""]
            [choices '("Use Dew Point" "Use Relative Humidity")]
            [callback on-calculation-method]))
      (hash-set!
       gui-controls
       'temperature-message
       (new message%
           [parent g]
           [label "Temperature: "]))
      (hash-set!
       gui-controls
       'temperature
       (new (validate-mixin
             validate-non-negative-rational
             (lambda (v) (~r v #:precision 1))
             (decorate-mixin
              (decorate-with "℃" #:validate validate-non-negative-rational)
              (cue-mixin
               "℃"
               (tooltip-mixin text-field%))))
            [parent g]
            [label ""]
            [min-width min-text-field-width]
            [tooltip "Outdoor temperature"]
            [stretchable-width #f]
            [valid-callback on-temperature]))
      (hash-set!
       gui-controls
       'dew-point-message
       (new message%
            [parent g]
            [label "Dew Point: "]))
      (hash-set!
       gui-controls
       'dew-point
       (new (validate-mixin
             validate-non-negative-rational
             (lambda (v) (~r v #:precision 1))
             (decorate-mixin
              (decorate-with "℃" #:validate validate-non-negative-rational)
              (cue-mixin
               "℃"
               (tooltip-mixin text-field%))))
            [parent g]
            [label ""]
            [min-width min-text-field-width]
            [tooltip "Dew Point"]
            [stretchable-width #f]
            [valid-callback on-dew-point]))
      (hash-set!
       gui-controls
       'humidity-message
       (new message%
            [parent g]
            [label "Relative Humidity: "]))
      (hash-set!
       gui-controls
       'humidity
       (new (validate-mixin
             (validate-rational-between 0 100)
             (lambda (v) (~r v #:precision 3))
             (decorate-mixin
              (decorate-with "%" #:validate (validate-rational-between 0 100))
              (cue-mixin
               "%"
               (tooltip-mixin text-field%))))
            [parent g]
            [label ""]
            [min-width min-text-field-width]
            [tooltip "Relative Humidity"]
            [stretchable-width #f]
            [valid-callback on-humidity]))
      (hash-set!
       gui-controls
       'pressure-message
       (new message%
            [parent g]
            [label "Station (actual) Pressure: "]))
      (hash-set!
       gui-controls
       'pressure
       (new (validate-mixin
             validate-positive-rational
             (lambda (v) (~r v #:precision 3))
             (decorate-mixin
              (decorate-with "hPa" #:validate validate-positive-rational)
              (cue-mixin
               "hPa"
               (tooltip-mixin text-field%))))
            [parent g]
            [label ""]
            [min-width min-text-field-width]
            [tooltip "Barometric Pressure (adjusted for altitude) in hPa"]
            [stretchable-width #f]
            [valid-callback on-pressure]))
      (hash-set!
       gui-controls
       'air-density-message
       (new message%
            [parent g]
            [label "Air Density: "]))
      (define larger-font
        (send the-font-list find-or-create-font 14 'default 'normal 'normal))
      (hash-set!
       gui-controls
       'air-density-value
       (new message%
            [parent g]
            [auto-resize #t]
            [font larger-font]
            [label "--- kg/m³"])))

    ))

(define the-dialog #f)

;; Open a dialog box that allows the user to view/edit air density and its
;; input values.  PARAMETERS is a hash table that contains input fields, and
;; the function returns a new hash table with updated values if the user
;; clicks "Save" , or #f if the user clicked "Cancel".
;;
;; The input PARAMETERS is a hash table containing some of the following keys:
;; 'temperature, 'dew-point, 'humidity, 'pressure and 'calculation-method,
;; which can be 'dew-point or 'humidity, some or all these might be missing,
;; in which case the user must supply them in the dialog, otherwise the "Save"
;; option is not available.  Additional keys in the hash table are ignored.
;;
;; The return value is a hash table with the same keys as the input one, with
;; updated values from the dialog.  An additional key, 'air-density contains
;; the air density value.
;;
(define (air-density-interactive parent-window parameters)
  (unless the-dialog
    (set! the-dialog (new air-density-dialog%)))
  (send the-dialog run-dialog parent-window parameters))

(provide/contract
 [air-density-interactive (-> (or/c (is-a?/c window<%>) #f) hash? (or/c #f hash?))])
