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


(require
 racket/gui/base
 racket/class
 racket/match
 "wkstep.rkt"
 "../fmt-util.rkt"
 "../widgets/main.rkt")

(provide wkstep-editor%)

(define (mk-input-field-pair parent cue-text min-value max-value)
      (define low
        (new number-input-field%
             [parent parent]
             [label "low "]
             [cue-text cue-text]
             [min-value min-value] [max-value max-value]))
      (define high
        (new number-input-field%
             [parent parent]
             [label "high "]
             [cue-text cue-text]
             [min-value min-value] [max-value max-value]))
      (values low high))

;; Dialog to edit a workout step (wkstep structure)
(define wkstep-editor%
  (class edit-dialog-base%
    (init)
    (super-new [title "Edit Workout Step"] [icon (edit-icon)]
               [min-width 600] [min-height 300])

    (define pane
      (new grid-pane% [parent (send this get-client-pane)]
           [columns 3] [border 10] [spacing 10] [alignment '(left center)]))

    (new message% [parent pane] [label "Step Type"] [stretchable-width #f])
    (define step-choice
      (new choice%
           [parent pane]
           [label ""]
           [stretchable-width #f]
           [choices (map cdr wkstep-type-names)]
           [callback (lambda (c e) (on-step-kind-selected))]))
    ;; dummy control, to fill the grid-pane% cell
    (new message% [parent pane] [label ""] [stretchable-width #t])

    (new message% [parent pane] [label "Duration"] [stretchable-width #f])
    (define duration-choice
      (new choice%
           [parent pane]
           [label ""]
           [stretchable-width #f]
           [choices (map cdr wkstep-dtype-names)]
           [callback (lambda (c e) (on-duration-type-selected))]))
    (define duration-panel (new horizontal-panel% [parent pane] [stretchable-width #t]))

    (define duration-memo (new message% [parent duration-panel] [label "min:sec"]))

    (define time-input
      (new duration-input-field% [parent duration-panel] [stretchable-width #f]))

    (define distance-input
      (new number-input-field%
           [parent duration-panel]
           [cue-text "km"]
           [stretchable-width #f]))

    (new message% [parent pane] [label "Target"] [stretchable-width #f])
    (define intensity-choice
      (new choice%
           [parent pane]
           [stretchable-width #f]
           [label ""]
           [choices (map cdr wkstep-ttype-names)]
           [callback (lambda (c e) (on-intensity-type-selected))]))
    (define intensity-panel (new horizontal-panel% [parent pane] [stretchable-width #t]))

    (define intensity-memo (new message% [parent intensity-panel] [label "% of FTP"]))

    (define-values (hr-low-input hr-high-input)
      (mk-input-field-pair intensity-panel "bpm" 0 255))
      
    (define pace-low-input
      (new pace-input-field% [parent intensity-panel] [label "low "]))

    (define pace-high-input
      (new pace-input-field% [parent intensity-panel] [label "high "]))

    (define-values (power-low-input power-high-input)
      (mk-input-field-pair intensity-panel "watts" 0 #f))

    (define-values (power-pct-low-input power-pct-high-input)
      (mk-input-field-pair intensity-panel "%" 0 #f))

    (define (get-selected-type)
      (define selection (send step-choice get-selection))
      (car (list-ref wkstep-type-names selection)))

    (define (get-selected-dtype)
      (define selection (send duration-choice get-selection))
      (car (list-ref wkstep-dtype-names selection)))

    (define (get-selected-ttype)
      (define selection (send intensity-choice get-selection))
      (car (list-ref wkstep-ttype-names selection)))

    (define (on-step-kind-selected)
      (void))

    (define (on-duration-type-selected)
      (define dtype (get-selected-dtype))
      (send duration-panel change-children
            (lambda (old)
              (case dtype
                ((time) (list time-input duration-memo))
                ((distance) (list distance-input duration-memo))
                ((open) '())
                (else (error dtype)))))
      (send duration-memo set-label
            (case dtype
              ((time) "min:sec")
              ((distance) "km")
              ((open) "")
              (else (error dtype)))))

    (define (on-intensity-type-selected)
      (define ttype (get-selected-ttype))
      (send intensity-panel change-children
            (lambda (old)
              (case ttype
                ((open) '())
                ((heart-rate) (list hr-low-input hr-high-input intensity-memo))
                ((speed) (list pace-low-input pace-high-input intensity-memo))
                ((power) (list power-low-input power-high-input intensity-memo))
                ((power-ftp-pct) (list power-pct-low-input power-pct-high-input intensity-memo))
                (else (error ttype)))))
      (send intensity-memo set-label
            (case ttype
              ((open) "")
              ((heart-rate) "BPM")
              ((speed)
               (if (eq? (al-pref-measurement-system) 'metric)
                   "min/km" "min/mile"))
              ((power) "watts")
              ((power-ftp-pct) "% of FTP")
              (else (error ttype)))))

    (on-duration-type-selected)
    (on-intensity-type-selected)

    (define/override (has-valid-data?)
      (define (valid? field)
        (and (send field has-valid-value?)
             (not (eq? 'empty (send field get-converted-value)))))
      (and
       (case (get-selected-dtype)
         ((time) (valid? time-input))
         ((distance) (valid? distance-input))
         ((open) #t)
         (else #f))
       (case (get-selected-ttype)
         ((open) #t)
         ((heart-rate) (and (valid? hr-low-input) (valid? hr-high-input)))
         ((speed) (and (valid? pace-low-input) (valid? pace-high-input)))
         ((power) (and (valid? power-low-input) (valid? power-high-input)))
         ((power-ftp-pct) (and (valid? power-pct-low-input) (valid? power-pct-high-input)))
         (else #f))))

    (define/public (show-dialog parent step)
      (match-define (wkstep type dtype dvalue ttype tlow thigh ramp?) step)

      (let ((index (wkstep-type->index type)))
        (send step-choice set-selection index)
        (on-step-kind-selected))
      (let ((index (wkstep-dtype->index dtype)))
        (send duration-choice set-selection index)
        (on-duration-type-selected))
      (case dtype
        ((time) (send time-input set-duration-value dvalue))
        ;; NOTE: metric/imperial
        ((distance) (send distance-input set-numeric-value (/ dvalue 1000.0))))
      (let ((index (wkstep-ttype->index ttype)))
        (send intensity-choice set-selection index)
        (on-intensity-type-selected))
      (case ttype
        ((heart-rate)
         (send hr-low-input set-numeric-value tlow)
         (send hr-high-input set-numeric-value thigh))
        ((speed)
         (send pace-low-input set-value (pace->string tlow))
         (send pace-high-input set-value (pace->string thigh)))
        ((power)
         (send power-low-input set-numeric-value tlow)
         (send power-high-input set-numeric-value thigh))
        ((power-ftp-pct)
         (send power-pct-low-input set-numeric-value tlow)
         (send power-pct-high-input set-numeric-value thigh)))
      (and (send this do-edit parent)
           (let* ((t (get-selected-type))
                  (dkind (get-selected-dtype))
                  (dval (case dkind
                          ((time) (send time-input get-converted-value))
                          ;; NOTE: metric imperial!
                          ((distance) (* (send distance-input get-converted-value) 1000.0))
                          (else #f)))
                  (ikind (get-selected-ttype))
                  (ilow (case ikind
                          ((heart-rate) (send hr-low-input get-converted-value))
                          ((speed) (send pace-low-input get-converted-value))
                          ((power) (send power-low-input get-converted-value))
                          ((power-ftp-pct) (send power-pct-low-input get-converted-value))
                          (else #f)))
                  (ihigh (case ikind
                           ((heart-rate) (send hr-high-input get-converted-value))
                           ((speed) (send pace-high-input get-converted-value))
                           ((power) (send power-high-input get-converted-value))
                           ((power-ftp-pct) (send power-pct-high-input get-converted-value))
                           (else #f))))
             (wkstep t dkind dval ikind ilow ihigh #f))))

    ))
