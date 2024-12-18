#lang racket/base
;; edit-preferences.rkt -- edit global preferences
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015, 2020, 2022, 2023 Alex Harsányi <AlexHarsanyi@gmail.com>
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
         racket/gui/base
         "../al-widgets.rkt"
         "../fmt-util.rkt"
         "../models/ec-util.rkt"
         "../widgets/main.rkt"
         map-widget)

(provide get-preferences-dialog)

(define edit-preferences-dialog%
  (class edit-dialog-base%
    (init)
    (super-new [title "Preferences"]
               [icon (edit-icon)]
               [min-width 600]
               [min-height 300])

    (define measurement-system-choice #f)
    (define tablet-friendly-checkbox #f)
    (define allow-map-tile-download-check-box #f)
    (define fix-elevation-on-import-check-box #f)
    (define map-provider-choice #f)

    (let ((p (send this get-client-pane)))

      (let ((p1 (new group-box-panel% [parent p] [label "General"]
                     [border 10]
                     [spacing 10]
                     [horiz-margin 10]
                     [alignment '(left top)]
                     [stretchable-height #f])))
        (set! measurement-system-choice
              (new choice% [parent p1]
                   [label "Measurement System "]
                   [choices '("Metric" "Imperial")]))
        (set! tablet-friendly-checkbox
              (new check-box% [label "Tablet friendly dialogs (requires application restart)"]
                   [parent p1])))

      (let ((p1 (new group-box-panel% [parent p] [label "Elevation Correction"]
                     [horiz-margin 10]
                     [alignment '(left top)]
                     [stretchable-height #f])))
        (set! fix-elevation-on-import-check-box
              (new check-box% [label "Fix Elevation On Import"]
                   [parent p1])))

      (let ((p1 (new group-box-panel% [parent p] [label "Maps"]
                     [spacing 10]
                     [horiz-margin 10]
                     [border 10]
                     [alignment '(left top)]
                     [stretchable-height #f])))
        (set! allow-map-tile-download-check-box
              (new check-box% [label "Allow map tile download"]
                   [parent p1]))
        (set! map-provider-choice
              (new choice% [label "Map tiles "] [parent p1]
                   [choices (get-tile-providers)])))
      #f)

    (define (setup)
      (let ((ms (al-pref-measurement-system)))
        (send measurement-system-choice set-selection
              (if (eq? ms 'metric) 0 1)))
      (let ((tablet-friendly? (al-pref-tablet-friendly?)))
        (send tablet-friendly-checkbox set-value (if tablet-friendly? #t #f)))
      (let ((allow? (allow-tile-download)))
        (send allow-map-tile-download-check-box set-value (if allow? #t #f)))
      (let ([fix? (fix-elevation-on-import)])
        (send fix-elevation-on-import-check-box set-value fix?))
      (let ((index (for/first ([(p idx) (in-indexed (get-tile-providers))]
                               #:when (equal? p (current-tile-provider)))
                     idx)))
        (when index
          (send map-provider-choice set-selection index))))

    (define (save-preferences)
      (let ((val (send measurement-system-choice get-selection)))
        (set-al-pref-measurement-system (if (eqv? val 0) 'metric 'statute)))

      (let ((val (send tablet-friendly-checkbox get-value)))
        (unless (eq? val (al-pref-tablet-friendly?))
          (al-pref-tablet-friendly? val)))

      (let ((val (send allow-map-tile-download-check-box get-value)))
        (unless (eq? val (allow-tile-download))
          (set-allow-tile-download val)))

      (let ((index (send map-provider-choice get-selection)))
        (set-current-tile-provider (list-ref (get-tile-providers) index)))

      (let ([val (send fix-elevation-on-import-check-box get-value)])
        (unless (equal? val (fix-elevation-on-import))
          (set-fix-elevation-on-import val))))

    (define/public (run parent)
      (setup)
      (let ((result (send this do-edit parent)))
        (when result
          (save-preferences))
        result))

    ))

(define the-edit-preferences-dialog #f)

(define (get-preferences-dialog)
  (unless the-edit-preferences-dialog
    (set! the-edit-preferences-dialog (new edit-preferences-dialog%)))
  the-edit-preferences-dialog)
