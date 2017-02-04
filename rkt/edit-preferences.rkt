#lang racket/base
;; edit-preferences.rkt -- edit global preferences
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015 Alex Harsanyi (AlexHarsanyi@gmail.com)
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
         "al-log.rkt"
         "al-prefs.rkt"
         "fmt-util.rkt"
         "icon-resources.rkt"
         "map-tiles.rkt"
         "weather.rkt"
         "widgets.rkt"
         "al-widgets.rkt"
         )

(provide get-preferences-dialog)

(define edit-preferences-dialog%
  (class al-edit-dialog%
    (init)
    (super-new [title "Edit Preferences"]
               [icon edit-icon]
               [min-width 600]
               [min-height 300])

    (define measurement-system-choice #f)
    (define tablet-friendly-checkbox #f)
    (define allow-weather-download-check-box #f)
    (define wu-api-key-text-box #f)
    (define allow-map-tile-download-check-box #f)

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

      (let ((p1 (new group-box-panel% [parent p] [label "Weather"]
                     [horiz-margin 10]
                     [alignment '(left top)]
                     [stretchable-height #f])))
        (set! allow-weather-download-check-box
              (new check-box% [label "Allow weather data download"]
                   [parent p1])))

      (let ((p1 (new group-box-panel% [parent p] [label "Maps"]
                     [spacing 10]
                     [horiz-margin 10]
                     [border 10]
                     [alignment '(left top)]
                     [stretchable-height #f])))
        (set! allow-map-tile-download-check-box
              (new check-box% [label "Allow map tile download"]
                   [parent p1])))


      #f)

    (define (setup)
      (let ((ms (al-pref-measurement-system)))
        (send measurement-system-choice set-selection
              (if (eq? ms 'metric) 0 1)))
      (let ((tablet-friendly? (al-pref-tablet-friendly?)))
        (send tablet-friendly-checkbox set-value (if tablet-friendly? #t #f)))
      (let ((allow? (allow-tile-download)))
        (send allow-map-tile-download-check-box set-value (if allow? #t #f)))
      (let ((allow? (allow-weather-download)))
        (send allow-weather-download-check-box set-value (if allow? #t #f))))

    (define (save-preferences)
      (let ((val (send measurement-system-choice get-selection)))
        (al-pref-measurement-system (if (eqv? val 0) 'metric 'statute)))

      (let ((val (send tablet-friendly-checkbox get-value)))
        (unless (eq? val (al-pref-tablet-friendly?))
          (al-pref-tablet-friendly? val)))

      (let ((val (send allow-map-tile-download-check-box get-value)))
        (unless (eq? val (allow-tile-download))
          (set-allow-tile-download val)))

      (let ((val (send allow-weather-download-check-box get-value)))
        (unless (eq? val (allow-weather-download))
          (set-allow-weather-download val))))

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
