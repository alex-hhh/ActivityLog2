#lang racket/base
;; run.rkt -- toplevel file to run the ActivityLog2 application

;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015, 2019, 2020 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require framework/splash
         racket/class
         racket/draw
         racket/lazy-require
         "rkt/check-missing-modules.rkt"
         "rkt/app-info.rkt")

;; Check and inform the user that these packages need to be installed...
(check-missing-modules
 the-application
 tzinfo
 tzgeolookup
 data-frame
 plot-container
 gui-widget-mixins
 map-widget
 geoid)

;; Start up a splash screen, rest of the application will be lazy-required
;; below.  This will make the splash screen show up while the rest of the app
;; is loading.
(define font
  (send the-font-list find-or-create-font 24 'default 'normal 'normal))
(define small-font
  (send the-font-list find-or-create-font 8 'default 'normal 'normal))
(define color (make-color #x69 #x69 #x69))
(define message "ActivityLog2 is loading ...")

(define (draw-splash dc gauge max-gauge width height)
  (send dc clear)
  (send dc set-smoothing 'smoothed)

  (send dc set-text-foreground color)
  (let-values (((cw ch) (send dc get-size)))
    (let-values (([w h x y] (send dc get-text-extent message font #t)))
      (let ((msg-x (- (/ cw 2) (/ w 2)))
            (msg-y (- (/ ch 2) (/ h 2))))
        (send dc set-font font)
        (send dc draw-text message msg-x msg-y)))
    (let ((version (string-append "Version " (app-version))))
      (let-values (([w h x y] (send dc get-text-extent version small-font #t)))
        (let ((msg-x 5)
              (msg-y (- ch h 5)))
          (send dc set-font small-font)
          (send dc draw-text version msg-x msg-y))))))

;; Remove the progress bar.  The progress works fine while running the
;; application using racket, but does not work (shows no progress) when the
;; application is compiled into an executable...
(set-splash-progress-bar?! #f)

(start-splash (vector draw-splash 400 100) "ActivityLog2" 100)

(lazy-require ("rkt/main.rkt" (main)))
(main)
