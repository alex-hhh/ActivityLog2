#lang racket/base
;; about-frame.rkt -- show the about dialog for the application
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

(require browser/external
         racket/class
         racket/gui/base
         racket/runtime-path
         "dbapp.rkt"
         "utilities.rkt")

(define-runtime-path logo-file "../img/logo/ActivityLog2.png")

(define hyperlink-style
  (let ([delta (new style-delta%)])
    (send delta set-delta-foreground "blue")
    (send delta set-delta 'change-underline #t)
    delta))

(define header-style
  (let ([delta (new style-delta%)])
    (send delta set-size-add 2)
    (send delta set-weight-on 'bold)
    delta))

(define (insert-hyperlink editor text callback)
  (let ((p (send editor last-position)))
    (send editor insert (make-object string-snip% text))
    (send editor change-style hyperlink-style p (send editor last-position))
    (send editor set-clickback 
          p (send editor last-position)
          (lambda (editor s e) (callback))
          hyperlink-style)))

(define (insert-heading editor text)
  (let ((p (send editor last-position)))
    (send editor insert (make-object string-snip% text))
    (send editor change-style header-style p (send editor last-position)))
  (insert-newline editor)
  (insert-newline editor))

(define (insert-reference editor text link)
  ;(send editor insert (make-object tab-snip%))
  (send editor insert (make-object string-snip% "\t• "))
  (send editor insert (make-object string-snip% text))
  (send editor insert (make-object string-snip% " ("))
  (insert-hyperlink editor link (lambda () (send-url link)))
  (send editor insert (make-object string-snip% ")")))

(define (insert-newline editor)
  (let ((s (make-object string-snip% "\n")))
    (send s set-flags (cons 'hard-newline (send s get-flags)))
    (send editor insert s)))

(define (insert-paragraph editor text)
  (let ((s (make-object string-snip% text)))
    (send editor insert s)
    (insert-newline editor)
    (insert-newline editor)))

(define (insert-text editor text)
  (let ((s (make-object string-snip% text)))
    (send editor insert s)))

(define (setup-about-text editor)

  (send editor set-tabs '(8) 8 #f)
  (send editor auto-wrap #t)

  (define logo-snip
    (make-object image-snip% (read-bitmap logo-file)))
  (send editor insert logo-snip)
  
  (insert-newline editor)
  (insert-heading editor "ActivityLog2 - analyze data from swim, bike and run activities")
  (insert-text editor "Copyright (C) 2015 - 2017, Alex Harsanyi")
  (insert-newline editor)
  (insert-newline editor)
  (insert-text editor "Project source: ")
  (insert-hyperlink editor "https://github.com/alex-hhh/ActivityLog2"
                    (lambda () (send-url "https://github.com/alex-hhh/ActivityLog2")))
  (insert-newline editor)
  (insert-newline editor)
  (insert-text editor (format "Version: ~a" (app-version)))
  (insert-newline editor)
  (insert-text editor (format "Build Id: ~a" (app-commit-id)))
  (insert-newline editor)
  (insert-text editor (format "Build Timestamp: ~a" (app-build-timestamp)))
  (insert-newline editor)
  (insert-text editor (format "Requires database version: ~a" (schema-version)))
  (insert-newline editor)
  (insert-text editor (format "Racket version: ~a" (version)))
  (insert-newline editor)
  (insert-newline editor)
  (insert-heading editor "Licence")
  (insert-paragraph editor "This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.")
  (insert-paragraph editor "This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.")
  (send editor insert (make-object string-snip% "You should have received a copy of the GNU General Public License along with this program.  If not, see <"))
  (insert-hyperlink editor "http://www.gnu.org/licenses/" (lambda () (send-url "http://www.gnu.org/licenses/")))
  (send editor insert (make-object string-snip% ">"))
  (insert-newline editor)
  (insert-newline editor)
  (insert-heading editor "Built using")
  (insert-reference editor "Programmed using Racket" "http://racket-lang.org/")
  (insert-newline editor)
  (insert-reference editor "Graphs made using the Plot package" "http://docs.racket-lang.org/plot/")
  (insert-newline editor)
  (insert-reference editor "Data stored in SQLite" "http://sqlite.org")
  (insert-newline editor)
  (insert-reference editor "Icons from Icons8" "https://www.icons8.com")
  (insert-newline editor)
  (insert-reference editor "Weather data from Wunderground" "http://www.wunderground.com/")
  (insert-newline editor)
  (insert-reference editor "Maps from Thunderforest" "http://www.thunderforest.com/")
  (insert-newline editor)
  (insert-reference editor "Map data from OpenStreetMap" "https://www.openstreetmap.org")
  (send editor move-position 'home))

(define (make-about-frame)
  (define f (new frame% [label "About ActivityLog2"]
                 [width 600]
                 [height 600]))
  (define c (new editor-canvas% [parent f] [style '(no-hscroll)]))
  (define t (new text%))
  (send c set-editor t)
  (setup-about-text t)
  (send t lock #t)
  f)
(provide make-about-frame)

