#lang racket/gui
(require "al-interactive.rkt"
         "../rkt/database.rkt")
(require "../rkt/session-inspector/inspect-histogram.rkt")

;; Get a session id from "Activtity/Copy session id to clipboard" menu
(define sid 2968)

(define df (sid->df sid))
(define session (db-fetch-session sid (current-database)))

(define frame (new frame% [label "histogram-laps-test"] [width 800] [height 400]))
(define hpp (new histogram-plot-panel% [parent frame]))

(send hpp set-session session df)
(send frame show #t)
