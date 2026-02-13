#lang racket/base

;; SPDX-License-Identifier: GPL-3.0-or-later
;; dbutil-gui.rkt -- GUI database utilities
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2025 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require db/base
         racket/gui
         framework/splash
         "dbutil.rkt"
         "utilities.rkt"
         "widgets/main.rkt")

(provide/contract
 (interactive-open-database
  (-> path-string?
      (->* ((or/c 'memory path-string?))
           ((or/c #f progress-callback/c))
           connection?)
      connection?)))

;; Open a database by calling OPEN-DATABASE-FN with DATABASE-PATH, and show a
;; GUI progress bar if the database needs to be upgraded (OPEN-DATABASE-FN is
;; open-activity-log or similar)
(define (interactive-open-database database-path open-database-fn)

  (define progress-frame #f)
  (define message-field #f)
  (define progress-bar #f)
  (define last-message #f)
  (define database #f)

  (define (make-the-progress-frame)
    (shutdown-splash)
    (close-splash)
    (set! progress-bar
          (new frame%
               [width 400]
               [height 250]
               [stretchable-width #f]
               [stretchable-height #f]
               [label (if (file-exists? database-path)
                          "Opening database"
                          "Creating database")]))
    (let ([pane (new horizontal-pane%
                     [parent progress-frame]
                     [border 20]
                     [spacing 20])])
      (new message%
           [parent pane]
           [label (sql-export-icon)]
           [stretchable-height #f]
           [stretchable-width #f])
      (let ([pane (new vertical-pane%
                       [parent pane]
                       [spacing 20]
                       [alignment '(left top)])])
        (set! message-field (new message%
                                 [parent pane]
                                 [label ""]
                                 [min-width 200]))
        (set! progress-bar (new gauge%
                                [parent pane]
                                [label ""]
                                [range 100]))))
    (send progress-bar set-value 0)
    (send progress-frame center 'both)
    (send progress-frame show #t))

  (define (progress-callback message current maximum)
    ;; Make the frame the first time we are called.  If we are not called at
    ;; all, the frame it not shown.
    (unless progress-frame (make-the-progress-frame))
    ;; Setting the same message causes it to flicker.  Avoid doing that.
    (when (and message (not (equal? last-message message)))
      (set! last-message message)
      (send message-field set-label message))
    (when (and current maximum)
      (let ([p (if (> maximum 0)
                   (exact-round (* 100 (/ current maximum)))
                   0)])
        (send progress-bar set-value p))))

  (define (db-open-thread)
    (with-handlers
      (((lambda (e) #t)
        (lambda (e)
          (define error-message
            (cond ((db-exn-bad-version? e)
                   (format (string-append
                            "This version of ActivityLog2 requires database version ~a, "
                            "and the database file ~a is at version ~a. "
                            "Don't know how to upgrade the database.")
                           (db-exn-bad-version-expected e)
                           database-path
                           (db-exn-bad-version-actual e)))
                  (#t
                   (format "~a : ~a" database-path e))))
          (dbglog "interactive-open-database: ~a" error-message)
          (message-box
           "Database open error"
           error-message
           progress-frame
           '(ok stop)
           #:dialog-mixin al2-message-box-mixin))))
      (set! database (open-database-fn database-path progress-callback))))

  (yield
   (thread/dbglog
    #:name "interactive-open-database-dialog%/interactive-open-database"
    db-open-thread))

  ;; Close the frame if it was created.
  (when progress-frame
    (send progress-frame show #f))

  database)
