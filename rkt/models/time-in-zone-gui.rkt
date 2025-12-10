#lang racket/base

;; SPDX-License-Identifier: GPL-3.0-or-later
;; time-in-zone-gui.rkt -- gui based section of "time-in-zone.rkt"
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2023, 2025 Alex Harsányi <AlexHarsanyi@gmail.com>
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
         racket/class
         racket/contract
         racket/gui/base
         racket/math
         "../utilities.rkt"
         "../widgets/main.rkt"
         "../sport-charms.rkt"
         "time-in-zone.rkt")

;; Update some session metrics (by calling `update-some-session-metrics` for
;; all the sessions in the database.  This displays a dialog box allowing the
;; user to select "begin" to start the update (the user may also cancel, in
;; which case the update is canceled, with partial progress only).  This is
;; intended to be used from the "Tools" menu of AL2, and should not normally
;; be needed, since the application keeps this data consistent -- it might be
;; useful if the user modifies the database outside the application.
(define (update-tiz/interactive database sport-charms [parent-window #f])

  (define progress-dialog
    (new progress-dialog%
         [title "Update metrics"]
         [description "Rebuild time-in-zone metrics for all activities"]
         [icon (sql-export-icon)]))

  (define (task progress-dialog)
    (send progress-dialog set-message "Fetching list of sessions...")
    (define sessions (query-list database "select id from A_SESSION"))
    (define num-sessions (length sessions))
    (send progress-dialog set-message "Starting update...")
    (dbglog "interactive-update-time-in-zone-data started")
    (for ([sid sessions]
          [n (in-range num-sessions)])
      #:break (let ((progress (exact-round (* 100 (/ (+ 1 n) num-sessions)))))
                (not (send progress-dialog set-progress progress)))
      (with-handlers
        (((lambda (e) #t)
          (lambda (e)                   ; log the exception, than propagate it
            (dbglog "while updating session ~a: ~a" sid e)
            (raise e))))
        (update-some-session-metrics sid database sport-charms)))
    (dbglog "interactive-update-time-in-zone-data complete"))

  (send progress-dialog run parent-window task))

;; Update metrics by calling `update-some-session-metrics` for each of the
;; session id in SESSIONS.  This pops up a dialog box showing progress, but
;; does not wait for the user to click any button, the process will start as
;; soon as the dialog box is shown.
;;
;; This function is intended to be used by code which modifies sport zones and
;; need to update the affected sessions.  See also `get-tiz-outdated-sessions`
;; to determine which sessions need to be updated.
;;
(define (update-tiz-for-sessions/interactive sessions database sport-charms parent-window)

  (define frame #f)
  (define message-field #f)
  (define progress-bar #f)
  (define last-msg #f)
  (define title "Updating Sessions")

  (define (cb msg crt max)
    ;; Setting the same message causes it to flicker.  Avoid doing that.
    (when (and msg (not (equal? last-msg msg)))
      (set! last-msg msg)
      (send message-field set-label msg))
    (when (and crt max)
      (let ((new-progress (exact-round (* 100 (/ crt max)))))
        (send progress-bar set-value new-progress))))

  (define (task-thread)
    (with-handlers
      (((lambda (e) #t)
        (lambda (e) (dbglog-exception "interactive-update-time-in-zone" e))))
      (dbglog "interactive-update-time-in-zone started")
      (define num-sessions (length sessions))
      (for ([(sid n) (in-indexed sessions)])
        (with-handlers
          (((lambda (e) #t)
            (lambda (e)                   ; log the exception, than propagate it
              (dbglog "while updating session ~a: ~a" sid e)
              (raise e))))
          (update-some-session-metrics sid database sport-charms)
          (cb (format "Updating session ~a" sid) n num-sessions)))
      (dbglog "interactive-update-time-in-zone completed")
      (send frame show #f)))

  (set! frame (new
               (class dialog%
                 (super-new)
                 (define/override (on-superwindow-show show?)
                   (thread/dbglog
                    #:name "interactive-update-time-in-zone"
                    task-thread)))
               [width 400] [height 250]
               [parent parent-window]
               [stretchable-width #f] [stretchable-height #f]
               [label title]))
  (let ((pane (make-horizontal-pane frame)))
    (send pane border 20)
    (new message% [parent pane] [label (sql-export-icon)]
         [stretchable-height #f] [stretchable-width #f])
    (let ((p2 (make-vertical-pane pane)))
      (set! message-field (new message% [parent p2] [label ""] [min-width 200]))
      (set! progress-bar (new gauge% [parent p2] [label ""] [range 100]))))
  (send progress-bar set-value 0)
  (send frame show #t)      ; on-superwindow-show will start the update thread
  (void))



(provide/contract
 (update-tiz-for-sessions/interactive (-> (listof exact-nonnegative-integer?)
                                          connection?
                                          (is-a?/c sport-charms%)
                                          (or/c (is-a?/c frame%) (is-a?/c dialog%) #f)
                                          any/c))
 (update-tiz/interactive (-> connection?
                             (is-a?/c sport-charms%)
                             (or/c (is-a?/c frame%) (is-a?/c dialog%) #f)
                             any/c)))
