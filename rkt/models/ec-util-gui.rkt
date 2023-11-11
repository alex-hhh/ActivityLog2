#lang racket/base

;; SPDX-License-Identifier: GPL-3.0-or-later
;; ec-util-gui.rkt -- gui based elevation correction utilities
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

(require db/base
         racket/class
         racket/contract
         racket/math
         "../widgets/main.rkt"
         "ec-util.rkt")


;; Same as `fixup-elevation-for-segment`, but show a progress dialog for the
;; user -- this is used when the operation is invoked from the ActivityLog2
;; menubar.
(define (interactive-fixup-elevation-for-segment database segment-id [parent-window #f])

  (define progress-dialog
    (new progress-dialog%
         [title "Update elevation data for segment"]
         [icon (sql-export-icon)]))

  (define progress-monitor
    (class object% (init-field progress-dialog) (super-new)

      (define num-items 100)

      (define/public (begin-stage msg max-items)
        (send progress-dialog set-message msg)
        (send progress-dialog set-progress 0)
        (set! num-items max-items))

      (define/public (set-progress n)
        (let ((pct (exact-round (* 100.0 (if (> num-items 0) (/ n num-items) 1.0)))))
          (send progress-dialog set-progress pct)))

      (define/public (finished)
        (send progress-dialog set-progress 100))))

  (define (task progress-dialog)
    (let ((m (new progress-monitor [progress-dialog progress-dialog])))
      (when segment-id
        (fixup-elevation-for-segment database segment-id m))))

  (send progress-dialog run parent-window task))

;; Same as `fixup-elevation-for-session` (when SESSION-ID is specified), or
;; `fixup-elevation-for-all-sessions` (when SESSION-ID is #f), except it shows
;; a GUI progress dialog for the operation.  This is invoked from the
;; ActivityLog2 menu bar when the user requests a "fixup" operation.
(define (interactive-fixup-elevation database session-id [parent-window #f])

  (define progress-dialog
    (new progress-dialog%
         [title "Update elevation data"]
         [icon (sql-export-icon)]))

  (define progress-monitor
    (class object% (init-field progress-dialog) (super-new)

      (define num-items 100)

      (define/public (begin-stage msg max-items)
        (send progress-dialog set-message msg)
        (send progress-dialog set-progress 0)
        (set! num-items max-items))

      (define/public (set-progress n)
        (let ((pct (exact-round (* 100.0 (if (> num-items 0) (/ n num-items) 1.0)))))
          (send progress-dialog set-progress pct)))

      (define/public (finished)
        (send progress-dialog set-progress 100))))

  (define (task progress-dialog)
    (let ((m (new progress-monitor [progress-dialog progress-dialog])))
      (if session-id
          (fixup-elevation-for-session database session-id m)
          (fixup-elevation-for-all-sessions database m))))

  (send progress-dialog run parent-window task))


(provide/contract
 (interactive-fixup-elevation (->* (connection?
                                    (or/c #f
                                          exact-nonnegative-integer?
                                          (listof exact-nonnegative-integer?)))
                                   (any/c) ; the parent window
                                   any/c))
 (interactive-fixup-elevation-for-segment (->* (connection?
                                                (or/c #f
                                                      exact-nonnegative-integer?
                                                      (listof exact-nonnegative-integer?)))
                                               (any/c) ; the parent window
                                               any/c)))
