#lang racket/base
;; inspect-laps.rkt -- lap summary view for a session
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
         racket/match
         "activity-util.rkt"
         "al-widgets.rkt")

(provide laps-panel%)

(define laps-panel%
  (class object% (init parent) (super-new)

    (define the-session #f)
    (define data-frame #f)

    (define panel (new vertical-panel%
                       [parent parent]
                       ;; [style '(border)]
                       [border 0]
                       [spacing 5]
                       [alignment '(center top)]))

    (define (on-lap-selected n lap)
      (when (= (session-sport the-session) 5) ; swim sessions
        (send swim-lengths-view set-lap lap)))

    (define lap-view (new lap-view%
                          [parent panel]
                          [tag 'activity-log:lap-view]
                          [callback on-lap-selected]))

    (define swim-lengths-view (new swim-lengths-view% [parent panel] [tag 'activity-log:lengths-view]))

    (define/public (save-visual-layout)
      (send lap-view save-visual-layout)
      (send swim-lengths-view save-visual-layout))

    (define/public (set-session session df)

      (set! the-session session)
      (set! data-frame df)

      ;; Set column setup for the current sport
      (match-let (((vector sport sub-sport) (send data-frame get-property 'sport)))
        (send lap-view set-tag (string->symbol (format "activity-log:lap-view-~a-~a" sport (or sub-sport 0)))))

      ;; Setup the session after we setup the columns
      (send lap-view set-session session)

      (define is-lap-swim? (send data-frame get-property 'is-lap-swim?))
      (send swim-lengths-view show! is-lap-swim?))

    ))
