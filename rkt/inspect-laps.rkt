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
         "activity-util.rkt"
         "al-widgets.rkt")

(provide laps-panel%)

(define laps-panel%
  (class object%
    (init parent)
    (super-new)

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

    (define the-session #f)

    (define/public (save-visual-layout)
      (send lap-view save-visual-layout)
      (send swim-lengths-view save-visual-layout))

    (define generation -1)

    (define/public (set-session session)
      (set! generation (+ 1 generation))
      (when the-session
        (let ((sport (session-sport the-session)))
          (when (= sport 5)
            (send swim-lengths-view show! #f))))

      (set! the-session session)
      (send lap-view set-session session)

      (let* ((sport (session-sport the-session)))
        (when (= sport 5)
          (send swim-lengths-view show! #t))))
    
    (define/public (get-generation) generation)
      
    ))
