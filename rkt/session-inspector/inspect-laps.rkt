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
         "../al-widgets.rkt"
         "../data-frame/df.rkt")

(provide laps-panel%)

(define laps-panel%
  (class object% (init parent) (super-new)

    (define the-session #f)
    (define data-frame #f)

    (define panel (new vertical-panel%
		       [parent parent]
		       ;; [style '(border)]
                       [border 5] [spacing 5]
		       [alignment '(center top)]))

    ;; Holds the widgets that control the look of the plot
    (define control-panel
      (new horizontal-panel% 
           [parent panel] [spacing 10] [border 0]
           [alignment '(center center)]
           [stretchable-height #f]))

    (define interval-choice (new interval-choice% [tag 'interval-choice-laps] [parent control-panel]))

    (define (is-lap-swim?)
      (and data-frame (df-get-property data-frame 'is-lap-swim?)))

    (define (on-lap-selected n lap)
      (when (is-lap-swim?)
        (send swim-lengths-view set-lap lap)))

    (define interval-view (new interval-view%
                          [parent panel]
                          [tag 'activity-log:lap-view]
                          [callback on-lap-selected]))
    (send interval-choice set-interval-view interval-view)
    (define swim-lengths-view (new swim-lengths-view% [parent panel] [tag 'activity-log:lengths-view]))

    (define/public (save-visual-layout)
      (send interval-view save-visual-layout)
      (send interval-choice save-visual-layout)
      (send swim-lengths-view save-visual-layout))

    (define/public (set-session session df)

      (set! the-session session)
      (set! data-frame df)

      (send swim-lengths-view show! (is-lap-swim?))
      (send interval-choice set-session session df))
    
    ))
