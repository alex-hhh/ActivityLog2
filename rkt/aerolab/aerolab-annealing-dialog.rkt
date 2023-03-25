#lang racket/base

;; aerolab-annealing-dialog.rkt -- estimate aerolab parameters interactively
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

(require racket/gui
         math/statistics
         "aerolab-annealing.rkt"
         "aerolab-utilities.rkt"
         "../widgets/grid-pane.rkt"
         "../fmt-util.rkt"
         "../utilities.rkt")

(define max-progress-bar-range 1000)

(define aerolab-annealing-dialog%
  (class object%
    (super-new)

    (define/private (make-toplevel-dialog parent)
      (new
       (class dialog% (init) (super-new)
         (define/augment (can-close?) #t)
         (define/augment (on-close) (on-close-dialog)))
       [label "Estimate CdA and Crr parameters"]
       [min-width 400]
       [parent parent]))

    (define toplevel-window (make-toplevel-dialog #f))

    (define gui-controls (make-hash))

    (define/private (value-of control-name)
      (define control (hash-ref gui-controls control-name))
      (cond ((is-a? control text-field%)
             (send control get-value/validated))
            ((is-a? control check-box%)
             (send control get-value))
            ((is-a? control slider%)
             (send control get-value))
            ((is-a? control choice%)
             (send control get-selection))
            (#t
             (error "value-of: unknown control type"))))

    (define/private (put-value control-name value)
      (define control (hash-ref gui-controls control-name))
      (cond ((is-a? control text-field%)
             (send control set-value value))
            ((is-a? control choice%)
             (send control set-selection value))
            ((is-a? control message%)
             (send control set-label value))
            ((is-a? control check-box%)
             (send control set-value value))
            ((is-a? control gauge%)
             (send control set-value value))
            ((is-a? control slider%)
             (send control set-value value))
            ((is-a? control button%)
             (send control set-label value))
            (#t
             (error "put-value: unknown control type"))))

    (define dialog-pane
      (let ([pane (new horizontal-pane%
                       [parent toplevel-window]
                       [border 0]
                       [spacing 0])])
        (new vertical-panel%
             [parent pane]
             [border 20]
             [spacing 10]
             [alignment '(left top)])))

    (let ([pane (new horizontal-pane%
                     [parent dialog-pane]
                     [border 0]
                     [stretchable-height #f]
                     [alignment '(left center)])])
      (new message%
           [parent pane]
           [label "Estimate CdA and Crr parameters"]
           [font (send the-font-list find-or-create-font 12 'default 'normal 'normal)]))

    (let ([pane (new vertical-panel%
                     [parent dialog-pane]
                     [border 0]
                     [spacing 5]
                     [stretchable-height #f]
                     [alignment '(left center)])])
      (hash-set! gui-controls
                 'progress-bar
                 (new gauge%
                      [parent pane]
                      [label ""]
                      [range max-progress-bar-range]))
      (hash-set! gui-controls
                 'progress-message
                 (new message%
                      [parent pane]
                      [label ""]
                      [min-width 200]
                      [auto-resize #t])))

    (let ([pane (new grid-pane%
                     [parent dialog-pane]
                     [columns 4]
                     [border 0]
                     [spacing 10]
                     [alignment '(left center)])]
          [larger-font
           (send the-font-list find-or-create-font 16 'default 'normal 'normal)]
          [medium-font
           (send the-font-list find-or-create-font 14 'default 'normal 'normal)])
      (new message%
           [parent pane]
           [label "Crr"])
      (hash-set! gui-controls
                 'crr-message
                 (new message%
                      [parent pane]
                      [auto-resize #t]
                      [stretchable-width #t]
                      [font larger-font]
                      [label "---"]))
      (new message%
           [parent pane]
           [label "StdDev"])
      (hash-set! gui-controls
                 'crr-stddev-message
                 (new message%
                      [parent pane]
                      [auto-resize #t]
                      [stretchable-width #t]
                      [font medium-font]
                      [label "---"]))
      (new message%
           [parent pane]
           [label "CdA"])
      (hash-set! gui-controls
                 'cda-message
                 (new message%
                      [parent pane]
                      [auto-resize #t]
                      [stretchable-width #t]
                      [font larger-font]
                      [label "---"]))
      (new message%
           [parent pane]
           [label "StdDev"])
      (hash-set! gui-controls
                 'cda-stddev-message
                 (new message%
                      [parent pane]
                      [auto-resize #t]
                      [stretchable-width #t]
                      [font medium-font]
                      [label "---"]))

      (new message%
           [parent pane]
           [label "Initial Altitude"])
      (hash-set! gui-controls
                 'initial-altitude-message
                 (new message%
                      [parent pane]
                      [auto-resize #t]
                      [stretchable-width #t]
                      [font larger-font]
                      [label "---"]))
      (new message%
           [parent pane]
           [label "StdDev"])
      (hash-set! gui-controls
                 'initial-altitude-stddev-message
                 (new message%
                      [parent pane]
                      [auto-resize #t]
                      [stretchable-width #t]
                      [font medium-font]
                      [label "---"]))

      (new message%
           [parent pane]
           [label "Wind Speed"])
      (hash-set! gui-controls
                 'wind-speed-message
                 (new message%
                      [parent pane]
                      [auto-resize #t]
                      [stretchable-width #t]
                      [font larger-font]
                      [label "---"]))
      (new message%
           [parent pane]
           [label "StdDev"])
      (hash-set! gui-controls
                 'wind-speed-stddev-message
                 (new message%
                      [parent pane]
                      [auto-resize #t]
                      [stretchable-width #t]
                      [font medium-font]
                      [label "---"]))

      (new message%
           [parent pane]
           [label "Wind Direction"])
      (hash-set! gui-controls
                 'wind-direction-message
                 (new message%
                      [parent pane]
                      [auto-resize #t]
                      [stretchable-width #t]
                      [font larger-font]
                      [label "---"]))
      (new message%
           [parent pane]
           [label "StdDev"])
      (hash-set! gui-controls
                 'wind-direction-stddev-message
                 (new message%
                      [parent pane]
                      [auto-resize #t]
                      [stretchable-width #t]
                      [font medium-font]
                      [label "---"]))

      (new message%
           [parent pane]
           [label "Matching Cost"])
      (hash-set! gui-controls
                 'cost-message
                 (new message%
                      [parent pane]
                      [auto-resize #t]
                      [stretchable-width #t]
                      [font larger-font]
                      [label "---"]))
      (new message%
           [parent pane]
           [label "StdDev"])
      (hash-set! gui-controls
                 'cost-stddev-message
                 (new message%
                      [parent pane]
                      [auto-resize #t]
                      [stretchable-width #t]
                      [font medium-font]
                      [label "---"])))

    (let ([pane (new horizontal-panel%
                     [parent dialog-pane]
                     [border 0]
                     [spacing 10]
                     [stretchable-height #f]
                     [alignment '(right center)])])
      (hash-set! gui-controls 'button-panel pane)
      (hash-set! gui-controls
                 'begin-annealing
                 (new button%
                      [label "Start Estimation"]
                      [parent pane]
                      [callback (lambda (b e) (on-begin-annealing))]))
      (hash-set! gui-controls
                 'save-button
                 (new button%
                      [label "Save Results"]
                      [parent pane]
                      [callback (lambda (b e) (on-save-results))]))
      (hash-set! gui-controls
                 'cancel-button
                 (new button%
                      [label "Cancel"]
                      [parent pane]
                      [callback (lambda (b e) (on-close-dialog))])))

    (define work-start-timestamp (current-inexact-monotonic-milliseconds))
    (define terminate-requested? #f)
    (define annealing-thread #f)
    (define annealing-data (hash))
    (define result #f)

    (define/private (on-begin-annealing)
      (send (hash-ref gui-controls 'begin-annealing) enable #f)
      (put-value 'progress-message "Waiting for data to come through...")

      (define (progress-callback percent-complete state)
        (queue-callback
         (lambda () (on-update-progress percent-complete state)))
        ;; Need to return #t if we wish to continue...
        (not terminate-requested?))

      (define (worker)
        (set! result
          (find-aerolab-params/many
           annealing-data
           #:run-count 25
           #:worker-count (max 1 (- (processor-count) 2))
           #:progress-callback progress-callback))
        (queue-callback
         (lambda ()
           (define elapsed (/ (- (current-inexact-monotonic-milliseconds)
                            work-start-timestamp)
                         1000.0))
           (put-value 'progress-message
                      (format "Finished in ~a" (duration->string elapsed)))
           (on-update-progress 1.0 result)
           (send (hash-ref gui-controls 'button-panel)
                 change-children
                 (lambda (_old)
                   (list (hash-ref gui-controls 'save-button)
                         (hash-ref gui-controls 'cancel-button)))))))

      (set! annealing-thread
            (thread/dbglog #:name "aerolab-annealing-dialog" worker))
      (set! work-start-timestamp
            (current-inexact-monotonic-milliseconds)))

    (define (on-update-progress percent-complete state)
      (define elapsed (/ (- (current-inexact-monotonic-milliseconds)
                            work-start-timestamp)
                         1000.0))
      (define remaining-estimate
        (if (> percent-complete 0)
            (let ([total-estimate (/ elapsed percent-complete)])
              (- total-estimate elapsed))
            0))
      (define progress (exact-ceiling (* max-progress-bar-range percent-complete)))
      (put-value 'progress-bar progress)
      (put-value 'progress-message
                 (format "Estimated Time Remaining ~a (elapsed ~a)"
                         (duration->string remaining-estimate)
                         (duration->string elapsed)))
      (let ([cda (hash-ref state "cda" #f)])
        (when cda
          (put-value 'cda-message
                     (format "~a m²" (~r (statistics-mean cda) #:precision '(= 4))))
          (put-value 'cda-stddev-message (~r (statistics-stddev cda) #:precision '(= 4)))))
      (let ([crr (hash-ref state "crr" #f)])
        (when crr
          (put-value 'crr-message (~r (statistics-mean crr) #:precision '(= 5)))
          (put-value 'crr-stddev-message (~r (statistics-stddev crr) #:precision '(= 5)))))
      (let ([initial-altitude (hash-ref state "initial-altitude" #f)])
        (when initial-altitude
          (put-value 'initial-altitude-message
                     (vertical-distance->string (statistics-mean initial-altitude) #t))
          (put-value 'initial-altitude-stddev-message
                     (~r (statistics-stddev initial-altitude) #:precision '(= 1)))))
      (let ([wind-speed (hash-ref state "wind-speed" #f)])
        (when wind-speed
          (put-value 'wind-speed-message
                     (format "~a km/h" (~r (m/s->km/h (statistics-mean wind-speed)) #:precision '(= 1))))
          (put-value 'wind-speed-stddev-message
                     (~r (m/s->km/h (statistics-stddev wind-speed)) #:precision '(= 1)))))
      (let ([wind-direction (hash-ref state "wind-direction" #f)])
        (when wind-direction
          (put-value 'wind-direction-message
                     (wind-direction->string (statistics-mean wind-direction)))
          (put-value 'wind-direction-stddev-message
                     (~r (statistics-stddev wind-direction) #:precision '(= 1)))))
      (let ([cost (hash-ref state "cost" #f)])
        (when cost
          (put-value 'cost-message
                     (~r (statistics-mean cost) #:precision '(= 1)))
          (put-value 'cost-stddev-message
                     (~r (statistics-stddev cost) #:precision '(= 1))))))

    (define/private (on-close-dialog)
      (set! terminate-requested? #t)
      (and annealing-thread (sync annealing-thread))
      (set! result #f)
      (send toplevel-window show #f))

    (define/private (on-save-results)
      (send toplevel-window show #f))

    (define/public (run-dialog parent parameters)

      (send (hash-ref gui-controls 'begin-annealing) enable #t)
      (send (hash-ref gui-controls 'button-panel)
            change-children
            (lambda (_old)
              (list (hash-ref gui-controls 'begin-annealing)
                    (hash-ref gui-controls 'cancel-button))))
      (put-value 'progress-message "")
      (put-value 'progress-bar 0)
      (put-value 'cda-message "---")
      (put-value 'cda-stddev-message "---")
      (put-value 'crr-message "---")
      (put-value 'crr-stddev-message "---")
      (put-value 'initial-altitude-message "---")
      (put-value 'initial-altitude-stddev-message "---")
      (put-value 'wind-speed-message "---")
      (put-value 'wind-speed-stddev-message "---")
      (put-value 'wind-direction-message "---")
      (put-value 'wind-direction-stddev-message "---")
      (put-value 'cost-message "---")
      (put-value 'cost-stddev-message "---")

      (set! annealing-data parameters)
      (set! result #f)

      (let ([old-tl toplevel-window])
        (let ([tl (if parent (make-toplevel-dialog parent) toplevel-window)])
          (send dialog-pane reparent tl)
          (set! toplevel-window tl))
        ;; TODO: fill it in
        (send toplevel-window show #t) ; will block
        (send dialog-pane reparent old-tl)
        (set! toplevel-window old-tl)
        result))

    ))

(define the-dialog #f)

(define (aerolab-annealing-interactive parent-window parameters)
  (unless the-dialog
    (set! the-dialog (new aerolab-annealing-dialog%)))
  (send the-dialog run-dialog parent-window parameters))

(provide/contract
 [aerolab-annealing-interactive (-> (or/c (is-a?/c window<%>) #f) hash? (or/c #f hash?))])
