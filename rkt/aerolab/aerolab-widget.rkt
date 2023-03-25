#lang racket/base

;; aerolab-widget.rkt --
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

(require data-frame
         racket/class
         racket/gui/base
         racket/match
         geoid/geodesy
         racket/math
         racket/format
         gui-widget-mixins
         plot-container
         math/statistics
         plot
         plot-container/hover-util
         racket/dict
         "../fmt-util.rkt"
         "../widgets/widget-utilities.rkt"
         "../widgets/grid-pane.rkt"
         (prefix-in ct: "../color-theme.rkt")
         "air-density.rkt"
         "air-density-dialog.rkt"
         "aerolab-utilities.rkt"
         "aerolab-annealing.rkt"
         "aerolab-annealing-dialog.rkt")

(provide aerolab-widget%)

(define min-text-field-width 120)
(define min-cda 0.1)
(define max-cda 0.9)
(define min-crr 0.002)
(define max-crr 0.01)
(define min-wind-speed 0)
(define max-wind-speed 50)              ; km/h
(define min-wind-direction -180)
(define max-wind-direction 180)
(define slider-clicks 10000)

(define (value->slider-clicks v low high)
  (define pct (/ (- v low) (- high low)))
  (exact-round (max 0 (min slider-clicks (* slider-clicks pct)))))

(define (slider-clicks->value k low high)
  (+ low (* (/ k slider-clicks) (- high low))))

(define (add-acceleration-series! df)
  (df-add-derived!
   df
   "acceleration"
   '("elapsed" "spd")
   (lambda (prev current)
     (if (and prev current)
         (match-let ([(list e0 s0) prev]
                     [(list e1 s1) current])
           (if (and e0 s0 e1 s1)
               (/ (- s1 s0) (- e1 e0))
               #f))
         #f))))

(define (maybe-add-bearing-series! df)
  (when (df-contains? df "lat" "lon")
    (df-add-derived!
     df
     "bearing"
     '("lat" "lon")
     (lambda (prev current)
       (if prev
           (match-let ([(list lat1 lon1) prev]
                       [(list lat2 lon2) current])
             (if (and lat1 lon1 lat2 lon2)
                 (final-bearing lat1 lon1 lat2 lon2)
                 #f))
           #f)))
    df))

(define (add-air-speed-series! df #:wind-speed wind-speed #:wind-direction wind-direction)
  (define air-speed #f)
  (df-add-derived!
   df
   "air-spd"
   '("bearing" "spd")
   (lambda (data)
     (match-define (list bearing spd) data)
     (if (and bearing spd)
         (let* ([apparent-wdir (+ bearing wind-direction)]
                [apparent-wspeed (* wind-speed (cos (degrees->radians apparent-wdir)))])
           (when (rational? apparent-wspeed)
             ;; NOTE regarding use of max: we assume that the wind speed will
             ;; never push a cyclist backwards.  We expect Aerolab tests to be
             ;; carried out in light wind conditions.
             (set! air-speed (max 0.0 (- spd apparent-wspeed))))
           air-speed)
         spd)))

  (define (speed-km/h val)
    (define spd (car val))
    (if spd (m/s->km/h spd) spd))
  (define (speed-mi/h val)
    (define spd (car val))
    (if spd (m/s->mi/h spd) #f))

  (df-add-derived!
   df
   "air-speed"
   '("air-spd")
   (if (eq? (al-pref-measurement-system) 'metric)
       speed-km/h speed-mi/h)))

(define (add-valt0-series!
         df
         #:crr crr
         #:cda cda
         #:total-weight total-weight
         #:air-density air-density
         #:trim-start (trim-start 0)
         #:trim-end (trim-end 0))
  (define g 9.807)                      ; Earth Gravitational Constant (m/s^2)
  (define altitude 0.0)
  (define current-slope 0.0)
  ;; Delete the valt series, since it is derived form valt0 which we now
  ;; replace
  (when (df-contains? df "valt")
    (df-del-series! df "valt"))
  (when (df-contains? df "virtual-altitude")
    (df-del-series! df "virtual-altitude"))
  (define speed-series
    (if (df-contains? df "air-speed")
        "air-spd"
        "spd"))
  (define min-distance trim-start)
  (define max-distance (- (df-ref df (sub1 (df-row-count df)) "distance") trim-end))
  (df-add-derived!
   df
   "valt0"
   (list "distance" "elapsed" "pwr" "spd" "acceleration" speed-series)
   (lambda (prev next)
     (if (and prev next)
         (match-let ([(list distance elapsed1 pwr spd acceleration air-speed) next])
           (if (and distance
                    (>= distance min-distance)
                    (< distance max-distance))
               (let ()
                 (define elapsed0 (list-ref prev 1))
                 (define slope
                   (if (and elapsed1 pwr spd acceleration)
                       (let* ([term0 (/ pwr (* total-weight g spd))]
                              [term1 crr]
                              [term2 (/ acceleration g)]
                              [term3 (/ (* cda air-density (* air-speed air-speed))
                                        (* 2 total-weight g))])
                         (- term0 term1 term2 term3))
                       current-slope))
                 (set! current-slope slope)
                 (define dt (- elapsed1 elapsed0))
                 (define delta-height (* current-slope (* (or spd 0) dt)))
                 (set! altitude (+ altitude delta-height))
                 altitude)
               #f))
         #f))))

(define (add-valt-series! df #:initial-altitude initial-altitude)
  (df-add-derived!
   df
   "valt"
   '("valt0")
   (lambda (data)
     (define alt (list-ref data 0))
     (and alt (+ alt initial-altitude)))))

(define (add-virtual-altitude-series! df)
  (define (altitude-meters val)
    (car val))
  (define (altitude-feet val)
    (define alt (car val))
    (and alt (m->ft alt)))
  (df-add-derived!
   df
   "virtual-altitude"
   '("valt")
   (if (eq? (al-pref-measurement-system) 'metric)
       altitude-meters
       altitude-feet)))

;; NOTE: matching cost is calculated on "valt" / "calt" or "alt", so it is
;; independent of the measurement system used for display.
(define (calculate-valt-match-cost
         df
         #:altitude-series (aseries "calt")
         #:start (start 0)
         #:stop (stop (df-row-count df)))
  (for/sum ([(valt alt) (in-data-frame df "valt" aseries #:start start #:stop stop)])
    (if (and (rational? alt) (rational? valt))
        (let ([d (- valt alt)])
          (* d d))
        0)))

(define (make-speed-base-plot df)
  (define data (df-select* df "distance" "speed" #:filter valid-only))
  (define scolor (cdr (assq 'speed (ct:series-colors))))
  (define acolor (cdr (assq 'air-speed (ct:series-colors))))
  (define s (df-statistics df "speed"))
  (parameterize ([plot-x-label "Distance"]
                 [plot-y-label "Speed + Air Speed"])
    (plot-snip
     (list
      (tick-grid)
      (lines data #:color scolor #:width 1.5 #:label "Speed")
      ;; Put a dummy renderer, to get a legend entry
      (lines #((0 0)) #:color acolor #:width 1.7 #:label "Air Speed"))
     #:y-min 0 #:y-max (* 2.0 (statistics-max s)))))

(define (make-air-speed-renderer df)
  (define data (df-select* df "distance" "air-speed" #:filter valid-only))
  (define c (cdr (assq 'air-speed (ct:series-colors))))
  (lines data #:color c #:width 1.7))

(define (make-altitude-base-plot df aseries)
  (if aseries
      (let ([data (df-select* df "distance" aseries #:filter valid-only)]
            [s (df-statistics df aseries)]
            [acolor (cdr (assq (string->symbol aseries) (ct:series-colors)))]
            [vcolor (cdr (assq 'virtual-altitude (ct:series-colors)))])
        (define extend (* 0.5 (statistics-range s)))
        (parameterize ([plot-x-label "Distance"]
                       [plot-y-label "Altitude + Virtual Altitude"])
          (plot-snip
           (list
            (tick-grid)
            (lines data #:color acolor #:width 1.5 #:label "Altitude")
            ;; Put a dummy renderer, to get a legend entry
            (lines #((0 0)) #:color vcolor #:width 1.7 #:label "Virtual Altitude"))
           #:y-min (- (statistics-min s) extend)
           #:y-max (+ (statistics-max s) extend))))
      #f))

(define (make-virtual-altitude-renderer df)
  (define data (df-select* df "distance" "virtual-altitude" #:filter valid-only))
  (define c (cdr (assq 'virtual-altitude (ct:series-colors))))
  (lines data #:color c #:width 1.7))

(define (print-wind-direction v)
  (define n (string->number v))
  (if (and n (rational? n) (>= n -180) (<= n 180))
      (format "~a° (~a)" (~r n #:precision 1) (degrees->wind-rose n))
      v))


;;....................................................... aerolab-panel% ....

(define aerolab-widget%
  (class object%
    (init parent)
    (init-field [save-parameters (lambda (p) (void))])
    (super-new)

    (define data-frame #f)
    (define session-data #f)

    ;; Whether the parameters displayed in the UI have been modified by the
    ;; user.  Used to know if we need to enable the "Reset to saved" or "Reset
    ;; to last estimation" buttons or to enable the "Save Parameters" button.
    (define parameters-modified? #f)

    ;; "Current" set of parameters for the widget.  This hash is incomplete,
    ;; used by the air density calculator.  Use `get-aerolab-parameters` to
    ;; get the actual current parameters.
    (define current-parameters (hash))
    ;; Parameters that were saved.
    (define initial-parameters #f)
    ;; Parameters that were obtained by the last estimation, a call to
    ;; `on-estimate-crr-cda`
    (define last-estimated-parameters #f)

    ;; Name of the altitude series in the data-frame -- "corrected-altitude"
    ;; by default, or if that one is missing, "altitude"
    (define altitude-series-name #f)
    ;; Same as `altitude-series-name` but for "calt"/"alt" series.
    (define alt-series-name #f)
    (define speed-plot #f)
    ;; Min value on the Y axis for the speed plot -- used to place overlay
    ;; labels when user hovers with the mouse over the plot
    (define speed-plot-min-y #f)
    (define air-speed-renderer #f)
    (define altitude-plot #f)
    ;; Min value on the Y axis for the altitude plot -- used to place overlay
    ;; labels when user hovers with the mouse over the plot
    (define altitude-plot-min-y #f)
    (define virtual-altitude-renderer #f)
    (define lap-markers '())

    ;; Put the GUI controls which need updating in a hash table for easier
    ;; access.
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
            ((is-a? control slider%)
             (send control set-value value))
            (#t
             (error "put-value: unknown control type"))))

    ;; Enable or Disable the "estimate-crr-cda" button.  The button is enabled
    ;; if all input fields that are needed by the Crr/CdA estimation are
    ;; valid.  This method should be called everytime a parameter that affects
    ;; the calculation is modified by the user.
    (define/private (enable-disable-estimate-parameters)
      (define enable?
        (and (let* ([c (hash-ref gui-controls 'total-weight-text-box)]
                    [v (send c get-value/validated)])
               (rational? v))
             (let* ([c (hash-ref gui-controls 'trim-start)]
                    [v (send c get-value/validated)])
               (or (rational? v) (equal? 'empty v)))
             (let* ([c (hash-ref gui-controls 'trim-end)]
                    [v (send c get-value/validated)])
               (or (rational? v) (equal? 'empty v)))
             (let* ([c (hash-ref gui-controls 'air-density-text-box)]
                    [v (send c get-value/validated)])
               (rational? v))
             (or (value-of 'dont-use-wind?)
                 (and
                  (let* ([c (hash-ref gui-controls 'wind-speed-text-box)]
                         [v (send c get-value/validated)])
                    (rational? v))
                  (let* ([c (hash-ref gui-controls 'wind-direction-text-box)]
                         [v (send c get-value/validated)])
                    (rational? v))))))
      (send (hash-ref gui-controls 'estimate-crr-cda) enable enable?))

    (define/private (on-parameters-modified)
      (set! parameters-modified? #t)
      (let ([enable? (hash? last-estimated-parameters)]
            [button (hash-ref gui-controls 'reset-params-to-last-estimate-button)])
        (send button enable enable?))
      (let ([enable? (hash? initial-parameters)]
            [button (hash-ref gui-controls 'reset-params-to-saved)])
        (send button enable enable?))
      (let ([button (hash-ref gui-controls 'save-parameters)])
        (send button enable #t)))

    (define (on-total-weight control valid?)
      (update-virtual-altitude)
      (enable-disable-estimate-parameters)
      (on-parameters-modified))

    (define (on-air-density control valid?)
      (update-virtual-altitude)
      (enable-disable-estimate-parameters)
      (on-parameters-modified))

    (define (on-air-density-calculate _control _event)
      (define new-parameters
        (air-density-interactive
         (send parent get-top-level-window)
         current-parameters))
      (when new-parameters        ; will be #f if the user canceled the dialog
        (set! current-parameters new-parameters)
        (put-value 'air-density-text-box
                   (hash-ref new-parameters 'air-density (lambda () "")))
        (update-virtual-altitude)
        (enable-disable-estimate-parameters)
        (on-parameters-modified)))

    (define (on-estimate-crr-cda _control _event)
      (define wind-speed (km/h->m/s (value-of 'wind-speed-text-box)))
      (define wind-direction (value-of 'wind-direction-text-box))
      (define-values (start-index stop-index)
        (let ([start (value-of 'trim-start)]
              [stop (value-of 'trim-end)])
          (values
           (if (rational? start)
               (df-index-of data-frame "distance" start)
               0)
           (if (rational? stop)
               (let ([m (df-ref data-frame (sub1 (df-row-count data-frame)) "distance")])
                 (df-index-of data-frame "distance" (- m stop)))
               (df-row-count data-frame)))))
      (define parameters
        (hash-set*
         (prepare-aerolab-data-for-annealing data-frame start-index stop-index)
         'total-weight (value-of 'total-weight-text-box)
         'air-density (value-of 'air-density-text-box)
         'wind-speed (cons (* wind-speed 0.5) (* wind-speed 1.5))
         'wind-direction (cons (- wind-direction 50) (+ wind-direction 50))
         'crr (cons min-crr max-crr)
         'cda (cons min-cda max-cda)))
      (validate-user-parameters parameters) ; sanity-check, this will throw
      (define ndata
        (aerolab-annealing-interactive
         (send parent get-top-level-window)
         parameters))
      (when ndata
        (for ([(k v) (in-hash ndata)])
        (let ([stats (hash-ref ndata "crr" #f)])
          (when stats
            (put-crr (statistics-mean stats))))
        (let ([stats (hash-ref ndata "cda" #f)])
          (when stats
            (put-cda (statistics-mean stats))))
        (let ([stats (hash-ref ndata "wind-speed" #f)])
          (when stats
            (put-wind-speed (statistics-mean stats))))
        (let ([stats (hash-ref ndata "wind-direction" #f)])
          (when stats
            (put-wind-direction (statistics-mean stats))))
        (let ([stats (hash-ref ndata "initial-altitude" #f)])
          (when stats
            (put-value 'altitude-offset-text-box (statistics-mean stats))))
        (update-air-speed)
        (update-virtual-altitude)
        (set! last-estimated-parameters (get-aerolab-parameters))
        (on-parameters-modified)
        (let ([button (hash-ref gui-controls 'reset-params-to-last-estimate-button)])
          (send button enable #f)))))

    (define (on-reset-to-last-estimate _control _event)
      (when last-estimated-parameters
        (setup-parameters last-estimated-parameters)
        (on-parameters-modified)
        (let ([button (hash-ref gui-controls 'reset-params-to-last-estimate-button)])
          (send button enable #f))))

    (define (on-reset-to-saved _control _event)
      (when initial-parameters
        (set! parameters-modified? #f)
        (setup-parameters initial-parameters)
        (let ([button (hash-ref gui-controls 'reset-params-to-saved)])
          (send button enable #f))
        (let ([button (hash-ref gui-controls 'save-parameters)])
          (send button enable #f))))

    (define (on-save _control _event)
      (let ([p (get-aerolab-parameters)])
        (save-parameters p)
        (set! initial-parameters p))
      (set! parameters-modified? #f)
      (let ([button (hash-ref gui-controls 'save-parameters)])
        (send button enable #f))
      (let ([button (hash-ref gui-controls 'reset-params-to-saved)])
        (send button enable #f)))

    (define (on-lap-count control valid?)
      (update-lap-markers)
      (on-parameters-modified))

    (define (on-trim-start control valid?)
      (update-lap-markers)
      (update-virtual-altitude)
      (enable-disable-estimate-parameters)
      (on-parameters-modified))

    (define (on-trim-end control valid?)
      (update-lap-markers)
      (update-virtual-altitude)
      (enable-disable-estimate-parameters)
      (on-parameters-modified))

    (define (on-altitude-offset control valid?)
      (update-virtual-altitude)
      (on-parameters-modified))

    (define (on-use-wind-data control event)
      (define use? (not (send control get-value)))
      (send (hash-ref gui-controls 'wind-speed-text-box) enable use?)
      (send (hash-ref gui-controls 'wind-speed-slider) enable use?)
      (send (hash-ref gui-controls 'wind-direction-text-box) enable use?)
      (send (hash-ref gui-controls 'wind-direction-slider) enable use?)
      (update-air-speed)
      (update-virtual-altitude)
      (enable-disable-estimate-parameters)
      (on-parameters-modified))

    (define (on-wind-speed control valid?)
      (when valid?
        (define v
          (let ([v (send control get-value/validated)])
            (if (eq? v 'empty) min-wind-speed v)))
        (define point (value->slider-clicks v min-wind-speed max-wind-speed))
        (put-value 'wind-speed-slider point))
      (update-air-speed)
      (update-virtual-altitude)
      (enable-disable-estimate-parameters)
      (on-parameters-modified))

    (define (on-wind-speed-slider control event)
      (define v (slider-clicks->value (send control get-value) min-wind-speed max-wind-speed))
      (put-value 'wind-speed-text-box v)
      (update-air-speed)
      (update-virtual-altitude)
      (enable-disable-estimate-parameters)
      (on-parameters-modified))

    (define (on-wind-direction control valid?)
      (when valid?
        (define v
          (let ([v (send control get-value/validated)])
            (if (eq? v 'empty) min-wind-direction v)))
        (define point (value->slider-clicks v min-wind-direction max-wind-direction))
        (put-value 'wind-direction-slider point))
      (update-air-speed)
      (update-virtual-altitude)
      (enable-disable-estimate-parameters)
      (on-parameters-modified))

    (define (on-wind-direction-slider control event)
      (define v (slider-clicks->value (send control get-value) min-wind-direction max-wind-direction))
      (put-value 'wind-direction-text-box v)
      (update-air-speed)
      (update-virtual-altitude)
      (enable-disable-estimate-parameters)
      (on-parameters-modified))

    (define (on-crr control valid?)
      (when valid?
        (define v
          (let ([v (send control get-value/validated)])
            (if (eq? v 'empty) min-crr v)))
        (define point (value->slider-clicks v min-crr max-crr))
        (put-value 'crr-slider point))
      (update-virtual-altitude)
      (on-parameters-modified))

    (define (on-crr-slider control event)
      (define v (slider-clicks->value (send control get-value) min-crr max-crr))
      (put-value 'crr-text-box v)
      (update-virtual-altitude)
      (on-parameters-modified))

    (define (on-cda control valid?)
      (when valid?
        (define v
          (let ([v (send control get-value/validated)])
            (if (eq? v 'empty) min-cda v)))
        (define point (value->slider-clicks v min-cda max-cda))
        (put-value 'cda-slider point))
      (update-virtual-altitude)
      (on-parameters-modified))

    (define (on-cda-slider control event)
      (define v (slider-clicks->value (send control get-value) min-cda max-cda))
      (put-value 'cda-text-box v)
      (update-virtual-altitude)
      (on-parameters-modified))

    (define panel
      (new horizontal-panel%
           [parent parent]
           [border 5]
           [spacing 5]
           [alignment '(center top)]))

    ;; Attempt to avoid flickering when constructing the widgets inside this
    ;; panel -- note that there's an `end-container-sequence` after we're done
    ;; constructing the widgets.
    (send panel begin-container-sequence)

    (define control-panel
      (new vertical-panel%
           [parent panel]
           [border 0]
           [spacing 15]
           [alignment '(center top)]
           [stretchable-width #f]))

    (define data-panel
      (new vertical-panel%
           [parent panel]
           [border 0]
           [spacing 0]
           [alignment '(center top)]
           [stretchable-width #t]))

    (let ([p (new horizontal-panel%
                  [parent data-panel]
                  [border 15]
                  [spacing 15]
                  [alignment '(center center)]
                  [stretchable-width #t]
                  [stretchable-height #f])])
      (define larger-font
        (send the-font-list find-or-create-font 16 'default 'normal 'normal))
      (new message% [parent p] [label "Crr"])
      (hash-set! gui-controls
                 'crr-info-message
                 (new message%
                      [parent p]
                      ;; [auto-resize #t]
                      [stretchable-width #t]
                      [font larger-font]
                      [label "---"]))
      (new message% [parent p] [label "CdA"])
      (hash-set! gui-controls
                 'cda-info-message
                 (new message%
                      [parent p]
                      ;; [auto-resize #t]
                      [stretchable-width #t]
                      [font larger-font]
                      [label "---"]))
      (new message% [parent p] [label "Matching Cost"])
      (hash-set! gui-controls
                 'cost-info-message
                 (new (tooltip-mixin message%)
                      [parent p]
                      [tooltip "Smaller is better"]
                      ;; [auto-resize #t]
                      [stretchable-width #t]
                      [font larger-font]
                      [label "---"]))
      (hash-set!
       gui-controls
       'estimate-crr-cda
       (new button%
            [parent p]
            [label "Estimate Parameters..."]
            [callback on-estimate-crr-cda])))

    (define plot-container
      (new plot-container% [parent data-panel]))
    (send plot-container accept-tab-focus #f)
    (send plot-container allow-tab-exit #t)

    (send panel end-container-sequence)

    (define (plot-hover-callback snip event x y)
      (if (good-hover? snip x y event)
          (begin
            (when speed-plot
              (let ([s (and (df-contains? data-frame "spd")
                            (df-lookup data-frame "distance" "spd" x))]
                    [as (and (df-contains? data-frame "air-spd")
                             (df-lookup data-frame "distance" "air-spd" x))])
                (send speed-plot set-overlay-renderers
                      (list (or air-speed-renderer '())
                            lap-markers
                            (vrule x)
                            (hover-label
                             x speed-plot-min-y
                             (format "Speed: ~a" (if s (speed->string s #t) "N/A"))
                             (format "Air Speed: ~a" (if as (speed->string as #t) "N/A"))))))
              (when altitude-plot
                (let ([a (and alt-series-name
                              (df-lookup data-frame "distance" alt-series-name x))]
                      [va (and (df-contains? data-frame "valt")
                               (df-lookup data-frame "distance" "valt" x))])
                  (send altitude-plot set-overlay-renderers
                        (list (or virtual-altitude-renderer '())
                              lap-markers
                              (vrule x)
                              (hover-label
                               x altitude-plot-min-y
                               (format "Altitude: ~a" (if a (vertical-distance->string a #t) "N/A"))
                               (format "Virtual Altitude: ~a" (if va (vertical-distance->string va #t) "N/A")))))))))
          (begin
            (when speed-plot
              (send speed-plot set-overlay-renderers
                    (cons (or air-speed-renderer '()) lap-markers)))
            (when altitude-plot
              (send altitude-plot set-overlay-renderers
                    (cons (or virtual-altitude-renderer '()) lap-markers))))))

    (define (update-air-speed)
      (set! air-speed-renderer #f)
      (df-del-series! data-frame "air-spd")
      (df-del-series! data-frame "air-speed")
      (unless (value-of 'dont-use-wind?)
        (let ([wind-speed (value-of 'wind-speed-text-box)]
              [wind-direction (value-of 'wind-direction-text-box)])
          (when (and (rational? wind-speed) (rational? wind-direction))
            (add-air-speed-series!
             data-frame
             #:wind-speed (km/h->m/s wind-speed) #:wind-direction wind-direction)
            (set! air-speed-renderer (make-air-speed-renderer data-frame)))))
      (update-air-speed-plot))

    (define/private (update-air-speed-plot)
      (when speed-plot
        (let ([r lap-markers])
          (when air-speed-renderer
            (set! r (cons air-speed-renderer r)))
          (send speed-plot set-overlay-renderers r))))

    (define (update-virtual-altitude)
      (set! virtual-altitude-renderer #f)
      (let ([crr (value-of 'crr-text-box)]
            [cda (value-of 'cda-text-box)]
            [total-weight (value-of 'total-weight-text-box)]
            [air-density (value-of 'air-density-text-box)]
            [trim-start (value-of 'trim-start)]
            [trim-end (value-of 'trim-end)]
            [cost #f])
        (when (and (rational? crr)
                   (rational? cda)
                   (rational? total-weight)
                   (rational? air-density))
          (add-valt0-series!
           data-frame
           #:crr crr
           #:cda cda
           #:total-weight total-weight
           #:air-density air-density
           #:trim-start (if (rational? trim-start) trim-start 0)
           #:trim-end (if (rational? trim-end) trim-end 0))
          (let ([offset (value-of 'altitude-offset-text-box)])
            (add-valt-series! data-frame #:initial-altitude (if (rational? offset) offset 0))
            (add-virtual-altitude-series! data-frame)
            (set! virtual-altitude-renderer (make-virtual-altitude-renderer data-frame))

            (let ([start (value-of 'trim-start)]
                  [stop (value-of 'trim-end)])
              (define start-index
                (if (rational? start)
                    (df-index-of data-frame "distance" start)
                    0))
              (define stop-index
                (if (rational? stop)
                    (let ([m (df-ref data-frame (sub1 (df-row-count data-frame)) "distance")])
                      (df-index-of data-frame "distance" (- m stop)))
                    (df-row-count data-frame)))
              (set! cost
                    (calculate-valt-match-cost
                     data-frame
                     #:start start-index
                     #:stop stop-index
                     #:altitude-series alt-series-name)))))
        (update-virtual-altitude-plot)
        ;; Put the values for CRR, CDA and cost
        (put-value 'crr-info-message
                   (if (rational? crr) (~r crr #:precision 4) "---"))
        (put-value 'cda-info-message
                   (if (rational? cda)
                       (format "~a m²" (~r cda #:precision 4))
                       "---"))
        (put-value 'cost-info-message
                   (if (rational? cost) (~r cost #:precision 1) "---"))))

    (define/private (update-virtual-altitude-plot)
      (when altitude-plot
        (let ([r lap-markers])
          (when virtual-altitude-renderer
            (set! r (cons virtual-altitude-renderer r)))
          (send altitude-plot set-overlay-renderers r))))

    (define/private (update-lap-markers)
      (set! lap-markers '())
      (let ([lap-count (value-of 'lap-count)]
            [trim-start (value-of 'trim-start)]
            [trim-end (value-of 'trim-end)]
            [max-distance (df-ref data-frame (sub1 (df-row-count data-frame)) "distance")])
        (when (rational? lap-count)
          (define lap-length
            (/ (- max-distance
                  (if (rational? trim-start) trim-start 0)
                  (if (rational? trim-end) trim-end 0))
               lap-count))
          (define start (if (rational? trim-start) trim-start 0))
          (set! lap-markers
                (for/list ([l (in-range (add1 lap-count))])
                  (vrule (+ start (* l lap-length))
                         #:color "darkmagenta" #:width 1.75 #:style 'short-dash)))))
      (update-virtual-altitude-plot)
      (update-air-speed-plot))

    (let* ([gb (make-group-box-panel control-panel "Athlete and Bike")]
           [g (new grid-pane%
                   [parent gb]
                   [columns 3]
                   [alignment '(left center)])])
      (new message%
           [parent g]
           [label "Athlete + Bike Weight: "])
      (hash-set!
       gui-controls
       'total-weight-text-box
       (new (validate-mixin
             validate-positive-rational
             ~a
             (decorate-mixin
              (decorate-with "kg" #:validate validate-positive-rational)
              (tooltip-mixin text-field%)))
            [tooltip "Pick up your bike and step on a bathroom scale"]
            [parent g]
            [label ""]
            [min-width min-text-field-width]
            [stretchable-width #f]
            [valid-callback on-total-weight]))
      (make-spacer g 5 #t))

    (let* ([gb (make-group-box-panel control-panel "Course Details")]
           [g (new grid-pane%
                   [parent gb]
                   [columns 3]
                   [alignment '(left center)])])
      (new message%
           [parent g]
           [label "Number of Laps: "])
      (hash-set!
       gui-controls
       'lap-count
       (new (validate-mixin
             validate-positive-integer
             ~a
             (cue-mixin
              "integer"
              (tooltip-mixin text-field%)))
            [parent g]
            [label ""]
            [tooltip "Number of laps around the course"]
            [min-width min-text-field-width]
            [stretchable-width #f]
            [allow-empty? #t]
            [valid-callback on-lap-count]))
      (make-spacer g 5 #t)
      (new message%
           [parent g]
           [label "Trim Start: "])
      (hash-set!
       gui-controls
       'trim-start
       (new (validate-mixin
             validate-non-negative-rational
             (lambda (v) (~r v #:precision 3))
             (decorate-mixin
              (decorate-with "km" #:validate validate-non-negative-rational)
              (cue-mixin
               "km"
               (tooltip-mixin text-field%))))
            [parent g]
            [allow-empty? #t]
            [label ""]
            [min-width min-text-field-width]
            [tooltip "Distance to discard at the start of the activity"]
            [stretchable-width #f]
            [valid-callback on-trim-start]))
      (make-spacer g 5 #t)
      (new message%
           [parent g]
           [label "Trim End: "])
      (hash-set!
       gui-controls
       'trim-end
       (new (validate-mixin
             validate-non-negative-rational
             (lambda (v) (~r v #:precision 3))
             (decorate-mixin
              (decorate-with "km" #:validate validate-non-negative-rational)
              (cue-mixin
               "km"
               (tooltip-mixin text-field%))))
            [parent g]
            [allow-empty? #t]
            [label ""]
            [tooltip "Distance to discard at the end of the activity"]
            [min-width min-text-field-width]
            [stretchable-width #f]
            [valid-callback on-trim-end]))
      (make-spacer g 5 #t)
            (new message%
           [parent g]
           [label "Altitude Offset: "])
      (hash-set!
       gui-controls
       'altitude-offset-text-box
       (new (validate-mixin
             validate-rational
             (lambda (v) (~r v #:precision 1))
             (decorate-mixin
              (decorate-with "m" #:validate validate-rational)
              (cue-mixin
               "meters"
               (tooltip-mixin text-field%))))
            [parent g]
            [label ""]
            [min-width min-text-field-width]
            [stretchable-width #f]
            [tooltip "Value for initial virtual altitude"]
            [allow-empty? #t]
            [valid-callback on-altitude-offset]))
      (make-spacer g 5 #t))

    (let ([gb (make-group-box-panel control-panel "Weather Conditions")])
      (hash-set!
       gui-controls
       'dont-use-wind?
       (new check-box%
            [parent gb]
            [label "Don't account for wind"]
            [value #f]
            [callback on-use-wind-data]))
      (let ([g (new grid-pane%
                    [parent gb]
                    [columns 3]
                    [alignment '(left center)])])
        (new message%
             [parent g]
             [label "Wind Speed: "])
        (hash-set!
         gui-controls
         'wind-speed-text-box
         (new (validate-mixin
               (validate-rational-between min-wind-speed max-wind-speed)
               (lambda (v) (~r v #:precision 2))
               (decorate-mixin
                (decorate-with "km/h" #:validate validate-non-negative-rational)
                (cue-mixin "km/h" text-field%)))
              [parent g]
              [label ""]
              [valid-callback on-wind-speed]
              [min-width min-text-field-width]
              [stretchable-width #f]))
        (hash-set!
         gui-controls
         'wind-speed-slider
         (new slider%
              [parent g]
              [label ""]
              [style '(plain horizontal)]
              [min-value 0]
              [max-value slider-clicks]
              [callback on-wind-speed-slider]))
        (new message%
             [parent g]
             [label "Wind Direction: "])
        (hash-set!
         gui-controls
         'wind-direction-text-box
         (new (validate-mixin
               (validate-rational-between min-wind-direction max-wind-direction)
               (lambda (v) (~r v #:precision 1))
               (decorate-mixin
                print-wind-direction
                (cue-mixin (format "~a to ~a" min-wind-direction max-wind-direction)
                           text-field%)))
              [parent g]
              [label ""]
              [min-width min-text-field-width]
              [valid-callback on-wind-direction]
              [stretchable-width #f]))
        (hash-set!
         gui-controls
         'wind-direction-slider
         (new slider%
              [parent g]
              [label ""]
              [style '(plain horizontal)]
              [min-value 0]
              [max-value slider-clicks]
              [callback on-wind-direction-slider]))
        (new message%
             [parent g]
             [label "Air Density: "])
        (hash-set!
         gui-controls
         'air-density-text-box
         (new (validate-mixin
               validate-positive-rational
               (lambda (v) (~r v #:precision 4))
               (decorate-mixin
                (decorate-with "kg/m³" #:validate validate-positive-rational)
                (cue-mixin "kg/m³" text-field%)))
              [parent g]
              [label ""]
              [valid-callback on-air-density]
              [min-width min-text-field-width]
              [stretchable-width #f]))
        (define p (new vertical-pane%
                       [parent g]
                       [border 0]
                       [spacing 0]
                       [alignment '(center center)]
                       [stretchable-width #t]))
        (new button%
             [parent p]
             [label "Calculate..."]
             [callback on-air-density-calculate])))

    (let* ([gb (make-group-box-panel control-panel "Crr && CdA")]
           [g (new grid-pane%
                   [parent gb]
                   [columns 3]
                   [alignment '(left center)])])
      (new message%
           [parent g]
           [label "Rolling Resistance (Crr): "])
      (hash-set!
       gui-controls
       'crr-text-box
       (new (validate-mixin
             (validate-rational-between min-crr max-crr)
             (lambda (v) (~r v #:precision 6))
             (tooltip-mixin text-field%))
            [parent g]
            [label ""]
            [tooltip (format "Value between ~a and ~a" min-crr max-crr)]
            [min-width min-text-field-width]
            [valid-callback on-crr]
            [stretchable-width #f]))
      (hash-set!
       gui-controls
       'crr-slider
       (new slider%
            [parent g]
            [label ""]
            [style '(plain horizontal)]
            [callback on-crr-slider]
            [min-value 0]
            [max-value slider-clicks]))
      (new message%
           [parent g]
           [label "Drag Area Coefficient (CdA): "])
      (hash-set!
       gui-controls
       'cda-text-box
       (new (validate-mixin
             (validate-rational-between min-cda max-cda)
             (lambda (v) (~r v #:precision 4))
             (decorate-mixin
              (decorate-with "m²" #:validate (validate-rational-between min-cda max-cda))
              (tooltip-mixin text-field%)))
            [parent g]
            [tooltip (format "Value between ~a and ~a" min-cda max-cda)]
            [label ""]
            [min-width min-text-field-width]
            [valid-callback on-cda]
            [stretchable-width #f]))
      (hash-set!
       gui-controls
       'cda-slider
       (new slider%
            [parent g]
            [label ""]
            [style '(plain horizontal)]
            [min-value 0]
            [max-value slider-clicks]
            [callback on-cda-slider])))

    (let* ([gb (make-group-box-panel control-panel "")]
           [g (new horizontal-panel%
                   [stretchable-height #f]
                   [parent gb]
                   [border 5]
                   [spacing 5])])
      (send gb stretchable-height #f)   ; does not seem to work?
      (hash-set!
       gui-controls
       'reset-params-to-last-estimate-button
       (new button%
            [parent g]
            [enabled #f]
            [label "Revert to last estimate"]
            [callback on-reset-to-last-estimate]))
      (hash-set!
       gui-controls
       'reset-params-to-saved
       (new button%
            [parent g]
            [label "Revert to saved"]
            [callback on-reset-to-saved]))
      (hash-set!
       gui-controls
       'save-parameters
       (new button%
            [parent g]
            [label "Save Parameters"]
            [callback on-save])))

    (define/public (save-visual-layout)
      (void))

    (define/private (setup-parameters p)
      ;; Setup the state such that it borrows Weather data from the activity
      ;; for any keys that are missing from state.  Note that a session can
      ;; have multiple Weather Conditions records, we currently pick the first
      ;; one (they should be in timestamp order).
      (define weather
        (let ([w (dict-ref session-data 'weather-conditions (lambda () #f))])
          (if w (car w) null)))

      (define keys
        '(wind-speed wind-direction temperature dew-point humidity pressure))

      (define params
        (for/fold ([params p])
                  ([key (in-list keys)])
          (cond ((hash-has-key? params key)
                 params)
                ((dict-has-key? weather key)
                 (hash-set params key (dict-ref weather key)))
                (#t
                 params))))

      (set! current-parameters params)
      (populate-gui-from-params params))

    (define/private (calculate-air-density-from-params s)
      (let ([t (hash-ref s 'temperature (lambda () #f))]
            [dp (hash-ref s 'dew-point (lambda () #f))]
            [rh (hash-ref s 'humidity (lambda () #f))]
            [p (hash-ref s 'pressure (lambda () #f))])
        (if (and (rational? t) (rational? p))
            (cond ((rational? dp)
                   (air-density/dew-point p t dp))
                  ((rational? rh)
                   (air-density/relative-humidity p t rh))
                  (#t
                   #f))
            #f)))

    (define/private (put-wind-speed wind-speed)
      (let ([ws (if (rational? wind-speed)
                    (m/s->km/h wind-speed)
                    "")])
        (put-value 'wind-speed-text-box ws)
        (when (rational? wind-speed)
          (put-value 'wind-speed-slider
                     (value->slider-clicks ws min-wind-speed max-wind-speed)))))

    (define/private (put-wind-direction wind-direction)
      (put-value 'wind-direction-text-box wind-direction)
      (when (rational? wind-direction)
        (put-value 'wind-direction-slider
                   (value->slider-clicks
                    wind-direction
                    min-wind-direction
                    max-wind-direction))))

    (define/private (put-crr crr)
      (put-value 'crr-text-box crr)
      (when (rational? crr)
        (put-value 'crr-slider (value->slider-clicks crr min-crr max-crr))))

    (define/private (put-cda cda)
      (put-value 'cda-text-box cda)
      (when (rational? cda)
        (put-value 'cda-slider (value->slider-clicks cda min-cda max-cda))))

    (define/private (populate-gui-from-params s)
      (put-value 'total-weight-text-box (hash-ref s 'total-weight (lambda () "")))
      (put-value 'lap-count (hash-ref s 'lap-count (lambda () "")))
      (put-value 'trim-start (hash-ref s 'trim-start (lambda () "")))
      (put-value 'trim-end (hash-ref s 'trim-end (lambda () "")))
      (put-value 'altitude-offset-text-box (hash-ref s 'altitude-offset (lambda () "")))
      (put-value 'dont-use-wind? (not (hash-ref s 'use-wind? (lambda () #t))))
      (put-wind-speed (hash-ref s 'wind-speed (lambda () "")))
      (put-wind-direction (hash-ref s 'wind-direction (lambda () "")))

      ;; Use an explicit air density value from the state, but if not present,
      ;; try to calculate it using weather data from the state and session...
      (put-value 'air-density-text-box
                 (hash-ref s
                           'air-density
                           (lambda ()
                             (calculate-air-density-from-params s))))
      (put-crr (hash-ref s 'crr (lambda () "")))
      (put-cda (hash-ref s 'cda (lambda () "")))
      (update-lap-markers)
      (update-air-speed)
      (update-virtual-altitude))

    (define/public (get-aerolab-parameters)
      ;; Wind speed is stored in m/s but shown in the dialog as km/h...
      (define wind-speed
        (let ([wind-speed (value-of 'wind-speed-text-box)])
          (and (rational? wind-speed) (km/h->m/s wind-speed))))
      (define params
        (hash-set*
         current-parameters    ; contains weather data from air density widget
         ;; Collect data from the widgets
         'total-weight (value-of 'total-weight-text-box)
         'lap-count (value-of 'lap-count)
         'trim-start (value-of 'trim-start)
         'trim-end (value-of 'trim-end)
         'altitude-offset (value-of 'altitude-offset-text-box)
         'use-wind? (not (value-of 'dont-use-wind?))
         'wind-direction (value-of 'wind-direction-text-box)
         'wind-speed wind-speed
         'air-density (value-of 'air-density-text-box)
         'cda (value-of 'cda-text-box)
         'crr (value-of 'crr-text-box)))
      ;; Filter out empty or invalid values.  Note that 'use-wind? key is a
      ;; boolean and we treat it differently...
      (for/hash ([(k v) (in-hash params)]
                 #:unless (and (not (equal? k 'use-wind?))
                               (or (equal? v #f)
                                   (equal? v 'empty))))
        (values k v)))

    (define/public (unsaved-edits?)
      parameters-modified?)

    (define/public (clear)
      (set! session-data #f)
      (set! data-frame #f)
      (set! initial-parameters #f)
      (set! last-estimated-parameters #f)
      (set! parameters-modified? #f))

    (define/public (setup session df parameters)
      (set! session-data session)
      (set! data-frame df)
      (set! initial-parameters parameters)
      (set! last-estimated-parameters #f)
      (set! parameters-modified? #f)

      (set! altitude-series-name
            (cond ((df-contains? df "corrected-altitude") "corrected-altitude")
                  ((df-contains? df "altitude") "altitude")
                  (#t #f)))
      (set! alt-series-name
            (cond ((df-contains? df "calt") "calt")
                  ((df-contains? df "alt") "alt")
                  (#t #f)))

      (add-acceleration-series! df)
      (maybe-add-bearing-series! df)

      (set! speed-plot (make-speed-base-plot df))
      (set! speed-plot-min-y
            (and speed-plot
                 (let ([bounds (send speed-plot get-plot-bounds)])
                   (vector-ref (vector-ref bounds 1) 0))))
      (when speed-plot
        (send speed-plot set-mouse-event-callback plot-hover-callback))

      (set! altitude-plot (make-altitude-base-plot df altitude-series-name))
      (set! altitude-plot-min-y
            (and altitude-plot
                 (let ([bounds (send altitude-plot get-plot-bounds)])
                   (vector-ref (vector-ref bounds 1) 0))))
      (when altitude-plot
        (send altitude-plot set-mouse-event-callback plot-hover-callback))

      (send plot-container set-plot-snips speed-plot altitude-plot)

      (send (hash-ref gui-controls 'reset-params-to-last-estimate-button) enable #f)
      (send (hash-ref gui-controls 'reset-params-to-saved) enable #f)
      (send (hash-ref gui-controls 'save-parameters) enable #f)

      (setup-parameters parameters))

    ))
