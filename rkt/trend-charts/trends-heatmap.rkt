#lang racket/base

;; trends-heatmap.rkt -- route heat maps displayed on a map
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2016, 2018, 2019, 2020, 2021, 2022, 2023 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require db
         geoid
         map-widget
         plot-container/hover-util
         racket/class
         racket/format
         racket/gui/base
         racket/hash
         racket/match
         racket/math
         "../dbutil.rkt"
         "../metrics.rkt"
         "../utilities.rkt"
         "../widgets/esc-controls.rkt"
         "../widgets/main.rkt"
         "trends-chart.rkt")

(provide heatmap-chart%)


;;.................................................... map-control-snip% ....

(define map-control-snip-class
  (make-object
   (class snip-class%
     (super-new)
     (send this set-classname "map-snip"))))
(send (get-the-snip-class-list) add map-control-snip-class)

(define background (make-object color% #xff #xf8 #xdc 0.75))
(define item-color (make-object color% #x2f #x4f #x4f))
(define button-color (make-object color% #xdc #xdc #xdc))
(define hover-color (make-object color% #xef #xef #xef))
(define pushed-color (make-object color% #xd0 #xd0 #xd0))
(define fill-color (make-object color% #x00 #x64 #x00))

;; A snip providing controls for the map: shows a progress bar as activity
;; data is loaded, has zoom in + out and fit to window buttons.
(define map-control-snip%
  (class snip%
    (init-field [width #f]
                [height #f]
                [on-zoom-in (lambda () (void))]
                [on-zoom-out (lambda () (void))]
                [on-show-map-layer (lambda (show?) (void))]
                [on-fit-to-window (lambda () (void))])
    (super-new)

    (define controls '())

    (send this set-snipclass map-control-snip-class)
    (let ((flags (send this get-flags)))
      (unless (member 'handles-events flags)
        (set! flags (cons 'handles-events flags)))
      (unless (member 'handles-all-mouse-events flags)
        (set! flags (cons 'handles-all-mouse-events flags)))
      (send this set-flags flags))

    (define/private (get-editor)
      (let ((admin (send this get-admin)))
        (when admin
          (send admin get-editor))))

    (define/private (request-update)
      (let ((admin (send this get-admin)))
        (and admin (send admin needs-update this 0 0 width height))))

    (define/override (copy)
      (new map-control-snip%))

    (define/override (on-event dc x y editorx editory event)
      (define inside-snip?
        (let ((sx (- (send event get-x) x))
              (sy (- (send event get-y) y)))
          (and (<= 0 sx width) (<= 0 sy height))))
      (define result
        (and inside-snip?
             (for/or ([b (in-list controls)])
               (send b on-event dc x y editorx editory event))))
      (unless result
        ;; Since we didn't handle the event, we send it to the pasteboard%,
        ;; maybe it knows what to do with it.
        (let ((editor (get-editor)))
          (and editor (send editor on-default-event event)))))

    (define/override (on-char dc x y editorx editory event)
      ;; On char is received when we have focus, so no need to check if we are
      ;; inside the snip -- this event is for us.
      (define result
        (for/or ([b (in-list controls)])
          (send b on-char dc x y editorx editory event)))
      (unless result
        ;; Since we didn't handle the event, we send it to the pasteboard%,
        ;; maybe it knows what to do with it.
        (let ((editor (get-editor)))
          (and editor (send editor on-default-char event)))))

    (define/override (draw dc x y . _)
      (define old-smoothing (send dc get-smoothing))
      (send dc set-smoothing 'smoothed)
      (send dc set-brush
            (send the-brush-list find-or-create-brush
                  background 'solid))
      (send dc set-pen
            (send the-pen-list find-or-create-pen item-color 0.5 'solid))
      (send dc draw-rectangle x y width height)
      (for ([b (in-list controls)])
        (send b draw dc x y))
      (send dc set-smoothing old-smoothing))

    (define/override (resize w h)
      (set! width w)
      (set! height h)
      (arrange-controls)
      (send (send this get-admin) resized this #t))

    (define/override (get-extent dc x y [w #f] [h #f] [descent #f]
                                 [space #f] [lspace #f] [rspace #f])
      (unless (and width height)
        (let-values ([(w h) (min-size dc)])
          (set! width w)
          (set! height h))
        (arrange-controls))
      (when w (set-box! w width))
      (when h (set-box! h height))
      (when descent (set-box! descent 0.0))
      (when space (set-box! space 0.0))
      (when lspace (set-box! lspace 0.0))
      (when rspace (set-box! rspace 0.0)))

    (define load-gauge
      (new esc-gauge% [parent-snip this]
           [color item-color]
           [fill-color fill-color]))
    (define point-count-label
      (new esc-label% [parent-snip this]
           [label "XXX.Xk GPS points"]
           [color item-color]))
    (define zoom-label
      (new esc-label% [parent-snip this]
           [label "Zoom"]
           [color item-color]))
    (define zoom-plus-button
      (new esc-button% [parent-snip this]
           [label 'plus]
           [text-color item-color]
           [color button-color]
           [hover-color hover-color]
           [pushed-color pushed-color]
           [callback on-zoom-in]))
    (define zoom-minus-button
      (new esc-button% [parent-snip this]
           [label 'minus]
           [text-color item-color]
           [color button-color]
           [hover-color hover-color]
           [pushed-color pushed-color]
           [callback on-zoom-out]))
    (define fit-to-window-button
      (new esc-button% [parent-snip this]
           [label "Fit to Window"]
           [width 100]
           [text-color item-color]
           [color button-color]
           [hover-color hover-color]
           [pushed-color pushed-color]
           [callback on-fit-to-window]))
    (define show-map-tiles-checkbox
      (new esc-checkbox% [parent-snip this]
           [label "Show Map"]
           [color item-color]
           [fill-color fill-color]
           [callback on-show-map-layer]))

    (set! controls
          (list load-gauge
                point-count-label
                zoom-label
                zoom-plus-button
                zoom-minus-button
                fit-to-window-button
                show-map-tiles-checkbox))

    (define border 10)
    (define vspacing 12)
    (define hspacing 5)

    ;; Determine the minimum size of this snip based of all the controls that
    ;; are contained in it, returns two values, the minimum width and height.
    ;;
    ;; NOTE: this is somewhat manual process, the etc/esc-demo.rkt file
    ;; contains some container structures which would simplify the layout
    ;; process, but for not they are not used here.
    (define (min-size dc)
      (define-values (lg-width lg-height) (send load-gauge min-size dc))
      (define-values (pc-width pc-height) (send point-count-label min-size dc))
      (define-values (zl-width zl-height) (send zoom-label min-size dc))
      (define-values (zp-width zp-height) (send zoom-plus-button min-size dc))
      (define-values (zm-width zm-height) (send zoom-minus-button min-size dc))
      (define-values (fw-width fw-height) (send fit-to-window-button min-size dc))
      (define-values (sm-width sm-height) (send show-map-tiles-checkbox min-size dc))

      (send load-gauge size lg-width lg-height)
      (send point-count-label size pc-width pc-height)
      (send zoom-label size zl-width zl-height)
      (send zoom-plus-button size zp-width zp-height)
      (send zoom-minus-button size zm-width zm-height)
      (send fit-to-window-button size fw-width fw-height)
      (send show-map-tiles-checkbox size sm-width sm-height)

      (define width
        (+ border
           (max lg-width
                pc-width
                (+ zl-width hspacing zp-width hspacing zm-width)
                fw-width sm-width)
           border))
      (define height
        (+ border
           lg-height
           vspacing
           pc-height
           vspacing
           vspacing
           (max zl-height zp-height zm-height)
           vspacing
           fw-height
           vspacing
           sm-height
           vspacing
           border))

      (values width height))

    ;; Place and resize the controls in this snip to fill out the snip area
    ;; nicely.  As with `min-size` this is a manual process, and it would be
    ;; nice to update the code to use some container snips.
    (define (arrange-controls)
      (define-values (lg-width lg-height) (send load-gauge size))
      (define-values (pc-width pc-height) (send point-count-label size))
      (define-values (zl-width zl-height) (send zoom-label size))
      (define-values (zp-width zp-height) (send zoom-plus-button size))
      (define-values (zm-width zm-height) (send zoom-minus-button size))
      (define-values (fw-width fw-height) (send fit-to-window-button size))
      (define-values (sm-width sm-height) (send show-map-tiles-checkbox size))

      (define y border)

      (send load-gauge position border y)
      (send load-gauge size (- width border border) lg-height)
      (set! y (+ y lg-height vspacing))

      (send point-count-label position border y)
      (set! y (+ y pc-height vspacing vspacing))

      (let ([row-height (max zl-height zp-height zm-height)])
        (send zoom-label position
              border
              (+ y (/ (- row-height zl-height) 2)))
        (send zoom-plus-button position
              (+ border zl-width hspacing)
              (+ y (/ (- row-height zp-height) 2)))
        (send zoom-minus-button position
              (+ border zl-width hspacing zp-width hspacing)
              (+ y (/ (- row-height zm-height) 2)))
        (set! y (+ y row-height vspacing)))

      (send fit-to-window-button position border y)
      (send fit-to-window-button size (- width border border) fw-height)
      (set! y (+ y fw-height vspacing))
      (send show-map-tiles-checkbox position border y)
      (set! y (+ y sm-height vspacing)))

    (define/public (set-load-progress percent)
      (send load-gauge value percent))

    (define/public (set-show-map-layer flag)
      (send show-map-tiles-checkbox value flag))

    (define/public (set-point-count pc)
      (define label
        (cond ((> pc 1e6)
               (format "~a m GPS points" (~r (/ pc 1e6) #:precision 2)))
              ((> pc 1e3)
               (format "~a k GPS points" (~r (/ pc 1e3) #:precision 2)))
              (#t
               (format "~a GPS points" (~r pc #:precision 1)))))
      (send point-count-label set-label label))

    ))


;;............................................................ SQL query ....

;; SQL query to retrieve GPS data for an activity from the database
(define-runtime-path sql-query-path "../../sql/queries/geoidpoints.sql")
(define sql-query (define-sql-statement sql-query-path))

(define (fetch-session-geoids database session-id)
  (for/list ([g (in-query database (sql-query) session-id #:fetch 1000)])
    (sqlite-integer->geoid g)))

;; Return a list of candidate sessions selected by PARAMS, which are retrieved
;; from the settings for this chart.
(define (candidate-sessions db params)
  (match-define (cons start end) (hash-ref params 'timestamps))
  (let ((sport (hash-ref params 'sport))
        (labels (hash-ref params 'labels))
        (equipment (hash-ref params 'equipment)))
    (fetch-candidate-sessions db (car sport) (cdr sport) start end
                              #:label-ids labels #:equipment-ids equipment)))


;;.................................................... heatmap-settings% ....

;; A dialog box to edit the settings for this trends chart -- selects which
;; activities to be shown on the heat map by type date range, labels, etc.
(define heatmap-settings%
  (class* edit-dialog-base% (chart-settings-interface<%>)
    (init-field database
                [default-name "Heatmap"]
                [default-title "Heatmap"])
    (super-new [title "Heatmap Settings"]
               [icon (edit-icon)]
               [min-height 10])

    (define name-gb (make-group-box-panel (send this get-client-pane)))
    (define name-field (new text-field% [parent name-gb] [label "Name "]))
    (send name-field set-value default-name)
    (define title-field (new text-field% [parent name-gb] [label "Title "]))
    (send title-field set-value default-title)

    (define (on-sport-selected sport)
      #f)

    (define session-filter (new session-filter%
                                [parent (send this get-client-pane)]
                                [database database]
                                [sport-selected-callback on-sport-selected]))

    (define/public (get-chart-settings)
      (hash-union
       (send session-filter get-restore-data)
       (hash
        'name (send name-field get-value)
        'title (send title-field get-value))))

    (define/public (put-chart-settings data)
      (send session-filter restore-from data)
      (send name-field set-value (hash-ref data 'name "Hist"))
      (send title-field set-value (hash-ref data 'title "Histogram Chart")))

    (define/public (show-dialog parent)
      (send session-filter on-before-show-dialog)
      (and (send this do-edit parent) (get-chart-settings)))

    ))

(define heatmap-chart%
  (class trends-chart%
    (init-field database) (super-new)

    (define cached-data #f)
    (define generation 0)
    (define map-snip #f)
    (define map-control-snip #f)

    (define (get-generation) generation)

    (define/override (make-settings-dialog)
      (new heatmap-settings% [database database]))

    (define/override (invalidate-data)
      (set! cached-data #f)
      (set! map-snip #f))

    (define/override (is-invalidated-by-events? events)
      (or (hash-ref events 'database-opened #f)
          (hash-ref events 'session-deleted #f)
          (hash-ref events 'session-created #f)
          (hash-ref events 'session-updated-data #f)))

    (define/override (export-data-to-file file formatted?)
      ;; Not sure how to implement this?
      #f)

    ;; Add GPS data for each activity selected by PARAMS.  Note that this
    ;; method runs in a separate thread and has to actually add the track to
    ;; the map using `queue-callback`
    (define/private (add-points params saved-generation)
      (let* ([candidates (candidate-sessions database params)]
             [map-snip map-snip]
             [total (length candidates)]
             [total-points 0]
             [last-progress 0.0])
        (when (> total 0)
          (send map-snip begin-edit-sequence)
          (send map-snip auto-resize-to-fit #t)
          (for ([c (in-list candidates)]
                [index (in-naturals)])
            (define geoids (fetch-session-geoids database c))
            (send map-snip add-to-point-cloud geoids #:format 'ordered-geoids)
            (define progress (/ (add1 index) total))
            (set! total-points (+ total-points (length geoids)))
            (when (> (- progress last-progress) 0.02)
              (set! last-progress progress)
              (send map-snip end-edit-sequence)
              (queue-callback
               (lambda ()
                 (when (and (equal? saved-generation generation) map-control-snip)
                   (define-values (c t) (send map-snip get-point-count))
                   (send map-control-snip set-load-progress
                         (if (> total-points 0) (/ c total-points) 0))
                   (send map-control-snip set-point-count c))))
              (sleep 0.1)      ; let other threads (mainly the GUI thread) run
              (send map-snip begin-edit-sequence)))
          (send map-snip end-edit-sequence)
          ;; Wait for all points to be processed, while updating the progress bar
          (let loop ([last-c 0])
            (define-values (c t) (send map-snip get-point-count))
            (unless (or (>= c t) (> generation saved-generation))
              ;; only queue callback if the processed point count changed
              (when (> c last-c)
                (queue-callback
                 (lambda ()
                   (when map-control-snip
                     (send map-control-snip set-load-progress (/ c total-points))
                     (send map-control-snip set-point-count c)))))
              (sleep 0.5)
              (loop c)))
          (queue-callback
           ;; Set the load progress to 100%, in case we had some empty
           ;; activities.
           (lambda ()
             (when (and (equal? saved-generation generation) map-control-snip)
               (send map-control-snip set-load-progress 1.0)))))))

    (define/override (put-plot-snip canvas)
      (let-values (((w h) (send (send canvas get-dc) get-size)))
        (let* ((hinset (send canvas horizontal-inset))
               (vinset (send canvas vertical-inset))
               (iw (exact-round (- w (* 2 hinset))))
               (ih (exact-round (- h (* 2 vinset))))
               (snip (new map-snip% [width iw] [height ih]))
               (ctl (new map-control-snip%
                         [on-zoom-in (lambda ()
                                       (when map-snip
                                         (let ([zl (send map-snip zoom-level)])
                                           (send map-snip zoom-level (add1 zl)))))]
                         [on-zoom-out (lambda ()
                                       (when map-snip
                                         (let ([zl (send map-snip zoom-level)])
                                           (send map-snip zoom-level (sub1 zl)))))]
                         [on-fit-to-window (lambda ()
                                             (when map-snip
                                               (send map-snip resize-to-fit)))]
                         [on-show-map-layer (lambda (flag)
                                              (when map-snip
                                                (send map-snip show-map-layer flag)))])))
          (set! map-snip snip)
          (set! map-control-snip ctl)
          (send map-control-snip set-show-map-layer #t)
          (send map-snip show-map-layer #t)
          (send canvas set-snip map-snip)
          (send canvas set-floating-snip map-control-snip 0 0)
          ;; Move the control snip to the top-right corner of the map -- the
          ;; 10000 value for the x coordinate is our lazy way to let
          ;; `move-snip-to` move it to the right...
          (move-snip-to map-control-snip '(10000.0 . 10.0))
          (send map-control-snip set-point-count 0)
          (set! generation (add1 generation))
          (let ([saved-generation generation]
                [params (send this get-chart-settings)])
            ;; Actual data is retrieved in a separate thread to minimize GUI
            ;; blocking
            (queue-task
             "heatmap-chart%/put-plot-snip"
             (lambda () (add-points params saved-generation)))))))

    (define/override (save-plot-image file-name width height)
      ;; NOTE: we ignore width and height parameters here :-(
      (send map-snip export-image-to-file file-name))

    ))
