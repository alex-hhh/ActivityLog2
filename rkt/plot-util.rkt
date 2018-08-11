#lang racket/base
;; plot-util.rkt -- plot helpers and utilities
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2018 Alex Harsanyi (AlexHarsanyi@gmail.com)
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

(require racket/contract
         racket/class
         racket/gui/base
         racket/draw
         racket/snip
         racket/match
         racket/math
         pict
         plot/no-gui
         pict/snip
         plot/utils
         embedded-gui
         "utilities.rkt")

(provide/contract
 ;; NOTE all these are actually instances of 2d-plot-snip%, but the plot
 ;; library does not export that type.
 (set-mouse-event-callback (-> (is-a?/c snip%) (-> (is-a?/c snip%) (is-a?/c mouse-event%) (or/c #f number?) (or/c #f number?) any/c) any/c))
 (set-overlay-renderers (-> (is-a?/c snip%) (or/c (treeof renderer2d?) #f null) any/c))
 (pu-vrule (-> real? renderer2d?))
 (pu-label (->* (real? real?) () #:rest (listof (or/c string? pict? #f)) renderer2d?))
 (pu-vrange (-> real? real? (is-a?/c color%) renderer2d?))
 (pu-markers (-> (listof (vector/c real? real?)) renderer2d?))
 (make-hover-badge (-> (listof (listof (or/c #f string?))) pict?))
 (move-snip-to (-> (is-a?/c snip%) (or/c #f (cons/c number? number?)) any/c))
 (get-snip-location (-> (or/c #f (is-a?/c snip%)) (or/c #f (cons/c number? number?))))
 (xposition->histogram-slot (->* (number?) (number? number?)
                                 (values (or/c #f exact-nonnegative-integer?)
                                         (or/c #f exact-nonnegative-integer?)))))

;; NOTE: pict-snip% is from pict/snip
(provide snip-canvas% pict-snip% good-hover?)

;; Resources for drawing overlays on the plots.  Defined in one place to
;; ensure consistency across all the plots.

(define hover-tag-background (make-object color% #xff #xf8 #xdc 0.8))
(define hover-tag-item-color (make-object color% #x2f #x4f #x4f))
(define hover-tag-label-color (make-object color% #x77 #x88 #x99))
(define hover-tag-title-font (send the-font-list find-or-create-font 12 'default 'normal 'normal))
(define hover-tag-item-font (send the-font-list find-or-create-font 12 'default 'normal 'normal))
(define hover-tag-label-font (send the-font-list find-or-create-font 10 'default 'normal 'normal))
(define hover-tag-title-face (cons hover-tag-item-color hover-tag-title-font))
(define hover-tag-item-face (cons hover-tag-item-color hover-tag-item-font))
(define hover-tag-label-face (cons hover-tag-label-color hover-tag-label-font))

;; Pen used to draw vertical line overlays on plots
(define vrule-pen (send the-pen-list find-or-create-pen "black" 1.0 'short-dash))

;; Pen used to draw the circle marker overlay on plots
(define marker-pen (send the-pen-list find-or-create-pen "red" 3 'solid))

;; Can we add overlays to plot-snip% instances? This functionality is only
;; present in a development branch of the plot package, if that package is not
;; installed, the method and the overlay functionality will not be available.
(define have-plot-overlays? 'unknown)

;; Test if we can use plot overlays or not, the check is done only once and
;; the cached value is returned from than on.  If we cannot use overlays, log
;; a message as well.
(define (can-use-plot-overlays? plot-snip)
  (when (eq? have-plot-overlays? 'unknown)
    (set! have-plot-overlays?
          (object-method-arity-includes? plot-snip 'set-mouse-event-callback 1))
    (unless have-plot-overlays?
      (dbglog "plot overlays disabled")))
  have-plot-overlays?)

;; Add CALLBACK as a mouse hover callback to PLOT-SNIP.  The plot snip is
;; checked to see if it actually has that method (since this is only present
;; in a development branch of the plot package).
(define (set-mouse-event-callback plot-snip callback)
  (when (can-use-plot-overlays? plot-snip)
    (send plot-snip set-mouse-event-callback callback)))

(define (set-overlay-renderers plot-snip renderer-tree)
  (when (can-use-plot-overlays? plot-snip)
    (send plot-snip set-overlay-renderers
          (if (null? renderer-tree) #f renderer-tree))))

;; Create a vertical rule renderer at position X to be used as an overlay.
;; This is the renderer used for all VRULES in our plots, ensuring
;; consistency.
(define (pu-vrule x)
  (vrule x #:width 1 #:style 'short-dash #:color "black"))

;; `point-pict` is introduced recently (post Racket 6.12) in the plot package,
;; might not be available.  This magic incantation avoids a compilation error
;; -- this code would not be called anyway, as we check for
;; `set-mouse-event-callback` and none of these functions are called unless
;; that one is present.
(define point-pict-1 (dynamic-require 'plot 'point-pict (λ () #f)))

;; Create a renderer that draws label, which can be either a string, a pict or
;; a list of them, to be used as an overlay.  The label is drawn at position
;; X, Y in plot coordinates.
;;
;; NOTES: any #f values in LABELS are discarded (this makes the use of
;; `pu-label` more convenient).  If multiple labels are provided they are
;; stacked vertically using `vl-append`.
(define (pu-label x y . labels)
  (when point-pict-1
    (if (and (= (length labels) 1) (pict? (car labels)))
        ;; Special case: a single pict passed in is displayed as is...
        (point-pict-1 (vector x y) (car labels) #:point-sym 'none #:anchor 'auto)
        ;; Otherwise create new picts and pack them into a final pict
        (let* ((p0 (for/list ((label (in-list labels)) #:when label)
                     (if (pict? label)
                         label
                         (text label hover-tag-item-font))))
               (p1 (if (= (length p0) 1)
                       (car p0)
                       (apply vl-append 3 p0)))
               (p2 (cc-superimpose
                    (filled-rounded-rectangle (+ (pict-width p1) 10)
                                              (+ (pict-height p1) 10) -0.1
                                              #:draw-border? #f
                                              #:color hover-tag-background)
                    p1)))
          (point-pict-1 (vector x y) p2 #:point-sym 'none #:anchor 'auto)))))

;; Create a vertical rectangle overlay renderer between XMIN and XMAX using
;; COLOR.  The rectangle will cover the entire height of the plot between XMIN
;; and XMAX. This can be used as an overlay to highlight a region, so COLOR
;; should have an alpha channel to ensure it is transparent.
(define (pu-vrange xmin xmax color)
  (rectangles
   (list (vector (ivl xmin xmax) (ivl -inf.0 +inf.0)))
   #:line-style 'transparent
   #:alpha (send color alpha)
   #:color color))

;; Create a renderer that draws the MARKERS, which are a list of 2d positions.
;; These can be used as overlays.
(define (pu-markers markers)
  (points markers #:sym 'circle #:size 10 #:color "red" #:line-width 3))

;; Return a pict object representing a badge for displaying information on a
;; plot.  The ITEMS is a list of key-value string pairs and these are arranged
;; in a table format.
;;
;; As a special case, key can be #f, in which case only the value is rendered
;; using the "key" font and color.  This can be used to display additional
;; information about a value which will be shown underneath the value.
;;
;; NOTE: the returned pict object can be placed on a plot using
;; `add-pict-overlay`.
(define (make-hover-badge items)
  (define column-count
    (for/fold ((column-count 0)) ((item (in-list items)))
      (max column-count (length item))))
  (define picts '())
  (for ((item (in-list items)))
    (let* ((key (car item))
           (vals (reverse (cdr item)))
           (face (if key hover-tag-item-face hover-tag-label-face)))
      (for ((dummy (in-range (- column-count (add1 (length vals))))))
        (set! picts (cons (text "" face) picts)))
      (for ((val (in-list vals)))
        (set! picts (cons (text val face) picts)))
      (set! picts (cons (text (or key "") hover-tag-label-face) picts))))
  (let ((p0 (table column-count picts lc-superimpose cc-superimpose 15 3)))
    (cc-superimpose
     (filled-rounded-rectangle (+ (pict-width p0) 20) (+ (pict-height p0) 20) -0.1
                               #:draw-border? #f
                               #:color hover-tag-background)
     p0)))

;; return the location of SNIP as a (cons X Y), or return #f if SNIP is not
;; shown inside an editor.
(define (get-snip-location snip)
  (and snip
       (let* ((x (box 0))
              (y (box 0))
              (a (send snip get-admin))
              (e (if a (send a get-editor) #f)))
        (and e
             (send e get-snip-location snip x y #f)
             (cons (unbox x) (unbox y))))))

;; Move SNIP to LOCATION, adjusting it as necessary to remain fully visible
;; inside the canvas.  Assumes the SNIP is added to an editor.
(define (move-snip-to snip location)
  (match-let (((cons x y) (or location (cons 50 50))))
    (define editor (send (send snip get-admin) get-editor))
    (define canvas (send editor get-canvas))
    ;; Adjust the coordinates X Y such that the snip is placed inside the
    ;; canvas.
    (let-values (((width height) (send canvas get-size)))
      (let ((adjusted-x (max 0 (min x (- width (snip-width snip)))))
            (adjusted-y (max 0 (min y (- height (snip-height snip))))))
        (send editor move-to snip adjusted-x adjusted-y)))))

;; Convert the X position received by the hover callback in a histogram plot
;; back to the series and the slot withing that series.  SKIP and GAP are the
;; #:skip and #:gap arguments passed to the histogram renderer, they default
;; to DISCRETE-HISTOGRAM-GAP and DISCRETE-HISTOGRAM-SKIP parameters with
;; values of 1 and 1/8 respectively.
(define (xposition->histogram-slot xposition
                                   (skip (discrete-histogram-skip))
                                   (gap (discrete-histogram-gap)))
  (let* ((slot (exact-floor (/ xposition skip)))
         (offset (- xposition (* skip slot)))
         (series (exact-floor offset))
         (on-bar? (< (/ gap 2) (- offset series) (- 1 (/ gap 2)))))
    (if on-bar?
        (values series slot)
        (values #f #f))))

;; Return #t when the X, Y and EVENT passed on to a plot mouse callback are
;; valid to display hover information.  They are valid when X and Y are not #f
;; (they are #f when they are inside the plot snip but not on the plot itself,
;; for example in the axes area).  The mouse event must also be a motion
;; event.
(define (good-hover? x y event)
  (and (real? x) (real? y)
       (is-a? event mouse-event%)
       (eq? (send event get-event-type) 'motion)))

;; Draw MSG using FONT in the center of DC
(define (draw-centered-message dc msg font)
  (let-values (([cw ch] (send dc get-size))
               ([w h x y] (send dc get-text-extent msg font #t)))
    (send dc set-font font)
    (send dc set-text-foreground "gray")
    (let ((ox (- (/ cw 2) (/ w 2)))
          (oy (- (/ ch 2) (/ h 2))))
      (send dc draw-text msg ox oy))))

;; A read only pasteboard (used by snip-canvas%).  Can hold a "main snip"
;; which is always resized to the size of the canvas and any number of
;; floating snips.
(define read-only-pb%
  (class pasteboard%
    (define writable? #t)
    (define main-snip #f)
    (define floating-snips '())
    ;; Message to be shown when there is no main snip in the canvas.
    (define no-main-snip-message #f)
    (define message-font
      (send the-font-list find-or-create-font 36 'default 'normal 'normal))

    (define/public (set-writable w?) (set! writable? w?))

    ;; (define/augment (can-change-style? start len) writable?)
    (define/augment (can-delete? snip) writable?)
    (define/augment (can-insert? snip before x y) writable?)
    (define/augment (can-load-file? filename format) writable?)
    (define/augment (can-save-file? filename format) writable?)
    (define/augment (can-move-to? snip x y dragging?)
      (or (not dragging?) (not (eq? snip main-snip))))
    (define/override (can-do-edit-operation? op [recursive? #t])
      (case op
        [(copy select-all)  #t]
        [else    writable?]))

    (define/augment (on-insert snip before x y)
      (unless (send this find-first-snip)
        (set! main-snip snip)))
      
    (define/augment (after-insert snip before x y)
      (when (eq? main-snip snip)
        (send this move-to snip 0 0))
      (when (and main-snip (not (eq? snip main-snip)))
        (send this set-before snip main-snip)))

    (define/public (set-background-message msg)
      (set! no-main-snip-message msg))

    (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
      (when before?
        ;; Draw a message when there is no snip in the pasteboard.
        (unless (send this find-first-snip)
          (send dc clear)
          (when no-main-snip-message
            (draw-centered-message dc no-main-snip-message message-font)))))

    (super-new)
    ;;(send this hide-caret #t)
    (send this set-selection-visible #f)
    ))

;; Editor canvas specialized for holding the ActivityLog plots.  A single main
;; snip can be added using 'set-snip' -- this snip will always be resized to
;; match the size of the canvas.  Any number of floating snips can be added
;; using 'set-floating-snip' -- these snips can be dragged around but not
;; resized.  Finally, 'set-background-message' can be used to make the canvas
;; display a message when there is no main snip.
;;
;; Calling 'set-snip' a second time will remove all the previous snips.
;;
;; The ActivityLog2 application uses the main snip as the plot snip and
;; floating snips to display additional information (such as power-duration
;; model data).
(define snip-canvas%
  (class editor-canvas%
    (init parent
          [style null]
          [label #f]
          [horizontal-inset 5]
          [vertical-inset 5]
          [enabled #t]
          [vert-margin 0]
          [horiz-margin 0]
          [min-width 0]
          [min-height 0]
          [stretchable-width #t]
          [stretchable-height #t])

    (define snip #f)
    (define pb (new read-only-pb%))
    (send pb set-writable #f)

    (define/public (get-snip) snip)

    (define/override (on-size w h)
      (update-snip w h)
      (super on-size w h))

    (define (update-snip w h)
      (when snip
        (let ((snip-w (max 0 (- w (* 2 horizontal-inset))))
              (snip-h (max 0 (- h (* 2 vertical-inset))))
              (bw (box 0))
              (bh (box 0)))
          (send snip get-extent (send this get-dc) 0 0 bw bh)
          (unless (and (= (unbox bw) snip-w) (= (unbox bh) snip-h))
            (send snip resize snip-w snip-h)
            (send pb move-to snip 0 0)))))

    (define/public (set-snip s)
      (set! snip s)
      (send this suspend-flush)
      (send pb set-writable #t)
      (send pb begin-edit-sequence #f)
      (send pb erase)
      (when snip
        (send pb insert snip)
        (let-values (([w h] (send (send this get-dc) get-size)))
          (update-snip w h)))
      (send pb end-edit-sequence)
      (send pb set-writable #f)
      (send this resume-flush))

    (define/public (set-floating-snip s)
      (send pb set-writable #t)
      (send pb insert s)
      (send pb set-writable #f))

    (define/public (export-image-to-file file-name (width #f) (height #f))
      (let-values (((cw ch) (send this get-size)))
        (unless (and width height)
          (set! width (or width cw))
          (set! height (or height ch)))
        (let* ((bitmap (if (regexp-match #px".*\\.(?i:svg)" file-name)
                           #f
                           (make-bitmap width height #t)))
               (dc (if bitmap
                       (new bitmap-dc% [bitmap bitmap])
                       (new svg-dc%
                            [width width] [height height]
                            [output file-name]
                            [exists 'truncate/replace]))))
          ;; NOTE: scaling works, but makes the entire plot blurry
          (send dc scale (/ width cw) (/ height ch))
          (unless bitmap
            (send dc start-doc "export to file"))
          ;; NOTE: print-to-dc handles start-page/end-page calls
          (send (send this get-editor) print-to-dc dc 0)
          (unless bitmap
            (send dc end-doc))
          (when bitmap
            (send bitmap save-file file-name 'png)))))

    (super-new [parent parent]
               [editor pb]
               [horizontal-inset horizontal-inset]
               [vertical-inset vertical-inset]
               [label label]
               [enabled enabled]
               [style (list* 'no-hscroll 'no-vscroll style)]
               [vert-margin vert-margin]
               [horiz-margin horiz-margin]
               [min-width min-width]
               [min-height min-height]
               [stretchable-width stretchable-width]
               [stretchable-height stretchable-height])

    (define/public (set-background-message msg)
      (send pb set-background-message msg)
      (send this refresh))

    (send this lazy-refresh #t)))
