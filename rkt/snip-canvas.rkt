#lang racket/base

;; The snip-canvas% class from unstable gui, with the folowing modifications
;;
;; -- the snip-canvas% can be empty (no snip), in which case a message is
;; displayed in the center
;; -- the snip can be replaced with another one at any time.

(require racket/class
         racket/gui/base
         pict)

(provide snip-canvas% pict-snip% read-only-text%)

(define pict-snip-class
  (make-object (class snip-class% (super-new) (send this set-classname "pict-snip"))))
(send (get-the-snip-class-list) add pict-snip-class)

(define pict-snip%
  (class snip%
    (init-field pict)
    (super-new)
    (send this set-snipclass pict-snip-class)
    (send this set-count 1)

    (define draw-fn (make-pict-drawer pict))

    (define/override (get-extent dc x y w h descent space lspace rspace)
      (when w (set-box! w (pict-width pict)))
      (when h (set-box! h (pict-height pict)))
      (when descent (set-box! descent (pict-descent pict)))
      (when space (set-box! space 10.0))
      (when lspace (set-box! lspace 10.0))
      (when rspace (set-box! rspace 10.0)))

    (define/override (draw dc x y . other)
      (draw-fn dc x y))
      
    ))

;; Draw MSG using FONT in the center of DC
(define (draw-centered-message dc msg font)
  (let-values (([cw ch] (send dc get-size))
               ([w h x y] (send dc get-text-extent msg font #t)))
    (send dc set-font font)
    (send dc set-text-foreground "gray")
    (let ((ox (- (/ cw 2) (/ w 2)))
          (oy (- (/ ch 2) (/ h 2))))
      (send dc draw-text msg ox oy))))

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
    (define text (new read-only-text%))
    (send text set-writable #f)

    (define/public (get-snip) snip)

    (define/override (on-size w h)
      (update-snip w h)
      (super on-size w h))

    (define (update-snip w h)
      (define snip-w (max 0 (- w (* 2 horizontal-inset))))
      (define snip-h (max 0 (- h (* 2 vertical-inset))))
      (when snip
        (send snip resize snip-w snip-h)
        (send text move-to snip 0 0)))

    (define/public (set-snip s)
      (set! snip s)
      (send this suspend-flush)
      (send text set-writable #t)
      (send text begin-edit-sequence #f)
      (send text erase)
      (when snip
        (let-values (([w h] (send (send this get-dc) get-size)))
          (update-snip w h))
        (send text insert snip))
      (send text end-edit-sequence)
      (send text set-writable #f)
      (send this resume-flush))

    (define/public (set-floating-snip s)
      (send text set-writable #t)
      (send text insert s)
      (send text set-writable #f))

    (define/public (export-image-to-file file-name)
      (when snip
        (let ((bmp (send snip get-bitmap)))
          (send bmp save-file file-name 'png))))

    (super-new [parent parent]
               [editor text]
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
      (send text set-background-message msg)
      (send this refresh))

    (send this lazy-refresh #t)))

(define read-only-text%
  (class pasteboard%
    (define writable? #t)
    (define main-snip #f)
    (define floating-snips '())
    
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
      (set! empty-message msg))

    (define empty-message #f)
    (define empty-message-font
      (send the-font-list find-or-create-font 36 'default 'normal 'normal))

    (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
      (when before?
        ;; Draw a message when there is no snip in the pasteboard.
        (unless (send this find-first-snip)
          (send dc clear)
          (when empty-message
            (draw-centered-message dc empty-message empty-message-font)))))

    (super-new)
    ;;(send this hide-caret #t)
    (send this set-selection-visible #f)
    ))
