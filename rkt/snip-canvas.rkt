#lang racket/gui

;; The snip-canvas% class from unstable gui, with the folowing modifications
;;
;; -- the snip-canvas% can be empty (no snip), in which case a message is
;; displayed in the center
;; -- the snip can be replaced with another one at any time.

(require racket/class)

(provide snip-canvas%)
(provide read-only-text%)

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
        (send snip resize snip-w snip-h)))
    
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
  (class text%
    (define writable? #t)
    (define/public (set-writable w?) (set! writable? w?))
    
    (define/augment (can-change-style? start len) writable?)
    (define/augment (can-delete? start len) writable?)
    (define/augment (can-insert? start len) writable?)
    (define/augment (can-load-file? filename format) writable?)
    (define/augment (can-save-file? filename format) writable?)
    (define/override (can-do-edit-operation? op [recursive? #t])
      (case op
        [(copy select-all)  #t]
        [else    writable?]))

    (define/public (set-background-message msg)
      (set! empty-message msg))

    (define empty-message "No Data Available")
    (define empty-message-font
      (send the-font-list find-or-create-font 36 'default 'normal 'normal))

    (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
      (when before?
        ;; Draw a message when there is no snip in the pasteboard.
        (unless (send this find-first-snip)
          (send dc clear)
          (draw-centered-message dc empty-message empty-message-font))))
    
    (super-new)
    (send this hide-caret #t)))
