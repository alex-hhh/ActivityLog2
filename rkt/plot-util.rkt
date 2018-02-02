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
         pict/snip
         plot
         embedded-gui)

(provide/contract
 ;; NOTE all these are actually instances of 2d-plot-snip%, but the plot
 ;; library does not export that type.
 (move-snip-to (-> (is-a?/c snip%) (or/c #f (cons/c number? number?)) any/c))
 (get-snip-location (-> (or/c #f (is-a?/c snip%)) (or/c #f (cons/c number? number?))))
 )

;; NOTE: pict-snip% is from pict/snip
(provide snip-canvas% pict-snip%)

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
      (define snip-w (max 0 (- w (* 2 horizontal-inset))))
      (define snip-h (max 0 (- h (* 2 vertical-inset))))
      (when snip
        (send snip resize snip-w snip-h)
        (send pb move-to snip 0 0)))

    (define/public (set-snip s)
      (set! snip s)
      (send this suspend-flush)
      (send pb set-writable #t)
      (send pb begin-edit-sequence #f)
      (send pb erase)
      (when snip
        (let-values (([w h] (send (send this get-dc) get-size)))
          (update-snip w h))
        (send pb insert snip))
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
