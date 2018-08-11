#lang racket/base
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2018 Alex Harsanyi <AlexHarsanyi@gmail.com>
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

(require racket/gui/base racket/class racket/string embedded-gui pict)
(provide tag-input-field%)

(define the-default-tag-font
  (send the-font-list find-or-create-font 10 'default 'normal 'normal))

(define tag-snip-class
  (make-object
   (class snip-class%
     (inherit set-classname)
     (super-new)
     (set-classname "tag-snip"))))

(send (get-the-snip-class-list) add tag-snip-class)

;; Snip representing a tag in the tag-input-field% object
(define tag-snip%
  (class snip%
    (init name
          [data #f]
          [font the-default-tag-font]
          [color '(235 214 161)]
          [selected-color '(255 144 89)])

    (super-new)
    (inherit set-snipclass set-count get-admin)
    (set-snipclass tag-snip-class)
    (set-count 1)

    (define tag-name name)
    (define tag-data data)

    (define (make-tag-pict selected?)
      (let* ((tag (text tag-name font))
             (bg-rect (filled-rounded-rectangle
                       (+ (pict-width tag) 20)
                       (+ (pict-height tag) 3)
                       10
                       #:draw-border? #t)))
        (cc-superimpose
         (colorize bg-rect (if selected? selected-color color)) tag)))

    (define tag-pict (make-tag-pict #f))
    (define draw-pict-fn (make-pict-drawer tag-pict))

    (define/override (get-extent dc x y w h descent space lspace rspace)
      (when w (set-box! w (pict-width tag-pict)))
      (when h (set-box! h (+ (pict-height tag-pict))))
      (when descent (set-box! descent (pict-descent tag-pict)))
      (when space (set-box! space 10.0))
      (when lspace (set-box! lspace 10.0))
      (when rspace (set-box! rspace 10.0)))

    (define/override (draw dc x y . other)
      (draw-pict-fn dc x y))

    (define/public (select flag)
      (set! tag-pict (make-tag-pict flag))
      (set! draw-pict-fn (make-pict-drawer tag-pict))
      (send (get-admin) needs-update
            this 0 0 (pict-width tag-pict) (pict-height tag-pict)))

    (define/public (get-tag-name) tag-name)
    (define/public (get-tag-data) tag-data)

    ))

;; This could be defined in embedded-gui...
(define (snip-x snip)
  (let ((pasteboard (snip-parent snip))
        (left (box 0)))
    (send pasteboard get-snip-location snip left (box 0) #f)
    (unbox left)))

(define (snip-y snip)
  (let ((pasteboard (snip-parent snip))
        (right (box 0)))
    (send pasteboard get-snip-location snip (box 0) right #f)
    (unbox right)))

;; Pasteboard for holding and arraging tags
(define tag-pasteboard%
  (class pasteboard%
    (init [cue-text #f] [callback #f])
    (super-new)

    (inherit set-caret-owner delete begin-edit-sequence end-edit-sequence
             find-first-snip move-to insert set-before add-selected
             no-selected add-undo get-canvas local-to-global)

    ;; When true, new tags can be entered via a text-edit field
    (define allow-text-editor? #f)

    ;; When true, the text edit field is active
    (define editor-is-active? #f)

    ;; Menu to insert pre-defined tags into the pasteboard.  Use
    ;; set-available-tags to determine which tags can be entered.
    (define tag-insert-menu (new popup-menu%))

    ;; Text to display in the pasteboard when it contains no items
    (define cue cue-text)

    ;; Callback to call when the contents of the pasteboard has changed.
    (define contents-changed-cb callback)

    ;; The editor snip allows the user to type in new tags.  It is managed by
    ;; this pasteboard and shown/hidden as needed.
    (define the-editor-snip
      (new editor-snip%
           [min-width 50]
           [editor (make-object
                    (class text%
                      (init)
                      (super-new)
                      (inherit get-admin global-to-local)

                      (define/override (on-focus on?)
                        ;; If we loose editor focus make a new tag and remove
                        ;; the editor.
                        (unless on? (make-new-tag-from-editor))
                        (enable-editor-snip #f))

                      (define/override (on-default-char event)
                        (if (member (send event get-key-code)
                                    '( #\, #\space #\return #\tab))
                            (make-new-tag-from-editor)
                            (super on-default-char event)))

                      (define/override (on-default-event event)
                        (super on-default-event event)
                        (cond ((eq? (send event get-event-type) 'right-up)
                               (let ((x (box (send event get-x)))
                                     (y (box (send event get-y))))
                                 (global-to-local x y)
                                 (send (get-admin) popup-menu
                                       tag-insert-menu (unbox x) (unbox y))))))
                      ))]))


    (define (make-tag-insert-menu available-tags)
      (let ((menu (new popup-menu%)))
        (for-each (lambda (tag)
                    (new menu-item% [label tag] [parent menu]
                         [callback
                          (lambda (m e)
                            (let ((snip (make-object tag-snip% tag)))
                              (snarf-editor-snip-contents)
                              (enable-editor-snip #f)
                              (insert snip max-x max-y)))]))
                  available-tags)
        (set! tag-insert-menu menu)))

    ;; Enable or disable a menu item with a specific LABEL.  Used to prevent
    ;; the user from entering duplicate tags.
    (define (enable-tag-insert-menu-item label enable?)
      (for-each (lambda (mi)
                  (when (string=? label (send mi get-plain-label))
                    (send mi enable enable?)))
                (send tag-insert-menu get-items)))

    (define (enable-editor-snip enable?)
      (when allow-text-editor?
        (unless (eq? enable? editor-is-active?)
          (if enable?
              (begin
                (insert the-editor-snip)
                (no-selected)
                (add-selected the-editor-snip)
                (let ((admin (send the-editor-snip get-admin)))
                  (send admin set-caret-owner the-editor-snip 'global)))
              (begin
                (set-caret-owner #f 'global)
                (delete the-editor-snip)))
          (set! editor-is-active? enable?))))

    ;; Grab the contents of the text editor field and return them a a string.
    ;; The contents of the editor are also cleared.
    (define (snarf-editor-snip-contents)
      (let* ((e (send the-editor-snip get-editor))
             (t (send e get-flattened-text)))
        (send e select-all)
        (send e delete)
        (string-trim t)))

    ;; Insert a new tag using the text contents of the editor.
    (define (make-new-tag-from-editor)
      (let ((tag-text (snarf-editor-snip-contents)))
        (when (> (string-length tag-text) 0)
          (let ((snip (make-object tag-snip% tag-text)))
            (insert snip max-x max-y)))))

    (define (with-edit-sequence thunk)
      (begin-edit-sequence)
      (thunk)
      (end-edit-sequence))

    ;; Return the snip list ordered by their X location.  The editor snip, if
    ;; present, is always last.
    (define (get-snip-list)
      (let* ((found-editor? #f)
             (snip-list (let loop ((snip (find-first-snip)) (snip-list '()))
                          (if snip
                              (loop (send snip next)
                                    (if (eq? snip the-editor-snip)
                                        (begin (set! found-editor? #t) snip-list)
                                        (cons snip snip-list)))
                              snip-list))))
        ;; NOTE: we sort on greater-than and reverse the list
        (let* ((sort-key (lambda (s1 s2)
                           (let* ((y1 (snip-y s1))
                                  (y2 (snip-y s2))
                                  (y-diff (- y1 y2)))
                             (cond ((< y-diff -0.5) #f)
                                   ((> y-diff 0.5) #t)
                                   (#t
                                    (let ((x1 (snip-x s1))
                                          (x2 (snip-x s2)))
                                      (> x1 x2)))))))
               (sorted (sort snip-list sort-key)))

          ;; (let ((fn (lambda (s) (list (send s get-tag-text) (snip-x s) (snip-y s)))))
          ;;   (display (format "max x :  ~a max y:~a~%" max-x max-y))
          ;;   (display (format "unsorted: ~a~%" (map fn (reverse snip-list))))
          ;;   (display (format "sorted: ~a~%" (map fn (reverse sorted)))))

          (reverse (if found-editor? (cons the-editor-snip sorted) sorted)))))

    (define max-snip-height 0)

    (define (get-max-snip-height snip-list)
      (set! max-snip-height
            (foldl (lambda (snip h)
                     ;; (when (> (snip-height snip) 16)
                     ;;   (printf "sh: ~a~%" snip))
                     (max h (snip-height snip)))
                   0 snip-list))
      max-snip-height)

    (define display-width #f)
    (define max-x 0)
    (define max-y 0)
    (define arranging-snips #f)

    (define (arrange-snips snip-to-ignore)
      (let* ((spacing 5)
             (snips (get-snip-list))
             (row-height (get-max-snip-height snips))
             (get-target-y (lambda (snip)
                             (+ spacing (/ (- row-height (snip-height snip)) 2)))))
        (unless display-width
          (let ((canvas (get-canvas)))
            (when canvas
              (let-values (([w h] (send canvas get-client-size)))
                (set! display-width w)))))
        (with-edit-sequence
         (lambda ()
           (set! arranging-snips #t)
           (let ((x 0) (y 0))
             (for-each (lambda (snip)
                         (let ((new-x (+ x (snip-width snip) spacing)))
                           (when (> new-x display-width)
                             (set! y (+ y row-height spacing))
                             (set! x 0))
                           (unless (eq? snip snip-to-ignore)
                             (move-to snip x (+ y (get-target-y snip))))
                           (set! x (+ x (snip-width snip) spacing))))
                       snips)
             (set! max-x (+ x 1))
             (set! max-y (+ y row-height)))
           (set! arranging-snips #f)))))

    (define/augment (on-select snip on?)
      (when (is-a? snip tag-snip%)
        (send snip select on?)))

    ;; TODO: do-paste needs to split the tag, also need to implement do-cut
    ;; and do-copy to copy out the text of the snippets.
    (define/override (do-paste time)
      (printf "clipboard: ~a~%" (send the-clipboard get-clipboard-data "TEXT" time))
      (let ((t (send the-clipboard get-clipboard-string time)))
        (when (> (string-length t) 0)
          (printf "pasting ~a~%" t)
          (insert (make-object tag-snip% t)))
        (arrange-snips #f)))

    (define/override (on-default-event event)
      (super on-default-event event)
      (cond ((eq? (send event get-event-type) 'left-up)
             (when (and (not editor-is-active?)
                        max-x (>= (send event get-x) max-x)
                        max-y (>= (send event get-y) max-y))
               (enable-editor-snip #t)))
            ((eq? (send event get-event-type) 'right-up)
             (send (get-canvas) popup-menu
                   tag-insert-menu
                   (send event get-x)
                   (send event get-y)))))

    (define/augment (after-move-to snip x y dragging?)
      ;; put the snip being dragged in front, so it is de
      (when dragging? (set-before snip #f))
      (unless arranging-snips
        (arrange-snips (if dragging? snip #f))))

    (define snip-count 0)

    (define/augment (after-insert snip before x y)
      (unless (eq? snip the-editor-snip)
        (enable-tag-insert-menu-item (send snip get-tag-name) #f))
      (set! snip-count (+ snip-count 1))
      (when (= snip-count 1) (send (get-canvas) refresh))
      (when contents-changed-cb
        (contents-changed-cb this))
      (arrange-snips #f))

    (define/augment (after-delete snip)
      (when (eq? snip the-editor-snip)
        (set! editor-is-active? #f))
      (unless (eq? snip the-editor-snip)
        (enable-tag-insert-menu-item (send snip get-tag-name) #t))
      (set! snip-count (- snip-count 1))
      (when (= snip-count 0) (send (get-canvas) refresh))
      (when contents-changed-cb
        (contents-changed-cb this))
      (arrange-snips #f))

    (define/augment (after-resize snip w h resized?)
      (when resized? (arrange-snips #f)))

    (define/augment (on-display-size)
      (let ((canvas (get-canvas)))
        (when canvas
          (let-values (([w h] (send canvas get-client-size)))
            (set! display-width w)
            (arrange-snips #f)))))

    (send this set-paste-text-only #t)
    (send this set-selection-visible #f)

    ;; Debug draw the max-x max-y position
    ;; (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
    ;;   (when (not before?)
    ;;     (let ((x (box max-x))
    ;;           (y (box max-y)))
    ;;       (local-to-global x y)
    ;;       (send dc set-pen "red" 3 'solid)
    ;;       (send dc draw-ellipse (unbox x) (unbox y) 10 10))))

    (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
      (when before?
        (send dc clear)
        (when (and (not (find-first-snip)) cue)
          (send dc set-text-foreground "gray")
          (send dc draw-text cue 0 0))))

    ;; Public interface

    (define/public (clear-contents)
      (enable-editor-snip #f)
      (let ((snips (get-snip-list)))
        (with-edit-sequence
         (lambda () (map (lambda (snip)
                           (send this delete snip))
                         snips)))))

    (define/public (set-contents tag-list)
      ;; NOTE: set contents-changed-cb to #f to avoid getting callbacks when
      ;; we set the contents.  We restore it at the end.
      (let ((cb contents-changed-cb))
        (set! contents-changed-cb #f)
        (clear-contents)
        (with-edit-sequence
          (lambda ()
            (for ((tag (in-list tag-list)))
              (let ((snip (make-object tag-snip% tag)))
                (insert snip)))))
        (set! contents-changed-cb cb)))

    (define/public (set-available-tags tag-list)
      (make-tag-insert-menu tag-list))

    (define/public (get-contents)
      (enable-editor-snip #f)
      (let ((snips (get-snip-list)))
        (map (lambda (snip) (send snip get-tag-name)) snips)))

    ))

(define tag-input-field%
  (class object%
    (init parent [label #f] [cue-text #f] [callback #f])
    (super-new)

    (define pb #f)
    (define cb callback)

    (let ((p (new horizontal-pane%
                  [stretchable-height #f]
                  [parent parent] [alignment '(left top)] [spacing 5])))
      (when label
        (new message% [parent p] [label label]))
      (set! pb (new tag-pasteboard%
                    [cue-text cue-text]
                    [callback (lambda (pb) (when callback (callback this)))]))
      (new editor-canvas% [parent p] [editor pb]
           [style '(no-border hide-hscroll)]
           [min-height 60]
           [min-width 250]
           [stretchable-height #f]
           [horizontal-inset 0]
           [vertical-inset 0]))

    (define/public (clear-contents)
      (send pb clear-contents))

    (define/public (set-contents tag-list)
      (send pb set-contents tag-list))

    (define/public (set-available-tags tag-list)
      (send pb set-available-tags tag-list))

    (define/public (get-contents)
      (send pb get-contents))

    ))
