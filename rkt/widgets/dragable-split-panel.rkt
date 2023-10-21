#lang racket/base
;; SPDX-License-Identifier: GPL-3.0-or-later

;; dragable-split-panel.rkt -- a panel where the user can resize the children
;; by draging the spacing between elements
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

(require racket/class
         racket/gui/base
         racket/match
         racket/math)

(provide horizontal-dragable-split-panel%
         vertical-dragable-split-panel%)

;; Represents an item in a dragable-split-panel, keeps track of its size,
;; minimum size and whether the item can be resized or not.
(define panel-item%
  (class object%
    (init-field widget)
    (super-new)

    ;; A locked item has reached its minimum size and cannot be shrunk or
    ;; grown, so the item will not be resized anymore even if the user drags
    ;; on the "space" between this item and its neighbor.
    (define is-locked? #f)

    ;; Position of this item in the child list of the dragable-split-panel.
    ;; This is either an X position if this is a horizontal panel or Y when
    ;; vertical.
    (define the-position 0)

    ;; The actual size of this item along the horizontal/vertical orientation
    ;; of the panel.  That is, this represents item width when the panel has a
    ;; horizontal layout and item height when the panel has a vertical layout.
    (define the-actual-size 1)

    ;; Minimum size of this item along the horizontal/vertical orientation.
    (define the-min-size 1)

    (define/public (get-widget)
      widget)

    (public min-size)
    (define min-size
      (case-lambda
        (() the-min-size)
        ((n) (set! the-min-size n) n)))

    (public actual-size)
    (define actual-size
      (case-lambda
        (() (if is-locked?
                the-min-size
                the-actual-size))
        ((n)
         (set! the-actual-size (max the-min-size n)))))

    ;; Scale the actual size of this item by SCALE.  If this results in a size
    ;; smaller than the minimum size, the item is locked at its minimum size.
    (define/public (scale-size! scale)
      (if is-locked?
          (error 'panel-item% "attempting to scale a locked item")
          (let ([nsize (exact-truncate (* the-actual-size scale))])
            (if (< nsize the-min-size)
                (begin
                  (actual-size the-min-size)
                  (locked? #t)
                  #f)
                (begin
                  (actual-size nsize)
                  #t)))))

    (public position)
    (define position
      (case-lambda
        [() the-position]
        [(n) (set! the-position n) the-position]))

    (public locked?)
    (define locked?
      (case-lambda
        [() is-locked?]
        [(flag) (set! is-locked? flag) is-locked?]))

    (define/public (can-adjust-by? diff)
      (and (not is-locked?)
           (> (+ (actual-size) diff) (min-size))))

    ))

;; Calculate the total sizes used by PANEL-ITEMS (a list of panel-item%
;; objects).  Returns two values, the total size of locked items (the fixed
;; size) and the total size of non-locked items (the elastic size)
(define (panel-item-total-sizes panel-items)
  (for/fold ([fixed 0]
             [elastic 0])
            ([i (in-list panel-items)])
    (define s (send i actual-size))
    (if (send i locked?)
        (values (+ fixed s) elastic)
        (values fixed (+ elastic s)))))

;; Adjust, in-place, the sizes of items in PANEL-ITEMS (a list of panel-item%
;; objects) to fill in AVAILABLE-SIZE with a BORDER and SPACING around the
;; items.  Locked items will not change size, and more items might become
;; locked if they would shrink below their minimum size.
;;
;; TODO: since item sizes are truncated down, we end up with a few unused
;; pixels in the container (at most one for each widget).  For a few widgets,
;; this is not noticeable, but it would be good to re-distribute these to the
;; stretchable items.
(define (adjust-panel-item-sizes! panel-items available-size border spacing)
  (define item-count (length panel-items))
  (define-values (fixed elastic) (panel-item-total-sizes panel-items))
  (unless (equal? elastic 0)
    (let ([useable-size (- available-size
                           border border
                           (* spacing (sub1 item-count))
                           fixed)])
      (if (< useable-size 0)
          (for ([i (in-list panel-items)])
            (send i actual-size (send i min-size)))
          (let ([scale (/ useable-size elastic)])
            (define re-run?
              (for/fold ([re-run? #f])
                        ([i (in-list panel-items)])
                (if (send i locked?)
                    (values re-run?)
                    (begin
                      (send i scale-size! scale)
                      ;; Did the item became locked as we tried to scale it?
                      (values (or re-run? (send i locked?)))))))
            (when re-run?
              (adjust-panel-item-sizes! panel-items available-size border spacing))))))
  (void))

;; Adjust, in-place, the positions of objects in PANEL-ITEMS based on their
;; size plus the container BORDER and SPACING, so they are placed one after
;; the other.
(define (adjust-panel-item-positions! panel-items border spacing)
  (for/fold ([current-position border])
            ([i (in-list panel-items)])
    (send i position current-position)
    (+ current-position (+ (send i actual-size) spacing)))
  (void))

;; Produce a new list of panel-item% objects based on the list of WIDGETS and
;; previous PANEL-ITEMS, previous panel items are re-used if they correspond
;; to existing widgets and new panel-item% objects are created for new
;; WIDGETS.
(define (update-panel-items widgets infos panel-items vertical-layout?)
  (for/list ([w (in-list widgets)]
             [info (in-list infos)])
    (match-define (list min-width min-height hstretch? vstretch?) info)
    (define i (for/first ([i (in-list panel-items)]
                          #:when (equal? w (send i get-widget)))
                i))
    (define u (or i (new panel-item% [widget w])))
    (send u min-size (if vertical-layout? min-height min-width))
    ;; Lock this item immediately, if it does not allow stretching.
    (send u locked? (if vertical-layout? (not vstretch?) (not hstretch?)))
    u))

;; A panel that allows the user to resize items by dragging on the spacing
;; between the items.  Can have either a vertical or a horizontal layout.
(define dragable-split-panel%
  (class panel%
    (init-field vertical-layout?)
    (super-new)

    (inherit border
             spacing
             get-alignment
             set-cursor
             container-flow-modified)

    ;; Cursor to display when mouse is in the area that the user can drag.
    (define gap-cursor (make-object cursor% (if vertical-layout? 'size-n/s 'size-e/w)))

    ;; List of panel-item% objects, one for each widget in the container.
    (define items '())

    ;; Stores percentages set by `set-percentages` before a layout operation
    ;; has happened, since ITEMS is only populated on a container layout
    ;; operation, when `place-children` is invoked.
    (define delayed-percentages #f)

    ;; Index in ITEMS where a resizing operation is taking place (when the
    ;; user is actively dragging the split between items).  Resizing happens
    ;; between this index and the next one.
    (define resizing-index #f)

    ;; Last position of the mouse event during a resizing operation, used to
    ;; determine how much to increment the size of the item based this value
    ;; and the location of a new mouse event
    (define resizing-previous-position #f)

    ;; Calculate the minimum size of the container given the minimum size
    ;; requirements of its children.
    (define/override (container-size info)
      (define-values (total-width total-height min-width min-height)
        (for/fold ([total-width 0]
                   [total-height 0]
                   [min-width 0]
                   [min-height 0])
                  ([item (in-list info)])
          (match-define (list width height _hstretch? _vstretch?) item)
          (values (+ total-width width)
                  (+ total-height height)
                  (max min-width width)
                  (max min-height height))))

      (define total-border (* 2 (border)))
      (define total-spacing (* (sub1 (length info)) (spacing)))

      (if vertical-layout?
          (values
           (+ total-border min-width)
           (+ total-border total-height total-spacing))
          (values
           (+ total-border total-width total-spacing)
           (+ total-border min-height))))

    ;; Calculate placement information for all children, given their minimum
    ;; size requirements in INFO and the dimensions of the container (WIDTH,
    ;; HEIGHT).
    ;;
    ;; Returns a list of positions and sizes for each child (in the same order
    ;; as the items in INFO).
    (define/override (place-children infos width height)
      (set! items (update-panel-items (send this get-children) infos items vertical-layout?))
      (define available-size (if vertical-layout? height width))
      (when delayed-percentages
        (for ([i (in-list items)]
              [p (in-list delayed-percentages)])
          (send i actual-size (* available-size p)))
        (set! delayed-percentages #f))
      (adjust-panel-item-sizes! items available-size (border) (spacing))
      (adjust-panel-item-positions! items (border) (spacing))
      (define-values (halign valign) (get-alignment))

      (for/list ([item (in-list items)]
                 [info (in-list infos)])
        (match-define (list min-width min-height hstretch? vstretch?) info)
        (define pos (send item position))
        (define size (send item actual-size))
        (define-values (cx cw)
          (let ([basex (if vertical-layout? (border) (send item position))]
                [basew (if vertical-layout? (- width (* 2 (border))) (send item actual-size))])
            (if hstretch?
                (values basex basew)
                (values
                 (+ basex (case halign
                            ((left) 0)
                            ((right) (- basew min-width))
                            (else (* 0.5 (- basew min-width)))))
                 min-width))))
        (define-values (cy ch)
          (let ([basey (if vertical-layout? (send item position) (border))]
                [baseh (if vertical-layout? (send item actual-size) (- height (* 2 (border))))])
            (if vstretch?
                (values basey baseh)
                (values
                 (+ basey (case valign
                            ((left) 0)
                            ((right) (- baseh min-height))
                            (else (* 0.5 (- baseh min-height)))))
                 min-height))))
        (list (exact-truncate cx)
              (exact-truncate cy)
              (exact-truncate cw)
              (exact-truncate ch))))

    (define/private (gap-index event)
      (define dim (if vertical-layout? (send event get-y) (send event get-x)))
      (for/or ([item (in-list items)]
               [index (in-naturals)])
        (define extent (+ (send item position)
                          (send item actual-size)))
        (if (< extent dim (+ extent (spacing))) index #f)))

    (define/override (on-subwindow-event receiver event)
      (if (equal? receiver this)
          (let ([gap-index (gap-index event)])
            (set-cursor (and (or gap-index resizing-index)
                             (send gap-cursor ok?)
                             gap-cursor))
            (cond ((and gap-index (send event button-down? 'left))
                   (set! resizing-index gap-index)
                   (set! resizing-previous-position
                         (if vertical-layout? (send event get-y) (send event get-x))))
                  ((send event button-up? 'left)
                   (set! resizing-index #f)
                   (set! resizing-previous-position #f))
                  ((and resizing-index resizing-previous-position (send event moving?))
                   (define new-position
                     (if vertical-layout? (send event get-y) (send event get-x)))
                   (define diff (- resizing-previous-position new-position))
                   (define item (list-ref items resizing-index))
                   (define next-item (list-ref items (add1 resizing-index)))
                   (set! resizing-previous-position new-position)
                   (when (and (send item can-adjust-by? (- diff))
                              (send next-item can-adjust-by? diff))
                     (send item actual-size (- (send item actual-size) diff))
                     (send next-item actual-size (+ (send next-item actual-size) diff))
                     (container-flow-modified)))
                  (else
                   (super on-subwindow-event receiver event))))
          (begin
            (set-cursor #f)
            (super on-subwindow-event receiver event))))

    (define/public (get-percentages)
      ;; NOTE: allow getting percentages for widgets that were not shown yet!
      (if delayed-percentages
          delayed-percentages
          (let-values
              ([(sizes total)
                (for/fold ([sizes '()]
                           [total 0]
                           #:result (values (reverse sizes) total))
                          ([i (in-list items)])
                  (define size (send i actual-size))
                  (values (cons size sizes) (+ total size)))])
            (for/list ([s (in-list sizes)]) (/ s total)))))

    (define/public (set-percentages p)
      (set! delayed-percentages p)
      (container-flow-modified))

  ))

(define vertical-dragable-split-panel%
  (class dragable-split-panel%
    (init [spacing 5])
    (super-new [vertical-layout? #t]
               [spacing spacing])))

(define horizontal-dragable-split-panel%
  (class dragable-split-panel%
    (init [spacing 5])
    (super-new [vertical-layout? #f]
               [spacing spacing])))
