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

(require racket/gui/base racket/class racket/match racket/math)
(provide grid-pane%)

;; A pane% object that places its children in a grid.  The number of columns
;; is specified as the COLUMNS init argument and the number of rows is
;; determined by the number of children added to the grid pane.  Children will
;; be placed left to right top to bottom in the pane.
;;
;; If this object has a stretchable width or height, the available width or
;; height will be distributed only among the stretchable columns and rows.
;; The non-stretchable columns and rows will use their minimum size.  A column
;; or row is considered stretchable if *all* items in that column or row are
;; stretchable.  As a special case, if all columns or rows are not
;; stretchable, but this pane is, than all columns/rows will be stretched.
(define grid-pane%
  (class pane%
    (init-field columns)
    (super-new)
    (inherit border spacing get-alignment stretchable-width stretchable-height)

    ;; Return placement information for the child item given INFO containing
    ;; the minimum dimensions of the child and its horizontal/vertical stretch
    ;; options.  The child is in position INDEX (used to determine the
    ;; row/column) and CELL-WIDTH CELL-HEIGHT are the dimensions of a cell.
    ;;
    ;; Returns a list giving the position of the child and its dimensions.
    (define (child-placement info index x y cell-width cell-height)
      (match-define (list min-width min-height hstretch? vstretch?) info)
      (define-values (row col) (quotient/remainder index columns))
      (define-values (halign valign) (get-alignment))

      (define-values (cx cw)
        (cond (hstretch? (values x cell-width))
              ((eq? halign 'left) (values x min-width))
              ((eq? halign 'right)
               (let ((pad (- cell-width min-width)))
                 (if (>= pad 0)
                     (values (+ x pad) min-width)
                     (values x min-width))))
              (#t
               (let ((pad (exact-truncate (/ (- cell-width min-width) 2.0))))
                 (if (>= pad 0)
                     (values (+ x pad) min-width)
                     (values x min-width))))))

      (define-values (cy ch)
        (cond (vstretch? (values y cell-height))
              ((eq? halign 'top) (values y min-height))
              ((eq? halign 'bottom)
               (let ((pad (- cell-height min-height)))
                 (if (>= pad 0)
                     (values (+ y pad) min-height)
                     (values y min-height))))
              (#t
               (let ((pad (exact-truncate (/ (- cell-height min-height) 2.0))))
                 (if (>= pad 0)
                     (values (+ y pad) min-height)
                     (values y min-height))))))

      (list (exact-round cx) (exact-round cy) cw ch))

    ;; Calculate the minimum size of the container given the minimum size
    ;; requirements of its children.
    (define/override (container-size info)
      (define rows (exact-ceiling (/ (length info) columns)))
      (define column-widths (make-vector columns 0))
      (define row-heights (make-vector rows 0))

      (for (((item index) (in-indexed info)))
        (match-define (list min-width min-height hstretch? vstretch?) item)
        (define-values (row column) (quotient/remainder index columns))
        (vector-set! column-widths column (max (vector-ref column-widths column) min-width))
        (vector-set! row-heights row (max (vector-ref row-heights row) min-height)))

      (values
       (+ (border) (border) (* (sub1 columns) (spacing))
          (for/fold ((min-width 0))
                    ((w (in-vector column-widths)))
            (+ min-width w)))
       (+ (border) (border) (* (sub1 rows) (spacing))
          (for/fold ((min-height 0))
                    ((h (in-vector row-heights)))
            (+ min-height h)))))

    ;; Adjust (scale up) element sizes in ITEM-SIZES so they sum up to
    ;; TOTAL-SIZE.  ITEM-SIZES is a vector of numbers (sizes),
    ;; STRETCHABLE-ITEMS is a vector of boolean values, #t indicates that the
    ;; corresponding item in ITEM-SIZES can be scaled up.  The ITEM-SIZES
    ;; vector is modified in place.
    (define (adjust-item-sizes! item-sizes stretchable-items total-size)
      (define nitems (vector-length item-sizes))
      ;; Available size to distribute among items (excludes the borders and
      ;; spacing between the items)
      (define available (- total-size (border) (border) (* (sub1 nitems) (spacing))))
      ;; total size used by items which cannot be stretched and items that can
      ;; be stretched
      (define-values (fixed elastic)
        (for/fold ([fixed 0] [elastic 0])
                  ([width (in-vector item-sizes)]
                   [stretch? (in-vector stretchable-items)])
          (if stretch?
              (values fixed (+ elastic width))
              (values (+ fixed width) elastic))))
      ;; Calculate the scale for the item sizes: if we have some stretchable
      ;; items, they will use up all the slack and the fixed items will use
      ;; only their required space.  If we don't, all items will be stretched.
      (if (> elastic 0)
          (let ((scale (/ (- available fixed) elastic)))
            (for ([index (in-range nitems)] #:when (vector-ref stretchable-items index))
              (define nsize (* scale (vector-ref item-sizes index)))
              (vector-set! item-sizes index nsize)))
          (let ((scale (/ available fixed)))
            (for ([index (in-range nitems)])
              (define nsize (* scale (vector-ref item-sizes index)))
              (vector-set! item-sizes index (exact-round nsize))))))
  
    ;; Calculate placement information for all children, given their minimum
    ;; size requirements in INFO and the dimensions of the container (WIDTH,
    ;; HEIGHT).
    ;;
    ;; Returns a list of positions and sizes for each child (in the same order
    ;; as the items in INFO).
    (define/override (place-children info width height)
      (define rows (exact-ceiling (/ (length info) columns)))
      (define col-widths (make-vector columns 0))
      (define row-heights (make-vector rows 0))
      ;; Determines which rows and columns can be stretched: a row or column
      ;; can be stretched if *all* items in it can be stretched
      (define col-stretch (make-vector columns #t))
      (define row-stretch (make-vector rows #t))

      (for (((item index) (in-indexed info)))
        (match-define (list min-width min-height hstretch? vstretch?) item)
        (define-values (row column) (quotient/remainder index columns))
        (vector-set! col-widths column (max (vector-ref col-widths column) min-width))
        (vector-set! col-stretch column (and (vector-ref col-stretch column) hstretch?))
        (vector-set! row-heights row (max (vector-ref row-heights row) min-height))
        (vector-set! row-stretch row (and (vector-ref row-stretch row) vstretch?)))

      (when (stretchable-width)
        (adjust-item-sizes! col-widths col-stretch width))
      (when (stretchable-height)
        (adjust-item-sizes! row-heights row-stretch height))

      (for/list (((item index) (in-indexed info)))
        (define-values (row column) (quotient/remainder index columns))
        (define cell-width (vector-ref col-widths column))
        (define cell-height (vector-ref row-heights row))
        (define cell-x (+ (border)
                          (if (> column 0)
                              (for/sum ((x (in-vector col-widths 0 column)))
                                (+ x (spacing)))
                              0)))
        (define cell-y (+ (border)
                          (if (> row 0)
                              (for/sum ((y (in-vector row-heights 0 row)))
                                (+ y (spacing)))
                              0)))
        (child-placement item index cell-x cell-y cell-width cell-height)))

    ))
