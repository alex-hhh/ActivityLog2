#lang racket/base
;; qresults-list.rkt -- a sophisticated list box control, see qresults-list%
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2018, 2019, 2021, 2022 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require file/md5
         racket/class
         racket/gui/base
         racket/list
         racket/match
         racket/math
         racket/format
         "../utilities.rkt"
         "edit-dialog-base.rkt"
         "icon-resources.rkt"
         "widget-utilities.rkt")

(provide qresults-list% qcolumn)


;;.................................................. list-box% utilities ....

;; Return the column order and a list of widths for each column in the
;; list-box% LB.  This can be passed to `lb-set-visual-layout` to restore a
;; similar list-box% to the same look.
(define (lb-get-visual-layout lb)
  (let ((co (send lb get-column-order)))
    (cons co
          (for/list ((c (in-list co)))
            (call-with-values
                (lambda ()
                  (send lb get-column-width c))
                vector)))))

;; Set the column order and width in the list-box% LB from the visual layout
;; data VL, which was obtained from a call to `lb-get-visual-layout`
(define (lb-set-visual-layout lb vl)
  (send lb set-column-order (car vl))
  (for ((c (in-list (car vl)))
        (w (in-list (cdr vl))))
    (match-define (vector width min-width max-width) w)
    (send lb set-column-width c width min-width max-width)))

;; Resize header columns in the list-box% LB so that they fit in the client
;; width.  This is not always possible, but we do the best job we can, see
;; comments inside this function.
(define (lb-set-default-visual-layout lb)
  
  ;; get-label-font reports as unimplemented on win32, so we use
  ;; view-control-font as a fall back (we don't change fonts in any of our
  ;; views

  ;;(define font (send lb get-label-font))
  (define font view-control-font)
  (define dc (new bitmap-dc% [bitmap (make-screen-bitmap 100 100)]))
  (define headers (send lb get-column-labels))
  (define header-count (length headers))
  ;; We have no way to find out the minimum column width that would fit the
  ;; label without truncation, so we add a fudge factor to the size of the
  ;; label text.
  (define fudge 20)

  ;; Determine the ideal width of each column.  This is computed so that the
  ;; header text is completely visible, but we don't consider the cell
  ;; contents (which might not be present at this time anyway).
  (define column-widths
    (for/vector #:length header-count ((label (in-list headers)))
      (let-values (([w h x y] (send dc get-text-extent label font #t)))
        (+ w fudge))))

  (define total-width (for/sum ((w (in-vector column-widths))) w))
  (define client-width (let-values (((w h) (send lb get-client-size))) w))
  ;; NOTE: client-width might be 0 if the widget was not shown yet, if that is
  ;; the case, we cannot further adjust the column widths.
  (when (> client-width 0)
    (cond ((> total-width (- client-width fudge))
           ;; shrink large column widths so the total width fits in the client
           ;; area.  We won't shrink columns to less than 3 * fudge, so avoid
           ;; columns that are too small.
           (let ((fair-width (max (* 3 fudge) (/ client-width header-count))))
             (for ([index (in-range (vector-length column-widths))]
                   #:when (> (vector-ref column-widths index) fair-width))
               (vector-set! column-widths index fair-width))))
          ((< total-width (- client-width fudge))
           ;; Increase column widths to fill up client width
           (let ((adjust (/ (- client-width total-width fudge) header-count)))
             (for ([index (in-range (vector-length column-widths))])
               (let ((val (vector-ref column-widths index)))
                 (vector-set! column-widths index (+ val adjust))))))))

  (for (((width index) (in-indexed (in-vector column-widths))))
    (send lb set-column-width index (exact-truncate width) 0 10000)))

;; Make sure the list-box% LB has exactly COUNT rows.  New rows are added or
;; removed as needed.  The contents of any added rows will be the empty
;; string.
(define (lb-ensure-row-count lb count)
  (let ((actual (send lb get-number)))
    (cond ((> actual count)
           ;; if we need to delete more items than count, it is faster to just
           ;; clear the list box and add count items.
           (let ((ndeletes (- actual count)))
             (if (> ndeletes count)
                 (begin
                   (send lb clear)
                   (lb-ensure-row-count lb count))
                 (for ((i (in-range ndeletes)))
                   (send lb delete 0)))))
          ((< actual count)
           (for ((i (in-range (- count actual))))
             (send lb append ""))))))

;; Make sure the list-box% LB has exactly COUNT columns.  New columns are
;; added or removed as needed.
(define (lb-ensure-column-count lb count)
  (let ((actual (length (send lb get-column-order))))
    (cond ((> actual count)
           (for ((i (in-range (- actual count))))
             (send lb delete-column 0)))
          ((< actual count)
           (for ((i (in-range (- count actual))))
             (send lb append-column ""))))))

;; Clear (delete all columns) from the list-box% LB.
(define (lb-clear lb)
  (send lb clear)
  (let ((column-count (length (send lb get-column-order))))
    (for ((c (in-range (- column-count 1))))
      (send lb delete-column 1))))

;; Setup the column names in list-box% LB from HEADERS, a list of strings.
(define (lb-install-headers lb headers)
  (lb-ensure-column-count lb (length headers))
  (for ((i (in-range (length headers)))
        (h (in-list headers)))
    (send lb set-column-label i h)))

;; Fill the ROW-NUM row in the list-box% LB from DATA.  FORMATTER is a list of
;; formatting functions, one for each column in LB.
(define (lb-fill-row lb row-num formatter data)
  (send lb set-data row-num data)
  (for ((column (in-range (length formatter)))
        (fmt (in-list formatter)))
    (let ((v (fmt data)))
      (send lb set-string row-num v column))))



;;................................................. visible-column-edit% ....

;; A dialog box used to select which columns are visible in a qresults-list%
;; widget.  Used by qresults-list% when `interactive-setup-visible-columns`
;; method is called.
(define visible-column-edit%
  (class edit-dialog-base%
    (init)
    (super-new [icon (edit-icon)] [title "Edit Visible Columns"])

    (define panel
      (new vertical-panel%
           [parent (send this get-client-pane)]
           [spacing 5]
           [style '(vscroll)]
           [alignment '(left top)]))

    (define/public (begin-edit parent field-list visible-fields)
      ;; Create checkboxes, one for each field in FIELD-LIST, they are
      ;; selected or not based on VISIBLE-FIELDS.
      (define checkboxes
        (for/list ([label (in-list field-list)])
          (new check-box%
               [parent panel] [label label]
               [value (member label visible-fields)]
               [style '(deleted)]))) 
      (send panel change-children (lambda (old) checkboxes))
      ;; Run the dialog...
      (and (send this do-edit parent)
           ;; ... and collect visible fields if "Save" was clicked.
           (for/list ([control (in-list (send panel get-children))]
                      #:when (and (is-a? control check-box%)
                                  (send control get-value)))
             (send control get-label))))
    ))


;;....................................................... qresults-list% ....

;; Holds information about a column of data in a qresults-list% 
(struct qcol
  (name                                 ; column name
   ;; function taking a row and returning a string representing the value to
   ;; be displayed for that row in this column.
   formatter
   ;; function taking a row and returning a value that can be used to sort
   ;; this column, this can be either a string (in which case the column is
   ;; sorted by string< and string>, or a number in that case, the column is
   ;; sorted using < and >
   sort-key
   ;; Flag indicating if this column should be visible by default, when no
   ;; visible columns have been selected yet.  `qcolumn` always creates
   ;; visible columns by default, but it can be useful to define a set of
   ;; visible columns for setups which contain a lot of columns, like the
   ;; activity-list.
   default-visible?))

(define (qcolumn name formatter sort-key #:default-visible? (default-visible? #t))
  (qcol name formatter sort-key default-visible?))

;; Return a symbol that can be used to store visual preferences for a list-box
;; based on COLUMN-DEFINITIONS (a list of QCOLUMN objects).  The key is
;; composed of a TAG folowed by the md5 sum of the concatenated column titles.
(define (make-pref-key tag column-definitions)
  (let* ((title-str (apply string-append (map qcol-name column-definitions)))
         (title-md5 (bytes->string/latin-1 (md5 title-str #t))))
    (string->symbol (string-append (symbol->string tag) "--" title-md5))))


;; A sophisticated list-box%: can resize, reorder columns, sort them and has
;; its own dialog box for editing which columns should be shown and which
;; should be hidden.  Can also export its contents as CSV as well as
;; add/update/delete individual rows.
;;
;; Use the `setup-column-defs' to setup the columns and `set-data` to add data
;; to the list-box.  See the other public methods for more details.
(define qresults-list%
  (class object%
    ;; pref-tag is used as the base symbol for storing visual preferences (the
    ;; visible columns and their size and order)
    (init-field pref-tag)
    (init parent
          [label ""]
          ;; A popup-menu% to be popped up when the user right-clicks on a
          ;; row.
          [right-click-menu #f])

    (super-new)

    ;; key under which we store visual preferences, it is actually updated by
    ;; setup-column-defs
    (define pref-key pref-tag)

    (define column-defs '())            ; (listof qcolumn)
    (define visible-columns '())        ; (listof strings), the column names
    (define sort-column #f)             ; index of the column on which we sort
    (define sort-descending? #f)

    ;; Data that is displayed in the control.  This is a list of any values
    ;; (called rows), as the column formatter functions receive a row and must
    ;; produce a value for that column only.
    (define the-data '())

    (define default-export-file-name #f)
    (define setup-fields-dlg #f)

    (define (get-column-formatters)
      (for/list ([cdef (in-list column-defs)]
                 #:when (member (qcol-name cdef) visible-columns))
        (qcol-formatter cdef)))

    (define (refresh-contents)
      (with-busy-cursor
       (lambda ()
         (let ((formatters (get-column-formatters))
               (num-rows (length the-data)))
           (lb-ensure-row-count the-list-box num-rows)
           (for ((row-num (in-range (length the-data)))
                 (data (in-list the-data)))
             (lb-fill-row the-list-box row-num formatters data))))))

    (define (refresh-contents-1 row-num data)
      (let ((formatters (get-column-formatters)))
        (lb-fill-row the-list-box row-num formatters data)))

    (define (sort-by-column n)

      ;; Save the first visible position and the selection, after sorting we
      ;; will select the same item again and position it in the same visual
      ;; place on the list control.
      (define first-visible (send the-list-box get-first-visible-item))
      (define selection (send the-list-box get-selection))
      (define selected-item
        (and selection
             (< selection (length the-data))
             (list-ref the-data selection)))

      (if (eqv? sort-column n)
          ;; If we are sorting the same column, toggle the sort order.
          (set! sort-descending? (not sort-descending?))
          (begin
            ;; Restore previous column name, if any
            (when sort-column
              (send the-list-box set-column-label
                    sort-column
                    (list-ref visible-columns sort-column)))
            (set! sort-column n)
            (set! sort-descending? #f)))

      ;; Mark the sorted column
      (let ((col-name (string-append
                       (if sort-descending? "↓ " "↑ ")
                       (list-ref visible-columns n))))
        (send the-list-box set-column-label n col-name))

      ;; Sort the data
      (let* ((cname (list-ref visible-columns n))
             (key (qcol-sort-key
                   (findf (lambda (ci) (equal? (qcol-name ci) cname))
                          column-defs)))
             (cmp (cond ((= (length the-data) 0) <) ; doesn't matter
                        ((string? (key (car the-data)))
                         (if sort-descending? string>? string<?))
                        (#t
                         (if sort-descending? > <)))))
        (set! the-data
              (sort the-data
                    ;; Make sure our comparison function can handle #f, which
                    ;; we use to mark non-existent data.
                    (lambda (a b)
                      (cond ((not b) #t)
                            ((not a) #f)
                            (#t (cmp a b))))
                    #:key key)))

      ;; Update the list-box in place (no rows are added/deleted)
      (refresh-contents)

      ;; Select the original item (it has changed position during the sort)
      ;; and scroll the list box so it appears on screen in the same place.
      (when selected-item
        ;; New position of the selected item
        (define pos (for/first ([(item index)
                                 (in-indexed (in-list the-data))]
                                #:when (eq? selected-item item))
                      index))
        (send the-list-box set-selection pos)
        (when first-visible
          (define nfv (max 0 (- pos (- selection first-visible))))
          (send the-list-box set-first-visible-item nfv))))

    (define (lb-callback lb event)
      (let ((event-type (send event get-event-type)))
        (cond ((eq? event-type 'list-box-column)
               (sort-by-column (send event get-column)))
              ((eq? event-type 'list-box)
               (let ((sel (send lb get-selection)))
                 (when sel
                   (on-select sel (send lb get-data sel)))))
              ((eq? event-type 'list-box-dclick)
               (let ((sel (send lb get-selection)))
                 (when sel
                   (on-double-click sel (send lb get-data sel))))))))

    (define the-pane (new vertical-pane% [parent parent] [alignment '(left center)]))

    (define the-list-box
      (new
       (class list-box% (init [qobj #f]) (super-new)
         (inherit popup-menu)

         ;; HACK to allow the toplevel menu to access this qresults-list%
         ;; object.  It is not clear what a clean solution would be...
         (define the-qobj qobj)
         (define/public (get-qresults-object) the-qobj)

         (define/public (interactive-export-data formatted?)
           (on-interactive-export-data formatted?))

         (define/override (on-subwindow-event receiver event)
           (let ((event-type (send event get-event-type)))
             (cond ((eq? event-type 'right-down) #t)
                   ((eq? event-type 'right-up)
                    (when right-click-menu
                      (popup-menu right-click-menu (send event get-x) (send event get-y)))
                    #t)
                   (#t (super on-subwindow-event receiver event))))))
       [qobj this]
       [label label]
       [parent the-pane]
       [choices '()]
       [callback lb-callback]
       [style '(multiple
                single
                variable-columns
                clickable-headers
                column-headers
                reorderable-headers)]
       [columns '("")]))

    ;; Set a default file name to be used by `on-interactive-export-data` --
    ;; the user will still be prompted for a file name, but this one will show
    ;; as a default.
    (define/public (set-default-export-file-name name)
      (set! default-export-file-name name))

    ;; Export contents to a CSV file, asking the user for a filename first.
    (define/public (on-interactive-export-data formatted?)
      (let ((file (put-file "Select file to export to"
                            #f
                            #f
                            default-export-file-name
                            "csv" '()
                            '(("CSV Files" "*.csv") ("Any" "*.*")))))
        (when file
          (set! default-export-file-name file)
          (call-with-output-file file
            (lambda (out) (export-data-as-csv out formatted?))
            #:mode 'text #:exists 'truncate))))

    ;; Pop-up a dialog box to select the columns that are visible to the user
    (define/public (interactive-setup-visible-columns)
      (unless setup-fields-dlg
        (set! setup-fields-dlg (new visible-column-edit%)))
      (let ((visible-fields
             (send setup-fields-dlg begin-edit
                   (send the-pane get-top-level-window)
                   (map qcol-name column-defs)
                   visible-columns)))
        (when visible-fields
          ;; NOTE: visible-columns needs to be in the right order, so we scan
          ;; the column definitions.
          (set! visible-columns
                (for/list ([cdef (in-list column-defs)]
                           #:when (member (qcol-name cdef) visible-fields))
                  (qcol-name cdef)))
          (lb-install-headers the-list-box visible-columns)
          (lb-set-default-visual-layout the-list-box)
          (refresh-contents))))

    ;; Set a new tag to be used for storing preferences
    (define/public (set-tag new-tag)
      (save-visual-layout)              ; for the existing key, if any
      (set! pref-key #f)
      (set! pref-tag new-tag))

    ;; Save the visual layout of this listbox with the current visible columns
    ;; and their size and position.
    (define/public (save-visual-layout)
      ;; NOTE: we only save the preferences for the current pref key.  Saving
      ;; for previous ones (when the columns are changes) is done as part of
      ;; `setup-column-defs'.
      ;; (printf "put-pref ~a -- ~a~%" pref-key (get-visual-layout the-list-box))
      (when pref-key
        (put-pref pref-key
                     (cons visible-columns
                           (lb-get-visual-layout the-list-box)))))

    ;; Setup the columns of the list box to FD, which is a list of QCOLUMN
    ;; struct instances.  The previous visual layout will be saved and the
    ;; visual layout corresponding to this set of columns will be restored (if
    ;; there is one).
    (define/public (setup-column-defs fd)
      (save-visual-layout)              ; for the previous field definitions
      (set! column-defs fd)
      (set! pref-key (make-pref-key pref-tag column-defs))
      (set! sort-column #f)
      (let ((visual-layout (get-pref pref-key (lambda () #f))))
        (let ((visible-fields (if visual-layout
                                  (car visual-layout)
                                  (for/list ([c (in-list column-defs)] #:when (qcol-default-visible? c))
                                    (qcol-name c))))
              (lb-visual-layout (if visual-layout
                                    (cdr visual-layout)
                                    #f)))
          (set! visible-columns visible-fields)
          (lb-install-headers the-list-box visible-fields)
          (if lb-visual-layout
              (lb-set-visual-layout the-list-box lb-visual-layout)
              (lb-set-default-visual-layout the-list-box)))))

    ;; Set the data of this list box to ROWS, which is a list of any object:
    ;; the column formatters receive the object and must produce a value for
    ;; that column.  The qresults-list% object does not care what the actual
    ;; object is.
    (define/public (set-data rows)
      ;; De-select any previously selected item in the list box.
      (let ([sel (send the-list-box get-selection)])
        (when sel
          (on-deselect sel (send the-list-box get-data sel))
          (send the-list-box select sel #f)))
      (set! the-data rows)
      (if sort-column
          (begin
            ;; Hack to keep the sort order
            (set! sort-descending? (not sort-descending?))
            ;; NOTE: sort-by-column will call refresh-contents
            (sort-by-column sort-column))
          (refresh-contents)))

    ;; Export the contents of this object as CSV to the output port OUTP.  If
    ;; FORMATTED-VALUES? is #t, the column formatters are used to format the
    ;; data, otherwise, the sort key is used.
    (define/public (export-data-as-csv outp formatted-values?)
      (for/fold ([first-column? #t])
                ([c (in-list column-defs)])
        (unless first-column?
          (write-char #\, outp))
        (write-char #\" outp)
        (write-string (qcol-name c) outp)
        (write-char #\" outp)
        #f)
      (newline outp)
      (let ((key-list (map (if formatted-values? qcol-formatter qcol-sort-key) column-defs)))
        (for ([row (in-list the-data)])
          (for/fold ([first-column? #t])
                    ([key (in-list key-list)])
            (unless first-column?
              (write-char #\, outp))
            (define v (key row))
            (define o (cond ((equal? v #f) "")
                            ((number? v) (~a v))
                            (#t (format "\"~a\"" v))))
            (write-string o outp)
            #f)
          (newline outp))))

    ;; Return the index of the first selected row, or #f is no row is
    ;; selected.
    (define/public (get-selected-row-index)
      (let ((selected-items (send the-list-box get-selections)))
        (if (null? selected-items) #f (car selected-items))))

    ;; Return the data corresponding to ROW-INDEX.  This is equivalent to
    ;; (list-ref the-data (get-selected-row-index)), but possibly more
    ;; efficient.
    (define/public (get-data-for-row row-index)
      (send the-list-box get-data row-index))

    ;; Set the contents of ROW-INDEX to NEW-DATA.  If SELECT? is #t, the row
    ;; will also be selected and scrolled such that it is visible.
    (define/public (update-row row-index new-data (select? #t))
      (set! the-data
            (append
             (take the-data row-index)
             (list new-data)
             (drop the-data (+ row-index 1))))
      (refresh-contents-1 row-index new-data)
      (when select?
        (send the-list-box set-selection row-index)
        (send the-list-box set-first-visible-item row-index)))

    ;; Select the row at ROW-INDEX and make sure it is visible
    (define/public (select-row row-index)
      (send the-list-box set-selection row-index)
      (send the-list-box set-first-visible-item row-index))

    ;; Add a new row to the end of the list.
    (define/public (add-row data)
      (send the-list-box append "")
      (set! the-data (append the-data (list data)))
      (let ((row-index (- (send the-list-box get-number) 1)))
        (refresh-contents-1 row-index data)
        (send the-list-box set-selection row-index)
        (send the-list-box set-first-visible-item row-index)))

    ;; Delete the row at ROW-INDEX.
    (define/public (delete-row row-index)
      (set! the-data
            (append
             (take the-data row-index)
             (drop the-data (+ row-index 1))))
      (send the-list-box delete row-index))

    ;; Return the number of rows in the list box.
    (define/public (get-row-count)
      (send the-list-box get-number))

    ;; Clear the contents of the listbox.
    (define/public (clear)
      (send the-list-box clear)
      (set! the-data '()))

    ;; Can be overriden if the user is interested in a double click on an
    ;; intem
    (define/public (on-double-click row-index row-data)
      #f)

    ;; Can be overriden if the user wants to be notified when an item is
    ;; selected
    (define/public (on-select row-index row-data)
      #f)

    ;; Can be overriden if the user wants to be notified when an item is
    ;; deselected.
    (define/public (on-deselect row-index row-data)
      #f)

    ))

