#lang racket/base
;; view-calendar.rkt -- calendar panel
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015 Alex Harsanyi (AlexHarsanyi@gmail.com)
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

(require db/base
         pict
         racket/class
         racket/date
         racket/gui/base
         racket/list
         racket/math
         racket/string
         racket/match
         "dialogs/activity-edit.rkt"
         "fmt-util.rkt"
         "sport-charms.rkt"
         "widgets/main.rkt"
         "utilities.rkt")

(provide view-calendar%)


;;.................................................. database operations ....

;; Return a list of sessions (their name and basic info) between START and END
;; (which are timestamps).  The rows returned will be fed directly into an
;; `calendar-item-snip%' object.
(define (get-sessions-between-dates start end db)
  (query-rows
   db
   "select s.id,
           a.guid,
           s.start_time,
           s.sport_id,
           s.sub_sport_id,
           ifnull(s.name, 'Untitled') as name,
           ifnull(ss.total_distance, 0) as distance,
           ifnull(ss.total_timer_time, 0) as time
      from A_SESSION S,
           ACTIVITY A,
           SECTION_SUMMARY SS
     where S.summary_id = SS.id
       and S.activity_id = A.id
       and s.start_time between ? and ?"
   start end))

;; Return basic information about SESSION-ID.  Column order must match what
;; `get-sessions-between-dates' returns.
(define (get-session-by-id session-id db)
  (query-row
   db
   "select s.id,
           a.guid,
           s.start_time,
           s.sport_id,
           s.sub_sport_id,
           ifnull(s.name, 'Untitled') as name,
           ifnull(ss.total_distance, 0) as distance,
           ifnull(ss.total_timer_time, 0) as time
      from A_SESSION S,
           ACTIVITY A,
           SECTION_SUMMARY SS
     where S.summary_id = SS.id
       and S.activity_id = A.id
       and S.id = ?" session-id))

;; Return the meaningfull date range for the calendar.  This is the date of
;; the earliest and latest session recorded.  We use it to limit the range of
;; calendar movements only to these ranges.
(define (get-calendar-date-range db)
  (let ((row (query-row
              db "select min(S.start_time), max(S.start_time) from A_SESSION S")))
    ;; NOTE: row will always exist, as we are selecting min() and max(),
    ;; however they might be NULL if there are no sessions in the database
    (match-define (vector start end) row)
    (if (or (sql-null? start) (sql-null? end))
        (let ((now (current-seconds)))
          ;; Return the current year only...
          (values now now))
        (values start end))))


;;....................................................... date utilities ....

(define *day-as-seconds* (* 24 60 60))  ; number of seconds in a day

;; Return the timestamp of the start of the week (Monday, 0:00am) for the week
;; which contains the SECONDS timestamp.
(define (week-start seconds)
  (let ((d (seconds->date seconds #t)))
    (if (eqv? (date-week-day d) 0)      ; sunday
        (- seconds (* 6 *day-as-seconds*))
        (- seconds (* (- (date-week-day d) 1) *day-as-seconds*)))))

;; Return the timestamp of the end of the week (Sunday, 12:00pm) for the week
;; which contains the SECONDS timestamp.
(define (week-end seconds)
  (let ((d (seconds->date seconds #t)))
    (if (eqv? (date-week-day d) 0)      ; sunday
        seconds
        (+ seconds (* (- 7 (date-week-day d)) *day-as-seconds*)))))

;; Return the timestamps for the start and end of the month identified by
;; MONTH and YEAR.  The start timestamp is the start of the week which
;; contains the first day of the month, the end timestamp is the end of the
;; week which contains the last day of the month.
(define (calendar-month-range month year)
  (let ((month-start (find-seconds 0 0 0 1 month year #t))
        (month-end (- (find-seconds 0 0 0
                                    1
                                    (if (= month 12) 1 (+ month 1))
                                    (if (= month 12) (+ 1 year) year)
                                    #t)
                      1)))
    (let ((w-month-start (week-start month-start))
          (w-month-end (week-end month-end)))
      (values w-month-start w-month-end))))


;;...................................................... paint resources ....

;; Shoudl these be somewhere else?

(define *color-16* (make-object color% 238 238 238))
(define *color-18* (make-object color% 204 204 204))

(define the-header-fg-color (make-object color% 255 255 255))
(define the-header-bg-color (make-object color% 0 74 127))

(define the-grid-pen
  (send the-pen-list find-or-create-pen *color-18* 0.5 'solid))
(define the-highlight-pen
  (send the-pen-list find-or-create-pen "black" 0.5 'solid))
(define the-transparent-pen
  (send the-pen-list find-or-create-pen "black" 0 'transparent))
(define the-shaded-brush
  (send the-brush-list find-or-create-brush *color-16* 'solid))
(define the-transparent-brush
  (send the-brush-list find-or-create-brush "black" 'transparent))
(define the-small-font
  (send the-font-list find-or-create-font 7 'default 'italic 'normal))
(define the-normal-font
  (send the-font-list find-or-create-font 10 'default 'normal 'normal))
(define the-header-font
  (send the-font-list find-or-create-font 12 'default 'normal 'normal))
(define the-charm-font
  (send the-font-list find-or-create-font 12 'default 'normal 'bold))


;;........................................................ calendar item ....

;; (send a-dc get-text-extent string font combine? offset)

;; Break TEXT into lines at word boundary so that each line has used ar most
;; WIDTH space when rendereded using FONT
(define (break-into-lines text dc font width)
  (let ((result '())
        (current #f))
    (for-each (lambda (item)
                (if (not current)
                    (set! current item)
                    (let ((candidate (string-append current " " item)))
                      (let-values (((w h x y) (send dc get-text-extent candidate font #t)))
                        (if (> w width)
                            (begin
                              (set! result (cons current result))
                              (set! current item))
                            (set! current candidate))))))
              (string-split text))
    (reverse (cons current result))))

;; Create the calendar text for an activity based on its name, duration
;; distance and sport.  We try to create the most compact message possible,
;; even if we loose some detail (e.g for activities shorter than an hour, we
;; just round to an exact number of minutes).
(define (make-headline name duration distance sport)
  (if (and (= duration 0) (= distance 0))
      name
      (format "~a - ~a ~a"
              name
              (if ; (and (= (remainder (exact-truncate duration) 60) 0)
               (< duration 3600)
               ;; Display an approximate time if the activity is under an hour
               (format "~a min" (exact-round (/ duration 60)))
               (duration->string duration))
              (if (< distance 100)        ; don't display distances less than 100m
                  ""
                  (if (eqv? sport 5)
                      (short-distance->string distance #t)
                      (distance->string distance #t))))))

;; Create a PICT that draws a calendar item.  This function determines how
;; calendar items are drawn.
(define (make-badge charm headline color width
                    #:internal-border [iborder 3]
                    #:charm-spacing [cspacing 5]
                    #:charm-font [cfont #f]
                    #:headline-font [hfont #f]
                    #:dc [dc #f])

  (define charm-font
    (or cfont
        (send the-font-list find-or-create-font 12 'default 'normal 'bold)))

  (define headline-font
    (or hfont
       (send the-font-list find-or-create-font 10 'default 'normal 'normal)))

  (define draw-context
    (or dc
        (new bitmap-dc% [bitmap (make-object bitmap% 256 256)])))

  (let* ((charm-pict (text charm charm-font))
         (text-width (- width iborder (pict-width charm-pict) cspacing))
         (lines (break-into-lines headline draw-context headline-font text-width))
         (text-pict (apply vl-append (for/list ([l (in-list lines)]) (text l headline-font))))
         (text-pict-width (pict-width text-pict)))

    (set! text-pict (inset/clip text-pict 0 0 (- text-width text-pict-width) 0))
    (let* ((t (ht-append cspacing charm-pict text-pict))
           (w (+ (pict-width t) iborder))
           (h (+ (pict-height t) iborder)))
      (let ((r1 (filled-rounded-rectangle w h -0.1))
            (r2 (linewidth 1.5 (rounded-rectangle w h -0.1))))
        (cc-superimpose (colorize r1 color)
                        t
                        (colorize r2 (scale-color 0.9 color)))
        ))))


;;.................................................. calendar-item-snip% ....

(define calendar-item-snip-class
  (let ((snip-class (make-object
                     (class snip-class% (super-new)
                       (send this set-classname "calendar-item-snip")))))
    (send (get-the-snip-class-list) add snip-class)
    snip-class))

(define calendar-item-snip%
  (class snip%
    (init db-row) (super-new)
    (inherit get-admin)

    ;; snip% specific setup
    (send this set-snipclass calendar-item-snip-class)
    (send this set-count 1)
    (send this set-flags (cons 'handles-all-mouse-events (send this get-flags)))

    (define the-session-id #f)
    (define the-activity-guid #f)
    (define the-start-time (current-seconds))
    (define the-sport #f)
    (define the-sub-sport #f)
    (define the-name "")
    (define the-total-distance 0)
    (define the-total-time 0)

    (define selected? #f)

    ;; Our snip will be this width (can be updated via set-target-dimensions)
    (define target-width 200)
    ;; Our snip will be at most this height.  It might be less if it fits, or
    ;; we will clip it to this value. (can be updated via
    ;; set-target-dimensions)
    (define target-height 100)
    ;; When #t, this ship could not fit into target-height and had to be
    ;; chopped.
    (define extra-height-required? #f)

    (define the-pict #f)

    ;; function to draw the picture, created using `make-pict-drawer`
    (define draw-fn #f)

    (define (setup-from-db-row db-row)
      (set! the-session-id (vector-ref db-row 0))
      (set! the-activity-guid (vector-ref db-row 1))
      (set! the-start-time (vector-ref db-row 2))
      (set! the-sport (vector-ref db-row 3))
      (when (sql-null? the-sport) (set! the-sport #f))
      (set! the-sub-sport (vector-ref db-row 4))
      (when (sql-null? the-sub-sport) (set! the-sub-sport #f))
      (set! the-name (vector-ref db-row 5))
      (set! the-total-distance (vector-ref db-row 6))
      (set! the-total-time (vector-ref db-row 7)))

    (setup-from-db-row db-row)

    (define (make-pict dc)
      (let* ((badge (make-badge (get-sport-letter the-sport the-sub-sport)
                                (make-headline the-name the-total-time the-total-distance the-sport)
                                (get-sport-color the-sport the-sub-sport)
                                target-width
                                #:charm-font the-charm-font
                                #:headline-font the-normal-font
                                #:dc dc))
             (actual-height (pict-height badge)))
        (set! extra-height-required? (> actual-height target-height))
        (if (and extra-height-required? (not selected?))
            (inset/clip badge 0 0 0 (- target-height actual-height))
            badge)))

    (define (rebuild-pict)
      (let ((dc (send (get-admin) get-dc)))
        (set! the-pict (make-pict dc))
        (set! draw-fn (make-pict-drawer the-pict))))

    (define/override (get-extent dc x y w h descent space lspace rspace)
      (when w (set-box! w (if the-pict (pict-width the-pict) target-width)))
      (when h (set-box! h (if the-pict (+ (pict-height the-pict)) target-height)))
      (when descent (set-box! descent (pict-descent the-pict)))
      (when space (set-box! space 10.0))
      (when lspace (set-box! lspace 10.0))
      (when rspace (set-box! rspace 10.0)))

    (define/override (draw dc x y . other)
      (unless draw-fn
        (rebuild-pict))
      (draw-fn dc x y))

    (define/override (on-event dc x y editorx editory event)
      (let ((event-type (send event get-event-type)))
        (cond ((eq? event-type 'right-down) #t)
              ((eq? event-type 'right-up)
               (let* ((editor (send (get-admin) get-editor))
                      (menu (send editor get-calendar-operations-menu this))
                      (menu-x (- (send event get-x) x))
                      (menu-y (- (send event get-y) y)))
                 (send (get-admin) popup-menu menu this menu-x menu-y))))))

    (define/public (select flag)
      (set! selected? flag)
      (rebuild-pict)
      (send (get-admin) resized this #t))

    (define/public (set-target-dimensions w h)
      (set! target-width w)
      (set! target-height h)
      (rebuild-pict)                    ; to get its real size
      (send (get-admin) resized this #t))

    (define/public (get-actual-height)
      (unless draw-fn
        (rebuild-pict))
      (pict-height the-pict))

    (define/public (need-extra-height?)
      extra-height-required?)

    (define/public (refresh db-row)
      ;; NOTE: the start-time of the calendar item might have changed, so we
      ;; need to delete ourselves and re-insert into the calendar.  The
      ;; calendar will refuse an insert if we are now outside of the calendar
      ;; range
      (let ((editor (send (get-admin) get-editor)))
        (send editor please-delete this)
        (setup-from-db-row db-row)
        (set! the-pict #f)
        (set! draw-fn #f)
        (send editor insert this)))

    (define/public (get-start-time) the-start-time)
    (define/public (get-session-id) the-session-id)
    (define/public (get-session-sport) (cons the-sport the-sub-sport))
    (define/public (get-activity-guid) the-activity-guid)
    (define/public (get-distance) the-total-distance)
    (define/public (get-duration) the-total-time)
    (define/public (get-sport) (cons the-sport the-sub-sport))

    ))


;;................................................. calendar paste board ....

(define (make-header-pict day w h)
  (let ((t (text day the-header-font))
        (r (filled-rectangle w h #:draw-border? #f)))
    (cc-superimpose (colorize r the-header-bg-color)
                    (colorize t the-header-fg-color))))

(define (make-totals-pict distance time w h)
  (let ((d-label (text "Total distance:" the-normal-font))
        (d-data (text (distance->string distance #t) the-header-font))
        (t-label (text "Total time:" the-normal-font))
        (t-data (text (duration->string time) the-header-font)))
    (cc-superimpose (colorize (filled-rectangle w h #:draw-border? #f) *color-16*)
                    (vl-append 3 d-label d-data t-label t-data))))

(define (make-pie-chart-pict w h chart-data)

  (define (draw-slice dc x y w h start end brush)
    (send dc set-brush brush)
    (send dc draw-arc x y w h start end))

  (define (draw-pie-chart dc x y w h data)
    (let ((start 0.2))
      (for ((element (in-list data)))
        (let ((size (* pi 2 (second element)))
              (brush (send the-brush-list find-or-create-brush (first element) 'solid)))
          (draw-slice dc x y w h start (+ start size) brush)
          (set! start (+ start size))))))

  (let* ((width (exact-truncate w))
         (height (exact-truncate h))
         (size (min width height))
         (inset 0))
    (let ((bmp (make-bitmap size size)))
      (let ((dc (new bitmap-dc% [bitmap bmp])))
        (send dc set-smoothing 'smoothed)
        (send dc set-pen
              (send the-pen-list find-or-create-pen "black" 2 'transparent))
        (draw-pie-chart dc inset inset (- size inset) (- size inset) chart-data))
      (bitmap bmp))))

(define *calendar-headers*
  '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday" "Totals"))

;; Amount of space (in pixels) around the text drawn on the header
(define *header-cell-inset* 3)

;; The width of the totals column as a percentage of the base cell width
(define *totals-width-pct* 0.7)

;; Cell inset (offset from the cell up-left corner, in pixels) where contents
;; should be drawn.
(define *cell-inset* 3)

;; Amount of vertical space (in pixels) between calendar items
(define *snip-spacing* 3)

(define calendar-pasteboard%
  (class pasteboard%
    (init-field double-click-callback activity-operations)
    (super-new)
    (inherit get-canvas move-to find-first-snip insert delete set-before
             begin-edit-sequence end-edit-sequence)

    (define (with-edit-sequence thunk)
      (begin-edit-sequence)
      (thunk)
      (end-edit-sequence))

    (define aop-menu 
      (new activity-operations-menu% [target activity-operations]))

    (define/public (get-calendar-operations-menu citem)
      (send activity-operations set-calendar-item citem)
      (send aop-menu get-popup-menu))

    ;; Date range displayed on the calendar (cons start-timestamp
    ;; end-timestamp), initialized to a default value.
    (define calendar-range
      (let-values (([start end] (calendar-month-range 1 2015)))
        (cons start end)))

    ;; Number of weeks displayed by the calendar. This would be 4 to 6 weeks
    ;; depending on the month and year.
    (define calendar-num-weeks 6)

    ;; The width/height of the cell displaying a day.  The dimensions of other
    ;; cells are defined as a proportion of this one.
    (define base-cell-width 0)
    (define base-cell-height 0)

    ;; Height of the header cell, which displays the day of the week
    (define header-cell-height 0)

    ;; Height of the header inside a day cell, which displays the date
    (define cell-header-height 0)

    ;; Vector of functions that draws the headers.  Each draw function is a
    ;; (lambda (dc x y) ...)
    (define header-draw-functions (make-vector 8 (lambda (dc x y) #f)))

    ;; Vector of functions that draw the totals column.  Each draw function is
    ;; a (lambda (dc x y) ...)
    (define totals-draw-functions (make-vector calendar-num-weeks (lambda (dc x y) #f)))

    ;; Snips are grouped in cells, one per calendar day.
    (define snip-cells (make-hash))

    ;; When true, cell layout by `arrange-snips-in-cell' is not performed.
    ;; Used to avoid positioning snips repeatedly when items are added in bulk.
    (define suspend-cell-layout? #f)

    (define removing-snips? #f)

    ;; Return the width of the cell in column COL.  If INSET? is #t, return
    ;; the width of the "inside" of the cell.
    (define (cell-width col inset?)
      (let ((w (if (< col 7)
                   base-cell-width
                   (* base-cell-width *totals-width-pct*))))
        (let ((w (if inset? (- w *cell-inset* *cell-inset*) w)))
          (max w 0))))

    ;; Return the height of the cell in row ROW.  If INSET? is #t, return the
    ;; width of the "inside" of the cell.
    (define (cell-height row inset?)
      (let ((h (if (eqv? row 0) header-cell-height base-cell-height)))
        (let ((h (if inset? (- h *cell-inset* *cell-inset*) h)))
          (max h 0))))

    ;; Return the (x y) coordinates for the top-left corner of the cell at
    ;; ROW, COL.  If INSET? is #t, return the origin for the inside of the
    ;; cell.
    (define (cell-origin row col inset?)
      (let ((c (get-canvas)))
        (if c
            (values
             (+ (send c horizontal-inset)
                (* col base-cell-width)
                (if inset? *cell-inset* 0))

             (+ (send c vertical-inset)
                (if (eqv? row 0) 0 (+ header-cell-height (* (- row 1) base-cell-height)))
                (if inset? *cell-inset* 0)))
            (values 0 0))))

    ;; convert SECONDS into a day index (0 being the first day displayed in
    ;; the calendar).  Return #f seconds is outside the calendar range, or
    ;; there is no calendar range defined.
    (define (seconds->day-index seconds)
      (if (and (>= seconds (car calendar-range))
               (< seconds (cdr calendar-range)))
          (quotient (- seconds (car calendar-range)) (* 24 60 60))
          #f))

    ;; Convert a day index (0 being the first day displayed in the calendar)
    ;; to a row column.
    (define (day-index->row-col day-index)
      (values
       (+ 1 (quotient day-index 7))
       (remainder day-index 7)))

    ;; Draw the header row of the calendar.
    (define (draw-headers dc)
      (for ((draw-fn (in-vector header-draw-functions))
            (col (in-range (vector-length header-draw-functions))))
        (let-values (([x y] (cell-origin 0 col #f)))
          (draw-fn dc x y))))

    ;; Draw the totals column of the calendar
    (define (draw-totals dc)
      (for ((draw-fn (in-vector totals-draw-functions))
            (row (in-range (vector-length totals-draw-functions))))
        (let-values (([x y] (cell-origin (+ row 1) 7 #t)))
          (draw-fn dc x y))))

    ;; Draw the grid lines for the calendar
    (define (draw-grid dc)
      (let-values (([w h] (send dc get-size)))
        (send dc set-pen the-grid-pen)
        ;; Vertical lines
        (let loop ((x 0) (step 0))
          (send dc draw-line x 0 x h)
          (when (< step 7)
            (loop (+ x base-cell-width) (+ step 1))))
        ;; Horizontal lines
        (let loop ((y header-cell-height) (step 0))
          (send dc draw-line 0 y w y)
          (when (< step (- calendar-num-weeks 1))
            (loop (+ y base-cell-height) (+ step 1))))))

    ;; Draw a selection for the cell at ROW, COL.  If FILL? is #t, the cell is
    ;; shaded using the-shaded-brush, otherwise the cell is "highlighted"
    ;; using a rectangle drawn with the-highlight-pen.
    (define (draw-cell-selection dc row col fill?)
      (let ((prev-brush (send dc get-brush)))
        (if fill?
            (begin
              (send dc set-brush the-shaded-brush)
              (send dc set-pen the-transparent-pen))
            (begin
              (send dc set-brush the-transparent-brush)
              (send dc set-pen the-highlight-pen)))
        (let-values (([x y] (cell-origin row col #f)))
          (send dc draw-rectangle x y (cell-width col #f) (cell-height row #f)))
        (send dc set-brush prev-brush)))

    ;; Shade the cells that don't display the current month.
    (define (shade-some-cells dc)
      (let ((month (date-month (seconds->date (exact-truncate (/ (+ (car calendar-range) (cdr calendar-range)) 2)) #t))))
        (let loop ((day (car calendar-range)) (cell 0))
          (when (< day (cdr calendar-range))
            (let ((d (seconds->date day)))
              (unless (eqv? (date-month d) month)
                (let-values (((row col) (day-index->row-col cell)))
                  (draw-cell-selection dc row col #t))))
              (loop (+ day (* 24 60 60)) (+ cell 1))))))

    ;; Draw the day in the top-left corner of each cell
    (define (draw-day-labels dc)
      (send dc set-font the-small-font)
      (let ((today (current-date)))
          (let loop ((day (car calendar-range))
                     (cell 0))
            (when (< day (cdr calendar-range))
              (let-values (((row col) (day-index->row-col cell)))
                (let* ((d (seconds->date day))
                       (label (format "~a" (date-day d))))
                  (let-values (((x y) (cell-origin row col #t)))
                    (send dc draw-text label x y))
                  (when (and (eqv? (date-day today) (date-day d))
                             (eqv? (date-month today) (date-month d))
                             (eqv? (date-year today) (date-year d)))
                    (draw-cell-selection dc row col #f))
                  (loop (+ day (* 24 60 60)) (+ cell 1))))))))

    ;; Set the position of all snips in `cell' (a key in snip-cells), so that
    ;; they are placed in order inside the day cell for their date.
    (define (arrange-snips-in-cell cell)

      ;; Return a list of the snips in CELL ordered by their start time.
      (define (get-snips-for-cell cell)
        (sort (hash-ref snip-cells cell '())
              <
              #:key (lambda (s) (send s get-start-time))))

      ;; Resize SNIPS to WIDTH and HEIGHT, return the Y position after the
      ;; last snip
      (define (resize-snips snips width height [y 0])
        (if (null? snips)
            y
            (let ((snip (car snips)))
              (send snip set-target-dimensions width height)
              (let ((sh (send snip get-actual-height)))
                (resize-snips (cdr snips) width height (+ y *snip-spacing* sh))))))

      ;; Position SNIPS starting at X, Y and stacking them vertically.
      (define (position-snips snips x y)
        (unless (null? snips)
          (let ((snip (car snips)))
            (move-to snip x y)
            (let ((sh (send snip get-actual-height)))
              (position-snips (cdr snips) x (+ y *snip-spacing* sh))))))

      ;; Return the number of snips that had to be clipped at the current
      ;; allocated height
      (define (count-tight-snips snips [ntight 0])
        (if (null? snips)
            ntight
            (count-tight-snips
             (cdr snips)
             (+ ntight (if (send (car snips) need-extra-height?) 1 0)))))

      (define (do-placement)
        (let-values (([row col] (day-index->row-col cell)))
          (let-values (([x y] (cell-origin row col #t))
                       ([cw ch] (values (cell-width col #t) (cell-height row #t))))
            (let* ((snips (get-snips-for-cell cell))
                   (max-snip-height
                    (if (> (length snips) 0)
                        (- (/ (- ch cell-header-height) (length snips))
                           *snip-spacing*)
                        ch))
                   (start-y (+ y cell-header-height))
                   (final-y (resize-snips snips cw max-snip-height start-y))
                   (remaining-height (- (+ y ch) final-y))
                   (ntight (count-tight-snips snips)))
              (when (and (> remaining-height 1) (> ntight 0))
                ;; NOTE: we do this once, but technically we should do it as
                ;; long as there is a remaining height and we have more tight
                ;; snips
                (let ((max-snip-height (+ max-snip-height (/ remaining-height ntight))))
                  (resize-snips snips cw max-snip-height start-y)))
              (position-snips snips x start-y)))))

      (unless suspend-cell-layout?
        (with-edit-sequence do-placement)))

    (define (arrange-snips-in-all-cells)
      (with-edit-sequence
       (lambda ()
         (for ((cell (in-hash-keys snip-cells)))
           (arrange-snips-in-cell cell)))))

    ;; Update totals-draw-functions with a new draw function for ROW
    (define (rebuild-totals row)
      
      (define (make-pict row)
        (let ((distance 0)
              (duration 0))
          (for ((col (in-range 7)))
            (let ((cell (+ (* row 7) col)))
              (for ((snip (in-list (hash-ref snip-cells cell '()))))
                (set! distance (+ distance (send snip get-distance)))
                (set! duration (+ duration (send snip get-duration))))))
          (make-totals-pict
           distance duration
           (cell-width 8 #t) (cell-height (+ 1 row) #t))))
      
      (let* ((index (- row 1))
             (pict (make-pict index)))
        (vector-set! totals-draw-functions index (make-pict-drawer pict))))

    ;; The on-paint method draws the calendar "background" (grid, headers,
    ;; day-labels, etc)
    (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
      (when before?
        (send dc clear)
        (send dc set-smoothing 'smoothed)
        (shade-some-cells dc)
        (draw-headers dc)
        (draw-grid dc)
        (draw-totals dc)
        (draw-day-labels dc)))

    (define/public (please-delete snip)
      (set! removing-snips? #t)
      (delete snip)
      (set! removing-snips? #f))

    (define/augment (can-insert? snip . rest)
      (let ((start-time (send snip get-start-time)))
        (number? (seconds->day-index start-time))))

    (define/augment (after-insert snip before x y)
      (let* ((start-time (send snip get-start-time))
             (cell (seconds->day-index start-time)))
        (hash-set! snip-cells
                   cell
                   (cons snip (hash-ref snip-cells cell '())))
        (arrange-snips-in-cell cell)
        (let-values (([row col] (day-index->row-col cell)))
          (rebuild-totals row))))

    (define/augment (can-delete? snip) removing-snips?)

    (define/augment (after-delete snip)
      (let* ((start-time (send snip get-start-time))
             (cell (seconds->day-index start-time)))
        (hash-set! snip-cells
                   cell
                   (remove snip (hash-ref snip-cells cell '())))
        (arrange-snips-in-cell cell)
        (let-values (([row col] (day-index->row-col cell)))
          (rebuild-totals row))))

    (define/augment (after-interactive-move event)
      (let loop ((cells '())
                 (snip (send this find-next-selected-snip #f)))
        (if snip
            (let* ((start-time (send snip get-start-time))
                   (cell (seconds->day-index start-time)))
              (loop (cons cell cells)
                    (send this find-next-selected-snip snip)))
            (for ((cell (remove-duplicates cells)))
              (arrange-snips-in-cell cell)))))

    (define/augment (on-select snip on?)
      (send snip select on?)
      (when on?
        (send activity-operations set-calendar-item snip)
        (set-before snip #f)))

    (define/augment (on-display-size)
      (let ((dc (send (get-canvas) get-dc)))
        (let-values (([w h] (send dc get-size)))

          ;; Determine the height of the header cells
          (let ((min-header-height 0))
            (for ([text (in-list *calendar-headers*)])
              (let-values (((w h x y) (send dc get-text-extent text the-header-font #t)))
                (set! min-header-height (max min-header-height h))))
            (set! header-cell-height (+ min-header-height (* 2 *header-cell-inset*))))

          ;; Determine the height of the header inside each cell (NOTE: this
          ;; could be done technically only once, but we need a dc<%> to
          ;; calculate it)
          (let-values (([w h x y] (send dc get-text-extent "123456789" the-small-font #t)))
            (set! cell-header-height (+ h 2)))

          (set! base-cell-width (/ w (+ 7 *totals-width-pct*)))
          (set! base-cell-height (/ (- h header-cell-height) calendar-num-weeks))

          ;; Rebild the header draw functions, as their witdh might have
          ;; changed.
          (set! header-draw-functions
                (for/vector ((h (in-list *calendar-headers*))
                             (c (in-range (length *calendar-headers*))))
                            (make-pict-drawer
                             (make-header-pict h (cell-width c #f) (cell-height 0 #f)))))

          ;; Rebuild the totals draw function (the size of their cell just
          ;; changed.
          (for ([row (in-range calendar-num-weeks)])
            (rebuild-totals (+ 1 row)))

          ;; Finally, re-arange the snips inside each cell
          (arrange-snips-in-all-cells))))
      
    (define/public (set-calendar-range start end)
      (set! suspend-cell-layout? #t)
      (send (get-canvas) suspend-flush)
      (with-edit-sequence
       (lambda ()
         (set! removing-snips? #t)
         (send this erase)
         (set! removing-snips? #f)))
      (set! suspend-cell-layout? #f)

      (set! snip-cells (make-hash))
      (set! calendar-range (cons start end))
      (set! calendar-num-weeks (exact-ceiling (/ (/ (- end start) (* 24 60 60)) 7)))
      (set! totals-draw-functions (make-vector calendar-num-weeks (lambda (dc x y) #f)))
      
      (on-display-size)         ; to re-compute the cell sizes and other stuff
      
      (send (get-canvas) resume-flush)
      (send (get-canvas) refresh))

    (define/public (bulk-insert snips)
      (send (get-canvas) suspend-flush)
      (set! suspend-cell-layout? #t)
      (with-edit-sequence
       (lambda ()
         (map (lambda (snip) (insert snip)) snips)))
      (set! suspend-cell-layout? #f)
      (arrange-snips-in-all-cells)
      (send (get-canvas) resume-flush)
      (send (get-canvas) refresh))

    ;; (send this set-selection-visible #f)

    ))


;;....................................................... calendar-view% ....

(define *months*
  (vector "XXX" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(define view-calendar%
  (class* object% (activity-operations<%>)
    (init parent)
    (init-field database select-activity-callback)
    (super-new)

    (define pane (new vertical-pane% [parent parent] [alignment '(left center)]))

    (define the-month-message #f)
    
    (define next-button #f)
    (define prev-button #f)
    (define year-choice #f)
    (define month-choice #f)

    (let ((sel-pane (new horizontal-pane% [parent pane]
                         [spacing 20]
                         [border 0]
                         [stretchable-height #f]
                         [stretchable-width #t]
                         [alignment '(left center)])))

      (make-spacer sel-pane)
      (new message% [parent sel-pane] [label (planner-icon)])

      (let ((p (new horizontal-pane%
                    [parent sel-pane]
                    [alignment '(left center)]
                    [border 10]))
            (f (send the-font-list find-or-create-font 16 'default 'normal 'normal)))
        
        (set! month-choice (new choice% [parent p] [label ""] [font f]
                                [choices '("January" "February" "March" "April" "May" "June"
                                           "July" "August" "September" "October" "November" "December")]
                                [callback (lambda (c e) (on-date-selected))]))

        (set! year-choice (new choice% [parent p] [label ""]
                               [choices '("2014" "2015")]
                               [font f]
                               [callback (lambda (c e) (on-date-selected))]))

        (make-spacer p 50 #t)

        (set! prev-button
              (new button% [parent p]
                   [label "Prev"]
                   [callback (lambda (b e) (jump-to-prev-month))]))
        (set! next-button
              (new button% [parent p]
                   [label "Next"]
                   [callback (lambda (b e) (jump-to-next-month))]))
        (new button% [parent p]
             [label "Today"]
             [callback (lambda (b e) (jump-to-today))]))

      (make-spacer sel-pane))

    (define the-calendar
      (let ((calendar (new calendar-pasteboard%
                           [activity-operations this]
                           [double-click-callback select-activity-callback])))
        (new editor-canvas%
             [parent pane]
             [editor calendar]
             [style '(no-border hide-hscroll hide-vscroll)]
             [horizontal-inset 0]
             [vertical-inset 0])
        calendar))

    (define the-start-year 2014)
    (define the-end-year 2020)
    (define the-month 5)
    (define the-year 2014)

    (define (init-date-range)
      (let-values (([start end] (get-calendar-date-range database)))
        (let* ((today (seconds->date (current-seconds)))
               (start-year (date-year (seconds->date start)))
               (end-year (max (date-year (seconds->date end))
                              (date-year today))))
          (send year-choice clear)
          (set! the-start-year start-year)
          (set! the-end-year end-year)
          (for ([year (in-range start-year (+ 1 end-year))])
            (send year-choice append (format "~a" year))))))

    (define (jump-to-next-month)
      (set! the-month (+ the-month 1))
      (when (> the-month 12)
        (begin
          (set! the-month 1)
          (set! the-year (+ the-year 1))))
      (update-calendar))

    (define (jump-to-prev-month)
      (set! the-month (- the-month 1))
      (when (< the-month 1)
        (begin
          (set! the-month 12)
          (set! the-year (- the-year 1))))
      (update-calendar))

    (define (jump-to-today)
      (let ((today (current-date)))
        (set! the-month (date-month today))
        (set! the-year (date-year today))
        (update-calendar)))

    (define (on-date-selected)
      (set! the-year (+ the-start-year (send year-choice get-selection)))
      (set! the-month (+ 1 (send month-choice get-selection)))
      (update-calendar))

    (define (update-calendar)
      (with-busy-cursor
       (lambda ()
         (let ((yr-index (- the-year the-start-year)))
           (send year-choice set-selection yr-index))
         (send month-choice set-selection (- the-month 1))
         (if (and (eqv? the-year the-end-year)
                  (eqv? the-month 12))
             (send next-button enable #f)
             (send next-button enable #t))
         (if (and (eqv? the-year the-start-year)
                  (eqv? the-month 1))
             (send prev-button enable #f)
             (send prev-button enable #t))
         (let-values (([start end] (calendar-month-range the-month the-year)))
           (send the-calendar set-calendar-range start end)
           (let ((snips (for/list ([session (in-list (get-sessions-between-dates start end database))])
                          (new calendar-item-snip% [db-row session]))))
             (send the-calendar bulk-insert snips))))))

    (define dirty? #t)
    (define change-notification-source (make-log-event-source))

    (define (find-snip-by-sid sid)
      (let loop ((snip (send the-calendar find-first-snip)))
        (if snip
            (if (= sid (send snip get-session-id))
                snip
                (loop (send snip next)))
            #f)))

    (define (maybe-update sid) 
      (let ((snip (find-snip-by-sid sid)))
        (when snip
          (send snip refresh (get-session-by-id sid database))
          ;; The refresh below forces the re-display of the totals column
          (send (send the-calendar get-canvas) refresh))))

    (define (maybe-delete sid)
      (let ((snip (find-snip-by-sid sid)))
        (when snip
          (send the-calendar please-delete snip)
          ;; The refresh below forces the re-display of the totals column
          (send (send the-calendar get-canvas) refresh))))

    (define (maybe-add sid)
      (let* ((row (get-session-by-id sid database))
             (snip (new calendar-item-snip% [db-row row])))
        ;; Will silently fail if the new sid is outside the date range of the
        ;; calendar
        (send the-calendar insert snip)
        ;; The refresh below forces the re-display of the totals column
        (send (send the-calendar get-canvas) refresh)))

    (define/public (activated)
      ;; Get the full list of events, but we will discard them if the calendar
      ;; is "dirty" and has to do a full refresh anyway
      (define events (collect-events change-notification-source))
      (if dirty?
          (begin
            (init-date-range)
            (jump-to-today)
            (set! dirty? #f))
          (begin
            ;; Process changes that happened while we were inactive
            (for ((sid (hash-ref events 'session-deleted '())))
              (maybe-delete sid))
            (for ((sid (hash-ref events 'session-updated '())))
              (maybe-update sid))
            (for ((sid (hash-ref events 'session-created '())))
              (maybe-add sid)))))

    (define/public (refresh)
      (init-date-range)
      (collect-events change-notification-source) ; discard the events
      (update-calendar))

    (define/public (save-visual-layout)
      #f)

    ;; Activity operations interface

    (define selected-item #f) ; calendar item (snip) on which the menu items operate.

    (define/public (set-calendar-item item)
      (set! selected-item item))

    (define/public (get-top-level-window)
      (send pane get-top-level-window))

    (define/public (get-database) database)

    (define/public (get-selected-sid)
      (if selected-item
          (send selected-item get-session-id)
          #f))

    (define/public (get-selected-sport)
      (if selected-item
          (send selected-item get-session-sport)
          #f))

    (define/public (get-selected-guid)
      (if selected-item
          (send selected-item get-activity-guid)
          #f))

    (define/public (after-update sid)
      (activated))

    (define/public (after-new sid)
      (activated))

    (define/public (can-delete? sid)
      (not (eq? selected-item #f)))

    (define/public (after-delete sid)
      (activated))

    (define/public (before-popup)
      #f)

    (define/public (after-popdown)
      (set! selected-item #f))

    ))
