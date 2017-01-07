#lang racket/base
;; widgets.rkt -- some extra GUI widgets built on top of racket/gui
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

(require embedded-gui
         file/md5
         pict
         racket/class
         racket/date
         racket/math
         racket/gui/base
         racket/match
         (rename-in srfi/48 (format format-48))
         racket/list
         racket/string
         "dbglog.rkt"
         "al-prefs.rkt"
         "fmt-util.rkt")

(provide validating-input-field%)
(provide date-input-field%)
(provide time-of-day-input-field%)
(provide duration-input-field%)
(provide number-input-field%)
(provide date-range-selector%)
(provide number-range-selector%)
(provide qresults-list%)
(provide tag-input-field%)
(provide column-info)
(provide with-busy-cursor)
(provide make-horizontal-pane)
(provide make-group-box-panel)
(provide make-spacer)
(provide al-edit-dialog%)
(provide al-progress-dialog%)
(provide tab-selector%)
(provide notification-banner%)


;;.............................................. validating-input-field% ....

;; An input field that allows validating its input and converting it to
;; different types.  For example, can be used to read numbers or dates.
(define validating-input-field%
  (class text-field%
    (init validate-fn convert-fn [valid-value-cb #f] [cue-text ""] [label ""])
    (super-new [label label])

    (inherit get-value get-field-background set-field-background get-editor)

    (define vfn validate-fn)
    (define cvfn convert-fn)
    (define cb valid-value-cb)
    (define old-value #f)
    (define cue cue-text)  ; text to display when field is empty and unfocused
    (define showing-cue? #f)
    (define original-value #f)

    ;; When set to #t, mark this field as invalid, regardless of the VFN
    ;; result, until the next edit is performed.
    (define global-invalid #f)

    ;; The background of the field will change to `bad-bg` if the value it
    ;; contains is invalid.
    (define good-bg (get-field-background))
    (define bad-bg (make-object color% 255 120 124)) ; red

    ;; Text style for cue text
    (define cue-fg
      (let ((grey-text (new style-delta%)))
        (send grey-text set-delta-foreground "gray")
        grey-text))

    ;; Text style for normal text
    (define text-fg
      (let ((black-text (new style-delta%)))
        (send black-text set-delta-foreground "black")
        black-text))

    (define (clear-cue-text)
      (when showing-cue?
        (let ([editor (get-editor)])
          (send editor erase)
          (send editor clear-undos)
          (send editor change-style text-fg 'start 'end #f)
          (set! showing-cue? #f))))

    ;; Insert the cue text if the input field is empty
    (define (maybe-insert-cue-text)
      (unless (or showing-cue? (> (string-length (get-value)) 0))
        (let ([editor (get-editor)])
          (send editor change-style cue-fg 'start 'end #f)
          (send editor insert cue)
          (set! showing-cue? #t))))

    ;; Validate the contents and, if valid, setup a 500ms timer for the
    ;; valid-value-cb (unless report-valid-value? is #f).  The timer is used
    ;; so that we don't fire a sequence of valid values when the user is
    ;; typing.
    ;;
    ;; Note that this is public and it allows the user to write validation
    ;; functions that depend on some external factors.
    (define/public (validate [report-valid-value? #t])
      (let* ((value (if showing-cue? "" (get-value)))
             (valid? (and (vfn value) (not global-invalid))))
        (set-field-background (if valid? good-bg bad-bg))
        (if (and report-valid-value? valid?)
            (send cb-timer start 500 #t)
            (send cb-timer stop))))

    (define/public (mark-valid flag)
      (set! global-invalid (not flag))
      (validate))

    ;; The user is notified of a valid value via a timer.  This prevents the
    ;; callback to fire multiple times as the user enters the value (e.g, when
    ;; the user types "123", we don't want to fire the callback with 1, 12 and
    ;; 123)
    (define cb-timer
      (new timer% [notify-callback 
                   (lambda () 
                     (let ((value (get-value)))
                       (unless (equal? old-value value)
                         (when cb (cb (cvfn value)))
                         (set! old-value value))))]))

    (define/override (on-focus on?)
      (if on? (clear-cue-text) (maybe-insert-cue-text))
      (super on-focus on?))

    (define/override (on-subwindow-char receiver event)
      ;; Let the parent handle the on-subwindow-char, after it is finished, we
      ;; validate the contents.
      (let ((result (super on-subwindow-char receiver event)))
        (set! global-invalid #f)
        (validate)
        result))

    (define/override (set-value v)
      (clear-cue-text)
      (super set-value v)
      (set! original-value v)
      (validate #f)         ; dont report valid values for externally set data
      (maybe-insert-cue-text))

    (define/public (has-valid-value?)
      ;; NOTE: the empty value might be valid, let vfn decide
      (vfn (if showing-cue? "" (get-value))))

    (define/public (has-changed?)
      (let ((new-value (if showing-cue? "" (get-value))))
        (not (string=? new-value original-value))))

    (define/public (get-converted-value)
      (let ((v (if showing-cue? "" (get-value))))
        (if (vfn v) (cvfn v) #f)))

    (define/public (set-cue-text text)
      (set! cue text)
      (maybe-insert-cue-text))

    (maybe-insert-cue-text)))


;;.................................................. number-input-field% ....

(define (valid-number? data min max)
  (let ((t (string-trim data)))
    (or (= (string-length t) 0)
        (let ((v (string->number t)))
          (and v
               (or (not min) (>= v min))
               (or (not max) (<= v max)))))))

(define (convert-to-number data)
  (let ((t (string-trim data)))
    (if (= (string-length data) 0)
        'empty
        (string->number data))))

;; An input field for reading numbers (integer or floating
;; point). `valid-value-cb` needs to be specified to read the value.
(define number-input-field%
  (class validating-input-field%
    (init [min-value #f] [max-value #f])
    (super-new [validate-fn (lambda (v) (valid-number? v min-value max-value))]
               [convert-fn convert-to-number])
    (inherit set-value)

    (define/public (set-numeric-value n)
      (set-value
       (if (integer? n) (format "~a" n) (format-48 "~1,2F" n))))

    ))


;;.................................................... date-input-field% ....

;; TODO: accept dates using 10-12-2014, plus other combinations
(define (convert-to-date data)
  (let ((t (string-trim data)))
    (if (= (string-length t) 0)
        'empty
        (let ((m (regexp-match "^([0-9]+)/([0-9]+)/([0-9]+)$" t)))
          (if m
              (let ((day (string->number (list-ref m 1)))
                    (month (string->number (list-ref m 2)))
                    (year (string->number (list-ref m 3))))
                ;; Limit the year to a reasonable range, as a big year range
                ;; causes performance problems in different places.  If this
                ;; app is still in use after 2100, it is a succesful one :-)
                (if (or (> year 2100) (< year 1900))
                    #f
                    (with-handlers (((lambda (e) #t) (lambda (e) #f)))
                                   (find-seconds 0 0 0 day month year))))
              #f)))))

(define date-input-field%
  (class validating-input-field%
    (init)
    (super-new
     [cue-text "dd/mm/yyyy"]
     [validate-fn convert-to-date]
     [convert-fn convert-to-date])
    (inherit set-value)

    (define/public (set-date-value seconds)
      (if (or (eq? seconds #f) (eq? seconds 'empty)) ; special case
          (set-value "")
          (let ((d (seconds->date seconds #t)))
            (set-value
             (string-replace
              (format-48 "~2F/~2F/~2F" (date-day d) (date-month d) (date-year d)) " " "0")))))
    ))


;;............................................. time-of-day-input-field% ....

;; TODO: check for valid ranges for hours, minutes
(define (convert-to-time-of-day data)
  (let ((t (string-trim data)))
    (cond ((= (string-length t) 0) 'empty)
          ((regexp-match "^([0-9]+):([0-9]+):?([0-9]+)? *((?i:am|pm))?$" t) =>
           (lambda (m)
             (let ((hour (string->number (list-ref m 1)))
                   (minute (string->number (list-ref m 2)))
                   (seconds (let ((s (list-ref m 3)))
                              (if s (string->number s) 0)))
                   (am-pm? (list-ref m 4)))
               (when (string? am-pm?)
                 (set! am-pm? (string-upcase am-pm?)))
                (+ (* (+ (* hour 60) minute) 60) seconds
                   (if am-pm?
                       (cond ((and (string=? am-pm? "AM") (= hour 12))
                              (* -12 60 60))
                             ((and (string=? am-pm? "PM") (not (= hour 12)))
                              (* 12 60 60))
                             (#t 0))
                       0)))))
          (#t #f))))

(define time-of-day-input-field%
  (class validating-input-field%
    (init)
    (super-new
     [cue-text "hh:mm AM/PM"]
     [validate-fn convert-to-time-of-day]
     [convert-fn convert-to-time-of-day])
    (inherit set-value)

    (define/public (set-time-of-day-value seconds)
      (let ((d (seconds->date seconds #t)))
        (set-value
         (string-replace
          (format-48 "~2F:~2F:~2F" (date-hour d) (date-minute d) (date-second d)) " " "0"))))
    ))


;;.............................................. duration-input-field% ....

;; TODO: check for valid ranges for hours, minutes, seconds
(define (convert-to-duration data)
  (let ((t (string-trim data)))
    (cond ((= (string-length t) 0) 'empty)
          ((regexp-match "^([0-9]+):([0-9]+):([0-9]+)$" t) =>
           (lambda (m)
             (let ((hours (string->number (list-ref m 1)))
                   (minutes (string->number (list-ref m 2)))
                   (seconds (string->number (list-ref m 3))))
               (if (and (< minutes 60) (< seconds 60))
                   (+ (* (+ (* hours 60) minutes) 60) seconds)
                   #f))))
          ((regexp-match "^([0-9]+):([0-9]+)$" t) =>
           (lambda (m)
             (let ((minutes (string->number (list-ref m 1)))
                   (seconds (string->number (list-ref m 2))))
               (if (and (< minutes 60) (< seconds 60))
                   (+ (* minutes 60) seconds)
                   #f))))
          (#t #f))))

(define duration-input-field%
  (class validating-input-field%
    (init)
    (super-new
     [cue-text "hh:mm:ss"]
     [validate-fn convert-to-duration]
     [convert-fn convert-to-duration])
    (inherit set-value)

    (define/public (set-duration-value duration)
      (set-value (duration->string duration)))

    ))



;;................................................... date-time-selector ....

(define (this-year-start)
  (let ((now (current-date)))
    (date->seconds
     (date 0 0 0
           1 1 (date-year now)
           0 0 0 (date-time-zone-offset now)))))

(define (last-year-start)
  (let ((now (current-date)))
    (date->seconds
     (date 0 0 0
           1 1 (- (date-year now) 1)
           0 0 0 (date-time-zone-offset now)))))

(define (this-month-start)
  (let ((now (current-date)))
    (date->seconds
     (date 0 0 0
           1 (date-month now) (date-year now)
           0 0 0 (date-time-zone-offset now)))))

(define (last-month-start)
  (let* ((now (current-date))
         (month (date-month now))
         (year (date-year now)))
    (date->seconds
     (date 0 0 0
           1 (if (= month 1) 12 (- month 1)) (if (= month 1) (- year 1) year)
           0 0 0
           (date-time-zone-offset now)))))

(define (this-week-start)
  (let* ((seconds (current-seconds))
         (now (seconds->date seconds))
         (week-day (date-week-day now))
         (delta (+ (* 24 3600 week-day)
                   (* 3600 (date-hour now))
                   (* 60 (date-minute now))
                   (date-second now))))
    ;; NOTE: calculation makes the week start on a Sunday, adjust for that.
    (+ (* 24 3600) (- seconds delta))))

(define (last-week-start)
  (- (this-week-start) (* 7 24 3600)))

(define (this-day-start)
  (let ((now (current-date)))
    (date->seconds
     (date 0 0 0
           (date-day now) (date-month now) (date-year now)
           0 0 0 (date-time-zone-offset now)))))

(define (last-7-days-start)
  (- (this-day-start) (* 6 24 3600)))

(define (last-30-days-start)
  (- (this-day-start) (* 29 24 3600)))

(define (last-365-days-start)
  (- (this-day-start) (* 364 24 3600)))

(define (this-day-end)
  (+ (this-day-start) (* 24 3600)))

(define (get-tag-position tag)
  (let loop ((tp *time-periods*)
             (index 0))
    (if (null? tp)
        #f
        (if (eq? (time-period-tag (car tp)) tag)
            index
            (loop (cdr tp) (+ index 1))))))

(struct time-period (tag name start-ts end-ts sql-fragment))

(define *time-periods*
  (list
   (time-period 'all-days "all days"
                (lambda () 0) this-day-end
                "is not null")
   (time-period 'custom-dates "custom dates"
                (lambda () 'custom-dates) this-day-end
                "between ~a and ~a")
   (time-period 'seasons "seasons"
                (lambda () 'seasons) this-day-end
                "between (select start_date from SEASON where name = ?) and (select end_date from SEASON where name = ?)")
   (time-period 'last-7-days "last 7 days"
                last-7-days-start this-day-end
                "> strftime('%s', 'now', '-7 days')")
   (time-period 'this-week "this week"
                this-week-start this-day-end
                "> strftime('%s', 'now', '-6 days', 'weekday 1')")
   (time-period 'last-week "last week"
                last-week-start this-week-start
                "between strftime('%s', 'now', '-13 days', 'weekday 1') and strftime('%s', 'now', '-6 days', 'weekday 1')")
   (time-period 'last-30-days "last 30 days"
                last-30-days-start this-day-end
                "> strftime('%s', 'now', '-30 days')")
   (time-period 'this-month "this month"
                this-month-start this-day-end
                "> strftime('%s', 'now', 'start of month')")
   (time-period 'last-month "last month"
                last-month-start this-month-start
                "between strftime('%s', 'now', 'start of month', '- 1 month') and strftime('%s', 'now', 'start of month')")
   (time-period 'last-365-days "last 365 days"
                last-365-days-start this-day-end
                "> strftime('%s', 'now', '-365 days')")
   (time-period 'this-year "this year"
                this-year-start this-day-end
                "> strftime('%s', 'now', 'start of year')")
   (time-period 'last-year "last year"
                last-year-start this-year-start
                "between strftime('%s', 'now', 'start of year', '- 1 year') and strftime('%s', 'now', 'start of year')")
   ))


(define date-range-selector%
  (class object%
    (init parent [callback (lambda (x) #f)] [initial-selection #f])
    (super-new)

    (define time-selection-callback callback)

    ;; start/end dates selected by custom fields
    (define start-date (- (current-seconds) (* 30 24 3600)))
    (define end-date (current-seconds))

    (define seasons '())

    (define (get-date-range selection)
      (let ((range (list-ref *time-periods* selection)))
        (cons ((time-period-start-ts range)) ((time-period-end-ts range)))))

    (define pane (new vertical-pane%
                       [parent parent]
                       [border 0]
                       [spacing 5]
                       [vert-margin 0]
                       [horiz-margin 0]
                       [stretchable-height #f]
                       [stretchable-width #f]
                       [alignment '(left center)]))

    (define the-selector (new choice%
                              [parent pane]
                              [label "Time period "]
                              [callback (lambda (c event) (choice-cb c event))]
                              [choices (map time-period-name *time-periods*)]))

    (when initial-selection
      (let ((index (get-tag-position initial-selection)))
        (when index
          (send the-selector set-selection index))))

    (define custom-date-panel
      (new horizontal-panel% [parent pane] [stretchable-height #f] [spacing 0]))

    (send custom-date-panel show #f)

    (define (choice-cb cb event)
      (let ((date-range (get-date-range (send cb get-selection))))
        (cond ((eq? (car date-range) 'custom-dates)
               (when (> end-date start-date)
                 (time-selection-callback (cons start-date end-date)))
               (select-custom-date-panel 'custom-dates))
              ((eq? (car date-range) 'seasons)
               (select-custom-date-panel 'seasons)
               (on-season-selected (send season-choice get-selection)))
              (#t
               (select-custom-date-panel #f)
               (time-selection-callback date-range)))))

    (define (select-custom-date-panel mode)
      (cond ((eq? mode 'custom-dates)
             (send custom-date-panel change-children
                   (lambda (old) (list custom-range-start custom-range-end)))
             (send custom-date-panel show #t))
            ((eq? mode 'seasons)
             (send custom-date-panel change-children
                   (lambda (old) (list season-choice)))
             (send custom-date-panel show #t))
            (#t
             (send custom-date-panel show #f))))

    (define custom-range-start
      (new date-input-field%
           [valid-value-cb (lambda (v)
                             (set! start-date (if (number? v) v 0))
                             (when (> end-date start-date)
                               (time-selection-callback (cons start-date end-date))))]
           [parent custom-date-panel] [label ""]
           [style '(single)] [stretchable-width #f] [min-width 1]))
    (define custom-range-end
      (new date-input-field%
         [parent custom-date-panel] [label "--"]
         [valid-value-cb
          (lambda (v)
            ;; Add 24 hours to end-date to make it inclusive
            (set! end-date (+ (* 24 60 60)
                              (if (number? v) v (current-seconds))))
            (when (> end-date start-date)
              (time-selection-callback (cons start-date end-date))))]
         [style '(single)] [stretchable-width #f] [min-width 1]))
    (define season-choice
      (new choice%
           [parent custom-date-panel] [label ""]
           [stretchable-width #t]
           [choices '()]
           [callback (lambda (c event) (on-season-selected (send c get-selection)))]))

    (define (on-season-selected index)
      (unless (null? seasons)
        (let ((season (list-ref seasons index)))
          (time-selection-callback
           (cons (vector-ref season 1) (vector-ref season 2))))))

    (define/public (get-selection)
      (let ((dr (get-date-range (send the-selector get-selection))))
        (cond ((eq? (car dr) 'custom-dates)
               (if (> end-date start-date)
                   (cons start-date end-date)
                   #f))
              ((eq? (car dr) 'seasons)
               (if (pair? seasons)
                   (let ((season (list-ref seasons (send season-choice get-selection))))
                     (cons (vector-ref season 1) (vector-ref season 2)))
                   #f))
              (#t
               dr))))

    (define/public (set-seasons s)
      ;; Set a new list of seasons and attempt to select the same season
      ;; again.  This is needed since set-seasons will can be called
      ;; repeatedly on refresh calls on the GUI.
      ;;
      ;; We use two methods to restore the previous season: if we find a
      ;; season with the same name as the previously selected one, we use it,
      ;; otherwise we try to use the previous index position in the list (if
      ;; at least that many seasons are available.
      (let* ((index (send season-choice get-selection)) ; might be #f
             (name (if index (vector-ref (list-ref seasons index) 0) #f)))
        (set! seasons s)
        (send season-choice clear)
        (for ([season (in-list seasons)]
              [n (in-range 0 (length seasons))])
          (let ((sname (vector-ref season 0)))
            (send season-choice append sname)
            (when (equal? name sname) (set! index n))))
        (when (and index (< index (length seasons)))
          (send season-choice set-selection index))))

    (define/public (get-restore-data)
      (let ((tp (list-ref *time-periods* (send the-selector get-selection)))
            (sn (and (pair? seasons) (send season-choice get-selection))))
        (list
         (time-period-tag tp)
         (and sn (vector-ref (list-ref seasons sn) 0))
         (send custom-range-start get-converted-value)
         (send custom-range-end get-converted-value))))

    (define/public (restore-from data)
      (let ((tag (first data))
            (season (second data))
            (start (third data))
            (end (fourth data)))
        (when tag
          (let ((index (get-tag-position tag)))
            (when index
              (send the-selector set-selection index)))
          (select-custom-date-panel tag))
        (when season
          (let loop ((s seasons) (index 0))
            (unless (null? s)
              (if (equal? season (vector-ref (car s) 0))
                  (send season-choice set-selection index)
                  (loop (cdr s) (+ 1 index))))))
        (when (number? start)
          (set! start-date start))
        (when (number? end)
          (set! end-date end))
        (send custom-range-start set-date-value start)
        (send custom-range-end set-date-value end)))

    ))


;....................................................... range-selector ....

(define number-range-selector%
  (class object%
    (init parent label callback)
    (super-new)

    (define cb callback)
    (define low #f)
    (define high #f)

    (define pane (new horizontal-pane% [parent parent] [stretchable-height #f] 
                      [spacing 0] [border 0] [alignment '(left center)]))

    (new message% [parent pane] [label label] [min-width 70])
    (new number-input-field%
         [valid-value-cb (lambda (v) (set! low (if (eq? v 'empty) #f v)) (cb (cons low high)))]
         [parent pane] [label ""]
         [style '(single)] [stretchable-width #f] [min-width 1])
    ;; (new message% [parent pane] [label "--"])
    (new number-input-field%
         [parent pane] [label "--"]
         [valid-value-cb (lambda (v) (set! high (if (eq? v 'empty) #f v)) (cb (cons low high)))]
         [style '(single)] [stretchable-width #f] [min-width 1])
    ))


;;.................................................. list-box% utilities ....

(define (lb-get-visual-layout lb)
  (let ((co (send lb get-column-order)))
    (cons co
          (for/list ((c (in-list co)))
            (call-with-values
                (lambda ()
                  (send lb get-column-width c))
              vector)))))

(define (lb-set-visual-layout lb vl)
  (send lb set-column-order (car vl))
  (for ((c (in-list (car vl)))
        (w (in-list (cdr vl))))
    (send lb set-column-width c (vector-ref w 0)
          (vector-ref w 1) (vector-ref w 2))))

(define (lb-set-default-visual-layout lb)
  (let ((ncols (length (send lb get-column-order))))
    (for ((c (in-range ncols)))
      (send lb set-column-width c 100 0 10000))))

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

(define (lb-ensure-column-count lb count)
  (let ((actual (length (send lb get-column-order))))
    (cond ((> actual count)
           (for ((i (in-range (- actual count))))
             (send lb delete-column 0)))
          ((< actual count)
           (for ((i (in-range (- count actual))))
             (send lb append-column ""))))))

(define (lb-clear lb)
  (send lb clear)
  (let ((column-count (length (send lb get-column-order))))
    (for ((c (in-range (- column-count 1))))
      (send lb delete-column 1))))

;; Setup the list box LB with headers from HEADERS (a list of strings).  Use
;; PREF-KEY to retrieve a visual layout for these columns.
(define (lb-install-headers lb headers)
  (lb-ensure-column-count lb (length headers))
  (for ((i (in-range (length headers)))
        (h (in-list headers)))
    (send lb set-column-label i h)))

(define (lb-fill-row lb row-num formatter data)
  (send lb set-data row-num data)
  (for ((column (in-range (length formatter)))
        (fmt (in-list formatter)))
    (let ((v (fmt data)))
      (send lb set-string row-num v column))))


;;......................................................... fields-edit% ....

;; A dialog box used to edit the visible fields in a qresults-list% widget.
(define fields-edit%
  (class object%
    (init)
    (super-new)

    (define (make-toplevel-dialog parent)
      (new
       (class dialog% (init) (super-new)
         (define/augment (on-close) (on-cancel)))
       [label "Edit fields to display"]
       [min-width 300]
       [min-height 500]
       [parent parent]))

    (define toplevel-window (make-toplevel-dialog #f))

    ;; Result of running the dialog, the list of visible fields, if data was
    ;; saved, #f if it was not, this is what edit-equipment returns.
    (define dialog-result #f)

    (define fsel-pane #f)

    (define edit-pane
      (let ((p (new vertical-panel% [parent toplevel-window]
                    [spacing 10] [border 20]
                    [alignment '(center top)])))
        (set! fsel-pane
              (new vertical-panel% [parent p] [spacing 5] 
                   [style '(vscroll)]
                   [alignment '(left top)]))
        (let ((p (new horizontal-pane% [parent p] [border 0]
                       [stretchable-height #f] [alignment '(right center)])))
          (new button% [label "Save"] [parent p]
               [callback (lambda (b e) (on-save))])
          (new button% [label "Cancel"] [parent p]
               [callback (lambda (b e) (on-cancel))]))
        p))

    (define (on-save)
      (let ((selection '()))
        (for ((c (in-list (send fsel-pane get-children))))
          (when (and (is-a? c check-box%) (send c get-value))
            (set! selection (cons (send c get-label) selection))))
        (finish-edit selection)))

    (define (on-cancel)
      (finish-edit #f))

    (define (finish-edit result)
      (set! dialog-result result)
      (send toplevel-window show #f))

    (define (setup fields visible-fields)

      (define (make-check-box label)
        (new check-box% 
             [parent fsel-pane] [label label] 
             [value (member label visible-fields)]
             [style '(deleted)]))

      (send fsel-pane change-children 
            (lambda (old) (map make-check-box fields))))

    (define/public (begin-edit parent field-list visible-fields)
      (let ((old-toplevel toplevel-window))
        (let ((toplevel (if parent (make-toplevel-dialog parent) toplevel-window)))
          (send edit-pane reparent toplevel)
          (set! toplevel-window toplevel))
        (set! dialog-result #f)
        (setup field-list visible-fields)
        (send toplevel-window show #t) ; will block untill finish-dialog is called
        (send edit-pane reparent old-toplevel)
        dialog-result))

    ))


;;....................................................... qresults-list% ....

(struct column-info (name formatter sort-key))

;; Return a symbol that can be used to store visual preferences for a list-box
;; based on COLUMN-DEFINITIONS (a list of COLUMN-INFO objects).  The key is
;; composed of a TAG folowed by the md5 sum of the concatenated column titles.
(define (make-pref-key tag column-definitions)
  (let* ((title-str (apply string-append (map column-info-name column-definitions)))
         (title-md5 (bytes->string/latin-1 (md5 title-str #t))))
    (string->symbol (string-append (symbol->string tag) "--" title-md5))))

(define (with-busy-cursor thunk)
  (begin-busy-cursor)
  (thunk)
  (end-busy-cursor))

(define qresults-list%
  (class object%
    (init parent tag 
          [label ""]
          [right-click-menu #f])
    (super-new)

    (define the-tag tag)

    ;; key under which we store visual preferences (see al-prefs.rkt), it is
    ;; actually updated by setup-column-defs
    (define pref-key the-tag)

    (define column-defs '())            ; (listof column-info)
    (define visible-columns '())        ; (listof strings), the column names
    (define sort-column #f)             ; index of the column on which we sort
    (define sort-descending? #f)

    (define the-data '())

    (define rclick-menu right-click-menu)

    (define default-export-file-name #f)

    (define (get-column-formatters)
      (let ((formatters '()))
         (for ((cdef (in-list column-defs)))
           (when (member (column-info-name cdef) visible-columns)
             (set! formatters (cons (column-info-formatter cdef) formatters))))
         (reverse formatters)))

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
             (key (column-info-sort-key 
                   (findf (lambda (ci) (equal? (column-info-name ci) cname))
                          column-defs)))
             (cmp (cond ((= (length the-data) 0) <) ; doesn't matter
                        ((string? (key (car the-data)))
                         (if sort-descending? string>? string<?))
                        (#t
                         (if sort-descending? > <)))))
        (set! the-data (sort the-data cmp #:key key)))

      ;; Update the list-box in place (no rows are added/deleted)
      (refresh-contents))

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

    (define setup-fields-dlg #f)

    (define/public (interactive-setup-visible-columns)
      (unless setup-fields-dlg 
        (set! setup-fields-dlg (new fields-edit%)))
      (let ((visible-fields
             (send setup-fields-dlg begin-edit 
                   (send the-pane get-top-level-window)
                   (map column-info-name column-defs)
                   visible-columns)))
        (when visible-fields
          ;; NOTE: visible-columns needs to be in the right order.
          (set! visible-columns '())
          (for ((cdef (in-list column-defs)))
            (let ((name (column-info-name cdef)))
              (when (member name visible-fields)
                (set! visible-columns (cons name visible-columns)))))
          (set! visible-columns (reverse visible-columns))
          (lb-install-headers the-list-box visible-columns)
          (lb-set-default-visual-layout the-list-box)
          (refresh-contents))))

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
                    (when rclick-menu
                      (popup-menu rclick-menu (send event get-x) (send event get-y)))
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

    (define/public (set-tag new-tag)
      (save-visual-layout)              ; for the existing key, if any
      (set! pref-key #f)
      (set! the-tag new-tag))

    (define/public (save-visual-layout)
      ;; NOTE: we only save the preferences for the current pref key.  Saving
      ;; for previous ones (when the columns are changes) is done as part of
      ;; `setup-column-defs'.
      ;; (printf "put-pref ~a -- ~a~%" pref-key (get-visual-layout the-list-box))
      (when pref-key
        (al-put-pref pref-key 
                     (cons visible-columns 
                           (lb-get-visual-layout the-list-box)))))

    (define/public (setup-column-defs fd)
      (save-visual-layout)              ; for the previous field definitions
      (set! column-defs fd)
      (set! pref-key (make-pref-key the-tag column-defs))
      (set! sort-column #f)
      (let ((visual-layout (al-get-pref pref-key (lambda () #f))))
        (let ((visible-fields (if visual-layout 
                                  (car visual-layout) 
                                  (map column-info-name column-defs)))
              (lb-visual-layout (if visual-layout
                                    (cdr visual-layout)
                                    #f)))
          (set! visible-columns visible-fields)
          (lb-install-headers the-list-box visible-fields)
          (if lb-visual-layout
              (lb-set-visual-layout the-list-box lb-visual-layout)
              (lb-set-default-visual-layout the-list-box)))))

    (define/public (set-data rows)
      (set! the-data rows)
      (if sort-column
          (begin
            ;; Hack to keep the sort order
            (set! sort-descending? (not sort-descending?))
            ;; NOTE: sort-by-column will call refresh-contents
            (sort-by-column sort-column))
          (refresh-contents)))

    (define/public (export-data-as-csv outp formatted-values?)
      (for-each (lambda (c)
                  (write-string (column-info-name c) outp)
                  (write-string "," outp))
                column-defs)
      (newline outp)
      (let ((fn-list (map (if formatted-values?
                                 column-info-formatter
                                 column-info-sort-key)
                             column-defs)))
        (for-each (lambda (d)
                    (for-each (lambda (f)
                                (write-string (format "\"~a\"," (f d)) outp))
                              fn-list)
                    (newline outp))
                  the-data)))

    (define/public (get-selected-row-index)
      (let ((selected-items (send the-list-box get-selections)))
        (if (null? selected-items) #f (car selected-items))))

    (define/public (get-data-for-row row-index)
      (send the-list-box get-data row-index))

    (define/public (update-row row-index new-data)
      (set! the-data
            (append 
             (take the-data row-index)
             (list new-data)
             (drop the-data (+ row-index 1))))
      (refresh-contents-1 row-index new-data)
      (send the-list-box set-selection row-index)
      (send the-list-box set-first-visible-item row-index))

    (define/public (add-row data)
      (send the-list-box append "")
      (set! the-data (append the-data (list data)))
      (let ((row-index (- (send the-list-box get-number) 1)))
        (refresh-contents-1 row-index data)
        (send the-list-box set-selection row-index)
        (send the-list-box set-first-visible-item row-index)))
    
    (define/public (delete-row row-index)
      (set! the-data
            (append 
             (take the-data row-index)
             (drop the-data (+ row-index 1))))
      (send the-list-box delete row-index))

    (define/public (get-row-count)
      (send the-list-box get-number))

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

    ))


;;....................................................... tag-input-field% ....

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
      (clear-contents)
      (with-edit-sequence
       (lambda ()
         (map (lambda (tag)
                (let ((snip (make-object tag-snip% tag)))
                  (insert snip)))
              tag-list))))

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


;;...................................................... al-edit-dialog% ....

;; Convenience function to make a horizontal-pane as required by the
;; various dialog boxes
(define (make-horizontal-pane parent (stretchable-height? #t))
  (new horizontal-pane% [parent parent]
       [border 0] [spacing 5]
       [stretchable-height stretchable-height?]))

(define (make-group-box-panel parent (label ""))
  (let ((gb (new group-box-panel%
                 [parent parent] [label label] [alignment '(left center)]
                 [spacing 10] [border 5])))
    ;; The group box panel seems to ignore the internal border, so we create
    ;; an inner vertical pane and return that.
    (new vertical-pane% [parent gb] [spacing 10] [alignment '(left center)])))

;; Create a spacer widget, with a fixed width.
(define (make-spacer parent (witdh 5) (stretchable? #f))
  (new message% [parent parent] [label ""] [min-width witdh] [stretchable-width stretchable?]))

(define al-edit-dialog%
  (class object%
    (init-field title
          [icon #f]
          [save-button-name "Save"]
          [min-width 300]
          [min-height 300])
    (super-new)

    (define (make-toplevel-dialog parent)
      (new
       (class dialog% (init) (super-new)
         (define/augment (on-close) (on-cancel)))
       [label title]
       [min-width min-width]
       [min-height min-height]
       [parent (if parent (send parent get-top-level-window) #f)]))

    (define toplevel-window (make-toplevel-dialog #f))

    ;; The save button of the dialog box (created later), it is
    ;; enabled/disabled by validate-timer if data is valid and can be saved.
    (define save-button #f)

    (define client-pane #f)
    (define dialog-icon #f)

    (define edit-pane
      (let ((p (new vertical-panel% [parent toplevel-window]
                    [spacing 10]
                    [border 10]
                    [alignment '(left top)])))
        (let ((p0 (make-horizontal-pane p #f)))
          (set! dialog-icon
                (new message% [parent p0]
                     [label icon]
                     [stretchable-width #f]
                     [stretchable-height #f])))

        (set! client-pane
              (new vertical-panel% [parent p]
                   [border 0] [spacing 10]
                   [alignment '(left top)]))

        (let ((bp (new horizontal-pane% [parent p] [border 0]
                       [spacing 10]
                       [stretchable-height #f] [alignment '(right center)])))
          (set! save-button
                (new button% [label save-button-name] [parent bp]
                     [callback (lambda (b e) (on-save))]))
          (new button% [label "Cancel"] [parent bp]
               [callback (lambda (b e) (on-cancel))]))

        p))

    (define/public (get-client-pane)
      client-pane)

    (define/public (get-toplevel-window)
      toplevel-window)

    (define/public (set-icon icon)
      (send dialog-icon set-label icon))

    ;; Result of running the dialog, #t if data was saved, #f if it was not,
    ;; this is what edit-equipment returns.
    (define dialog-result #f)

    ;; Check for valid data in the dialog box and enable disable the save
    ;; button based on that.  This timer only runs while the dialog box is
    ;; shown.
    (define validate-timer
      (new timer%
           [notify-callback
            (lambda () (send save-button enable (has-valid-data?)))]))

    (define (finish-edit result)
      (set! dialog-result result)
      (send toplevel-window show #f)
      (send validate-timer stop))

    (define (on-save)
      ;; Don't allow finishing the edit with invalid data.  Our validate timer
      ;; might not have caught the invalid data to disable the save button in
      ;; time...
      (when (has-valid-data?)
        (finish-edit #t)))

    (define (on-cancel)
      (finish-edit #f))

    (define/public (do-edit parent)
      (let ((old-toplevel toplevel-window))
        (let ((toplevel (if parent (make-toplevel-dialog parent) toplevel-window)))
          (send edit-pane reparent toplevel)
          (set! toplevel-window toplevel))
        (set! dialog-result #f)
        (send save-button enable (has-valid-data?))
        (send validate-timer start 1000)
        (send toplevel-window show #t) ; will block until finish-dialog is called
        (send edit-pane reparent old-toplevel)
        (set! toplevel-window old-toplevel)
        dialog-result))

    (define/public (has-valid-data?)
      #t)

    ))


;;.................................................. al-progress-dialog% ....

(define al-progress-dialog%
  (class object%
    (init-field
     title
     [icon #f]
     [can-cancel? #t]
     [min-width 400])
    (super-new)

    (define (make-toplevel-dialog parent)
      (new
       (class dialog% (init) (super-new)
         (define/augment (can-close?) can-cancel?)
         (define/augment (on-close) (on-close-dialog)))
       [label title]
       [min-width min-width]
       [parent parent]))

    (define toplevel-window (make-toplevel-dialog #f))
    (define message-box #f)
    (define progress-bar #f)
    (define update-button #f)
    (define cancel-button #f)

    (define update-thread #f)
    (define task-fn #f)

    (define start-timestamp (current-inexact-milliseconds))
    (define update-complete-flag #f)
    (define terminate-update-flag #f)
    
    (define dialog-pane
      (let ((pane (new vertical-panel%
                       [parent toplevel-window] [border 20] [spacing 5]
                       [alignment '(left top)])))
        (let ((pane (new horizontal-pane% [parent pane])))
          (new message% [parent pane] [label title]
               [font (send the-font-list find-or-create-font 12 'default 'normal 'normal)]))
        (let ((pane (new horizontal-pane% [parent pane] [border 0] [spacing 20])))
          (when icon
            (new message% [parent pane] [label icon]
                 [stretchable-height #f] [stretchable-width #f]))
          (let ((pane (new vertical-pane% [parent pane] [spacing 1] [alignment '(left top)])))
            (set! progress-bar (new gauge% [parent pane] [label ""] [range 100]))
            (set! message-box (new message% [parent pane] [label ""] [min-width 200]))))
        (let ((pane (new horizontal-pane% [parent pane] [border 0]
                         [stretchable-height #f] [alignment '(right center)])))
          (set! update-button (new button%
                                   [label "Begin Update"]
                                   [parent pane]
                                   [callback (lambda (b e) (on-begin-update))]))
          (set! cancel-button (new button% [label "Cancel"]
                                   [parent pane]
                                   [callback (lambda (b e) (on-close-dialog))])))
        pane))

    (define/public (set-message msg)
      (send message-box set-label msg))
    
    (define/public (set-progress pct)
      (when (eqv? (inexact->exact pct) 0)
        (set! start-timestamp (current-inexact-milliseconds)))
      (send progress-bar set-value (inexact->exact pct))
      (when (> pct 0)
        (let* ((elapsed (- (current-inexact-milliseconds) start-timestamp))
               (remaining (- (/ elapsed (/ pct 100)) elapsed)))
          (send message-box set-label
                (format "Remaining time: ~a (elapsed ~a)"
                        (duration->string (/ remaining 1000))
                        (duration->string (/ elapsed 1000))))))
      (not terminate-update-flag))

    (define (on-begin-update)
      (if update-complete-flag
          (on-close-dialog)
          (begin
            (send update-button enable #f)
            (set! start-timestamp (current-inexact-milliseconds))
            (unless can-cancel? (send cancel-button enable #f))
            (set! update-thread
                  (thread/dbglog
                   #:name "al-progress-dialog% worker"
                   (lambda ()
                     (task-fn this)
                     (set! update-complete-flag #t)
                     (send update-button set-label "Close")
                     (send update-button enable #t)
                     (send cancel-button enable #f)))))))

    (define (on-close-dialog)
      (when update-thread
        (set! terminate-update-flag #t)
        (thread-wait update-thread))
      (send toplevel-window show #f))

    (define/public (run parent task)
      (let ((old-toplevel toplevel-window))
        (let ((toplevel (if parent (make-toplevel-dialog parent) toplevel-window)))
          (send dialog-pane reparent toplevel)
          (set! toplevel-window toplevel))
        (send update-button set-label "Begin Update")
        (send update-button enable #t)
        (send cancel-button enable #t)
        (send message-box set-label "")
        (send progress-bar set-value 0)
        (set! update-complete-flag #f)
        (set! terminate-update-flag #f)
        (set! update-thread #f)
        (set! task-fn task)
        (send toplevel-window show #t) ; will block
        (send dialog-pane reparent old-toplevel)
        (set! toplevel-window old-toplevel)))
    
    ))


;;......................................................... tab-selector ....

;; Hold information about a label in the tab-selector%
(struct lbl (text width height ncolor mcolor scolor))

;; A widget with a similar purpose as a list-box%: it displays a set of
;; labels, allows the user to click on one to select it and sends a
;; notification that a new label was selected.  The labels can be drawn either
;; horizontal or vertical and there's a small button allowing the user to
;; switch between the two.
(define tab-selector%
  (class object%
    (init-field
     parent                             ; parent widget for this one
     ;; called when the user clicks on an item; it is called with two
     ;; parameters: the current object and the index of the object being
     ;; selected.
     [callback #f]
     ;; called when the layout of the widget changes from vertical to
     ;; horizontal or reverse; it is called with two parameters: the current
     ;; object, and a flag which is #t for vertical, #f for horizontal
     [layout-changed-cb #f]
     ;; Tag used by this object to store its preferences with
     ;; `save-visual-layout'
     [tag 'tab-selector-prefs])
    (super-new)

    ;; Font used to draw the labels
    (define font (send the-font-list find-or-create-font 11 'default 'normal 'normal
                       #f 'smoothed))
    ;; Colors for drawing the labels.  Can be overridden for individual
    ;; labels.
    (define normal-color (make-object color% #xc1 #xcd #xcd)) ; azure3
    (define mouse-over-color (make-object color% #x83 #x8b #x8b)) ; azure4
    (define selected-color (make-object color% #x13 #x90 #xff)) ; dodger blue

    (define pen (send the-pen-list find-or-create-pen normal-color 1 'transparent))
    (define normal-brush (send the-brush-list find-or-create-brush normal-color 'solid))
    (define mouse-over-brush (send the-brush-list find-or-create-brush mouse-over-color 'solid))
    (define selected-brush (send the-brush-list find-or-create-brush selected-color 'solid))

    (define label-spacing 2) ; space between labels (both horizontal and vertical)
    (define text-offset 2)   ; text offset inside the label
    (define vertical? #t)    ; when #t, layout is vertical
    (define labels '())      ; list of LBL structures
    (define lwidth #f)       ; label text width, use (label-width)
    (define lheight #f)      ; label text height, use (label-height)
    (define selected-index #f)  ; index in labels for the selected item
    (define hover-index #f)     ; index in labels for the item under the mouse
    (define expand-label #f)    ; created below
    (define collapse-label #f)  ; created below

    ;; Restore preferences here
    (let ((visual-layout (al-get-pref tag (lambda () '(gen1 #t)))))
      (when (and visual-layout (list? visual-layout) (> (length visual-layout) 1))
        ;; Visual layout is valid, check version
        (when (eq? (car visual-layout) 'gen1)
          (match-define (list 'gen1 v?) visual-layout)
          (set! vertical? v?))))

    ;; Returns the width of the label text.  Does the right thing if the
    ;; layout is vertical.
    (define (label-width) (if vertical? lheight lwidth))
    ;; Returns the height of the label text.  Does the right thing if the
    ;; layout is vertical.
    (define (label-height) (if vertical? lwidth lheight))
    ;; Returns the width of the label box.  Does the right thing if the layout
    ;; is vertical.
    (define (box-width) (+ text-offset (label-width) text-offset))
    ;; Returns the height of the label box.  Does the right thing if the
    ;; layout is vertical.
    (define (box-height) (+ text-offset (label-height) text-offset))

    ;; Setup the drawing context DC with the right pen and brushes to draw the
    ;; label L (a LBL struct), MODE is the state in which the label should be
    ;; drawn: 'normal 'selected or 'mouse-over
    (define (setup-draw-context-for-label dc l mode)
      (case mode
        ((mouse-over)
         (if (lbl-mcolor l)
             ;; If the label specifies its own color, use it
             (let ((brush (send the-brush-list find-or-create-brush (lbl-mcolor l) 'solid)))
               (send dc set-brush brush))
             (send dc set-brush mouse-over-brush))
         (send dc set-text-foreground "black"))
        ((selected)
         (if (lbl-scolor l)
             ;; If the label specifies its own color, use it
             (let ((brush (send the-brush-list find-or-create-brush (lbl-scolor l) 'solid)))
               (send dc set-brush brush))
             (send dc set-brush selected-brush))
         (send dc set-text-foreground "white"))
        (else
         (if (lbl-ncolor l)
             ;; If the label specifies its own color, use it
             (let ((brush (send the-brush-list find-or-create-brush (lbl-ncolor l) 'solid)))
               (send dc set-brush brush))
             (send dc set-brush normal-brush))
         (send dc set-text-foreground "black"))))

    ;; Return the width and the height of the label box as two values.  Does
    ;; the right thing if LABEL is one of collapse-label and expand-label.
    (define (label-box-dimensions label)
      (if (or (eq? label collapse-label) (eq? label expand-label))
          (values
           (+ text-offset (lbl-width label) text-offset)
           (+ text-offset (lbl-height label) text-offset))
          (values (box-width) (box-height))))

    ;; Return the X Y coordinates where the text of LABEL should be drawn as
    ;; two values.  Does the right thing if LABEL is expand-label or
    ;; collapse-label and takes the layout (vertical or horizontal) into
    ;; consideration.
    (define (label-text-origin label)
      (if (or (eq? label collapse-label) (eq? label expand-label))
          (values
           (+ label-spacing text-offset)
           text-offset
           0)
          (values
           (+ label-spacing text-offset)
           (+ text-offset
              (if vertical?
                  (- (label-height) (* 0.5 (- (label-height) (lbl-width label))))
                  0))
           (if vertical? (/ pi 2) 0))))

    ;; Draw a label onto the device context DC starting at the Y coordinate.
    ;; The label is always drawn at a precomputed X coordinate.
    (define (draw-label dc l y)
      (send dc set-pen pen)
      (let ((text (lbl-text l)))
        (let-values (((w h) (label-box-dimensions l))
                     ((lx ly angle) (label-text-origin l)))
          (send dc draw-rectangle label-spacing y w h)
          (send dc draw-text text lx (+ y ly) #t 0 angle))))

    (define (on-paint canvas dc)
      (send dc clear)
      (send dc set-font font)
      (send dc set-smoothing 'smoothed)

      (if vertical?
          (begin
            (setup-draw-context-for-label
             dc
             expand-label
             (if (eq? hover-index 0) 'mouse-over 'normal))
            (draw-label dc expand-label label-spacing))
          (begin
            (setup-draw-context-for-label
             dc
             collapse-label
             (if (eq? hover-index 0) 'mouse-over 'normal))
            (draw-label dc collapse-label label-spacing)))

      (let-values (((bw bh) (label-box-dimensions (if vertical? expand-label collapse-label))))
        (for/fold ([y (+ label-spacing bh label-spacing)])
                  ([(l idx) (in-indexed labels)])
          (let ((mode (cond
                        ((eq? (add1 idx) selected-index) 'selected)
                        ((eq? (add1 idx) hover-index) 'mouse-over)
                        (#t 'normal))))
            (setup-draw-context-for-label dc l mode)
            (draw-label dc l y))
          (+ y (box-height) label-spacing))))

    (define (on-key canvas event)
      #f)

    ;; Return the index of the label that the mouse is hovering over
    ;; (according to EVENT).  Returns 0 if this is the expand or collapse
    ;; button, 1 for the first label, and #f if the mouse is not hovering over
    ;; any label.
    (define (hover-candidate canvas event)
      (let ((x (send event get-x))
            (y (send event get-y)))
        (let-values (((vx vy) (send canvas get-view-start))
                     ((cw ch) (send canvas get-virtual-size))
                     ((bw bh) (label-box-dimensions (if vertical? expand-label collapse-label))))
          (cond ((< y label-spacing) #f)
                ((< y (+ label-spacing bh))
                 ;; Maybe it is the collapse/expand button
                 (and (> x label-spacing) (< x (+ label-spacing bw))) 0)
                ((< y (+ label-spacing bh (* (length labels) (+ (box-height) label-spacing))))
                 ;; It is one of the buttons
                 (and (> x label-spacing) (< x (+ label-spacing (label-width)))
                      (add1 (exact-truncate (/ (- y (+ label-spacing bh)) (+ (box-height) label-spacing))))))
                (#t
                 #f)))))

    (define (on-mouse canvas event)
      (let ((type (send event get-event-type))
            (candidate (hover-candidate canvas event)))
        (cond ((eq? type 'motion)
               (unless (equal? candidate hover-index)
                 (set! hover-index candidate)
                 (send canvas refresh)))
              ((eq? type 'leave)
               (set! hover-index #f)
               (send canvas refresh))
              ((eq? type 'left-up)
               (cond ((not candidate)
                      ;; Mouse button not released over any button
                      #f)
                     ((eq? candidate 0)
                      (set! vertical? (not vertical?))
                      (adjust-canvas-size canvas)
                      (when layout-changed-cb (layout-changed-cb this vertical?)))
                     ((not (equal? selected-index candidate))
                      (set! selected-index candidate)
                      (when (and callback selected-index (> selected-index 0))
                        ;; NOTE that selected-index of 0 means the
                        ;; expand/collapse button, which we don't want to send
                        ;; out.
                        (callback this (sub1 selected-index)))))
               (send canvas refresh)))))

    (define (on-paint-wrapped canvas dc)
      (with-handlers
        (((lambda (x) #t)
          (lambda (x) (dbglog "al-section-selector%/on-paint-wrapped: ~a" x))))
        (send dc clear)
        (unless (null? labels)
          (on-paint canvas dc))))

    ;; Adjust the minimum canvas size so that the entire set of labels can be
    ;; displayed.  Takes layout into consideration.
    (define (adjust-canvas-size canvas)
      (let-values (((bw bh) (label-box-dimensions (if vertical? expand-label collapse-label))))
        (if (null? labels)
            (begin
              (send canvas min-client-height (exact-truncate (+ label-spacing bh label-spacing)))
              (send canvas min-client-width (exact-truncate (+ label-spacing bw label-spacing))))
            (begin
              (let ((height (+ label-spacing bh label-spacing (* (length labels) (+ (box-height) label-spacing))))
                    (width (+ label-spacing (box-width) label-spacing)))
                (send canvas min-client-height (exact-truncate height))
                (send canvas min-client-width (exact-truncate width)))))))

    (define canvas
      (new
       (class canvas% (init) (super-new)
         (define/override (on-char event) (on-key this event))
         (define/override (on-event event) (on-mouse this event)))
       [parent parent]
       [min-width 1] [min-height 1]
       [stretchable-width #f] [stretchable-height #f]
       [paint-callback on-paint-wrapped]))

    ;; Create the expand and collapse labels
    (let ((dc (send canvas get-dc)))
      (let-values (((w h x y) (send dc get-text-extent "«" font #t)))
        (set! collapse-label (lbl "«" w h "white" #f #f)))
      (let-values (((w h x y) (send dc get-text-extent "»" font #t)))
        (set! expand-label (lbl "»" w h "white" #f #f))))

    ;; Clear the contents of the widget
    (define/public (clear)
      (set! labels '())
      (set! lwidth #f)
      (set! lheight #f)
      (adjust-canvas-size canvas)
      (send canvas refresh))

    ;; Add a new label to the widget.  The normal, mouse-over and selected
    ;; colors can be overridden for this label.
    (define/public (append label-text
                           #:normal-color (normal-color #f)
                           #:mouse-over-color (mouse-over-color #f)
                           #:selected-color (selected-color #f))
      (let ((dc (send canvas get-dc)))
        (let-values (((w h x y) (send dc get-text-extent label-text font #t)))
          (let ((l (lbl label-text w h normal-color mouse-over-color selected-color)))
            (set! labels (reverse (cons l (reverse labels))))
            (set! lwidth (if lwidth (max lwidth w) w))
            (set! lheight (if lheight (max lheight h) h)))))
      (adjust-canvas-size canvas)
      (send canvas refresh))

    ;; Return the index of the currently selected item (0 is the first actual
    ;; label, NOT the collapse button).  Returns #f if no item is selected.
    (define/public (get-selection)
      (and selected-index (sub1 selected-index)))

    ;; Set the selected item to the INDEX label (0 is the first actual label,
    ;; NOT the collapse button).
    (define/public (set-selection index)
      ;; NOTE that index 0 is reserved for the expand/collapse button, our
      ;; internal indexes start at 1.
      (unless (eq? selected-index index)
        (set! selected-index (add1 index))
        (send canvas refresh)))

    ;; Select or de-select the item at INDEX depending on SELECT? (0 is the
    ;; first actual label, NOT the collapse button).
    (define/public (select index select?)
      (if (eq? selected-index index)
          ;; De-select the current item, if selected.
          (unless select?
            (set! selected-index #f)
            (send canvas refresh))
          (when select?
            (set-selection index))))

    (define/public (save-visual-layout)
      (al-put-pref tag (list 'gen1 vertical?)))

    ))

;;................................................. notification-banner% ....

;; Display a text at the top of the window with a "dismiss" button.  An
;; instance of this widget can be created as the first item in a frame%,
;; several notifications can be queued up using the `notify' method and they
;; will be dismissed by the user (once a notification is dismissed, the next
;; one is displayed).  A dismiss callback can be provided, so the application
;; can monitor when the notifications are dismissed.
(define notification-banner%
  (class object%
    (init-field
     parent                ; Parent widget for this object
     ;; Called when a notification is dismissed, with the message being
     ;; dismissed
     [dismiss-callback #f]
     ;; amount of time (milliseconds) to wait before displaying the next
     ;; notification
     [notification-delay 200])
    (super-new)

    ;; Font used to display the text
    (define font (send the-font-list find-or-create-font 11 'default 'normal 'normal
                       #f 'smoothed))

    ;; Background color for the banner
    (define bg-color (make-object color% #xff #xfa #xcd)) ; lemon chiffon
    ;; Foreground color, used to draw the text and the dismiss cross
    (define fg-color (make-object color% #x2f #x4f #x4f)) ; dark slate gray
    ;; Colors used for simulating the dismiss button
    (define hl2-color (make-object color% #xee #xe8 #xaa)) ; pale goldenrod
    (define hl-color (make-object color% #xda #xa5 #x20)) ; golden rod

    (define pen (send the-pen-list find-or-create-pen fg-color 1.5 'solid))
    (define brush (send the-brush-list find-or-create-brush hl-color 'solid))
    (define brush2 (send the-brush-list find-or-create-brush hl2-color 'solid))
    (define transparent-pen (send the-pen-list find-or-create-pen fg-color 1 'transparent))

    (define label-text #f)              ; text being currently displayed
    (define label-width #f)             ; width of the text being displayed
    (define label-height #f)            ; height of the text being displayed
    (define hoffset 20)                 ; horizontal offset of the text inside the box
    ;; minimum vertical offset of the text inside the box, used to set the
    ;; min-height of the widget only
    (define min-voffset 5)
    (define hover-close-box? #f)        ; #t when the mouse is over the dismiss button
    (define push-close-box? #f)         ; #t when the dismiss button is pushed in

    ;; List of messages waiting to be displayed.  Note that the message
    ;; currently being displayed is the first one in the list.  If the list is
    ;; empty, the banner is not visible at all.
    (define queued-messages '())

    ;; Return #t if the mouse is hovering over the dismiss button. EVENT is
    ;; received for an on-mouse event.
    (define (hover-on-close-box? canvas event)
      (let-values (((cw ch) (send canvas get-size)))
        (let ((voffset (* 0.5 (- ch label-height))))
          (let ((x1 (- cw hoffset label-height))
                (y1 voffset)
                (x2 (- cw hoffset))
                (y2 (+ voffset label-height))
                (x (send event get-x))
                (y (send event get-y)))
            (and (>= x x1) (<= x x2) (>= y y1) (<= y y2))))))

    (define (on-paint canvas dc)

      (send dc set-font font)
      (send dc set-smoothing 'smoothed)
      (send dc set-text-foreground fg-color)
      (send dc set-text-background bg-color)
      (send dc set-background bg-color)
      (send dc clear)

      (let-values (((cw ch) (send canvas get-size)))
        (let ((voffset (* 0.5 (- ch label-height))))
          (send dc draw-text label-text hoffset voffset))
        (let ((voffset (* 0.5 (- ch label-height))))
          (when (or hover-close-box? push-close-box?)
            (send dc set-pen transparent-pen)
            (send dc set-brush (if push-close-box? brush2 brush))
            (send dc draw-rectangle
                  (- cw hoffset label-height) voffset
                  label-height label-height))
          (send dc set-pen pen)
          (let ((x1 (+ (- cw hoffset label-height) (* 1/3 label-height)))
                (y1 (+ voffset (* 1/3 label-height)))
                (x2 (- cw hoffset (* 1/3 label-height)))
                (y2 (- (+ voffset label-height) (* 1/3 label-height))))
            (send dc draw-line x1 y1 x2 y2)
            (send dc draw-line x1 y2 x2 y1)))))
        
    (define (on-key canvas event)
      #f)

    (define (on-mouse canvas event)
      (let ((hover? (hover-on-close-box? canvas event)))
        (unless (eq? hover-close-box? hover?)
          (set! hover-close-box? hover?)
          (unless hover-close-box?
            (set! push-close-box? #f))
          (send canvas refresh))
        (case (send event get-event-type)
          ((leave)
           (set! hover-close-box? #f)
           (set! push-close-box? #f)
           (send canvas refresh))
          ((left-down)
           (when hover-close-box?
             (set! push-close-box? #t)
             (send canvas refresh)))
          ((left-up)
           (when hover-close-box?
             (on-dismiss)
             (set! push-close-box? #f)
             (send canvas refresh)))
          (else
           #f))))

    (define (on-paint-wrapped canvas dc)
      (with-handlers
        (((lambda (x) #t)
          (lambda (x) (dbglog "al-notification-box%/on-paint-wrapped: ~a" x))))
        (send dc clear)
        (when label-text
          (on-paint canvas dc))))

    ;;; Set the label in the banner to LABEL.  Also sets the minimum
    ;;; dimensions so that the label fits.
    (define (set-label label)
      (let ((dc (send canvas get-dc)))
        (let-values (((w h x y) (send dc get-text-extent label font #t)))
          (set! label-text label)
          (set! label-width w)
          (set! label-height h)
          ;; Setup the minimum width and height of the canvas, such that we
          ;; can display the current message
          (let ((w-min (exact-truncate (+ min-voffset label-height min-voffset)))
                (h-min (exact-truncate (+ hoffset label-width hoffset label-height hoffset))))
            (send canvas min-client-height w-min)
            (send canvas min-client-width h-min))))
      (send canvas refresh))

    ;; Show or hide the notification banner.
    (define (show-notification show?)
      (send panel change-children (lambda (old) (if show? (list canvas) '()))))

    ;; Handle on timer events.  If we have any messages in queued-messages, we
    ;; display the first one.
    (define (on-timer)
      (unless (null? queued-messages)
        (set-label (car queued-messages))
        (show-notification #t)))

    ;; Handle clicking the dismiss button: call the `dismiss-callback' and set
    ;; the timer to display the next notification message, if any.
    (define (on-dismiss)
      (when dismiss-callback (dismiss-callback (car queued-messages)))
      (show-notification #f)
      (set! queued-messages (cdr queued-messages))
      (unless (null? queued-messages)
        (send timer start notification-delay #t)))

    ;; The canvas is held inside a panel, so we can show/hide it using
    ;; `change-children'
    (define panel (new horizontal-panel% [parent parent]
                       [border 0] [spacing 0] [stretchable-height #f]))
    ;; The canvas that draws the entire widget.
    (define canvas
      (new
       (class canvas% (init) (super-new)
         (define/override (on-char event) (on-key this event))
         (define/override (on-event event) (on-mouse this event)))
       [parent panel] [style '(deleted)]
       [min-width 1] [min-height 1]
       [stretchable-width #t] [stretchable-height #f]
       [paint-callback on-paint-wrapped]))

    ;; Timer used to delay the display of the next notification.
    (define timer (new timer% [notify-callback on-timer] [just-once? #t]))

    ;; Display MESSAGE in the banner.  If no other messages are displayed, the
    ;; message will be displayed immediately, otherwise, the message will be
    ;; queued up and displayed once all previous messages have been dismissed.
    (define/public (notify message)
      (let ((empty? (null? queued-messages)))
        (set! queued-messages (reverse (cons message (reverse queued-messages))))
        (when empty? (on-timer))))
    
    ))

