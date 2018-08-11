#lang racket/base
;; edit-lap-swim.rkt -- edit recording errors in lap swimming sessions
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

(require racket/gui/base
         racket/class
         racket/format
         racket/math
         racket/match
         racket/list
         db/base
         "../sport-charms.rkt"
         "../widgets/main.rkt"
         "../utilities.rkt"
         "../fmt-util.rkt"
         "../dbutil.rkt")

(provide get-lap-swim-editor)


;;........................................................... swl-model% ....

;; A Swim Length, used by the editor to represent a length of pool swim.
(struct swl (timestamp lap-id length-id duration strokes stroke-type))

;; Produce a string representation of L, a SWL struct.  This is used in the
;; dialog to print user friendly information about a SWL.
(define (swl->string l)
  (format "~a ~a seconds~a~a"
          (get-swim-stroke-name (swl-stroke-type l))
          (~r (swl-duration l) #:precision 1)
          (if (and (swl-strokes l) (> (swl-strokes l) 0))
              (format ", ~a strokes" (swl-strokes l))
              "")
          (if (swl-length-id l) "" ", newly created")))

;; Split a swim length L into two equal swim lengths.  Returns the two new
;; swim lengths as two values.
(define (swl-split l)
  (let ((new-duration (* 0.5 (swl-duration l)))
        (new-strokes (let ((s (swl-strokes l))) (if s (* 0.5 s) #f))))
    (values
     (struct-copy swl l
                  (length-id #f)
                  (duration new-duration)
                  (strokes new-strokes))
     (struct-copy swl l
                  (length-id #f)
                  (timestamp (exact-round (+ (swl-timestamp l) new-duration)))
                  (duration new-duration)
                  (strokes new-strokes)))))

;; Join two swim lengths.  The returning swim length object is the sum
;; (duration, strokes, etc) of the two.
(define (swl-join l1 l2)
  (unless (equal? (swl-lap-id l1) (swl-lap-id l2))
    (error "cannot join lengths, mismatched lap id"))
  (unless (equal? (swl-stroke-type l1) (swl-stroke-type l2))
    (error "cannot join lengths, mismatched stroke type"))
  (struct-copy swl l1
               (length-id #f)
               (duration (+ (swl-duration l1) (swl-duration l2)))
               (strokes (+ (swl-strokes l1) (swl-strokes l2)))))

;; SQL statement to fetch swim lengths for a swimming activity.
(define swim-lengths-sql
  (virtual-statement
   (lambda (dbsys)
   "
select LE.start_time as timestamp,
       LA.id as lap_id,
       LE.id as length_id,
       SS.total_timer_time as duration,
       SS.total_cycles as strokes,
       SS.swim_stroke_id as stype
  from A_LAP LA, A_LENGTH LE, SECTION_SUMMARY SS
 where LA.session_id = ?
   and LE.lap_id = LA.id
   and LE.summary_id = SS.id
 order by LE.start_time")))

;; Read swim lengths from the database, DB for a session id, SID.  Returns a
;; hash mapping the timestamp to a SWL struct.  In our model, SWL's will be
;; referenced by their timestamp, which we assume to be unique inside a
;; session.
(define (read-swim-lengths db sid)
  (define (convert-sql-null v) (if (sql-null? v) #f v))
  (define rows (query-rows db swim-lengths-sql sid))
  (define result (make-hash))
  (for ([row rows])                ; NOTE: for/hash produces an immutable hash
    (let ((item (swl (vector-ref row 0)
                     (vector-ref row 1)
                     (vector-ref row 2)
                     (vector-ref row 3)
                     (convert-sql-null (vector-ref row 4))
                     (convert-sql-null (vector-ref row 5)))))
    (hash-set! result (swl-timestamp item) item)))
  result)

(define (next-timestamp data timestamp)
  (let ((ts (sort (hash-keys data) <)))
    (cond ((member timestamp ts)
           => (lambda (v) (if (null? (cdr v)) #f (car (cdr v)))))
          (#t #f))))

(define (prev-timestamp data timestamp)
  (let ((ts (sort (hash-keys data) <)))
    (cond ((null? ts) #f)
          ((equal? timestamp (car ts)) #f) ; no previous timestamp
          (#t
           (for/first ([p ts] [n (cdr ts)] #:when (equal? n timestamp)) p)))))

;; Return the list of SWL that have the same LAP-ID.  DATA is a hash mapping
;; timestamp to SWL structs.
;;
;; WARNING: The returned lengths are not ordered by timestamp!
(define (all-lap-lengths data lap-id)
  (for/list (((k v) (in-hash data)) #:when (equal? (swl-lap-id v) lap-id))
    v))

;; Hold a swim activity an allow operating on the individual lengths: join,
;; split, delete, change stroke operations, also allows undo operations.  The
;; data is modified in memory and a plan is prepared which can be executed
;; against the databse by 'fixup-swim-session' to make the changes permanent.
(define swl-model%
  (class object% (init database session-id) (super-new)

    ;; Contains a plan for fixing a swim activity in the database.  The plan
    ;; will be executed by 'fixup-swim-session' which will make the changes
    ;; permanent.  Note that the plan stored here is in reverse order, use
    ;; 'get-plan'
    (define plan '())

    ;; A list of undo functions, most recent first.  Each function will revert
    ;; one operation performed on the model.
    (define undo-stack '())

    ;; This is the current view of the swim data, edited by swl-model%
    ;; methods.
    (define swim-data (read-swim-lengths database session-id))

    ;; Return the list of SWL objects based on SWIM-DATA.  The list is orderer
    ;; by their timestamp.
    (define/public (get-data)
      (for/list ([idx (sort (hash-keys swim-data) <)])
        (hash-ref swim-data idx)))

    ;; Return the number of active lengths in the session.  These are actual
    ;; swimming lengths, the activity itself also records lengths for rest
    ;; intervals.
    (define/public (get-num-lengths)
      (for/sum (([k v] (in-hash swim-data)) #:when (swl-strokes v))
        1))

    ;; Split an ITEM and re-balance all the lengths in the lap.  This is used
    ;; for drill laps, which are not counted individually, but the lap time is
    ;; divided equally with the number of lengths.
    (define (split/rebalance item)
      (let* ((items (all-lap-lengths swim-data (swl-lap-id item)))
             (stimestamp (for/fold ([m (current-seconds)]) ((item items))
                           (min m (swl-timestamp item))))
             (nitems (length items))
             (ntime (for/sum ([item items]) (swl-duration item)))
             (nstrokes (for/sum ([item items]) (swl-strokes item)))
             ;; NOTE: should check that all items have the same stroke type...
             (stype (swl-stroke-type item)))
        (let ((time (/ ntime (add1 nitems)))
              (strokes (exact-round (/ nstrokes (add1 nitems)))))
          (let ((nitems (for/list ([index (in-range (add1 nitems))])
                          (let ((timestamp (exact-round (+ stimestamp (* index time)))))
                            ;; NOTE: new items are created with a lap-id of 0,
                            ;; so they can be operated on.  This is OK, as the
                            ;; re-balance happens on the LAP-ID and can be
                            ;; re-done any number of times.
                            (swl timestamp (swl-lap-id item) 0 time strokes stype)))))
            (for ([item items]) (hash-remove! swim-data (swl-timestamp item)))
            (for ([item nitems]) (hash-set! swim-data (swl-timestamp item) item))
            (set! plan (cons (list 'split/rebalance (swl-lap-id item)) plan))
            (set! undo-stack
                  (cons
                   (lambda ()
                     (for ([item nitems]) (hash-remove! swim-data (swl-timestamp item)))
                     (for ([item items]) (hash-set! swim-data (swl-timestamp item) item))
                     (set! plan (cdr plan)))
                   undo-stack))))))

    (define (split item)
      ;; A newly created item (by a join or a split) cannot be split again.
      ;; This is a limitation of our editor implementation...
      (unless (swl-lap-id item) "split: no lap id")
      (let-values (([s1 s2] (swl-split item)))
        (hash-remove! swim-data (swl-timestamp item))
        (hash-set! swim-data (swl-timestamp s1) s1)
        (hash-set! swim-data (swl-timestamp s2) s2)
        (set! plan (cons (list 'split (swl-length-id item)) plan))
        (set! undo-stack
              (cons
               (lambda ()
                 (hash-remove! swim-data (swl-timestamp s1))
                 (hash-remove! swim-data (swl-timestamp s2))
                 (hash-set! swim-data (swl-timestamp item) item)
                 (set! plan (cdr plan)))
               undo-stack))))

    ;; Split the length at TIMESTAMP.  If this length is part of a drill lap,
    ;; all other lengths are re-balanced, otherwise, the length is simply
    ;; split and the two resulting lengths will replace it in the lap.
    (define/public (split-at timestamp)
      (let ((item (hash-ref swim-data timestamp #f)))
        (unless item (error "item not found"))
        (if (equal? 4 (swl-stroke-type item))
            (split/rebalance item)
            (split item))))

    ;; Return #t if the length at TIMESTAMP can be joined with the following
    ;; or previous length (DIRECTION 'next or 'prev).  Lengths can only be
    ;; joined withing the same lap, so the last length in a lap cannot be
    ;; joined with the next one and the first length in a lap cannot be joined
    ;; with a previous one.
    (define/public (can-join? timestamp direction)
      (let* ((fn (if (eq? direction 'next) next-timestamp prev-timestamp))
             (other (fn swim-data timestamp)))
        (and other
             (let ((l1 (hash-ref swim-data timestamp #f))
                   (l2 (hash-ref swim-data other #f)))
               (and l1 l2
                    (equal? (swl-lap-id l1) (swl-lap-id l2))
                    (equal? (swl-stroke-type l1) (swl-stroke-type l2)))))))

    ;; Join an ITEM and re-balance all the lengths in the lap.  This is used
    ;; for drill laps, which are not counted individually, but the lap time is
    ;; divided equally with the number of lengths.
    ;;
    ;; NOTE: This is 99% identical to split/rebalance and should be
    ;; refactored.
    (define (join/rebalance item)
      (let* ((items (all-lap-lengths swim-data (swl-lap-id item)))
             (stimestamp (for/fold ([m (current-seconds)]) ((item items))
                           (min m (swl-timestamp item))))
             (nitems (length items))
             (ntime (for/sum ([item items]) (swl-duration item)))
             (nstrokes (for/sum ([item items]) (swl-strokes item)))
             ;; NOTE: should check that all items have the same stroke type...
             (stype (swl-stroke-type item)))
        (let ((time (/ ntime (sub1 nitems)))
              (strokes (exact-round (/ nstrokes (sub1 nitems)))))
          (let ((nitems (for/list ([index (in-range (sub1 nitems))])
                          (let ((timestamp (exact-round (+ stimestamp (* index time)))))
                            ;; NOTE: new items are created with a lap-id of 0,
                            ;; so they can be operated on.  This is OK, as the
                            ;; re-balance happens on tha LAP-ID and can be
                            ;; re-done any number of times.
                            (swl timestamp (swl-lap-id item) 0 time strokes stype)))))
            (for ([item items]) (hash-remove! swim-data (swl-timestamp item)))
            (for ([item nitems]) (hash-set! swim-data (swl-timestamp item) item))
            (set! plan (cons (list 'join/rebalance (swl-lap-id item)) plan))
            (set! undo-stack
                  (cons
                   (lambda ()
                     (for ([item nitems]) (hash-remove! swim-data (swl-timestamp item)))
                     (for ([item items]) (hash-set! swim-data (swl-timestamp item) item))
                     (set! plan (cdr plan)))
                   undo-stack))))))

    ;; Join ITEM with the 'next or 'prev one based on DIRECTION.  See also can-join?
    (define (join item direction)
      ;; A newly created item (by a join or a split) cannot be joined again.
      ;; This is a limitation of our editor implementation...
      (unless (swl-lap-id item) "join: no lap id")
      (let* ((fn (if (eq? direction 'next) next-timestamp prev-timestamp))
             (other (fn swim-data (swl-timestamp item)))
             (l1 item)
             (l2 (hash-ref swim-data other #f))
             (nl (swl-join l1 l2))
             (nts (if (eq? direction 'next) (swl-timestamp item) other)))
        (hash-remove! swim-data (swl-timestamp item))
        (hash-remove! swim-data other)
        (hash-set! swim-data nts nl)
        (set! plan (cons (list 'join (swl-length-id l1) (swl-length-id l2)) plan))
        (set! undo-stack
              (cons
               (lambda ()
                 (hash-remove! swim-data nts)
                 (hash-set! swim-data (swl-timestamp l1) l1)
                 (hash-set! swim-data (swl-timestamp l2) l2)
                 (set! plan (cdr plan)))
               undo-stack))))

    ;; Join the length at TIMESTAMP with the next or prev one based on
    ;; DIRECTION.  If the length is part of a drill lap, all other lengths
    ;; will be re-balanced, otherwise, the newly created length will replace
    ;; the two joined ones.
    (define/public (join-at timestamp direction)
      (if (can-join? timestamp direction)
          (let ((item (hash-ref swim-data timestamp #f)))
            (if (equal? 4 (swl-stroke-type item))
                (join/rebalance item)
                (join item direction)))
          (error "cannot join")))

    (define (delete item)
      ;; A newly created item (by a join or a split) cannot be deleted.  Use
      ;; UNDO. This is a limitation of our editor implementation...
      (unless (swl-lap-id item) "delete: no lap id")
      (hash-remove! swim-data (swl-timestamp item))
      (set! plan (cons (list 'delete (swl-length-id item)) plan))
      (set! undo-stack
            (cons
             (lambda ()
               (hash-set! swim-data (swl-timestamp item) item)
               (set! plan (cdr plan)))
             undo-stack)))

    ;; Delete the length at TIMESTAMP.  If the lap is a drill lap, rebalancing
    ;; will happen, otherwise the item is simply deleted.
    (define/public (delete-at timestamp)
      (let ((item (hash-ref swim-data timestamp #f)))
        (unless item (error "item not found"))
        (if (equal? 4 (swl-stroke-type item))
            (join/rebalance item)       ; same as a join, really
            (delete item))))

    ;; Change the stroke of the lap at TIMESTAMP.
    (define/public (change-stroke-at timestamp new-stroke)
      (let ((item (hash-ref swim-data timestamp #f)))
        (unless item (error "item not found"))
        (let ((nitem (struct-copy swl item (stroke-type new-stroke))))
          (hash-set! swim-data timestamp nitem)
          (set! plan (cons (list 'change-stroke (swl-length-id item) new-stroke) plan))
          (set! undo-stack
                (cons
                 (lambda ()
                   (hash-set! swim-data timestamp item)
                   (set! plan (cdr plan)))
                 undo-stack)))))

    ;; Return #t if there are any operations to undo.
    (define/public (can-undo?)
      (not (empty? undo-stack)))

    ;; Undo the last operation on the model
    (define/public (undo)
      (unless (empty? undo-stack)
        (let ((op (car undo-stack)))
          (op)
          (set! undo-stack (cdr undo-stack)))))

    ;; Return the plan that will apply to the database the changes made to
    ;; this model.  The plan can be executed with 'fixup-swim-session'
    (define/public (get-plan) (reverse plan))

    ))


;;............................................................ swl-view% ....

(define item-width 20)
(define item-spacing 10)
(define view-horizontal-margin 40)
(define view-top-margin 30)

;; Return the with in pixels required to represent DATA.
(define (get-view-width data)
  (let ((nitems (length data)))
    (+ view-horizontal-margin
       view-horizontal-margin
       (* item-width nitems)
       (* item-spacing (sub1 nitems)))))

(define (maybe-adjust-virtual-size canvas data)
  (let-values (([vx vy] (send canvas get-view-start))
               ([vw vh] (send canvas get-virtual-size))
               ([cw ch] (send canvas get-size)))
    (let ([required-width (get-view-width data)])
      (unless (= vw required-width)
        ;; Compute a new scroll position, such that we view the same area in
        ;; the canvas when we change the virtual size.
        (let* ((vscroll (min 1.0 (exact->inexact (/ vx (- required-width cw))))))
          (send canvas init-auto-scrollbars required-width #f vscroll 0))))))

(define (get-max-item-height data)
  (for/fold ([max-height 0]) ([item data])
    (max max-height (swl-duration item))))

;; This is a widget that displays the data from a swl-model% and provides
;; information about the selected item.
(define swl-view%
  (class object%
    (init parent)
    (init-field [on-hover-callback #f] [on-select-callback #f])
    (super-new)

    (define model #f)                   ; a swl-model% object

    ;; list of SWL structs as returned by swl-model%/get-data
    (define data #f)
    ;; max height (as duration) of the lengths in DATA.
    (define max-item-height #f)
    ;; index in DATA of the item under the mouse cursor
    (define hover-index #f)
    ;; index in DATA of the item that was selected (clicked on).
    (define selected-index #f)

    (define/public (set-model m)
      (set! model m)
      (refresh))

    ;; Return the selected item, but only if it is valid. Rest lengths are not
    ;; valid and if the mouse would select one of these, they would not be
    ;; returned.
    (define/public (get-selected-item)
      (if selected-index
          (let ((item (list-ref data selected-index)))
            (if (swl-strokes item) ; don't return the item if it is a "rest" block
                item
                #f))
          #f))

    ;; Return the item under the mouse cursor, but only if it is valid. Rest
    ;; lengths are not valid and if the mouse hovers above one of these, they
    ;; would not be returned.
    (define/public (get-hover-item)
      (if hover-index
          (let ((item (list-ref data hover-index)))
            (if (swl-strokes item) ; don't return the item if it is a "rest" block
                item
                #f))
          #f))

    (define (on-key canvas event)
      #f)

    ;; Track the mouse inside this widget, detemine the item that is selected
    ;; or hovered on and call any callbacks if needed.
    (define (on-mouse canvas event)
      (let ((type (send event get-event-type))
            (x (send event get-x))
            (y (send event get-y)))
        (cond ((eq? type 'motion)
               (let-values (((vx vy) (send canvas get-view-start))
                            ((cw ch) (send canvas get-virtual-size)))
                 (let ((candidate
                        (if (< (+ vx x) view-horizontal-margin)
                            -1
                            (exact-truncate
                             (/ (- (+ vx x) view-horizontal-margin)
                                (+ item-width item-spacing))))))
                   (if (and (>= candidate 0) (< candidate (length data)))
                       (let-values (((bx by bw bh) (get-item-box (list-ref data candidate) candidate ch)))
                         ;; Check if we are actually inside this item!
                         (when (or (> (+ x vx) (+ bx bw))
                                   (< (+ y vy) by)
                                   (> (+ y vy) (+ by bh)))
                           (set! candidate #f)))
                       (set! candidate #f))
                   (unless (equal? candidate hover-index)
                     (set! hover-index candidate)
                     (when on-hover-callback (on-hover-callback (get-hover-item))))
                   (send canvas refresh))))
              ((eq? type 'leave)
               (set! hover-index #f)
               (when on-hover-callback (on-hover-callback (get-hover-item)))
               (send canvas refresh))
              ((eq? type 'left-up)
               (unless (equal? selected-index hover-index)
                 (set! selected-index hover-index)
                 (when on-select-callback (on-select-callback (get-selected-item)))
                 (send canvas refresh)))
              )))

    ;; Return the X, Y, WIDTH, HEIGHT of ITEM at INDEX.
    (define (get-item-box item index canvas-height)
      (let* ((x-pos (+ view-horizontal-margin
                       (* item-width index)
                       (if (> index 0) (* item-spacing index) 0)))
             (slice (/ (swl-duration item) max-item-height))
             (height (* (- canvas-height view-top-margin) slice))
             (y-pos (- canvas-height height)))
        (values x-pos y-pos item-width height)))

    ;; Return the brush used to paint an item, this depends on the swim stroke
    ;; and whether the item is a newly created one or not.
    (define (get-item-brush item index)
      (let ((color (if #f "red" (get-swim-stroke-color (swl-stroke-type item)))))
        (send the-brush-list find-or-create-brush color
              ;; Items that were already split/joined are drawn in a special
              ;; way
              (if (swl-length-id item) 'opaque 'crossdiag-hatch))))

    ;; Return the pen used to draw the outline of an item.  This depends on
    ;; whether the item is under the mouse or is selected.
    (define (get-item-pen item index)
      (cond ((equal? selected-index index)
             (send the-pen-list find-or-create-pen "black" 3 'solid))
            ((equal? hover-index index)
             (send the-pen-list find-or-create-pen "black" 2 'solid))
            (#t
             (send the-pen-list find-or-create-pen "black" 1 'transparent))))

    ;; Draw an individual item
    (define (draw-item dc item index canvas-height)
      (let-values (((x y w h) (get-item-box item index canvas-height)))
        (send dc set-brush (get-item-brush item index))
        (send dc set-pen (get-item-pen item index))
        (send dc draw-rectangle x y w h)))

    ;; Draw a lap marker to delimit laps.
    (define (draw-lap-marker dc lap-num index canvas-height)
      (let ((x-pos (+ view-horizontal-margin
                      (* item-width index)
                      (if (> index 0) (* item-spacing index) 0)
                      (- (* item-spacing 0.5)))))
        (send dc set-pen (send the-pen-list find-or-create-pen "gray" 0.5 'long-dash))
        (send dc draw-line x-pos 0 x-pos canvas-height)
        (send dc set-font (send the-font-list find-or-create-font 10 'default 'normal 'normal))
        (send dc draw-text (format "Lap ~a" lap-num) (+ x-pos (* item-spacing 0.5)) 5)))

    ;; Paint the widtget.
    (define (on-paint canvas dc)
      (maybe-adjust-virtual-size canvas data)
      (send dc clear)
      (let ((lap-id #f)
            (lap-num 0))
        (let-values (([cw ch] (send canvas get-virtual-size)))
          (send dc set-pen (send the-pen-list find-or-create-pen "gray" 0.5 'solid))
          (send dc set-font (send the-font-list find-or-create-font 9 'default 'normal 'normal))
          (send dc set-text-foreground "gray")
          (for ([timestamp (in-range 20 max-item-height 20)])
            (let ((ypos (* (- 1 (/ timestamp max-item-height)) ch)))
              (send dc draw-line 0 ypos cw ypos)
              (send dc draw-text (~a timestamp "s") 0 (+ ypos 2))))
          (for (([item index] (in-indexed data)) #:when (swl-strokes item))
            (unless (equal? (swl-lap-id item) lap-id)
              (set! lap-num (add1 lap-num))
              (set! lap-id (swl-lap-id item))
              (draw-lap-marker dc lap-num index ch))
            (draw-item dc item index ch)))))

    (define (on-paint-wrapped canvas dc)
      (with-handlers
        (((lambda (x) #t)
          (lambda (x) (dbglog-exception "swl-view%/on-paint-wrapped" x))))
        (when data (on-paint canvas dc))))

    (define canvas
      (new
       (class canvas% (init) (super-new)
         (define/override (on-char event) (on-key this event))
         (define/override (on-event event) (on-mouse this event)))
       [parent parent]
       [style '(hscroll)]
       [paint-callback on-paint-wrapped]))

    (define/public (refresh)
      (set! data (send model get-data))
      (set! selected-index #f)
      (set! hover-index #f)
      (set! max-item-height (get-max-item-height data))
      (send canvas refresh)
      (on-hover-callback #f)
      (on-select-callback #f))

    ))


;;....................................................... lap-swim-edit% ....

(define message-font
  (send the-font-list find-or-create-font 12 'default 'normal 'normal))

(define lap-swim-edit%
  (class edit-dialog-base%
    (init)
    (super-new [title "Edit Swim Lengths"] [icon (edit-icon)])

    (define msg-headline #f)
    (define msg-start-time #f)
    (define msg-duration #f)
    (define msg-distance #f)
    (define msg-pool-length #f)

    (define pool-length 0)

    (define swl-view #f)
    (define swl-model #f)

    ;; Brief description about the selected or hovered swim length, see
    ;; swl->string.
    (define swl-desc #f)
    ;; When #t, a swim length is selected, and 'swl-desc' contains information
    ;; about it.  'swl-desc' will not be updated when the mouse hovers over
    ;; another swim length.
    (define swl-desc-locked #f)

    (define btn-undo #f)
    (define choice-stroke #f)
    (define btn-split #f)
    (define btn-join-next #f)
    (define btn-join-prev #f)
    (define btn-delete #f)

    (define swim-stroke-types (get-swim-stroke-names))

    ;; Things to do when a new swim length is selected in the view.  Enable /
    ;; Disable the various edit buttons and set the description label for it,
    ;; the label is also locked, so that hovering over other swim lengths will
    ;; not update it.
    (define (on-swl-selected swl)
      (send swl-desc set-label (if swl (swl->string swl) ""))
      (set! swl-desc-locked (not (eq? swl #f)))
      (send btn-undo enable (send swl-model can-undo?))
      (send choice-stroke clear)
      (when swl
        (for ((s swim-stroke-types))
          (send choice-stroke append (cdr s)))
        (let ((sindex (for/first (([s idx] (in-indexed swim-stroke-types))
                                  #:when (equal? (car s) (swl-stroke-type swl)))
                        idx)))
          (if sindex
              (send choice-stroke set-selection sindex)
              (begin
                (send choice-stroke append "Unknown")
                (send choice-stroke set-selection (length swim-stroke-types))))))
      (if (and swl (swl-length-id swl) (swl-strokes swl) (> (swl-strokes swl) 0))
          (send choice-stroke enable #t)
          (send choice-stroke enable #f))
      (send btn-split enable (and swl (swl-length-id swl) #t))
      (send btn-join-next enable
            (and swl
                 (swl-length-id swl)
                 (send swl-model can-join? (swl-timestamp swl) 'next)))
      (send btn-join-prev enable
            (and swl
                 (swl-length-id swl)
                 (send swl-model can-join? (swl-timestamp swl) 'prev)))
      (send btn-delete enable (and swl (swl-length-id swl) #t)))

    ;; Things to do when the mouse comes over a new swim length.  Update the
    ;; description label, if it is not locked (it is locked when a SWL is
    ;; selected).
    (define (on-swl-hovered swl)
      (unless swl-desc-locked
        (send swl-desc set-label (if swl (swl->string swl) ""))))

    ;; Update any relevant widtgets after an edit operation on the swl-model.
    (define (after-edit)
      (send msg-distance set-label
            (short-distance->string (* pool-length (send swl-model get-num-lengths))))
      (send swl-view refresh))
      
    (define (on-split b e)
      (let ((item (send swl-view get-selected-item)))
        (when item
          (send swl-model split-at (swl-timestamp item))
          (after-edit))))

    (define (on-join-next b e)
      (let ((item (send swl-view get-selected-item)))
        (when item
          (when (send swl-model can-join? (swl-timestamp item) 'next)
            (send swl-model join-at (swl-timestamp item) 'next)
            (after-edit)))))

    (define (on-join-prev b e)
      (let ((item (send swl-view get-selected-item)))
        (when item
          (when (send swl-model can-join? (swl-timestamp item) 'prev)
            (send swl-model join-at (swl-timestamp item) 'prev)
            (after-edit)))))

    (define (on-delete b e)
      (let ((item (send swl-view get-selected-item)))
        (when item
          (send swl-model delete-at (swl-timestamp item))
          (after-edit))))
    
    (define (on-change-stroke c e)
      (let ((selection (send c get-selection))
            (item (send swl-view get-selected-item)))
        (when item
          (send swl-model change-stroke-at (swl-timestamp item)
                (car (list-ref swim-stroke-types selection)))
          (after-edit))))

    (define (on-undo b e)
      (send swl-model undo)
      (after-edit))

    (let ((p (send this get-client-pane)))
      (let ((hp (make-horizontal-pane p #f)))
        (new message% [parent hp] [label "Pool Swim: "]
             [stretchable-width #f])
        (set! msg-headline (new message% [parent hp] [label "Untitled"]
                            [font message-font]
                            [stretchable-width #t])))
      (let ((hp (make-horizontal-pane p #f)))
        (new message% [parent hp] [label "Start time: "] [stretchable-width #f])
        (set! msg-start-time
              (new message% [parent hp] [label ""] [font message-font]
                   [min-width 150]
                   [stretchable-width #t]))
        (new message% [parent hp] [label "Duration: "] [stretchable-width #f])
        (set! msg-duration
              (new message%
                   [parent hp] [label ""] [font message-font]
                   [min-width 80]
                   [stretchable-width #t]))
        (new message% [parent hp] [label "Distance: "])
        (set! msg-distance
              (new message% [parent hp] [label "8888"]
                   [min-width 80]
                   [font message-font] [stretchable-width #t]))
        (new message% [parent hp] [label "Pool length: "])
        (set! msg-pool-length
              (new message% [parent hp] [label "8888"]
                   [min-width 80]
                   [font message-font] [stretchable-width #t])))
      (let ((hp (make-horizontal-pane p #f)))
        (set! btn-undo (new button% [parent hp] [label "Undo"] [callback on-undo]))
        (set! choice-stroke (new choice% [parent hp] [label "Stroke: "] [choices '("XXXXXXXXXXXXX")]
                                 [callback on-change-stroke]))
        (set! btn-split (new button% [parent hp] [label "Split"] [callback on-split]))
        (set! btn-join-next (new button% [parent hp] [label "Join with Next"] [callback on-join-next]))
        (set! btn-join-prev (new button% [parent hp] [label "Join with Previous"] [callback on-join-prev]))
        (set! btn-delete (new button% [parent hp] [label "Delete"] [callback on-delete])))
      (let ((hp (make-horizontal-pane p #t)))
        (send hp min-height 200)
        (set! swl-view (new swl-view% [parent hp]
                            [on-hover-callback on-swl-hovered]
                            [on-select-callback on-swl-selected])))

      (let ((hp (make-horizontal-pane p #f)))
        (set! swl-desc (new message% [parent hp] [label ""]
                            [stretchable-width #t]
                            [font message-font])))
      )

    (define (setup database sid)
      (define row (query-row database
                             "select S.name as headline,
                                     S.start_time as start_time,
                                     S.pool_length as pool_length,
                                     SS.total_timer_time as duration,
                                     SS.total_distance as distance
                                from A_SESSION S, SECTION_SUMMARY SS
                               where S.summary_id = SS.id
                                 and S.id = ?" sid))
      (send msg-headline set-label (sql-column-ref row 0 ""))
      (send msg-start-time set-label (date-time->string (sql-column-ref row 1 (current-seconds))))
      (send msg-pool-length set-label (short-distance->string (sql-column-ref row 2 0) #t))
      (send msg-duration set-label (duration->string (sql-column-ref row 3 0)))
      (set! pool-length (sql-column-ref row 2 0))
      (set! swl-model (new swl-model% [database database] [session-id sid]))
      (send msg-distance set-label
            (short-distance->string (* pool-length (send swl-model get-num-lengths))))
      (send swl-view set-model swl-model)
      (on-swl-selected #f))

    (define/override (has-valid-data?)
      ;; Only allow saving if we actually have a plan to execute.
      (> (length (send swl-model get-plan)) 0))

    (define/public (begin-edit parent database sid)
      (setup database sid)
      (let ((result (send this do-edit parent)))
        (if (and result (has-valid-data?))
            (let ((plan (send swl-model get-plan)))
              (fixup-swim-session database sid plan)
              #t)
            #f)))

    ))


;;............................................................. swim-fix ....

;; Code here can execute plans produced by swl-model% to make the actual edits
;; permanent in the database. The entry point is 'fixup-swim-session'

;; Hold data about a swim length, while we operate on it.
(struct ldata (lap-id length-id timestamp duration elapsed stroke-count stroke-type)
  #:transparent)

(define fetch-ldata-sql
  (virtual-statement
   (lambda (dbsys)
     "select L.lap_id, L.start_time, SS.total_timer_time,
       SS.total_elapsed_time, SS.total_cycles, SS.swim_stroke_id
from SECTION_SUMMARY SS, A_LENGTH L
where L.summary_id = SS.id and L.id = ?")))

;; Construct a LDATA object from the database for LENGTH-ID
(define (fetch-length-data db length-id)
  (match-define (vector lap-id timestamp duration elapsed stroke-count stroke-type)
    (query-row db fetch-ldata-sql length-id))
  (ldata lap-id length-id timestamp duration elapsed stroke-count stroke-type))

(define store-ldata1-sql
  (virtual-statement
   (lambda (dbsys)
     "insert into SECTION_SUMMARY(
    total_timer_time, total_elapsed_time, total_cycles,
    swim_stroke_id, avg_speed, avg_cadence)
values (?, ?, ?, ?, ?, ?)")))

(define store-ldata2-sql
  (virtual-statement
   (lambda (dbsys)
     "insert into A_LENGTH(lap_id, start_time, summary_id) values (?, ?, ?)")))

(define store-ldata3-sql
  (virtual-statement
   (lambda (dbsys)
     "insert into A_TRACKPOINT(length_id, timestamp, speed) values (?, ?, ?)")))

;; Insert a new length in the database based on LDATA, and return the length
;; ID. The lap will now be outdated, and will need to be updated.
(define (insert-length db pool-length ldata)
  (let ((avg-cadence (exact-truncate (* 60 (/ (ldata-stroke-count ldata) (ldata-elapsed ldata)))))
        (avg-speed (/ pool-length (ldata-elapsed ldata))))
    (query-exec db store-ldata1-sql
                (ldata-duration ldata)
                (ldata-elapsed ldata)
                (ldata-stroke-count ldata)
                (ldata-stroke-type ldata)
                avg-speed
                avg-cadence)
    (let ((summary-id (db-get-last-pk "SECTION_SUMMARY" db)))
      (query-exec db store-ldata2-sql
                  (ldata-lap-id ldata)
                  (ldata-timestamp ldata)
                  summary-id)
      (let ((length-id (db-get-last-pk "A_LENGTH" db)))
        (query-exec db store-ldata3-sql
                    length-id
                    (+ (ldata-timestamp ldata) (ldata-duration ldata))
                    avg-speed)
        length-id))))

;; Delete a length from the database, returns the LAP-ID that contained this
;; length.  The lap will now be outdated and will need to be updated.
(define (delete-length db length-id)
  (let ((lap-id (query-value db "select lap_id from A_LENGTH where id = ?" length-id)))
    (query-exec db "\
delete from SECTION_SUMMARY
 where id in (select L.summary_id from A_LENGTH L where L.id = ?)" length-id)
    (query-exec db "delete from A_TRACKPOINT where length_id = ?" length-id)
    (query-exec db "delete from A_LENGTH where id = ?" length-id)
    lap-id))

;; Change the stroke type for LENGTH-ID to NEW-STROKE, the LAP-ID is also
;; updated, if neded.
(define (change-length-stroke db length-id new-stroke)
  (query-exec
   db
   "update SECTION_SUMMARY
       set swim_stroke_id = ?
    where id in (select L.summary_id from A_LENGTH L where L.id = ?)"
   new-stroke length-id)

  ;; Check if the lap contains lengths of the same stroke or not.  Update the
  ;; lap stroke based on this.  We set it to Mixed (5) if the lap now contains
  ;; mixed strokes, note that we don't check for meddley (IM, 6).
  (let ((lap-strokes (query-list db "\
select SS.swim_stroke_id
  from SECTION_SUMMARY SS, A_LENGTH L1, A_LENGTH L2
 where L1.summary_id = SS.id
   and L1.lap_id = L2.lap_id
   and L2.id = ?" length-id)))
    (let ((same-stroke? (for/and ([s lap-strokes]) (equal? s new-stroke))))
      (query-exec
       db
       "update SECTION_SUMMARY
           set swim_stroke_id = ?
         where id in (select P.summary_id from A_LAP P, A_LENGTH L
                       where L.lap_id = P.id and L.id = ?)"
       (if same-stroke? new-stroke 5)
       length-id)))
  ;; lap summary does not need update
  #f)

;; Join two lengths L1, L2 (A_LENGTH.ID).  The resulting length is inserted
;; into the database, and the previous ones are removed.  The lap will now be
;; outdated, and will need to be updated.
(define (join-lengths db pool-length l1 l2)
  (let ((ldata1 (fetch-length-data db l1))
        (ldata2 (fetch-length-data db l2)))
    (unless (= (ldata-stroke-type ldata1) (ldata-stroke-type ldata2))
      (error "stroke mismatch"))
    (unless (= (ldata-lap-id ldata1) (ldata-lap-id ldata2))
      (error "lap mismatch"))
    (delete-length db l1)
    (delete-length db l2)
    (let ((timestamp (min (ldata-timestamp ldata1) (ldata-timestamp ldata2)))
          (duration (+ (ldata-duration ldata1) (ldata-duration ldata2)))
          (elapsed (+ (ldata-elapsed ldata1) (ldata-elapsed ldata2)))
          (stroke-count (+ (ldata-stroke-count ldata1) (ldata-stroke-count ldata2))))
      ;; NOTE: insert first, as delete-length might delete the actual lap if
      ;; we remove all its lengths!
      (insert-length db pool-length (ldata (ldata-lap-id ldata1)
                                           #f ; no length-id
                                           timestamp
                                           duration
                                           elapsed stroke-count (ldata-stroke-type ldata1)))
      (ldata-lap-id ldata1))))

;; Split a length L (A_LENGTH.ID) into two equal lengths.  The length will be
;; removed, and two new ones inserted into the database.  The lap will now be
;; outdated, and will need to be updated.
(define (split-length db pool-length l)
  (let* ((data (fetch-length-data db l))
         (duration (/ (ldata-duration data) 2.0))
         (elapsed (/ (ldata-elapsed data) 2.0))
         (stroke-count (/ (ldata-stroke-count data) 2.0)))
    (insert-length db pool-length
                   (ldata (ldata-lap-id data)
                          #f ; no length-id
                          (ldata-timestamp data)
                          duration
                          elapsed
                          stroke-count (ldata-stroke-type data)))
    (insert-length db pool-length
                   (ldata (ldata-lap-id data)
                          #f ; no length-id
                          (exact-round (+ (ldata-timestamp data) duration))
                          duration
                          elapsed
                          stroke-count (ldata-stroke-type data)))
    (delete-length db l)
    (ldata-lap-id data)))

;; Add a new length to LAP-ID and re-split the lap totals among the number of
;; lengts.  This is used for drill laps where only the total lap time is
;; counted, than this time is divided equally for the number of laps (the
;; Garmin watch will ask for the drill distance, if you get it wrong, you have
;; a chance to correct it here).
(define (split-length/rebalance db pool-length lap-id)
  (let ((lengths (query-list db "select id from A_LENGTH where lap_id = ?" lap-id))
        (row (query-row db "select P.start_time, SS.total_timer_time,
       SS.total_elapsed_time, SS.total_cycles, SS.swim_stroke_id
from SECTION_SUMMARY SS, A_LAP P
where P.summary_id = SS.id and P.id = ?" lap-id)))
    (match-define (vector timestamp duration elapsed stroke-count stroke-type) row)
    (for ([l lengths]) (delete-length db l))
    (let* ((nlengths (add1 (length lengths)))
           (lduration (/ duration nlengths))
           (lelapsed (/ elapsed nlengths))
           (lstroke-count (/ stroke-count nlengths)))
      (for ([idx (in-range nlengths)])
        (insert-length db pool-length
                       (ldata lap-id #f
                              (exact-round (+ timestamp (* idx lduration)))
                              lduration
                              lelapsed
                              lstroke-count
                              stroke-type)))))
  lap-id)

;; Remove a length from LAP-ID and re-split the lap totals among the number of
;; lengts.  This is used for drill laps where only the total lap time is
;; counted, than this time is divided equally for the number of laps (the
;; Garmin watch will ask for the drill distance, if you get it wrong, you have
;; a chance to correct it here).
;;
;; NOTE: this function is 99% identical to 'split-length/rebalance', and they
;; should be refactored.
(define (join-lengths/rebalance db pool-length lap-id)
  (let ((lengths (query-list db "select id from A_LENGTH where lap_id = ?" lap-id))
        (row (query-row db "select P.start_time, SS.total_timer_time,
       SS.total_elapsed_time, SS.total_cycles, SS.swim_stroke_id
from SECTION_SUMMARY SS, A_LAP P
where P.summary_id = SS.id and P.id = ?" lap-id)))
    (match-define (vector timestamp duration elapsed stroke-count stroke-type) row)
    (for ([l lengths]) (delete-length db l))
    (let* ((nlengths (sub1 (length lengths)))
           (lduration (/ duration nlengths))
           (lelapsed (/ elapsed nlengths))
           (lstroke-count (/ stroke-count nlengths)))
      (for ([idx (in-range nlengths)])
        (insert-length db pool-length
                       (ldata lap-id #f
                              (exact-round (+ timestamp (* idx lduration)))
                              lduration
                              lelapsed
                              lstroke-count
                              stroke-type)))))
  lap-id)

;; After the lengths of a session have been modified, the trackpoints will
;; contain the wrong distance.  We fix the distance by updating each
;; trackpoint in order, to increase the distance by POOL-LENGTH for each
;; point.
(define (rebuild-trackpoint-distance session-id pool-length db)
  (let ((trackpoints (query-list db "
select T.id as tid
from A_LAP L, A_LENGTH E, A_TRACKPOINT T
where L.session_id = ?
  and E.lap_id = L.id
  and T.length_id = E.id
order by T.timestamp" session-id)))
    (let loop ((tpoints trackpoints)
               (distance pool-length))
      (unless (null? tpoints)
        (query-exec
         db
         "update A_TRACKPOINT set distance = ? where id = ?"
         distance (car tpoints))
        (loop (cdr tpoints) (+ distance pool-length))))))

;; Update the lap summary after the lap's lengths have been changed.  We only
;; update relevant data.
(define (update-lap-summary db pool-length lap-id)
  (let ((row (query-row
              db
              "select count(L.id), total(SS.total_elapsed_time),
                      total(SS.total_cycles), max(SS.avg_speed), max(SS.avg_cadence)
                 from A_LENGTH L, SECTION_SUMMARY SS
                where L.lap_id = ? and L.summary_id = SS.id"
              lap-id)))
    (match-define (vector nlaps elapsed total-strokes max-speed max-cadence) row)
    (cond ((= nlaps 0)
           ;; all lengths were removed, remove the lap itself
           (query-exec db "\
delete from SECTION_SUMMARY
 where id in (select P.summary_id from A_LAP P where P.id = ?)" lap-id)
           (query-exec db "delete from A_LAP where id = ?" lap-id))
          (#t
           (let* ((total-distance (* pool-length nlaps))
                  (avg-speed (/ total-distance elapsed))
                  (avg-cadence (exact-round (* 60 (/ total-strokes elapsed))))
                  (avg-stroke-distance (/ total-distance total-strokes)))
             (query-exec db "update SECTION_SUMMARY
                            set total_distance = ?,
                                avg_speed = ?,
                                max_speed = ?,
                                avg_cadence = ?,
                                max_cadence = ?,
                                avg_cycle_distance = ?
                          where id = (select L.summary_id from A_LAP L where L.id = ?)"
                         total-distance avg-speed max-speed avg-cadence max-cadence
                         avg-stroke-distance lap-id))))))

;; Update the summary data for the session after its laps have been updated.
(define (update-session-summary db pool-length session-id)
  (let ((row (query-row
              db
              "select total(SS.total_distance), total(SS.total_elapsed_time),
                      total(SS.total_cycles), max(SS.avg_speed), max(SS.avg_cadence)
                 from A_LAP L, SECTION_SUMMARY SS
                where L.session_id = ? and L.summary_id = SS.id"
              session-id)))
    (match-define (vector total-distance elapsed stroke-count max-speed max-cadence) row)
    (when (> total-distance 0)
      (let* (;; Avg speed for swim activities only counts active laps
             (avg-speed (query-value db
                                     "select total(SS.total_distance) / total(SS.total_elapsed_time)
                                           from A_LAP L, SECTION_SUMMARY SS
                                          where L.session_id = ? and L.summary_id = SS.id
                                            and SS.total_distance > 0" session-id))
             ;; NOTE: we need to ignore DRILL and REST laps when computing the
             ;; average cadence.
             (avg-cadence (exact-round
                           (query-value db
                                        "select 60 * total(SS.total_cycles) / total(SS.total_elapsed_time)
                                           from A_LAP L, SECTION_SUMMARY SS
                                          where L.session_id = ? and L.summary_id = SS.id
                                            and SS.total_cycles > 0" session-id)))
             (avg-stroke-distance (/ total-distance stroke-count)))
        (query-exec db "update SECTION_SUMMARY
                            set total_distance = ?,
                                avg_speed = ?,
                                max_speed = ?,
                                avg_cadence = ?,
                                max_cadence = ?,
                                avg_cycle_distance = ?
                          where id = (select S.summary_id from A_SESSION S where S.id = ?)"
                    total-distance avg-speed max-speed avg-cadence max-cadence
                    avg-stroke-distance session-id)))))

;; fixup a swim session by executing the PLAN, as produced by swl-model%.
(define (fixup-swim-session db session-id plan)
  ;; NOTE the transaction, if anything fails, all the changes are rolled back.
  (call-with-transaction
   db
   (lambda ()
     (let ((pool-length (query-value db "select pool_length from A_SESSION where id = ?" session-id))
           (modified-laps '()))
       (for ([step plan])
         (case (car step)
           ((split/rebalance)
            (let ((lap-id (car (cdr step))))
              (split-length/rebalance db pool-length lap-id)
              (set! modified-laps (cons lap-id modified-laps))))
           ((split)
            (let ((lap-id (split-length db pool-length (car (cdr step)))))
              (when lap-id
                (set! modified-laps (cons lap-id modified-laps)))))
           ((join/rebalance)
            (let ((lap-id (car (cdr step))))
              (join-lengths/rebalance db pool-length lap-id)
              (set! modified-laps (cons lap-id modified-laps))))
           ((join)
            (let ((lap-id (join-lengths db pool-length (car (cdr step)) (car (cdr (cdr step))))))
              (when lap-id
                (set! modified-laps (cons lap-id modified-laps)))))
           ((delete)
            (let ((lap-id (delete-length db (car (cdr step)))))
              (when lap-id
                (set! modified-laps (cons lap-id modified-laps)))))
           ((change-stroke)
            (change-length-stroke db (car (cdr step)) (car (cdr (cdr step)))))))
       ;; Update summary data for any laps that were modified
       (for ([lap-id (remove-duplicates modified-laps)])
         (update-lap-summary db pool-length lap-id))
       ;; Update summary data for the session itself
       (update-session-summary db pool-length session-id)))))


;;............................................... the-swim-lengts-editor ....

(define the-lap-swim-editor #f)

(define (get-lap-swim-editor)
  (unless the-lap-swim-editor
    (set! the-lap-swim-editor (new lap-swim-edit%)))
  the-lap-swim-editor)
