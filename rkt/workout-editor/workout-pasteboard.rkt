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

(require racket/gui/base
         racket/class
         racket/list
         racket/match
         embedded-gui
         "wkstep.rkt"
         "workout-step-snip.rkt"
         "workout-repeat-snip.rkt")

(provide workout-pasteboard%)

(define (snip-position snip)
  (let ((pasteboard (snip-parent snip))
        (x (box 0))
        (y (box 0)))
    (if pasteboard
        (begin
          (send pasteboard get-snip-location snip x y #f)
          (values (unbox x) (unbox y)))
        (values 0 0))))

(define (snip-x snip)
  (define-values (x y) (snip-position snip))
  x)

(define (snip-y snip)
  (define-values (x y) (snip-position snip))
  y)

(define (with-edit-sequence pasteboard thunk)
  (dynamic-wind
    (lambda () (send pasteboard begin-edit-sequence))
    thunk
    (lambda () (send pasteboard end-edit-sequence))))

;; pasteboard for editing workouts: holds workout-step-snip%,
;; workout-repeat-snip% and workout-repeat-end-snip% snips, in the order of
;; the workout.  Allows drag and drop reordering and other operations.
(define workout-pasteboard%
  (class pasteboard%
    (init-field
     ;; Called to tell the parent that undo operations are available or not.
     ;; Can be used to enable/disable an undo button.
     [undo-available-callback #f]
     [contents-modified-callback #f])
    (super-new)

    ;; Don't allow selecting snips by dragging the mouse and selecting an
    ;; area.
    (send this set-area-selectable #f)
    (send this set-selection-visible #f)

    ;; List of undo operations.  Each operation is a lambda which, when
    ;; executed performs an undo operation.
    (define undo-stack '())

    ;; Flag indicating that the contents of the pasteboard are modified, and
    ;; therefore need saving
    (define is-modified? #f)

    ;; Space between snips -- snips are placed vertically
    (define spacing 5)

    ;; List of snips being dragged around (empty if no drag operation is in
    ;; progress)
    (define dragged-snips '())
    ;; Parent of the first dragged snip, or #f if no drag operation is in
    ;; progress.
    (define dragged-snips-parent #f)
    ;; Whether the snips in DRAGGED-SNIPS are in forward or reverse order (if
    ;; the drag operation is started by dragging the repeat-end snip, the
    ;; snips will be reverse order.
    (define reverse-dragged-snips? #f)
    ;; Height of the gap to leave when making the "drop" room for the snips
    ;; being dragged.  Has no meaning if no drag operation is in progress.
    (define drag-gap-height 0)

    ;; When not #f an animation is in progress.  An animation moves snips
    ;; slowly towards their target position, so the user can see what is going
    ;; on.  Animations are used when snips are moved around due to "undo"
    ;; operations and they allow the user to visualize the undo operation
    ;; itself.
    ;;
    ;; This value, is returned by CALCULATE-TARGET-POSITIONS, that defines the
    ;; final position of each snip.  Snips will be moved towards their target
    ;; location "step by step" by ON-ANIMATE.
    (define animation-targets #f)
    ;; Number of steps remaining in the animation
    (define animation-remaining-steps 0)
    ;; Time between each step of an animation (in milliseconds)
    (define animation-step-duration 20)
    ;; Animation timer -- this is the "tick" of the entire animation
    (define animate-timer (new timer% [notify-callback (lambda () (on-animate))]))

    ;; Perform an animation step: move each snip in ANIMATION-TARGETS closer
    ;; towards its target position and decrement ANIMATION-REMAINING-STEPS.
    ;; If there are steps remaining, schedule a new call to this function.
    (define (on-animate)
      (if animation-targets
          (begin
            (set! animation-remaining-steps (sub1 animation-remaining-steps))
            (if (> animation-remaining-steps 0)
                (begin
                  (apply-target-position animation-targets #t)
                  (send animate-timer start animation-step-duration))
                (begin
                  (apply-target-position animation-targets #f)
                  (set! animation-targets #f))))
          (begin
            (set! animation-targets #f)
            (set! animation-remaining-steps 0))))

    ;; Start an animation by moving snips towards TARGETS, as returned by
    ;; CALCULATE-TARGET-POSITIONS.
    (define (begin-animation targets)
      (set! animation-remaining-steps 20)
      (set! animation-targets targets)
      (send animate-timer start animation-step-duration))

    ;; Calculate target positions for each snip in the pasteboard.  Snips
    ;; *not* being dragged are placed one under the other in the same order
    ;; that they are added to the pasteboard, leaving a gap at the position
    ;; where the dragged snips would be dropped.  All dragged snips are placed
    ;; under the main snip being dragged -- the pasteboard% drags a single
    ;; snip, but all sub-snips of a repeat are dragged after the main one, and
    ;; this function ensures this happens.
    ;;
    ;; This function does not actually move the snips, instead it returns two
    ;; values: the gap snip -- the snip before which dragged snips would be
    ;; dropped, as well as a hash mapping a snip to its target location.
    (define (calculate-target-positions)
      (define snip-target-positions '())
      (define gap-snip #f)

      (define (target-position snip x y)
        (set! snip-target-positions (cons (vector snip x y) snip-target-positions)))

      (unless (null? dragged-snips)
        (if reverse-dragged-snips?
            (let* ((nsnips (reverse dragged-snips))
                   (anchor (car nsnips))
                   (anchor-x (snip-x anchor))
                   (anchor-y (snip-y anchor)))
              (let loop ((remaining (cdr nsnips))
                         (y (- anchor-y spacing)))
                (unless (null? remaining)
                  (target-position (car remaining) anchor-x (- y (send (car remaining) get-height)))
                  (loop (cdr remaining) (- y (send (car remaining) get-height) spacing)))))
            (let* ((snips dragged-snips)
                   (anchor (car snips))
                   (anchor-x (snip-x anchor))
                   (anchor-y (snip-y anchor)))
              (let loop ((remaining (cdr snips))
                         (y (+ anchor-y (send anchor get-height) spacing)))
                (unless (null? remaining)
                  (target-position (car remaining) anchor-x y)
                  (loop (cdr remaining) (+ y (send (car remaining) get-height) spacing)))))))
      (define drag-y
        (if (null? dragged-snips)
            0
            (snip-y (car dragged-snips))))

      (let loop ((snip (send this find-first-snip))
                 (y spacing))
        (when snip
          (if (memq snip dragged-snips)
              (begin
                (loop (send snip next) y))
              (let ((height (send snip get-height)))
                (if (and (not (null? dragged-snips))
                         (not gap-snip)
                         (> (+ y spacing (/ height 2)) drag-y))
                    (let ((y (+ y spacing drag-gap-height spacing)))
                      (set! gap-snip snip)
                      (target-position snip 0 y)
                      (loop (send snip next) (+ y spacing height)))
                    (begin
                      (target-position snip 0 y)
                      (loop (send snip next) (+ y spacing height))))))))
      (values gap-snip snip-target-positions))

    ;; Move snips to their TARGET-POSITIONS (value returned by
    ;; CALCULATE-TARGET-POSITIONS, which see).  If ANIMATING? is #t, the snips
    ;; are moved only partially *towards* their target position.
    (define (apply-target-position target-positions animating?)
      (with-edit-sequence
        this
        (lambda ()
          (for ([target (in-list target-positions)])
            (match-define (vector snip tx ty) target)
            (define-values (sx sy) (snip-position snip))
            (unless (and (eqv? tx sx) (eqv? ty sy))
              (if animating?
                  (let ((nx (+ sx (* 0.2 (- tx sx))))
                        (ny (+ sy (* 0.2 (- ty sy)))))
                    (send this move-to snip nx ny))
                  (send this move-to snip tx ty)))))))

    ;; Arrange all snips in the pasteboard: their position is calculated and
    ;; snips are moved there.  If animate? is #t an animation is started for
    ;; the move operation.
    (define (arrange-snips [animate? #f])
      (define-values (gap-snip target-positions) (calculate-target-positions))
      (if animate?
          (begin-animation target-positions)
          (apply-target-position target-positions #f))
      gap-snip)

    ;; Update nesting levels for all snips: snips inside repeats will have a
    ;; higher nesting level and will be drawn slightly indented for better
    ;; visual cues.
    (define (update-workout-step-nesting)
      (with-edit-sequence
        this
        (lambda ()
          (define nesting-level 0)
          (let loop ((snip (send this find-first-snip)))
            (when snip
              (cond ((is-a? snip workout-repeat-snip%)
                     (send snip set-nesting-level nesting-level)
                     (set! nesting-level (add1 nesting-level)))
                    ((is-a? snip workout-repeat-end-snip%)
                     (set! nesting-level (sub1 nesting-level))
                     (send snip set-nesting-level nesting-level))
                    (#t
                     (send snip set-nesting-level nesting-level)))
              (loop (send snip next)))))))

    ;; Called by the pasteboard% machinery after a snip is inserted.  Updates
    ;; the snip layout and nesting.
    (define/augment (after-insert snip before x y)
      (arrange-snips)
      (update-workout-step-nesting))

    ;; Called after a snip is selected -- we ensure only a single snip is
    ;; selected at a time.  While the selection is not visible, it affects
    ;; dragging of snips.
    (define/augment (after-select snip on?)
      (when on?
        (define selected-snips
          (let loop ((snip (send this find-next-selected-snip #f))
                     (result '()))
            (if snip
                (loop (send this find-next-selected-snip snip)
                      (cons snip result))
                result)))
        (when (> (length selected-snips) 1)
          (send this set-selected snip))))

    ;; Mark if an "internal" delete is happening, that is snips are deleted as
    ;; a result of deleting other snips -- when a repeat snip is deleted, all
    ;; "containing snips" need to be deleted as well.  This invokes the
    ;; "{on,can,after}-delete" procedures recursively.
    (define internal-delete #f)

    (define (with-internal-delete thunk)
      (dynamic-wind
        (lambda () (set! internal-delete #t))
        thunk
        (lambda () (set! internal-delete #f))))

    ;; List if additional snips to delete when a repeat snip is deleted.
    (define snips-to-delete '())

    ;; Called when a SNIP is deleted.  It will compute the list of additional
    ;; snips that need deleting.  Note that the pasteboard% is locked when
    ;; this method is called, so we cannot delete other snips here, instead
    ;; they will be deleted in after-delete.
    (define/augment (on-delete snip)

      ;; this method is also invoked when deleting the SNIPS-TO-DELETE, no
      ;; need to do anything in that case.
      (unless internal-delete

        ;; If this is a repeat snip, add all contained snips to
        ;; SNIPS-TO-DELETE, to be deleted later.
        (when (is-a? snip workout-repeat-snip%)
          (set! snips-to-delete
                (let loop ([snip (send snip next)]
                           [nesting 0]
                           [result '()])
                  (cond
                    ((eq? snip #f)        ; should not get here!
                     result)
                    ((is-a? snip workout-repeat-snip%)
                     (loop (send snip next) (add1 nesting) (cons snip result)))
                    ((is-a? snip workout-repeat-end-snip%)
                     (if (eqv? nesting 0)
                         (cons snip result)
                         (loop (send snip next) (sub1 nesting) (cons snip result))))
                    (#t
                     (loop (send snip next) nesting (cons snip result)))))))

        ;; Add an undo operation that restores deleted snips.
        (let ((parent (send snip previous))
              (snips (cons snip (reverse snips-to-delete))))
          (x-add-undo
           (lambda ()
             (with-edit-sequence
               this
               (lambda ()
                 (for ([s (in-list snips)])
                   (send this insert s))
                 (for ([p (in-list (cons parent snips))]
                       [s (in-list snips)])
                   (if p
                       (send this set-after s p)
                       (send this set-before s p)))))
             (arrange-snips #t)
             (update-workout-step-nesting))))))

    ;; This is invoked after SNIP was deleted.  We delete snips in
    ;; SNIPS-TO-DELETE here, because now the editor is unlocked.  Note that we
    ;; cannot compute the contents of SNIPS-TO-DELETE list here, as SNIP is
    ;; now orphan.
    (define/augment (after-delete snip)

      ;; This method is also invoked when SNIPS-TO-DELETE snips are being
      ;; deleted, but we don't need to do anything in that case.
      (unless internal-delete
        (with-internal-delete
          (lambda ()
            (with-edit-sequence
              this
              (lambda ()
                (for ((snip snips-to-delete))
                  (send this delete snip)))))))
        (set! snips-to-delete '())
        (arrange-snips #t)
        (update-workout-step-nesting))

    ;; This is invoked to check if SNIP can be deleted.  Only
    ;; workout-repeat-end-snip% cannot be deleted directly (the corresponding
    ;; workout-repeat-snip% needs to be deleted instead)
    (define/augment (can-delete? snip)
      ;; If internal-delete is #t, allow deleting any snip.
      (or internal-delete (not (is-a? snip workout-repeat-end-snip%))))

    ;; This is invoked when a drag operation is started on the selected snip.
    ;; Determines all the snips that are actually dragged (if a repeat snip is
    ;; being dragged, all the contained snips will be dragged as well).
    (define/augment (on-interactive-move event)
      (define selected (send this find-next-selected-snip #f))
      (cond ((is-a? selected workout-repeat-snip%)
             (set! dragged-snips
                   (let loop ([part selected]
                              [nesting 0]
                              [result '()])
                     (cond ((is-a? part workout-repeat-snip%)
                            (loop (send part next) (add1 nesting) (cons part result)))
                           ((is-a? part workout-repeat-end-snip%)
                            (if (eqv? nesting 1)
                                (reverse (cons part result))
                                (loop (send part next) (sub1 nesting) (cons part result))))
                           (part
                            (loop (send part next) nesting (cons part result)))
                           (#t
                            (reverse result)))))
             (set! reverse-dragged-snips? #f))
            ((is-a? selected workout-repeat-end-snip%)
             (set! dragged-snips
                   (let loop ([part selected]
                              [nesting 0]
                              [result '()])
                     (cond ((is-a? part workout-repeat-end-snip%)
                            (loop (send part previous) (add1 nesting) (cons part result)))
                           ((is-a? part workout-repeat-snip%)
                            (if (eqv? nesting 1)
                                (cons part result)
                                (loop (send part previous) (sub1 nesting) (cons part result))))
                           (part
                            (loop (send part previous) nesting (cons part result)))
                           (#t
                            result))))
             (set! reverse-dragged-snips? #t))
            (#t
             (set! dragged-snips (list selected))
             (set! reverse-dragged-snips? #f)))
      (set! dragged-snips-parent
            (let ([snip ((if reverse-dragged-snips? last first) dragged-snips)])
              (send snip previous)))
      ;; Put all dragged snips "before" all others, this will result in then
      ;; being drawn above all other snips.
      (for ([snip dragged-snips])
        (send this set-before snip #f))
      ;; Calculate the height of the gap required to drop the dragged snips in
      ;; a target location.
      (set! drag-gap-height
            (for/sum ([snip (in-list dragged-snips)])
              (+ (send snip get-height) spacing))))

    ;; Called after an interactive move has finished.  Reorder the snips as
    ;; needed.
    (define/augment (after-interactive-move event)
      (unless (null? dragged-snips)

        ;; Determine the gap snip -- somewhat inefficient...
        ;;(define-values (gap-snip _) (calculate-target-positions))
        (define gap-snip (arrange-snips))

        ;; save an undo operation that moves the dragged sips back, but only
        ;; if the drop location is different from they were dragged from.
        (unless (or (and gap-snip (eq? (send gap-snip previous) dragged-snips-parent))
                    (and (eq? gap-snip #f) dragged-snips-parent
                         (eq? (send dragged-snips-parent next) #f)))
          (let ((snips (if reverse-dragged-snips? (reverse dragged-snips) dragged-snips))
                (parent dragged-snips-parent))
            (x-add-undo
             (lambda ()
               (with-edit-sequence
                 this
                 (lambda ()
                   (for ([p (in-list (cons parent snips))]
                         [s (in-list snips)])
                     (if p
                         (send this set-after s p)
                         (send this set-before s p)))))
               (arrange-snips #t)
               (update-workout-step-nesting)))))

        ;; Link up the dragged snips to be in the dropped location.
        (let ((snips (reverse dragged-snips)))
          (if gap-snip
              (send this set-before (car snips) gap-snip)
              (send this set-after (car snips) #f))
          (for ((a snips)
                (b (cdr snips)))
            (send this set-before b a))))

      (set! dragged-snips '())
      (arrange-snips)
      (update-workout-step-nesting))

    ;; Called after a snip is moved.  If DRAGGING? is #t, the item is being
    ;; dragged.  Dynamically arrange items if this is the case, so a gap is
    ;; created where the item can be dropped.
    (define/augment (after-move-to snip x y dragging?)
      (when dragging?
        (arrange-snips)))

    ;; NOTE: this is a public method, because it is used by the snips
    ;; themselves when they change
    (define/public (x-add-undo undoer)
      (when (and (null? undo-stack) undo-available-callback)
        (undo-available-callback #t))
      (set! undo-stack (cons undoer undo-stack))
      (set! is-modified? #t)
      (when contents-modified-callback
        (contents-modified-callback is-modified?)))

    (define/public (x-undo)
      (unless (null? undo-stack)
        (let ((undoer (car undo-stack)))
          (set! undo-stack (cdr undo-stack))
          (undoer)
          (when (and (null? undo-stack) undo-available-callback)
            (undo-available-callback #f)))))

    ;; Add a new step to the workout.  The step is added at the end and the
    ;; user can drag it to the correct position and edit the step data.
    (define/public (add-new-step)
      (define step (wkstep 'active 'open #f 'open #f #f #f))
      (define snip (new workout-step-snip% [data step]))

      (send this insert snip)
      (x-add-undo
       (lambda ()
         (with-internal-delete
           (lambda ()
             (send this delete snip)))
         (arrange-snips #t)
         (update-workout-step-nesting))))

    ;; Add a new repeat to the workout, a step is also added inside the
    ;; repeat.  The repeat is added at the end and the user can drag it to the
    ;; correct position and edit the "times" count by pressing the plus and
    ;; minus buttons on the snip.
    (define/public (add-new-repeat)
      (define step (wkstep 'active 'open #f 'open #f #f #f))
      (define r-snip (new workout-repeat-snip% [times 5]))
      (define s-snip (new workout-step-snip% [data step]))
      (define e-snip (new workout-repeat-end-snip%))

      (with-edit-sequence
        this
        (lambda ()
          (send this insert r-snip)
          (send this insert s-snip)
          (send this insert e-snip)))

      (x-add-undo
       (lambda ()
         (with-internal-delete
           (lambda ()
             (with-edit-sequence
               this
               (lambda ()
                 (send this delete r-snip)
                 (send this delete s-snip)
                 (send this delete e-snip)))))
         (arrange-snips #t)
         (update-workout-step-nesting))))

    ;; Set the contents of this pasteboard% to WORKOUT -- a list of wkstep and
    ;; wkrepeat structures.
    (define/public (put-workout-steps workout-steps)

      (define (add steps)
        (for ([step (in-list steps)])
          (cond ((wkstep? step)
                 (define snip (new workout-step-snip% [data step]))
                 (send this insert snip))
                ((wkrepeat? step)
                 (match-define (wkrepeat times steps) step)
                 (define bsnip (new workout-repeat-snip% [times times]))
                 (send this insert bsnip)
                 (add steps)
                 (define esnip (new workout-repeat-end-snip%))
                 (send this insert esnip))
                (#t
                 (error step)))))

      (with-edit-sequence
        this
        (lambda ()
          (with-internal-delete
            (lambda () (send this erase)))
          (add workout-steps)
          (set! undo-stack '())
          (when undo-available-callback
            (undo-available-callback #f))))
      (arrange-snips #t)
      (update-workout-step-nesting)
      (clear-modified-flag))

    ;; Return the contents of the pasteboard as a hierarchical list of wksteps
    ;; (repeat steps are packed int wkrepeat structures, which makes this
    ;; hierarchical).
    (define/public (get-workout-steps)
      (define (make-repeat snip)
        (unless (is-a? snip workout-repeat-snip%)
          (error "expecting a workout-repeat-snip%"))
        (define times (send snip get-times))
        (let loop ((snip (send snip next))
                   (steps '()))
          (cond ((eq? snip #f) (error "missing workout-repeat-end-snip%"))
                ((is-a? snip workout-repeat-end-snip%)
                 (values (send snip next) (wkrepeat times (reverse steps))))
                ((is-a? snip workout-repeat-snip%)
                 (define-values (next-snip repeat) (make-repeat snip))
                 (loop next-snip (cons repeat steps)))
                (#t
                 (define step (send snip get-wkstep))
                 (loop (send snip next) (cons step steps))))))

      (let loop ((snip (send this find-first-snip))
                 (steps '()))
        (cond ((eq? snip #f)
               (reverse steps))
              ((is-a? snip workout-repeat-snip%)
               (define-values (next-snip repeat) (make-repeat snip))
               (loop next-snip (cons repeat steps)))
              ((is-a? snip workout-repeat-end-snip%)
               (error "unexpected workout-repeat-end-snip%"))
              (#t
               (define step (send snip get-wkstep))
               (loop (send snip next) (cons step steps))))))

    (define/public (clear-steps)
      (with-edit-sequence
        this
        (lambda ()
          (with-internal-delete
            (lambda () (send this erase)))
          (set! undo-stack '())
          (when undo-available-callback
            (undo-available-callback #f))
          (clear-modified-flag))))

    (define/public (clear-modified-flag)
      (when is-modified?
        (set! is-modified? #f)
        (when contents-modified-callback
          (contents-modified-callback is-modified?))))

    (define/public (unsaved-edits?)
      is-modified?)

    ))
