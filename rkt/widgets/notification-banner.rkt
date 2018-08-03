#lang racket/base
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2018 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require racket/gui/base racket/class racket/math)
(require "../utilities.rkt")            ; for dbglog
(provide notification-banner%)

;; Display a text at the top of the window with a "dismiss" button, when the
;; text is dismissed, the widget hides itself, taking up no space.  When a new
;; notification is posted, the widget is shown again.
;;
;; An instance of this widget should be created as the first item in a frame%
;; instance.  Several notifications can be queued up using the `notify' method
;; and they will be dismissed by the user (once a notification is dismissed,
;; the next one is displayed).  A dismiss callback can be provided, so the
;; application can monitor when the notifications are dismissed.
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
        (define message
          (let ((nmsg (length queued-messages)))
            (if (= nmsg 1)
                (car queued-messages)
                (format "~a (+ ~a more)" (car queued-messages) (sub1 nmsg)))))
        (set-label message)
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
      ;; NOTE: avoid duplicate messages in the message queue (not sure if this
      ;; is the right thing to do, but it looks nicer.
      (unless (member message queued-messages)
        (let ((empty? (null? queued-messages)))
          (set! queued-messages (reverse (cons message (reverse queued-messages))))
          (when empty? (on-timer)))))

    ))
