#lang racket/gui

(provide message-snip%)


;;............................................................ resources ....

(define transparent-pen
  (send the-pen-list find-or-create-pen "black" 0 'transparent))
(define active-text-color (make-color #x2f #x4f #x4f))


;;........................................................ message-snip% ....

(define message-snip-class
  (make-object
   (class snip-class% (super-new)
     (send this set-classname "message-snip"))))
(send (get-the-snip-class-list) add message-snip-class)

;; Display a message on the screen, with a timeout.
(define message-snip%
  (class snip%
    (init-field)

    (super-new)
    (send this set-snipclass message-snip-class)
    (send this set-count 1)

    (define bgcolor (make-color 244 236 220 0.8))
    (define text-color (make-color #x2f #x4f #x4f))
    (define text-font
      (send the-font-list find-or-create-font 28 'default 'normal 'normal))
    (define brush (send the-brush-list find-or-create-brush bgcolor 'solid))
    (define message "")
    (define message-timeout #f)
    (define message-stack '())

    (define width 0)
    (define height 0)
    (define margin 15)
    (define visible? #f)

    (define (set-visible flag)
      (set! visible? flag)
      (let ((flags (send this get-flags)))
        (if flag
            (begin
              (send this set-flags (remove 'invisible flags))
              (let ((admin (send this get-admin)))
                (when admin
                  (send admin resized this #f))))
            (unless (member 'invisible flags)
              (send this set-flags (cons 'invisible flags))))))

    (set-visible #f)

    (define (calculate-extents dc)
      (let-values (([w1 h1 x1 y1] (send dc get-text-extent message text-font)))
        (set! width (+ margin w1 margin))
        (set! height (+ margin h1 margin))))

    (define/override (get-extent dc x y w h descent space lspace rspace)
      (calculate-extents dc)
      (when w (set-box! w width))
      (when h (set-box! h height))
      (when descent (set-box! descent 0))
      (when space (set-box! space 10.0))
      (when lspace (set-box! lspace 10.0))
      (when rspace (set-box! rspace 10.0)))

    (define/override (draw dc x y . other)
      (when visible?
        (define (draw-centered-message xx xy msg font)
          (let-values (([w h x1 y1] (send dc get-text-extent msg font #t)))
            (send dc set-font font)
            (let ((ox (- xx (/ w 2)))
                  (oy (- xy (/ h 2))))
              (send dc draw-text msg (+ x ox) (+ y oy)))))

        (send dc set-smoothing 'smoothed)
        (send dc set-pen transparent-pen)
        (send dc set-brush brush)
        (send dc draw-rectangle x y width height)
        (send dc set-text-foreground text-color)
        (let ((cx (* 0.5 width))
              (cy (* 0.5 height)))
          (draw-centered-message cx cy message text-font))))

    (define (on-fade-out)
      (if (null? message-stack)
          (begin
            (set! message "")
            (set-visible #f))
          (match-let (((cons msg to) (car message-stack)))
            (set! message msg)
            (set! message-timeout to)
            (set! message-stack (cdr message-stack))
            (when to
              (send fade-out-timer start to #t))
            (set-visible #t))))
    
    (define fade-out-timer (new timer% [notify-callback on-fade-out]))

    ;; Display the message MSG with an optional FADE-OUT-TIME.  If a
    ;; FADE-OUT-TIME. is specified and a previous message is being displayed,
    ;; it will be set aside and restored when this message fades out.
    (define/public (set-message msg (fade-out-time #f))
      (if (and fade-out-time
               message
               (not (equal? message "")))
          (begin
            (set! message-stack (cons (cons message message-timeout) message-stack)))
          (set! message-stack '()))
      (set! message msg)
      (set! message-timeout fade-out-time)
      (send fade-out-timer stop)
      (if message
          (begin
            (when fade-out-time
              (send fade-out-timer start fade-out-time #t))
            (set-visible #t))
          (begin
            (set! message "")
            (set-visible #f))))

    ;; Display the message MSG with an optional FADE-OUT-TIME.  When the
    ;; message fades out, no message will replace it regardless of what was
    ;; displayed before.
    (define/public (replace-message msg (fade-out-time #f))
      (set! message-stack '())
      (set! message "")
      (set! message-timeout #f)
      (send fade-out-timer stop)
      (set-message msg fade-out-time))


    ))

