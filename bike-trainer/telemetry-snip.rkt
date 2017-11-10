#lang racket/gui

(provide telemetry-snip%)


;;............................................................ resources ....

(define active-text-color (make-color #x2f #x4f #x4f))
;; (define active-text-color (make-color 250 250 250))
(define aux-text-color (make-color 130 130 130))
(define main-value-font
  (send the-font-list find-or-create-font 14 'default 'normal 'normal))
(define main-label-font
  (send the-font-list find-or-create-font 8 'default 'normal 'normal))
(define aux-value-font
  (send the-font-list find-or-create-font 10 'default 'normal 'normal))
(define aux-label-font
  (send the-font-list find-or-create-font 6 'default 'normal 'normal))
(define margin 2)
(define spacing 1)
(define transparent-pen
  (send the-pen-list find-or-create-pen "black" 0 'transparent))
(define outline-pen
  (send the-pen-list find-or-create-pen active-text-color 0.5 'solid))


;;...................................................... telemetry-snip% ....

(define telemetry-snip-class
  (make-object
   (class snip-class% (super-new)
     (send this set-classname "telemetry-snip"))))

(send (get-the-snip-class-list) add telemetry-snip-class)

;; A snip that displays a value that is dynamically updated.  It can also
;; display the max and average for the value.
(define telemetry-snip%
  (class snip%
    (init-field
     ;; background color for the snip, can have an alpha value for a
     ;; semi-transparent snip
     bgcolor
     ;; telemetry value name (e.g. "Heart Rate" or "Power"), displayed on the
     ;; snip.
     name
     ;; number of digits used to print values for the current, avg and max
     ;; values.
     [precision 0]
     ;; max width of displayed values (includes all digits and the decimal
     ;; point)
     [max-field-width 4]
     ;; formatting function to create the string representation for the value.
     ;; This can be used to format time values for example.  If missing, the
     ;; `~r` function is used
     [fmt-function #f]
     ;; If #t, track max seen value for this telemetry
     [max-tracking-enabled? #t]
     ;; If #t, track average value for this telemetry
     [avg-tracking-enabled? #t]
     ;; time (in milliseconds) after which a value will become stale.
     ;; `update-value` will need to be called faster than this, or the display
     ;; will revert to "--"
     [stale-time 5000])

    (super-new)
    (send this set-snipclass telemetry-snip-class)
    (send this set-count 1)

    (define brush (send the-brush-list find-or-create-brush bgcolor 'solid))

    ;; The snip can be "active" or not.  If it is active, normal values are
    ;; displayed and updated when the telemetry value changes.  If the snip is
    ;; not active, "--" values are displayed for the values.
    ;;
    ;; The telemetry snip should be set to "not active" when there is no
    ;; connection from the sensor.
    (define active? #f)
    
    (define max-value 0)
    (define avg-value 0)
    (define value #f)                   ; current value for the telemetry, #f
                                        ; means no value was received yet

    (define last-update 0)              ; timestamp when the value was last updated
    (define last-timestamp 0)           ; last simulated timestamp
    (define accum-time 0)               ; accumulated time, used to track the average
    (define accum-value 0)              ; accumulated value, used to track the average

    ;; Snip dimensions, updated by `calculate-positions`
    (define width 150)
    (define height 150)

    ;; Positions of the items in the snip, updated by `calculate-positions`.
    ;; Positions are relative to the snip origin
    (define max-label "max")
    (define max-label-x 0)
    (define max-label-y 0)
    (define max-value-x 0)
    (define max-value-y 0)
    (define avg-label "avg")
    (define avg-label-x 0)
    (define avg-label-y 0)
    (define avg-value-x 0)
    (define avg-value-y 0)
    (define main-value-x 0)
    (define main-value-y 0)
    (define main-label-x 0)
    (define main-label-y 0)

    ;; Current text for the current, max and avg values.
    (define max-text "")
    (define avg-text "")
    (define main-text "")
      
    ;; Calculate positions of all items in this snip
    (define (calculate-positions dc)
      (define placeholder (make-string max-field-width #\9))
      (let-values (([w1 h1 x1 y1] (send dc get-text-extent placeholder aux-value-font))
                   ([w2 h2 x2 y2] (send dc get-text-extent "max" aux-label-font))
                   ([w3 h3 x3 y3] (send dc get-text-extent "avg" aux-label-font))
                   ([w4 h4 x4 y4] (send dc get-text-extent placeholder main-value-font))
                   ([w5 h5 x6 y5] (send dc get-text-extent name main-label-font)))
        ;; The width of the snip is such that the following will fit
        (set! width (max
                     ;; the two aux values (max and avg) next to each other
                     (+ margin w1 spacing margin spacing w1 margin)
                     ;; the two aux labels next to each other
                     (+ margin w2 spacing w3 margin)
                     ;; the main value by itself
                     (+ margin w4 margin)
                     ;; the main label by itself
                     (+ margin w5 margin)))
        ;; The height of the snip is such that all items fit
        (set! height (+ margin h1 spacing h2 spacing h4 spacing h5 margin))

        (set! max-value-x (+ margin (/ w1 2)))
        (set! max-value-y (+ margin (/ h1 2)))
        (set! max-label-x max-value-x)
        (set! max-label-y (+ margin h1 spacing (/ h2 2)))
        
        (set! avg-value-x (- width margin (/ w1 2)))
        (set! avg-value-y (+ margin (/ h1 2)))
        (set! avg-label-x avg-value-x)
        (set! avg-label-y (+ margin h1 spacing (/ h2 2)))

        (set! main-value-x (/ width 2)) ; center of the snip
        (set! main-value-y (+ margin h1 spacing h2 spacing (/ h4 2)))

        (set! main-label-x (/ width 2)) ; center of the snip
        (set! main-label-y (+ margin h1 spacing h2 spacing h4 spacing (/ h5 2)))))

    (define/override (get-extent dc x y w h descent space lspace rspace)
      (calculate-positions dc)
      (when w (set-box! w width))
      (when h (set-box! h height))
      (when descent (set-box! descent 0))
      (when space (set-box! space 10.0))
      (when lspace (set-box! lspace 10.0))
      (when rspace (set-box! rspace 10.0)))

    (define/override (draw dc x y . other)

      (define (draw-centered-message xx xy msg font)
        (let-values (([w h x1 y1] (send dc get-text-extent msg font #t)))
          (send dc set-font font)
          (let ((ox (- xx (/ w 2)))
                (oy (- xy (/ h 2))))
            (send dc draw-text msg (+ x ox) (+ y oy)))))

      (send dc set-smoothing 'smoothed)
      (send dc set-pen outline-pen)
      (send dc set-brush brush)
      (send dc draw-rectangle x y width height)
      (send dc set-text-foreground active-text-color)
      (when max-tracking-enabled?
        (draw-centered-message max-value-x max-value-y max-text aux-value-font))
      (when avg-tracking-enabled?
        (draw-centered-message avg-value-x avg-value-y avg-text aux-value-font))
      (draw-centered-message main-value-x main-value-y main-text main-value-font)
      (send dc set-text-foreground aux-text-color)
      (when max-tracking-enabled?
        (draw-centered-message max-label-x max-label-y max-label aux-label-font))
      (when avg-tracking-enabled?
        (draw-centered-message avg-label-x avg-label-y avg-label aux-label-font))
      (draw-centered-message main-label-x main-label-y name main-label-font)
      
      )

    ;; Return the text values to be displayed for the current, avg and max
    ;; telemetry value as a list of 3 strings
    (define (get-display-text-labels)
      (define stale? (> (- (current-milliseconds) last-update) stale-time))
      (if (and active? (not stale?) value)
          (if fmt-function
              (list
               (fmt-function value)
               (and avg-tracking-enabled? (fmt-function avg-value))
               (and max-tracking-enabled? (fmt-function max-value)))
              (if (zero? precision)
                  (list
                    ;; When precision is 0, '(= 0) will put a decimal point,
                    ;; which we don't want...
                   (~r value #:precision 0)
                   (and avg-tracking-enabled? (~r avg-value #:precision 0))
                   (and max-tracking-enabled? (~r max-value #:precision 0)))
                  (list
                   (~r value #:precision (list '= precision))
                   (and avg-tracking-enabled? (~r avg-value #:precision (list '= precision)))
                   (and max-tracking-enabled? (~r max-value #:precision (list '= precision))))))
          (list "--" (and avg-tracking-enabled? "--") (and max-tracking-enabled? "--"))))

    ;; Update the main, avg and max value texts and request an update if they
    ;; have changed.
    (define (maybe-refresh)
      (match-define (list main avg max) (get-display-text-labels))
      (let ((need-update? (or (not (equal? main main-text))
                              (and
                               avg-tracking-enabled?
                               (not (equal? avg avg-text)))
                              (and
                               max-tracking-enabled?
                               (not (equal? max max-text))))))
        (set! main-text main)
        (set! avg-text avg)
        (set! max-text max)
        (when need-update?
          (let ((admin (send this get-admin)))
            (when admin
              (send admin needs-update this 0 0 width height))))))

    ;; Set this telemetry to active or not active.  A telemetry should be not
    ;; active when there is no connection from the corresponding sensor
    (define/public (set-active flag)
      (unless (eq? flag active?)
        (set! active? flag)
        (maybe-refresh)))

    (define stale-timer (new timer% [notify-callback maybe-refresh]))

    ;; Reset the avg and max values for this telemetry.  The display will show
    ;; "--" until new value is received using `update-value`
    (define/public (reset-value)
      (set! accum-time 0)
      (set! accum-value 0)
      (set! max-value 0)
      (set! avg-value 0)
      (set! value #f)
      (set! last-update 0)
      (set! last-timestamp 0)
      (maybe-refresh))

    ;; Update the value of the snip to NEW-VALUE.  TIMESTAMP is the time (in
    ;; milliseconds) when the NEW-VALUE was read (it is used to track the
    ;; average for the value).
    (define/public (update-value new-value timestamp)
      (send stale-timer stop)
      (send stale-timer start stale-time #t)
      (if value
          (let ((delta (/ (- timestamp last-timestamp) 1000.0)))
            (set! accum-time (+ accum-time delta))
            (set! accum-value (+ accum-value (* delta (/ (+ new-value value) 2))))
            (set! max-value (max value new-value))
            (set! avg-value (if (> accum-time 0) (/ accum-value accum-time) new-value))
            (set! value new-value))
          (begin
            (set! accum-time 0)
            (set! accum-value 0)
            (set! value new-value)
            (set! max-value value)
            (set! avg-value value)))
      (set! last-timestamp timestamp)
      (set! last-update (current-milliseconds))
      (maybe-refresh))

    (define/public (get-width) width)
    (define/public (get-height) height)

    (maybe-refresh)
    ))
