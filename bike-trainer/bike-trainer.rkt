#lang racket/gui
(require racket/async-channel
         "../rkt/data-frame.rkt"
         "../rkt/fmt-util.rkt"
         "../rkt/map-widget.rkt"
         "../rkt/gpx.rkt"
         "telemetry-snip.rkt"
         "elevation-profile-snip.rkt"
         "message-snip.rkt"
         "sim-state.rkt")


;;................................................... the toplevel frame ....

(define toplevel (new frame% [width 800] [height 600] [label "Bike Trainer"]))
(define contents (new vertical-pane% [parent toplevel] [border 0] [spacing 0]))
(define map-widget (new map-widget% [parent contents]))
(define button-pane (new horizontal-pane% [parent contents]
                         [border 1] [spacing 2] [stretchable-height #f]))

;; channel used to send commands to the simulation task
(define ch (make-async-channel 1000))


;;............................................ telemetry and other snips ....

(define hr-snip
  (new telemetry-snip%
       [bgcolor (make-color 255 242 243 0.8)]
       [name "Heart Rate (BPM)"]))
(define cad-snip
  (new telemetry-snip%
       [bgcolor (make-color 255 249 229 0.8)]
       [name "Cadence (RPM)"]))
(define pwr-snip
  (new telemetry-snip%
       [bgcolor (make-color 255 229 255 0.8)]
       [name "Power (watts)"]))
(define spd-snip
  (new telemetry-snip%
       [bgcolor (make-color 229 247 255 0.8)]
       [name "Speed (km/h)"]
       [precision 1]))
(define slope-snip
  (new telemetry-snip%
       [bgcolor (make-color 229 255 233 0.8)]
       [name "Slope (%)"]
       [precision 1]))
(define dst-snip
  (new telemetry-snip%
       [bgcolor (make-color 244 236 220 0.8)]
       [name "Distance (km)"]
       [max-tracking-enabled? #f]
       [avg-tracking-enabled? #f]
       [precision 1]))
(define timer-snip
  (new telemetry-snip%
       [bgcolor (make-color 244 236 220 0.8)]
       [name "Time"]
       [max-tracking-enabled? #f]
       [avg-tracking-enabled? #f]
       [max-field-width 8]
       [fmt-function duration->string]))
(define elevation-profile-snip
  (new elevation-profile-snip%
       [bgcolor (make-color 229 255 233 0.8)]))
(define message-snip
  (new message-snip%))


;;.............................................. the bottom button panel ....

(define (on-load-gpx-track b e)
  (let ((file-name (get-file "Select GPX track"
                             toplevel
                             #f
                             #f
                             "gpx" '()
                             '(("GPX Files" "*.gpx") ("Any" "*.*")))))
    (when file-name
      (let ((df (call-with-input-file file-name df-read/gpx)))
        (async-channel-put ch df)         ; send it to the simulation task
      ))))

(define (on-start-simulation b e)
  (async-channel-put ch 'play))
(define (on-pause-simulation b e)
  (async-channel-put ch 'pause))
(define (on-reset-simulation b e)
  (async-channel-put ch 'reset))
(define (on-zoom-in b e)
  (send map-widget set-zoom-level
        (add1 (send map-widget get-zoom-level))))
(define (on-zoom-out b e)
  (send map-widget set-zoom-level
        (sub1 (send map-widget get-zoom-level))))
(define (on-zoom-to-fit b e)
  (send map-widget resize-to-fit))

(define load-track-button
  (new button%
       [label "Load GPX Track..."]
       [parent button-pane]
       [callback on-load-gpx-track]))
(define start-button
  (new button%
       [label "Start"]
       [parent button-pane]
       [callback on-start-simulation]))
(define pause-button
  (new button%
       [label "Pause"]
       [parent button-pane]
       [callback on-pause-simulation]))
(define reset-button
  (new button%
       [label "Back to start"]
       [parent button-pane]
       [callback on-reset-simulation]))
(define zoom-in-button
  (new button%
       [label "Zoom In"]
       [parent button-pane]
       [callback on-zoom-in]))
(define zoom-out-button
  (new button%
       [label "Zoom Out"]
       [parent button-pane]
       [callback on-zoom-out]))
(define zoom-to-fit
  (new button%
       [label "Zoom To Fit"]
       [parent button-pane]
       [callback on-zoom-to-fit]))


;;............................................................ GUI setup ....

(define margin 2)                       ; space between snips

;; Add SNIPS to the pasteboard% PB and line them up at the top.
(define (place-snips pb . snips)
  (define x margin)
  (define y margin)

  (for ((snip snips))
    (send pb insert snip)
    (send pb move snip x y)
    (set! x (+ x (send snip get-width) margin))))

(place-snips
 (send map-widget get-pasteboard)
 elevation-profile-snip slope-snip dst-snip timer-snip
 hr-snip pwr-snip cad-snip spd-snip)
(send elevation-profile-snip
      set-height
      (send slope-snip get-height))
(let ((pb (send map-widget get-pasteboard)))
  (send pb insert message-snip)
  (let ((x margin)
        (y (+ margin (send slope-snip get-height) margin)))
    (send pb move message-snip x y)))
(send toplevel show #t)
;; need to show the view before center map works...
(send map-widget set-zoom-level 14)
(send map-widget center-map)



;;.................................................... reading telemetry ....

(struct telemetry (hr spd cad pwr) #:transparent)

;; Read telemetry data from IN, an input-port? and send TELEMETRY structs to
;; OUT-CHANNEL (which will be received by the simulation task.  This function
;; should be called in a separate thread, as it runs until the input port is
;; closed.

(define (read-telemetry in out-channel)
  (define line (read-line in 'any))
  (unless (eq? line eof)
    (define hr #f)
    (define cad #f)
    (define spd #f)
    (define pwr #f)
    (define m (regexp-match #rx"TELEMETRY (.*)" line))
    (when m
      (for ((chunk (string-split (list-ref m 1) ";")))
        (define m (regexp-match #rx"([A-Z0-9]+): *([0-9.]+)" chunk))
        (when m
          (case (list-ref m 1)
            (("HR") (set! hr (string->number (list-ref m 2))))
            (("CAD") (set! cad (string->number (list-ref m 2))))
            (("SPD") (set! spd (string->number (list-ref m 2))))
            (("PWR") (set! pwr (string->number (list-ref m 2)))))))
      (async-channel-put out-channel (telemetry hr spd cad pwr)))
    (read-telemetry in out-channel)))


;;...................................................... simulation task ....

(define telemetry-snips
  (list hr-snip cad-snip pwr-snip spd-snip slope-snip dst-snip timer-snip))

(define (reset-telemetry-snips)
  (for ((snip telemetry-snips))
    (send snip reset-value)))

(define (update-telemetry-snips t sim-time)

  (define (update-snip snip value)
    (if value
        (begin
          (send snip set-active #t)
          (send snip update-value value sim-time))
        (begin
            (send snip set-active #f))))
  
  (when (and t (telemetry? t))
    (let ((spd (telemetry-spd t)))
      (update-snip spd-snip (if spd (* spd 3.6) #f)))
    (update-snip hr-snip (telemetry-hr t))
    (update-snip cad-snip (telemetry-cad t))
    (update-snip pwr-snip (telemetry-pwr t))))

(define (on-new-telemetry state telemetry delta)
  (let* ((pb (send map-widget get-pasteboard))
         (nstate (sim-state-update state (or (telemetry-spd telemetry) 0) delta))
         (timer (sim-state-timer nstate))
         (distance (sim-state-distance nstate))
         (position (sim-state-current-position nstate))
         (df (sim-state-df nstate)))
    (send pb begin-edit-sequence)
    (update-telemetry-snips telemetry timer)
    (when position
      (send map-widget set-current-location position))
    (send dst-snip set-active #t)
    (send dst-snip update-value (/ distance 1000.0) timer)
    (send elevation-profile-snip update-position distance)
    (send timer-snip set-active #t)
    (send timer-snip update-value (/ timer 1000.0) timer)
    (send slope-snip set-active #t)
    (send slope-snip update-value (sim-state-current-slope nstate) timer)
    (send pb end-edit-sequence)
    nstate))

(define (on-new-data-frame df)
  (define track
    (send df select* "lat" "lon" #:filter valid-only))
  (send map-widget clear-items)         ; any previous track
  ;; Setup the map
  (send map-widget add-track track 1)
  ;; needs to be done after the tracks are added
  (define pen (send the-pen-list find-or-create-pen "blue" 2 'solid))
  (send map-widget set-track-group-pen 1 pen)
  (send elevation-profile-snip set-elevation-profile df)
  ;; need to be done after the map is shown
  (send map-widget resize-to-fit 1)
  (send map-widget center-map 1)
  (send map-widget set-current-location (vector-ref track 0)))

(define (simulation-task in-channel)
  (send message-snip set-message "No tack data loaded")
  (let loop ((state (sim-state-make-initial)))
    (let ((task (async-channel-get in-channel)))
      (cond
        ((is-a? task data-frame%)
         (on-new-data-frame task)
         (send message-snip set-message "Simulation is paused")
         (when (send task get-property 'name)
           (send message-snip set-message
                 (send task get-property 'name)
                 5000))
         (reset-telemetry-snips)
         (loop (sim-state-set-data-frame state task)))
        ((output-port? task)
         (loop (sim-state-set-control state task)))
        ((telemetry? task)
         (let ((delta (sim-state-delta state)))
           (if (> delta 0)
               (loop (on-new-telemetry state task delta))
               (loop state))))
        ((eq? task 'reset)
         (send message-snip set-message "Simulation is paused")
         (when (sim-state-df state)
           (send message-snip set-message "Back to start" 5000))
         (reset-telemetry-snips)
         (loop (sim-state-reset state)))
        ((eq? task 'pause)
         (send message-snip set-message "Simulation is paused")
         (loop (sim-state-set-paused state #t)))
        ((eq? task 'play)
         (if (sim-state-df state)
             (begin
               (send message-snip replace-message "Resumed simulation" 5000)
               (loop (sim-state-set-paused state #f)))
             (begin
               (send message-snip set-message "Cannot Start.  Load a data track first" 5000)
               (loop state))))
        (#t
         (printf "Ignoring unknown task: ~a~%" task)
         (loop state))))))

(define task
  (thread (lambda () (simulation-task ch))))

(define (connect port)
  (define-values (in out) (tcp-connect "localhost" port))
  ;; (close-output-port out)
  (thread (lambda () (read-telemetry in ch)))
  ;; send the output port to the simulation task
  (async-channel-put ch out))
