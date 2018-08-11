#lang racket

;; This file ilustrates how to process a FIT file.  It will print out markers
;; from an activity.

(require "../rkt/fit-file/fit-file.rkt")
(require "../rkt/fit-file/fit-defs.rkt")
(require "../rkt/utilities.rkt")
(require "../rkt/fmt-util.rkt")

(define activity "5BN53355.FIT")        ; the activtiy we use

(define pp-markers%
  (class fit-event-dispatcher% (init) (super-new)

    (define (get-timestamp item)
      (or (assq1 'timestamp item)
          (let ((fit-ts (assq1 253 item)))
            (if fit-ts (fit-time->unix-time fit-ts) #f))))

    (define (output-timestamp item)
      (let ((ts (get-timestamp item)))
        (when ts
          (printf "~a " (time-of-day->string ts)))))

    ;; To determine the length of a pause, we need to keep track of the
    ;; timestamps for start and stop events.
    (define start-timestamp #f)
    (define stop-timestamp #f)

    ;; Do determine the length of a standing event, we need to keep track of
    ;; the timestamps (sitting/standing time is available for Vector pedals)
    (define standing-timestamp #f)
    (define sitting-timestamp #f)

    (define/override (on-file-id file-id)
      (output-timestamp file-id)
      (printf "file-id: ~a~%" file-id))

    (define/override (on-file-creator creator)
      (output-timestamp creator)
      (printf "file-creator: ~a~%" creator))

    (define/override (on-event event)
      (output-timestamp event)
      (let ((e (dict-ref event 'event #f))
            (t (dict-ref event 'event-type #f)))
        (cond
          ;; Timer start.
          ((and (eq? e 'timer) (eq? t 'start))
           (set! start-timestamp (get-timestamp event))
           (let ((pause (if (and start-timestamp stop-timestamp)
                            (- start-timestamp stop-timestamp)
                            #f)))
             (if pause
                 (printf "timer started (pause ~a)~%"
                         (duration->string pause))
                 (printf "timer started~%"))))
          ;; Timer stop
          ((and (eq? e 'timer) (eq? t 'stop-all))
           (set! stop-timestamp (get-timestamp event))
           (printf "timer stop-all~%"))
          ;; Recomended Recovery time
          ((and (eq? e 'recovery-time) (eq? t 'marker))
           (printf "recomented recovery time: ~a hours~%"
                   (exact-round (/ (dict-ref event 'data #f) 60.0))))
          ;; Recovery status
          ((and (eq? e 'recovery-status) (eq? t 'marker))
           (printf "recovery status: ~a hours~%"
                   (exact-round (/ (dict-ref event 'data #f) 60.0))))
          ;; Rider position change
          ((and (eq? e 'rider-position-change) (eq? t 'marker))
           (let ((standing? (eqv? (dict-ref event 'data #f) 1)))
             (if standing?
                 (set! standing-timestamp (get-timestamp event))
                 (set! sitting-timestamp (get-timestamp event)))
             (if standing?
                 (printf "rider-position-change: now standing~%")
                 (let ((stand-duration (if (and standing-timestamp sitting-timestamp)
                                           (- sitting-timestamp standing-timestamp)
                                           #f)))
                   (if stand-duration
                       (printf "rider-position-change: now sitting (standing for ~a)~%"
                               (duration->string stand-duration))
                       (printf "rider-position-change: now sitting~%"))
                   (set! standing-timestamp #f)))))
          ;; VO2 max
          ((and (eq? e 'vo2-max) (eq? t 'marker))
           (printf "vo2 max: ~a~%" (dict-ref event 'data #f)))
          ;; Anything else, just print the raw event
          (#t
           (printf " *** event: ~a~%" event)))))
    ))

(define (pp-markers file)
  (let ((stream (make-fit-data-stream file))
        (consumer (new pp-markers%)))
    (read-fit-records stream consumer)
    'done))
