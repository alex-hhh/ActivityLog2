#lang racket

;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015, 2018 Alex Harsányi <AlexHarsanyi@gmail.com>
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


(require "../rkt/fit-file/fit-file.rkt")

(define location-data-file "../1-1018-ANTFS-8-0.FIT")
;;(define data-file "../20140328-154605-1-1499-ANTFS-4-0.FIT")
;;define data-file "../20140404-154907-1-1499-ANTFS-4-0.FIT")
(define data-file "../1-1499-ANTFS-15-0.FIT")

(define location-fit-reader%
  (class fit-event-dispatcher%
    (init)
    (super-new)

    (define/override (on-file-id file-id)
      (printf "on-file-id ~a~%" file-id))
    (define/override (on-file-creator creator)
      (printf "on-file-creator ~a~%" creator))
    (define/override (on-location location)
      (printf "on-location ~a~%" location))

    ;; NOTE: on-activity and on-session are also events, so the user could
    ;; call on-event for those as well if needed.  this could be important if
    ;; timer-start/timer-stop events are tracked.
    (define/override (on-event event) #f)
    (define/override (on-other type data)
      (printf "on-other ~a~%" type)
      (for-each (lambda (item)
                  (printf "***   ~a~%" item))
                data))

    ))

(define device-info-monitor%
  (class fit-event-dispatcher%
    (init)
    (super-new)
    (define/override (on-device-info di)
      (printf "on-device-info ~a~%" di))))


(define (foo)
  (let ((reader (make-fit-data-stream data-file))
        (dispatcher (new device-info-monitor%)))
    (read-fit-records reader dispatcher)))
