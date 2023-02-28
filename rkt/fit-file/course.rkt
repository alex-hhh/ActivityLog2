#lang racket/base
;; course.rkt -- functions for reading and writing FIT course files
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2023 Alex Harsányi <AlexHarsanyi@gmail.com>
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
;;
;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.

(require data-frame
         racket/class
         racket/contract
         racket/dict
         racket/math
         "../intervals.rkt"
         "fit-defs.rkt"
         "fit-file.rkt")

(provide/contract
 (df-read/course (-> (or/c path-string? input-port?) data-frame?))
 (df-write/course (->* (data-frame? (or/c path-string? output-port?))
                       (#:name (or/c #f string?))
                       any/c)))

(define (get-timestamp item)
  ;; NOTE: timestamp is already transformed using `fit-time->unix-time` by the
  ;; record conversions in fit-defs.rkt
  (or (dict-ref item 'timestamp #f)
      (let ((fit-ts (dict-ref item 253 #f)))
        (if fit-ts (fit-time->unix-time fit-ts) #f))))

;; Reader for course FIT files, you want to use `df-read/course` instead.
(define course-reader%
  (class fit-event-dispatcher%
    (init)
    (super-new)

    (define course-name #f)
    (define course-sport #f)
    (define course-sub-sport #f)

    ;; This is a bit pattern (uint32z), unclear what the patterns mean.  The
    ;; sample file I have has a value of 3 (processed + valid) set.
    ;;
    ;; processed  0x00000001
    ;; valid      0x00000002
    ;; time       0x00000004
    ;; distance   0x00000008
    ;; position   0x00000010
    ;; heart_rate 0x00000020
    ;; power      0x00000040
    ;; cadence    0x00000080
    ;; training   0x00000100
    ;; navigation 0x00000200
    ;; bikeway    0x00000400
    (define course-capabilities #f)

    (define timestamp-series (make-series "timestamp" #:capacity 200))
    (define lat-series (make-series "lat" #:capacity 200))
    (define lon-series (make-series "lon" #:capacity 200))
    (define dst-series (make-series "dst" #:capacity 200))
    (define alt-series (make-series "alt" #:capacity 200))

    ;; These series are for the course points data frame, which records
    ;; important waypoints along the course -- most of the time, this will
    ;; contain navigation instructions (e.g. "turn left", "turn right")
    (define timestamp2-series (make-series "timestamp" #:capacity 20))
    (define lat2-series (make-series "lat" #:capacity 20))
    (define lon2-series (make-series "lon" #:capacity 20))
    (define dst2-series (make-series "dst" #:capacity 20))
    (define name2-series (make-series "name" #:capacity 20))
    (define type2-series (make-series "type" #:capacity 20))

    ;; The file-id record contains the serial number, manufacturer/product
    ;; combination and file type.  It contains no useful information for us,
    ;; so we discard it, except for checking that we have a course file (since
    ;; there can be other type of FIT files)
    (define/override (on-file-id file-id)
      (define type (dict-ref file-id 'type #f))
      (unless (equal? type 'course)
        (error (format "course-reader%: not a course file: ~a" type)))
      (void))

    ;; The file-creator record contains the hardware and software ID of the
    ;; "Device" that created the file.  It contains no useful information for
    ;; us, so we discard it.
    (define/override (on-file-creator creator)
      (void))

    (define/override (on-course c)
      (set! course-name (dict-ref c 'name #f))
      (set! course-capabilities (dict-ref c 'capabilities #f))
      (set! course-sport (dict-ref c 'sport #f))
      (set! course-sub-sport (dict-ref c 'sub-sport #f)))

    (define/override (on-course-point p)
      (series-push-back timestamp2-series (get-timestamp p))
      (series-push-back lat2-series (dict-ref p 'position-lat #f))
      (series-push-back lon2-series (dict-ref p 'position-long #f))
      (series-push-back dst2-series (dict-ref p 'distance #f))
      (series-push-back name2-series (dict-ref p 'name #f))
      (series-push-back type2-series (dict-ref p 'type #f)))

    ;; Events in a course file mark the start and end of the course, a "start"
    ;; event at the start and "stop-disable-all" event at the end.  They
    ;; contain no useful information for us, so we discard these.
    (define/override (on-event e)
      (void))

    ;; The lap record in a course file contains summary information about the
    ;; course and it is mandatory in a course file.  It contains start, end
    ;; locations, total time, total distance, total ascent, total descent.
    ;; Since all this can be inferred from the actual course records, we
    ;; discard this information here (technically it could be different from
    ;; the computed data...)
    (define/override (on-lap l)
      (void))

    (define/override (on-record r)
      (series-push-back timestamp-series (get-timestamp r))
      (series-push-back lat-series (dict-ref r 'position-lat #f))
      (series-push-back lon-series (dict-ref r 'position-long #f))
      (series-push-back dst-series (dict-ref r 'distance #f))
      (series-push-back alt-series (dict-ref r 'altitude #f)))

    ;; Construct the data frame from existing series and return it.
    (define/public (get-data-frame)
      (define cpdf (make-data-frame))
      (df-add-series! cpdf timestamp2-series)
      (df-add-series! cpdf lat2-series)
      (df-add-series! cpdf lon2-series)
      (df-add-series! cpdf dst2-series)
      (df-add-series! cpdf name2-series)
      (df-add-series! cpdf type2-series)
      (df-set-sorted! cpdf "timestamp" <)
      (df-set-sorted! cpdf "dst" <)

      (define df (make-data-frame))
      (df-add-series! df timestamp-series)
      (df-add-series! df lat-series)
      (df-add-series! df lon-series)
      (df-add-series! df dst-series)
      (df-add-series! df alt-series)
      (df-set-sorted! df "timestamp" <)
      (df-set-sorted! df "dst" <)
      (df-put-property! df 'course-points cpdf)
      (when course-name
        (df-put-property! df 'name (bytes->string/utf-8 course-name)))
      (when course-capabilities
        (df-put-property! df 'capabilities course-capabilities))
      (when course-sport
        (df-put-property! df 'sport course-sport))
      (when course-sub-sport
        (df-put-property! df 'sub-sport course-sub-sport))
      df)

    ))

;; Read a FIT file representing a course and return it as a data frame with
;; "lat", "lon", "dst", "alt" and "timestamp" data series.  A secondary data
;; frame is attached to the 'course-points property, this represents the
;; "points of interest" in the course, this data frame usually containns
;; navigation instructions.
(define (df-read/course port-or-file)
  (let ([stream (make-fit-data-stream port-or-file)]
        [consumer (new course-reader%)])
    (read-fit-records stream consumer)
    (send consumer get-data-frame)))

;; NOTE: Message definition is a list of elements each element contains: name
;; (tag), message id, number of items, underlying data type

(define course-message-number 26)
(define course-message-definition
  '((sport        4 1  enum)
    (name         5 15 string)
    (capabilities 6 1  uint32z)          ; use 3 as the capability value
    (sub-sport    7 1  enum)))

(define lap-message-number 19)
(define lap-message-definition
  '((timestamp           253 1 uint32)
    (start-time          2   1 uint32z)
    (start-position-lat  3   1 sint32)
    (start-position-long 4   1 sint32)
    (end-position-lat    5   1 sint32)
    (end-position-long   6   1 sint32)
    (total-elapsed-time  7   1 uint32)
    (total-timer-time    8   1 uint32)
    (total-distance      9   1 uint32)
    (avg-speed           13  1 uint16)
    (total-ascent        21  1 uint16)
    (total-descent       22  1 uint16)
    (message-index       254 1 uint16))) ; set to 0, not sure what this is

(define event-message-number 21)
(define event-message-definition
  '((timestamp   253 1 uint32)
    (event       0   1 enum)            ; timer = 0
    (event-type  1   1 enum)            ; start = 0; stop_disable_all = 9
    (event-group 4   1 uint8)))         ; use 0

(define record-message-number 20)
(define record-message-definition
  '((timestamp     253 1 uint32)
    (position-lat  0   1 sint32)
    (position-long 1   1 sint32)
    (altitude      2   1 uint16)
    (distance      5   1 uint32)))

(define course-point-message-number 32)
(define course-point-message-definition
  '((timestamp     1 1  uint32)
    (position-lat  2 1  sint32)
    (position-long 3 1  sint32)
    (distance      4 1  uint32)
    (type          5 1  enum)
    (name          6 15 string)))

;; Convert a sport symbol to its FIT sport numeric ID
(define (->sport-id sport)
  (cond
    ((exact-nonnegative-integer? sport) sport)
    ((symbol? sport)
     (define sport-id
       (for/first ([s (in-list *sport*)] #:when (equal? (cdr s) sport))
         (car s)))
     (if sport-id
         sport-id
         (error (format "->sport-id: unknown sport: ~a" sport))))
    (#t
     (error (format "->sport-id: unknown sport: ~a" sport)))))

;; Convert a sub-sport symbol to its FIT sub sport numeric ID
(define (->sub-sport-id sub-sport)
  (cond
    ((exact-nonnegative-integer? sub-sport) sub-sport)
    ((symbol? sub-sport)
     (define sub-sport-id
       (for/first ([s (in-list *sub-sport*)] #:when (equal? (cdr s) sub-sport))
         (car s)))
     (if sub-sport-id
         sub-sport-id
         (error (format "->sport-id: unknown sport: ~a" sub-sport))))
    ((equal? sub-sport #f)              ; undefined sub-sport is permitted
     #f)
    (#t
     (error (format "->sport-id: unknown sub-sport: ~a" sub-sport)))))

;; Writer of FIT course files, you want to use `df-write/course` instead
(define fit-course-file%
  (class fit-output-file%
    (init df             ; data-frame containing the course points
          [name #f]      ; if #f, taken from the data-frame 'name property
          [sport #f]     ; if #f, taken from the data-frame 'sport property
          [sub-sport #f] ; if #f, taken from the data-frame 'sub-sport property
          )
    (inherit put-message-definition put-message)

    (unless (> (df-row-count df) 0)
      (error "fit-output-file%: data frame cannot be empty"))
    (unless (df-contains? df "timestamp" "lat" "lon" "dst")
      (error "fit-course-file%: data frame missing required series"))
    (unless (df-contains/any? df "alt" "calt")
      (error "fit-course-file%: data frame missing altitude series"))

    (define time-created (exact-round (df-ref df 0 "timestamp")))

    ;; Initialize the parent class.  This will write the file id and file type
    ;; messages.
    (super-new
     [file-type 6]                      ; 6 is course file
     [time-created time-created])

    ;; Put the course mesage, note that local message index 0 can be reused
    ;; straight away, since there is only one course mesage in the file.
    (put-message-definition course-message-number 0 course-message-definition)
    (put-message
     course-message-number
     `((sport . ,(->sport-id (or sport (df-get-property df 'sport #f))))
       (name . ,(let ([name (or name (df-get-property df 'name #f))])
                  (and name (mk-fit-string name 15))))
       (capabilities . 3)
       (sub-sport . ,(->sub-sport-id (or sub-sport (df-get-property df 'sub-sport #f))))))

    ;; Put the lap message with summary about the course, note that local
    ;; message index can be reused straight away, since there is only one lap
    ;; message in the file.
    (put-message-definition lap-message-number 0 lap-message-definition)

    (define first-row 0)
    (define last-row (sub1 (df-row-count df)))
    (define altitude-series (if (df-contains? df "calt") "calt" "alt"))

    (let* ([start-lat (df-ref df first-row "lat")]
           [start-long (df-ref df first-row "lon")]
           [end-lat (df-ref df last-row "lat")]
           [end-long (df-ref df last-row "lon")]
           [elapsed (- (df-ref df last-row "timestamp")
                       (df-ref df first-row "timestamp"))]
           [distance (- (df-ref df last-row "dst")
                        (df-ref df first-row "dst"))]
           [avg-speed (/ distance elapsed)])
      (define-values (ascent descent)
        (total-ascent-descent df altitude-series first-row last-row))

      (put-message
       lap-message-number
       `((timestamp . ,(unix-time->fit-time time-created))
         (start-time . ,(unix-time->fit-time time-created))
         (start-position-lat . ,(degrees->semicircles start-lat))
         (start-position-long . ,(degrees->semicircles start-long))
         (end-position-lat . ,(degrees->semicircles end-lat))
         (end-position-long . ,(degrees->semicircles end-long))
         (total-elapsed-time . ,(exact-round (* elapsed 1000.0)))
         (total-timer-time . ,(exact-round (* elapsed 1000.0)))
         (total-distance . ,(exact-round (* distance 100.0)))
         (avg-speed . ,(exact-round (* avg-speed 1000.0)))
         (total-ascent . ,(exact-round ascent))
         (total-descent . ,(exact-round descent))
         (message-index . 0))))

    (put-message-definition event-message-number 0 event-message-definition)
    (put-message-definition record-message-number 1 record-message-definition)
    ;; NOT used for now
    ;;(put-message-definition course-point-message-number 2 course-point-message-definition)

    (put-message
     event-message-number
     `((timestamp . ,(unix-time->fit-time (df-ref df first-row "timestamp")))
       (event . 0)
       (event-type . 0)                 ; start
       (event-group . 0)))

    (for ([(ts lat lon alt dst)
           (in-data-frame df "timestamp" "lat" "lon" altitude-series "dst")])
      (put-message
       record-message-number
       `((timestamp . ,(unix-time->fit-time (exact-round ts)))
         (position-lat . ,(degrees->semicircles lat))
         (position-long . ,(degrees->semicircles lon))
         (altitude . ,(exact-round (* (+ alt 500) 5)))
         (distance . ,(exact-round (* dst 100))))))

    (put-message
     event-message-number
     `((timestamp . ,(unix-time->fit-time (df-ref df last-row "timestamp")))
       (event . 0)
       (event-type . 9)                 ; stop-disable-all
       (event-group . 0)))

    ))

;; Write a data frame as a FIT course file.  The data frame needs to have
;; "lat", "lon", "dst", "alt" and "timestamp" series (origin time for the
;; timestamp does not matter, but it must be present and valid).
;; Additionally, the course must either have a name property or NAME must be
;; passed on as an argument.
;;
;; NOTE: course points are not written to the file even though they are read
;; by `df-read/course`.  This is a limitation.  "Course Points" contain
;; instructions for the course, such as "left turn", "right turn", etc.
(define (df-write/course df outp #:name (name #f))
  (define data (send (new fit-course-file%
                          [df df]
                          [name name])
                     get-fit-data))
  (if (path-string? outp)
      (call-with-output-file outp
        #:mode 'binary
        #:exists 'truncate/replace
        (lambda (o)
          (write-bytes data o)))
      (write-bytes data outp)))
