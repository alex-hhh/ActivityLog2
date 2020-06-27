#lang racket/base
;; sport-charms.rkt -- utilities related to individual sports
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015, 2018, 2019, 2020 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require db/base
         pict
         racket/async-channel
         racket/class
         racket/contract
         racket/draw
         racket/list
         racket/match
         "color-theme.rkt"
         "dbapp.rkt"
         "dbutil.rkt"
         "utilities.rkt")

(define (color? c) (is-a? c color%))
(define (bitmap? b) (is-a? b bitmap%))
(define sport-id? (or/c (and/c number? (not/c negative?)) #f))
(define positive-number? (and/c number? (not/c negative?)))
(define zone-metric? (or/c 1 2 3))
(define swim-stroke? (or/c 0 1 2 3 4 5 6 #f))
(define sport-zones? (or/c #f (listof number?)))

(provide/contract
 [get-sport-color (->* (sport-id? sport-id?) (boolean?) color?)]
 [get-sport-name (-> sport-id? sport-id? string?)]
 [get-sport-letter (-> sport-id? sport-id? string?)]
 [get-sport-bitmap (-> sport-id? sport-id? bitmap?)]
 [get-sport-bitmap-colorized (-> sport-id? sport-id? bitmap?)]
 [get-swim-stroke-name (-> swim-stroke? string?)]
 [get-swim-stroke-names (-> (listof (cons/c swim-stroke? string?)))]
 [get-swim-stroke-color (-> swim-stroke? color?)]
 [get-sport-names (-> (listof (vector/c string? sport-id? sport-id?)))]
 [get-sport-names-in-use (-> (listof (vector/c string? sport-id? sport-id?)))]
 [get-athlete-ftp (->* () (connection?) (or/c #f positive-number?))]
 [put-athlete-ftp (->* (positive-number?) (connection?) any/c)]
 [get-athlete-swim-tpace (->* () (connection?) (or/c #f positive-number?))]
 [put-athlete-swim-tpace (->* (positive-number?) (connection?) any/c)]
 [get-athlete-gender (->* (connection?) () (or/c #f 0 1))]
 [put-athlete-gender (->* ((or/c 0 1)) (connection?) any/c)]
 [get-athlete-dob (->* (connection?) () (or/c #f positive-number?))]
 [put-athlete-dob (->* (positive-number?) (connection?) any/c)]
 [get-athlete-height (->* (connection?) () (or/c #f positive-number?))]
 [put-athlete-height (->* (positive-number?) (connection?) any/c)]
 )

(provide is-runnig?
         is-cycling?
         is-lap-swimming?
         is-swimming?)


;;...................................................... get-sport-color ....

(define-runtime-path walking-icon-file "../img/walking-64.png")
(define-runtime-path running-icon-file "../img/running-64.png")
(define-runtime-path biking-icon-file "../img/regular_biking-64.png")
(define-runtime-path swimming-icon-file "../img/swimming-64.png")
(define-runtime-path weightlift-icon-file "../img/weightlift-64.png")
(define-runtime-path mountain-biking-icon-file "../img/mountain_biking-64.png")
(define-runtime-path sail-boat-icon-file "../img/sail_boat-64.png")
(define-runtime-path trekking-icon-file "../img/trekking-64.png")
(define-runtime-path stopwatch-icon-file "../img/stopwatch-64.png")
(define-runtime-path yoga-icon-file "../img/yoga-64.png")
(define-runtime-path note-icon-file "../img/note-64.png")
(define-runtime-path skiing-icon-file "../img/skiing-64.png")

(define *large-bitmaps*
  (hash
   "walk" (read-bitmap walking-icon-file)
   "run" (read-bitmap running-icon-file)
   "bike" (read-bitmap biking-icon-file)
   "swim" (read-bitmap swimming-icon-file)
   "weight-lift" (read-bitmap weightlift-icon-file)
   "mountain-bike" (read-bitmap mountain-biking-icon-file)
   "sail" (read-bitmap sail-boat-icon-file)
   "hike" (read-bitmap trekking-icon-file)
   "timer" (read-bitmap stopwatch-icon-file)
   "yoga" (read-bitmap yoga-icon-file)
   "note" (read-bitmap note-icon-file)
   "ski" (read-bitmap skiing-icon-file)
   ))

(define *sport-letters*
  (hash
   "run" "R"
   "bike" "B"
   "swim" "S"
   "weight-lift" "W"
   "mountain-bike" "B"
   "timer" "O"
   "note" "N"))

(define *default-bitmap* "timer")

(struct sport-info (id parent-id name icon))

(define *sport-info* #f)
(define *sub-sport-info* #f)
(define *swim-stroke-names* #f)
(define *sport-names* '())

(define (init-sport-charms db)

  (let ((h (make-hash)))
    (for (((id name icon)
           (in-query db "select id, name, icon from E_SPORT where id != 254")))
      (hash-set! h id (sport-info id #f name icon)))
    (set! *sport-info* h))

  (let ((h (make-hash)))
    (for (((id parent-id name icon)
           (in-query db "select id, sport_id, name, icon from E_SUB_SPORT where id != 254")))
      (hash-set! h id (sport-info id parent-id name icon)))
    (set! *sub-sport-info* h))

  (let ((h (make-hash)))
    (for (((id name)
           (in-query db "select id, name from E_SWIM_STROKE")))
      (hash-set! h id name))
    (set! *swim-stroke-names* h))

  (let ((sport-names (list (vector "All Sports" #f #f))))
    (for ((sid (in-list (sort (hash-keys *sport-info*) <))))
      (let ((sport (hash-ref *sport-info* sid)))
        (set! sport-names (cons (vector (sport-info-name sport) (sport-info-id sport) #f)
                                sport-names))
        (for ((sub-sport (in-hash-values *sub-sport-info*)))
          (when (eqv? (sport-info-id sport) (sport-info-parent-id sub-sport))
            (set! sport-names (cons (vector
                                     (sport-info-name sub-sport)
                                     (sport-info-id sport) (sport-info-id sub-sport))
                                    sport-names)))))
      (set! *sport-names* (reverse sport-names)))))

(define log-event-source (make-log-event-source))

(define (maybe-init-sport-charms)

  (let loop ((item (async-channel-try-get log-event-source)))
    (when item
      (match-define (list tag data) item)
      (when (eq? tag 'database-opened)
        (set! *sport-info* #f)
        (set! *sub-sport-info* #f)
        (set! *swim-stroke-names* #f)
        (set! *sport-names* '()))
      (loop (async-channel-try-get log-event-source))))

  (unless *sport-info*
    (when (current-database)
      (init-sport-charms (current-database)))))

(define (get-sport-info sport sub-sport)
  (maybe-init-sport-charms)
  (if (or (not sub-sport) (= sub-sport 0))
      (if (hash? *sport-info*)
          (hash-ref *sport-info* sport #f)
          #f)
      (if (hash? *sub-sport-info*)
          (hash-ref *sub-sport-info* sub-sport #f)
          #f)))

(define (get-sport-name sport sub-sport)
  (let ((info (get-sport-info sport sub-sport)))
    (if info
        (sport-info-name info)
        "Other")))

(define (get-sport-color sport sub-sport [dark? #f])
  (let ((color-map (if dark? (sport-colors-dark) (sport-colors))))
    (or (hash-ref color-map (vector sport sub-sport) #f)
        (hash-ref color-map (vector sport #f) #f)
        (hash-ref color-map (vector 0 #f) #f))))

(define (get-sport-bitmap sport sub-sport)
  (let ((info (get-sport-info sport sub-sport)))
    (hash-ref *large-bitmaps*
              (if info (sport-info-icon info) *default-bitmap*)
              (hash-ref *large-bitmaps* *default-bitmap*))))

(define (get-sport-letter sport sub-sport)
  (let ((info (get-sport-info sport sub-sport)))
    (hash-ref *sport-letters*
              (if info (sport-info-icon info) *default-bitmap*)
              (hash-ref *sport-letters* *default-bitmap*))))

(define (get-sport-bitmap-colorized sport sub-sport)
  (let* ((b (bitmap (get-sport-bitmap sport sub-sport)))
         (r (filled-rounded-rectangle (+ (pict-width b) 10)
                                      (+ (pict-height b) 10)
                                      -0.05
                                      #:draw-border? #f)))
    (pict->bitmap
     (cc-superimpose (colorize r (get-sport-color sport sub-sport)) b))))

(define (get-swim-stroke-name swim-stroke-id)
  (maybe-init-sport-charms)
  (hash-ref *swim-stroke-names*
            swim-stroke-id
            "Unknown"))

(define (get-swim-stroke-names)
  (maybe-init-sport-charms)
  (for/list (((k v) (in-hash *swim-stroke-names*)))
    (cons k v)))

(define (get-swim-stroke-color stroke)
  (cond ((assq stroke (swim-stroke-colors)) => cdr)
        (#t "gray")))

(define (get-sport-names)
  (maybe-init-sport-charms)
  *sport-names*)

;; Implementation detail: We add the "generic" sports to the list if any of
;; their sub-sports show up.  E.g. if we have "Lap Swimming" activities (5,
;; 17) we add the "Swimming" activity (5, #f)

(define (get-sport-names-in-use)
  (maybe-init-sport-charms)
  (let ((in-use (query-rows (current-database) "select distinct S.sport_id, S.sub_sport_id from A_SESSION S")))
    (filter (lambda (s)
              (let ((sport (vector-ref s 1))
                    (sub-sport (vector-ref s 2)))
                (or (and (eq? sport #f) (eq? sub-sport #f)) ; all sports always gets through
                    (findf (lambda (u)
                             (let ((u-sport (vector-ref u 0))
                                   (u-sub-sport (vector-ref u 1)))
                               (set! u-sport (if (sql-null? u-sport) #f u-sport))
                               (set! u-sub-sport (if (sql-null? u-sub-sport) #f u-sub-sport))
                               (and (eq? sport u-sport)
                                    (or (eq? sub-sport #f)
                                        (eq? sub-sport u-sub-sport)))))
                           in-use))))
            *sport-names*)))

(define (get-athlete-ftp (db (current-database)))
  (let ((v (query-maybe-value db "select ftp from ATHLETE")))
    (if (sql-null? v) #f v)))

(define (put-athlete-ftp ftp (db (current-database)))
  (query-exec db "update ATHLETE set ftp = ?" (or ftp sql-null)))

(define (get-athlete-swim-tpace (db (current-database)))
  (let ((v (query-maybe-value db "select swim_tpace from ATHLETE")))
    (if (sql-null? v) #f v)))

(define (put-athlete-swim-tpace swim-tpace (db (current-database)))
  (query-exec db "update ATHLETE set swim_tpace = ?" (or swim-tpace sql-null)))

(define (get-athlete-gender (db (current-database)))
  (let ((v (query-maybe-value db "select gender from ATHLETE")))
    (if (sql-null? v) #f v)))

(define (put-athlete-gender gender (db (current-database)))
  (query-exec db "update ATHLETE set gender = ?" (or gender sql-null)))

(define (get-athlete-dob (db (current-database)))
  (let ((v (query-maybe-value db "select strftime('%s', dob) from ATHLETE")))
    (cond
      ((sql-null? v) #f)
      ((string? v) (string->number v))
      ((number? v) v)
      (#t (error "get-athlete-dob")))))

(define (put-athlete-dob dob (db (current-database)))
  (query-exec db "update ATHLETE set dob = date(?, 'unixepoch', 'localtime')" (or dob sql-null)))

(define (get-athlete-height (db (current-database)))
  (let ((v (query-maybe-value db "select height from ATHLETE")))
    (if (sql-null? v) #f v)))

(define (put-athlete-height height (db (current-database)))
  (query-exec db "update ATHLETE set height = ?" (or height sql-null)))

;; We use too many ways to represent sport ids :-(
(define (sport-id sport)
  (cond ((number? sport) sport)
        ((vector? sport) (vector-ref sport 0))
        ((cons? sport) (car sport))
        (#t #f)))

(define (sub-sport-id sport)
  (cond ((number? sport) #f)
        ((vector? sport) (vector-ref sport 1))
        ((cons? sport) (cdr sport))
        (#t #f)))

(provide sport-id sub-sport-id)

(define (is-runnig? sport)
  (eqv? (sport-id sport) 1))

(define (is-cycling? sport)
  (eqv? (sport-id sport) 2))

(define (is-lap-swimming? sport)
  (and (eqv? (sport-id sport) 5)
       (eqv? (sub-sport-id sport) 17)))

(define (is-swimming? sport)
  (eqv? (sport-id sport) 5))
