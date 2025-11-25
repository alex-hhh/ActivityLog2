#lang racket/base
;; sport-charms.rkt -- utilities related to individual sports
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015, 2018-2020, 2023-2025 Alex Harsányi <AlexHarsanyi@gmail.com>
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
         racket/class
         racket/contract
         racket/draw
         racket/list
         racket/class
         "color-theme.rkt"
         "dbutil.rkt")

(define (color? c) (is-a? c color%))
(define (bitmap? b) (is-a? b bitmap%))
(define sport-id? (or/c exact-nonnegative-integer? #f))
(define swim-stroke? (or/c 0 1 2 3 4 5 6 #f))

(define sport-charms%/c
  (class/c
   (init [dbc connection?])
   [get-sport-color (->*m (sport-id? sport-id?) (boolean?) color?)]
   [get-sport-name (->m sport-id? sport-id? string?)]
   [get-sport-letter (->m sport-id? sport-id? string?)]
   [get-sport-bitmap (->m sport-id? sport-id? bitmap?)]
   [get-sport-bitmap-colorized (->m sport-id? sport-id? bitmap?)]
   [get-swim-stroke-name (->m swim-stroke? string?)]
   [get-swim-stroke-names (->m (listof (cons/c swim-stroke? string?)))]
   [get-swim-stroke-color (->m swim-stroke? color?)]
   [get-sport-names (->m (listof (vector/c string? sport-id? sport-id?)))]
   [get-sport-names-in-use (->m (listof (vector/c string? sport-id? sport-id?)))]
   [get-athlete-ftp (->m (or/c #f positive?))]
   [put-athlete-ftp (->m positive? any/c)]
   [get-athlete-swim-tpace (->m (or/c #f positive?))]
   [put-athlete-swim-tpace (->m positive? any/c)]
   [get-athlete-gender (->m (or/c #f 0 1))]
   [put-athlete-gender (->m (or/c 0 1) any/c)]
   [get-athlete-dob (->m (or/c #f positive?))]
   [put-athlete-dob (->m positive? any/c)]
   [get-athlete-height (->m (or/c #f positive?))]
   [put-athlete-height (->m positive? any/c)]))



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
(define-runtime-path diving-icon-file "../img/diving-64.png")
(define-runtime-path sup-icon-file "../img/paddleboarding-64.png")
(define-runtime-path kayak-icon-file "../img/kayak-64.png")

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
   "sup" (read-bitmap sup-icon-file)
   "kayak" (read-bitmap kayak-icon-file)
   "diving" (read-bitmap diving-icon-file)
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

(define (init-sport-charms db)

  (define sport-info
    (for/hash (((id name icon)
                (in-query db "select id, name, icon from E_SPORT where id != 254")))
      (values id (sport-info id #f name icon))))

   (define sub-sport-info
     (for/hash (((id parent-id name icon)
                 (in-query db "select id, sport_id, name, icon from E_SUB_SPORT where id != 254")))
       (values id (sport-info id parent-id name icon))))

    (define swim-stroke-names
      (for/hash (((id name)
                  (in-query db "select id, name from E_SWIM_STROKE")))
        (values id name)))

    (define sport-names
      (let ((sport-names (list (vector "All Sports" #f #f))))
        (for ((sid (in-list (sort (hash-keys sport-info) <))))
          (let ((sport (hash-ref sport-info sid)))
            (set! sport-names (cons (vector (sport-info-name sport) (sport-info-id sport) #f)
                                    sport-names))
            (for ((sub-sport (in-hash-values sub-sport-info)))
              (when (eqv? (sport-info-id sport) (sport-info-parent-id sub-sport))
                (set! sport-names (cons (vector
                                         (sport-info-name sub-sport)
                                         (sport-info-id sport) (sport-info-id sub-sport))
                                        sport-names))))))
        (reverse sport-names)))

  (values sport-info sub-sport-info swim-stroke-names sport-names))

(define sport-charms%
  (class object%
    (init-field dbc)

    (define-values (sport-info sub-sport-info swim-stroke-names sport-names)
      (init-sport-charms dbc))

    (define/private (get-sport-info sport sub-sport)
      (if (or (not sub-sport) (= sub-sport 0))
          (hash-ref sport-info sport #f)
          (hash-ref sub-sport-info sub-sport #f)))

    (define/public (get-sport-name sport sub-sport)
      (let ((info (get-sport-info sport sub-sport)))
        (if info
            (sport-info-name info)
            "Other")))

    (define/public (get-sport-color sport sub-sport [dark? #f])
      (let ((color-map (if dark? (sport-colors-dark) (sport-colors))))
        (or (hash-ref color-map (vector sport sub-sport) #f)
            (hash-ref color-map (vector sport #f) #f)
            (hash-ref color-map (vector 0 #f) #f))))

    (define/public (get-sport-bitmap sport sub-sport)
      (let ((info (get-sport-info sport sub-sport)))
        (hash-ref *large-bitmaps*
                  (if info (sport-info-icon info) *default-bitmap*)
                  (hash-ref *large-bitmaps* *default-bitmap*))))

    (define/public (get-sport-letter sport sub-sport)
      (let ((info (get-sport-info sport sub-sport)))
        (hash-ref *sport-letters*
                  (if info (sport-info-icon info) *default-bitmap*)
                  (hash-ref *sport-letters* *default-bitmap*))))

    (define/public (get-sport-bitmap-colorized sport sub-sport)
      (let* ((b (bitmap (get-sport-bitmap sport sub-sport)))
             (r (filled-rounded-rectangle (+ (pict-width b) 10)
                                          (+ (pict-height b) 10)
                                          -0.05
                                          #:draw-border? #f)))
        (pict->bitmap
         (cc-superimpose (colorize r (get-sport-color sport sub-sport)) b))))

    (define/public (get-swim-stroke-name swim-stroke-id)
      (hash-ref swim-stroke-names swim-stroke-id "Unknown"))

    (define/public (get-swim-stroke-names)
      (for/list (((k v) (in-hash swim-stroke-names)))
        (cons k v)))

    (define/public (get-swim-stroke-color stroke)
      (cond ((assq stroke (swim-stroke-colors)) => cdr)
            (#t "gray")))

    (define/public (get-sport-names)
      sport-names)

    ;; Implementation detail: We add the "generic" sports to the list if any of
    ;; their sub-sports show up.  E.g. if we have "Lap Swimming" activities (5,
    ;; 17) we add the "Swimming" activity (5, #f)

    (define/public (get-sport-names-in-use)
      (let ((in-use (query-rows dbc "select distinct S.sport_id, S.sub_sport_id from A_SESSION S")))
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
                sport-names)))

    (define/public (get-athlete-ftp)
      (let ((v (query-maybe-value dbc "select ftp from ATHLETE")))
        (if (sql-null? v) #f v)))

    (define/public (put-athlete-ftp ftp)
      (query-exec dbc "update ATHLETE set ftp = ?" (or ftp sql-null)))

    (define/public (get-athlete-swim-tpace)
      (let ((v (query-maybe-value dbc "select swim_tpace from ATHLETE")))
        (if (sql-null? v) #f v)))

    (define/public (put-athlete-swim-tpace swim-tpace)
      (query-exec dbc "update ATHLETE set swim_tpace = ?" (or swim-tpace sql-null)))

    (define/public (get-athlete-gender)
      (let ((v (query-maybe-value dbc "select gender from ATHLETE")))
        (if (sql-null? v) #f v)))

    (define/public (put-athlete-gender gender)
      (query-exec dbc "update ATHLETE set gender = ?" (or gender sql-null)))

    (define/public (get-athlete-dob)
      (let ((v (query-maybe-value dbc "select strftime('%s', dob) from ATHLETE")))
        (cond
          ((sql-null? v) #f)
          ((string? v) (string->number v))
          ((number? v) v)
          (#t (error "get-athlete-dob")))))

    (define/public (put-athlete-dob dob)
      (query-exec dbc "update ATHLETE set dob = date(?, 'unixepoch', 'localtime')" (or dob sql-null)))

    (define/public (get-athlete-height)
      (let ((v (query-maybe-value dbc "select height from ATHLETE")))
        (if (sql-null? v) #f v)))

    (define/public (put-athlete-height height)
      (query-exec dbc "update ATHLETE set height = ?" (or height sql-null)))

    ))

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

(provide is-runnig?
         is-cycling?
         is-lap-swimming?
         is-swimming?
         (contract-out [sport-charms% sport-charms%/c]))
