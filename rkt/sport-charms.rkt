#lang racket/base
;; sport-charms.rkt -- utilities related to individual sports
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015 Alex Harsanyi (AlexHarsanyi@gmail.com)
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
         racket/runtime-path
         "dbapp.rkt"
         "dbutil.rkt"
         "color-theme.rkt")

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
 [get-sport-zones (->* (sport-id? sport-id? zone-metric?) ((or/c positive-number? #f)) sport-zones?)]
 [get-session-sport-zones (-> positive-number? zone-metric? (or/c #f sport-zones?))]
 [get-session-critical-power (-> positive-number?
                                 (or/c #f (list/c positive-number? ; CP
                                                  positive-number? ; WPRIME
                                                  (or/c #f positive-number?) ; TAU
                                                  )))]
 [put-sport-zones (-> sport-id? sport-id? zone-metric? sport-zones? any/c)]
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

 ;; NOTE: we might not want these to be contracts, as they are called too many
 ;; times...
 ;; [val->pct-of-max (-> number? sport-zones? number?)]
 ;; [val->zone (-> number? sport-zones? number?)]
 )

(provide
 val->pct-of-max
 val->zone)


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

(add-db-open-callback init-sport-charms)

(define (get-sport-info sport sub-sport)
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
  (hash-ref *swim-stroke-names*
            swim-stroke-id
            "Unknown"))

(define (get-swim-stroke-names)
  (for/list (((k v) (in-hash *swim-stroke-names*)))
    (cons k v)))

(define (get-swim-stroke-color stroke)
  (cond ((assq stroke (swim-stroke-colors)) => cdr)
        (#t "gray")))

(define (get-sport-names)
  *sport-names*)

;; Implementation detail: We add the "generic" sports to the list if any of
;; their sub-sports show up.  E.g. if we have "Lap Swimming" activities (5,
;; 17) we add the "Swimming" activity (5, #f)

(define (get-sport-names-in-use)
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

(define (get-zone-definition-id sport sub-sport zone-metric timestamp)

  ;; Use most recent zone, if none was specified
  (unless timestamp (set! timestamp (current-seconds)))
  
  (define q1 "
select max(zone_id) from V_SPORT_ZONE 
 where sport_id = ? 
   and sub_sport_id = ? 
   and zone_metric_id = ?
   and ? between valid_from and valid_until")

  (define q2 "
select max(zone_id) from V_SPORT_ZONE 
 where sport_id = ? 
   and sub_sport_id is null 
   and zone_metric_id = ?
   and ? between valid_from and valid_until")

  (define (get-zid)
    (cond ((and sport sub-sport)
           (or (query-maybe-value 
                (current-database) q1 sport sub-sport zone-metric timestamp)
               (query-maybe-value
                (current-database) q2 sport zone-metric timestamp)))
          (sport
           (query-maybe-value 
            (current-database) q2 sport zone-metric timestamp))
          (#t
           #f)))

  (let ((zid (get-zid)))
    (if (sql-null? zid) #f zid)))

(define (get-zone-definition-id-for-session session zone-metric)
  (define q1
    "select zone_id from V_SPORT_ZONE_FOR_SESSION where session_id = ? and zone_metric_id = ?")
  (query-maybe-value (current-database) q1 session zone-metric))
  
(define (get-sport-zones sport sub-sport zone-metric [timestamp #f])
  (if (current-database)
      (let ((zid (get-zone-definition-id sport sub-sport zone-metric timestamp)))
        (if zid
            (query-list
             (current-database)
             "select zone_value from SPORT_ZONE_ITEM where sport_zone_id = ? order by zone_number"
             zid)
            #f))
      #f))


(define (get-session-sport-zones session-id zone-metric)
  (if (current-database)
      (begin
        (let ((zid (get-zone-definition-id-for-session session-id zone-metric)))
          (if zid
              (query-list
               (current-database)
               "select zone_value from SPORT_ZONE_ITEM where sport_zone_id = ? order by zone_number"
               zid)
              #f)))
      #f))

(define scp-query
  "select CP.cp, CP.wprime, CP.tau
     from V_CRITICAL_POWER_FOR_SESSION VCPFS, CRITICAL_POWER CP
    where VCPFS.session_id = ? and VCPFS.cp_id = CP.id")

(define (get-session-critical-power session-id)
  (if (current-database)
      (let ((r (query-maybe-row (current-database) scp-query session-id)))
        (if r
            (let ((cp (sql-column-ref r 0))
                  (wprime (sql-column-ref r 1))
                  (tau (sql-column-ref r 2)))
              (list cp wprime tau))
            #f))
      #f))

(define (put-sport-zones sport sub-sport zone-metric zones)
  (call-with-transaction
   (current-database)
   (lambda ()
     (when (> (length zones) 3)
       (query-exec (current-database) 
                   "insert into SPORT_ZONE(sport_id, sub_sport_id, zone_metric_id, valid_from)
                    values (?, ?, ?, ?)"
                   sport (if sub-sport sub-sport sql-null) zone-metric (current-seconds))
       (let ((zid (db-get-last-pk "SPORT_ZONE" (current-database))))
         (for ((zone (in-list zones))
               (znum (in-range (length zones))))
           (query-exec (current-database)
                       "insert into SPORT_ZONE_ITEM(sport_zone_id, zone_number, zone_value)
                      values(?, ?, ?)" zid znum zone)))))))
                     
(define (val->pct-of-max val zones)
  (let ((max (last zones)))
    (* (/ val max) 100.0)))


(define (val->zone val zones)
  
  (define (classify val low high)
    (cond ((<= val low) 0.0)
          ((>= val high) 1.0)
          (#t
           (exact->inexact (/ (- val low) (- high low))))))

  (foldl (lambda (low high result)
           (+ result (classify val low high)))
         0.0
         (drop-right zones 1)
         (drop zones 1)))

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
