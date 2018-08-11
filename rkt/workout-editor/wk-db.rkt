#lang racket/base
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2018 Alex Harsanyi <AlexHarsanyi@gmail.com>
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
         racket/match
         racket/contract
         "wk-json.rkt"
         "wkstep.rkt"
         "../dbutil.rkt"
         "../utilities.rkt")

(provide/contract
 (store-workout (->* (connection? workout? number?)
                     (#:may-replace-serial? boolean?)
                     (values number? number?)))
 (fetch-workout (->* (connection? number?)
                     (#:workout-version-id (or/c #f number?) #:for-export? boolean?)
                     workout?))
 (delete-workout (-> connection? number? any/c)))

(define (get-workout-id db serial)
  (query-maybe-value db "select id from WORKOUT where serial = ?" serial))

;; Generate a serial number to be used for a workout.  We generate random
;; numbers and see if they exist in the database.  Returns the first number
;; that does not exist in the database.
(define (generate-new-workout-serial-number db)
  (let loop ((num-tries 1000)
             (serial (random 4294967087)))
    (cond ((eqv? num-tries 0)
           (error "failed to generate workout serial number"))
          ((get-workout-id db serial)
           ;; this serial is already in use
           (loop (sub1 num-tries) (random 4294967087)))
          (#t serial))))

;; Store the workout WK in the database under the workout library LIBRARY-ID.
;; If the workout does not have a serial number, a new one will be generated
;; using `generate-new-workout-serial-number`.  If the workout has a serial
;; number, a new version of the workout is stored (instead of a new workout
;; being created).
;;
;; if #:may-replace-serial? is #t, a new workout will be created and the
;; serial number changed if that serial exists in the database.  This is
;; intended to be used when a workout is imported from a FIT file -- in that
;; case we attempt to reuse the serial number, however if there is already a
;; workout with that serial number we generate a new one.
;;
;; Returns two values: the WORKOUT.id and WORKOUT_VERSION.id keys for the
;; corresponding rows in the database.
(define (store-workout db wk library-id #:may-replace-serial? (mrs #f))
  ;; if MRS is #t, we generate a new serial if this workouts serial number is
  ;; already present in the database.
  (define serial
    (if (and mrs
             (workout-serial wk)
             (query-maybe-value db "select id from WORKOUT where serial = ?"
                                (workout-serial wk)))
        (generate-new-workout-serial-number db)
        (or (workout-serial wk) (generate-new-workout-serial-number db))))
  (define timestamp (or (workout-timestamp wk) (current-seconds)))
  (define id (get-workout-id db serial))
  (define sport-id (if (eq? (workout-sport wk) 'running) 1 2))

  ;; Make sure the workout has a serial number and timestamp (since this is
  ;; what we will serialize as JSON.
  (define nwk (struct-copy workout wk
                           [serial serial]
                           [timestamp timestamp]))
  (define data (jsexpr->compressed-string (workout->jsexpr nwk)))

  (call-with-transaction
   db
   (lambda ()
     (if id
         (query-exec db "
update WORKOUT set library_id = ?, name = ?, sport_id = ?, sub_sport_id = ?, serial = ?
where id = ?" library-id (workout-name wk) sport-id sql-null serial id)
         (begin
           (query-exec db "
insert into WORKOUT(library_id, name, sport_id, sub_sport_id, serial)
values(?, ?, ?, ?, ?)" library-id (workout-name wk) sport-id sql-null serial)
           (set! id (db-get-last-pk "WORKOUT" db))))

     (query-exec db "
delete from WORKOUT_VERSION where workout_id = ? and timestamp = ?" id timestamp)
     ;; delete all non-exported versions for the workout, they are not needed
     (query-exec db "
delete from WORKOUT_VERSION where workout_id = ? and is_exported = 0" id)
     (query-exec
      db "
insert into WORKOUT_VERSION(workout_id, timestamp, data)
values (?, ?, ?)" id timestamp data)

     (define vid (db-get-last-pk "WORKOUT_VERSION" db))

     (values id vid))))

;; Fetch a workout from the database.  The workout is identified by
;; WORKOUT-ID.  When WORKOUT-VERSION-ID is #f, the latest version is
;; retrieved, otherwise, the specified version.  When for-export? is #t, the
;; is_exported field in the database will be updated to 1, to mark this
;; workout as exported
(define (fetch-workout db workout-id
                       #:workout-version-id (workout-version-id #f)
                       #:for-export? (for-export? #f))
  (define row
    (query-maybe-row db "
select name, sport_id, sub_sport_id, serial from WORKOUT where id = ?" workout-id))
  (unless row
    (error (format "no workout with id ~a" workout-id)))
  (match-define (vector name sport sub-sport serial) row)
  
  (define version-id
    (or workout-version-id
        (query-maybe-value db "
select id from WORKOUT_VERSION 
 where workout_id = ?
   and timestamp = (
   select max(timestamp) from WORKOUT_VERSION where workout_id = ?)" workout-id workout-id)))
    
  (unless version-id
    (error (format "could not find workout ~a/~a" workout-id workout-version-id)))

  ;; If this workout will be exported, mark it as such
  (when for-export?
    (query-exec db "update WORKOUT_VERSION set is_exported = 1 where id = ?" version-id))

  (define data (query-value db "select data from WORKOUT_VERSION where id = ?" version-id))
  (define wk (jsexpr->workout (compressed-string->jsexpr data)))

  (define mismatch #f)
  (unless (and (workout-serial wk) (eqv? serial (workout-serial wk)))
    (set! mismatch #t)
    (dbglog "workout id ~a, serial # mismatch db: ~a, json: ~a"
            workout-id serial (workout-serial wk)))
  (unless (equal? name (workout-name wk))
    (set! mismatch #t)
    (dbglog "workout id ~a, name mismatch db: ~a, json: ~a"
            workout-id name (workout-name wk)))
  (unless (or (and (eqv? sport 1) (sql-null? sub-sport) (eq? (workout-sport wk) 'running))
              (and (eqv? sport 2) (sql-null? sub-sport) (eq? (workout-sport wk) 'cycling)))
    (set! mismatch #t)
    (dbglog "workout id ~a, sport mismatch db: ~a, json: ~a"
            workout-id (vector sport sub-sport) (workout-sport wk)))

  (if mismatch
      (struct-copy workout wk
                   [name name]
                   [serial serial]
                   [sport (cond ((and (eq? sport 1) (sql-null? sub-sport))
                                 'running)
                                ((and (eq? sport 2) (sql-null? sub-sport))
                                 'cycling)
                                (#t
                                 (error "invalid sport for workout")))])
      wk))

;; Delete a workout and all its versions from the database.  The workout is
;; identified by WORKOUT-ID.
(define (delete-workout db workout-id)
  (call-with-transaction
   db
   (lambda ()
     (query-exec db "delete from WORKOUT_VERSION where workout_id = ?" workout-id)
     (query-exec db "delete from WORKOUT where id = ?" workout-id))))
