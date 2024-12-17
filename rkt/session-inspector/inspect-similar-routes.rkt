#lang racket/base

;; SPDX-License-Identifier: GPL-3.0-or-later
;; inspect-similar-routes.rkt -- show the routes similar to this one
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2024 Alex Harsányi <AlexHarsanyi@gmail.com>
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
         db/base
         geoid
         geoid/waypoint-alignment
         racket/gui
         "../dbutil.rkt"
         "../fmt-util-ut.rkt"
         "../fmt-util.rkt"
         "../gps-segments/gps-segments.rkt"
         "../session-df/session-df.rkt"
         "../sport-charms.rkt"
         "../utilities.rkt"
         "../widgets/qresults-list.rkt")

(provide similar-routes-panel%)

(define (get-start-geoid df)
  (and (df-contains? df "lat" "lon")
       (for/first ([(lat lng) (in-data-frame df "lat" "lon")]
                   #:when (and (rational? lat) (rational? lng)))
         (lat-lng->geoid lat lng))))

(define (fetch-session-summary db sid)
  (define result
    (query-row
     db
     "select S.id,
             S.start_time,
             (select name from E_TIME_ZONE ETZ where ETZ.id = S.time_zone_id) as time_zone,
             S.name,
             S.sport_id,
             SS.total_distance,
             SS.total_timer_time,
             SS.avg_speed,
             SS.avg_heart_rate,
             SS.avg_power,
             SS.normalized_power,
             S.training_stress_score
        from A_SESSION S, SECTION_SUMMARY SS
       where S.summary_id = SS.id and S.id = ?" sid))
  result)

(define fetch-similar-status
  (let ([query (virtual-statement
                (lambda (_dbsys)
                  "select are_similar
                     from SIMILAR_SESSION_CACHE
                    where first_session_id = ?
                      and second_session_id = ?"))])
    (lambda (db sid1 sid2)
      (if (equal? sid1 sid2)
          1                     ; they are identical, so yes, they are similar
          (let-values ([(s1 s2)
                        (if (< sid1 sid2)
                            (values sid1 sid2)
                            (values sid2 sid1))])
            (query-maybe-value db query s1 s2))))))

(define put-similar-status
  (let ([query (virtual-statement
                (lambda (_dbsys)
                  "insert into SIMILAR_SESSION_CACHE(
                     first_session_id, second_session_id, are_similar)
                   values(?, ?, ?)"))])
    (lambda (db sid1 sid2 are-similar?)
      (unless (equal? sid1 sid2)
        (let-values ([(s1 s2)
                      (if (< sid1 sid2)
                          (values sid1 sid2)
                          (values sid2 sid1))])
          (query-exec db query s1 s2 (if are-similar? 1 0)))))))

(define (extract-geoids df)
  (for/vector ([(lat lon) (in-data-frame df "lat" "lon")]
               #:when (and (rational? lat) (rational? lon)))
    (lat-lng->geoid lat lon)))

(define (find-similar-sessions db sid #:progress-callback (cb (lambda (n m) (void))))
  (define this-session-df (session-df db sid))
  (define this-session-summary (fetch-session-summary db sid))
  (define this-session-length (vector-ref this-session-summary 5))
  (define this-session-sport (vector-ref this-session-summary 4))

  (define this-session-geoids
    (let ([geoids #f])
      (lambda ()
        (unless geoids
          (set! geoids (extract-geoids this-session-df)))
        geoids)))

  (define candidate-sessions
    (let ([start-geoid (get-start-geoid this-session-df)])
      (if start-geoid
          (find-nearby-sessions db start-geoid #:search-geoid-level 15)
          null)))

  (define candidate-count (length candidate-sessions))

  (for/fold ([result '()])
            ([csid (in-list candidate-sessions)]
             [current-candidate-index (in-naturals)])

    (cb current-candidate-index candidate-count)

    (define cached-are-similar? (fetch-similar-status db sid csid))

    (if cached-are-similar?
        (if (> cached-are-similar? 0)
            (cons (fetch-session-summary db csid) result)
            result)
        (let ([summary (fetch-session-summary db csid)])
          (cond
            ((equal? sid csid)                ; same session
             (cons summary result))
            ((and (equal? this-session-sport (vector-ref summary 4))
                  (< 0.9 (/ (vector-ref summary 5) this-session-length) 1.1))
             (define df (session-df db csid))
             (define geoids (extract-geoids df))
             (define cost (waypoint-alignment-cost (this-session-geoids) geoids))
             (define are-similar?
               (good-segment-match?
                cost
                this-session-length (vector-length (this-session-geoids))
                (vector-ref summary 5) (vector-length geoids)
                15.0))
             (put-similar-status db sid csid are-similar?)
             (if are-similar?
                 (cons summary result)
                 result))
            (#t
             result))))))

;; Convenience function to construct a `qcolumn` entry which picks up values
;; from row vectors returned from the data base
(define (make-qcolumn name column-index default-value formatter
                      #:default-visible? (default-visible? #t))
  (let ([fn (lambda (row) (sql-column-ref row column-index default-value))])
    (qcolumn name
             (lambda (row)
               (let ([val (fn row)])
                 (if val (formatter val) "")))
             fn
             #:default-visible? default-visible?)))

;; These are the column definitions for the segments list view.
(define similar-routes-running-columns
  (let ([d->s (lambda (v) (distance->string v #t))]
        [t->s (lambda (v) (duration->string v))]
        [p->s (lambda (v) (pace->string v #t))])
    (list
     (qcolumn "Start Time"
              (lambda (row)
                (let ([st (sql-column-ref row 1 #f)]
                      [tz (sql-column-ref row 2 #f)])
                  (date-time->string st #:include-seconds? #t #:time-zone tz)))
              (lambda (row)
                (sql-column-ref row 1 #f))
              #:default-visible? #t)
     (make-qcolumn "Name" 3 #f ~a)
     (make-qcolumn "Distance" 5 #f d->s)
     (make-qcolumn "Duration" 6 #f t->s)
     (make-qcolumn "Avg Pace" 7 #f p->s)
     (make-qcolumn "Avg HR" 8 #f n->string)
     (make-qcolumn "Effort" 11 #f n->string))))

(define similar-routes-cycling-columns
  (let ([d->s (lambda (v) (distance->string v #t))]
        [t->s (lambda (v) (duration->string v))]
        [s->s (lambda (v) (speed->string v #t))]
        [w->s (lambda (v) (power->string v #t))])
    (list
     (qcolumn "Start Time"
              (lambda (row)
                (let ([st (sql-column-ref row 1 #f)]
                      [tz (sql-column-ref row 2 #f)])
                  (date-time->string st #:include-seconds? #t #:time-zone tz)))
              (lambda (row)
                (sql-column-ref row 1 #f))
              #:default-visible? #t)
     (make-qcolumn "Name" 3 #f ~a)
     (make-qcolumn "Distance" 5 #f d->s)
     (make-qcolumn "Duration" 6 #f t->s)
     (make-qcolumn "Avg Speed" 7 #f s->s)
     (make-qcolumn "Avg HR" 8 #f n->string)
     (make-qcolumn "Avg Power" 9 #f w->s)
     (make-qcolumn "ISO Power" 10 #f w->s)
     (make-qcolumn "Effort" 11 #f n->string))))

(define similar-routes-panel%
  (class object%
    (init-field parent database)
    (super-new)

    (define panel
      (new vertical-panel%
           [parent parent]
           [border 0]
           [spacing 0]))

    (define progress-bar-panel
      (new vertical-panel%
           [parent panel]
           [style '(deleted)]
           [spacing 5]))

    (define progress-bar
      (new gauge%
           [parent progress-bar-panel]
           [label ""]
           [range 100]
           [style '(horizontal)]))

    (define data-panel
      (new vertical-panel%
           [parent panel]
           [style '(deleted)]
           [spacing 5]))

    (define sessions-lv
      (new qresults-list%
           [parent data-panel]
           [pref-tag 'al2-inspect-similar-sessions:similar-sessions]
           [get-preference
            (lambda (name fail-thunk)
              (db-get-pref database name (lambda () (get-pref name fail-thunk))))]
           [put-preference
            (lambda (name value)
              (db-put-pref database name value))]))

    (define/public (save-visual-layout)
      (send sessions-lv save-visual-layout))

    (define generation 0)

    (define/public (set-session session df)
      (define sid (df-get-property df 'session-id))
      (set! generation (add1 generation))
      (send sessions-lv clear)
      (when sid
        (send progress-bar set-value 0)
        (send panel change-children (lambda (_old) (list progress-bar-panel)))
        (let ([this-generation generation])
          (thread/dbglog
           (lambda ()
             (define similar-sessions
               (find-similar-sessions
                database sid
                #:progress-callback
                (lambda (current total)
                  (when (equal? this-generation generation)
                    (define v (exact-round (* 100 (min 1.0 (/ current total)))))
                    (queue-callback
                     (lambda ()
                       (send progress-bar set-value v)))))))
             (when (equal? this-generation generation)
               (queue-callback
                (lambda ()
                  (define sport (df-get-property df 'sport))
                  (define column-definitions
                    (cond
                      ((is-runnig? sport)
                       similar-routes-running-columns)
                      ((is-cycling? sport)
                       similar-routes-cycling-columns)
                      (#t
                       similar-routes-cycling-columns)))
                  (send sessions-lv setup-column-defs column-definitions)
                  (send sessions-lv set-data similar-sessions)
                  (send panel change-children (lambda (_old) (list data-panel))))))
             )))))
    ))
