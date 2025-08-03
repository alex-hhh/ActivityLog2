#lang racket/base

;; SPDX-License-Identifier: GPL-3.0-or-later
;; inspect-similar-routes.rkt -- show the routes similar to this one
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2025 Alex Harsányi <AlexHarsanyi@gmail.com>
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
         plot-container
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

;; Find the a good LAT/LON for the session in DF and return its GEOID.  We
;; return a point around the middle of the path, to attempt to filter out as
;; many routes as possible, since many very different routes may start and end
;; in the same location, those geoids will produce too many bad candidates
;; which will need to be filtered out.
(define (get-search-geoid df)
  (and (df-contains? df "lat" "lon")
       (let ([search-start (exact-truncate (/ (df-row-count df) 2))])
         (for/first ([(lat lng) (in-data-frame df "lat" "lon" #:start search-start)]
                     #:when (and (rational? lat) (rational? lng)))
         (lat-lng->geoid lat lng)))))

;; Return a vector of summary values from the database DB for the session id
;; SID -- these are the values which we show in the GUI list box.
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

;; Return the cached "similar status" for two sessions SID1 and SID2.  This
;; looks data in the SIMILAR_SESSION_CACHE database table following the
;; convention that the smaller SID is recorded in the "first_session_id"
;; column.
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

;; Store the "similar status" for two sessions SID1 and SID1 into the database
;; table SIMILAR_SESSION_CACHE following the convention that the smaller SID
;; is recorded in the "first_session_id" column.
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

;; Get the GEOIDs out of a session data frame DF
(define (extract-geoids df)
  (for/vector ([(lat lon) (in-data-frame df "lat" "lon")]
               #:when (and (rational? lat) (rational? lon)))
    (lat-lng->geoid lat lon)))

;; Return #t if TRACK1 and TRACK2 (which are vectors of geoids) start and end
;; in the same place, that is their start and end locations are within
;; THRESHOLD meters of each other.
(define (same-start-and-end? track1 track2 (threshold 200.0))
  (and
   (> (vector-length track1) 0)
   (> (vector-length track2) 0)
   (let ([g1 (vector-ref track1 0)]
         [g2 (vector-ref track2 0)])
     (<= (distance-between-geoids g1 g2) threshold))
   (let ([g1 (vector-ref track1 (sub1 (vector-length track1)))]
         [g2 (vector-ref track2 (sub1 (vector-length track2)))])
     (<= (distance-between-geoids g1 g2) threshold))))

;; Find sessions similar to SID in the database DB and return a list of them
;; (entries returned by `fetch-similar-status` calls).
;;
;; This function will take a while to run, so it is invoked in a separate
;;thread.  It reports progress using a callback CB and also looks at the
;;result of the callback to stop the processing.
(define (find-similar-sessions db sid #:progress-callback (cb (lambda (n m) #t)))
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

  ;; First, we locate the sessions which start in the same area as our
  ;; session.  Note that this will find all sessions which cross the start
  ;; location (geoid) of our session, not just the ones that start in the same
  ;; place
  (define candidate-sessions
    (let ([start-geoid (get-search-geoid this-session-df)])
      (if start-geoid
          (find-nearby-sessions db start-geoid #:search-geoid-level 15)
          null)))

  (define candidate-count (length candidate-sessions))

  (let/ec return
    (for/fold ([result '()])
              ([csid (in-list candidate-sessions)]
               [current-candidate-index (in-naturals)])

      ;; If the callback returns #f, we interpret it as a request to cancel
      ;; the processing...
      (unless (cb (add1 current-candidate-index) candidate-count)
        (return '()))

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
               (if (same-start-and-end? geoids (this-session-geoids))
                   ;; the candidate has the same sport type and approximate
                   ;; session distance, same start and end, so we do a proper
                   ;; alignment cost (which is expensive) to find if it is
                   ;; actually similar.
                   ;;
                   ;; We use `dtw/window` here, since we expect the paths to
                   ;; either closely match or be wildly different.
                   (let ([cost (dtw/window (this-session-geoids) geoids)])
                     (define are-similar?
                       (good-segment-match?
                        cost
                        this-session-length (vector-length (this-session-geoids))
                        (vector-ref summary 5) (vector-length geoids)
                        25.0))
                     (put-similar-status db sid csid are-similar?)
                     (if are-similar?
                         (cons summary result)
                         result))
                   (begin
                     (put-similar-status db sid csid #f)
                     result)))
              (#t
               (put-similar-status db sid csid #f)
               result)))))))

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
    (init-field parent database select-activity-callback)
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
           [spacing 5]
           [border 10]))

    (define progress-message
      (new message%
           [parent progress-bar-panel]
           [label ""]
           [auto-resize #t]))

    (define progress-bar
      (new gauge%
           [parent progress-bar-panel]
           [label ""]
           [range 100]
           [style '(horizontal)]))

    (define no-similar-sessions-panel
      (new vertical-panel%
           [parent panel]
           [style '(deleted)]
           [spacing 5]
           [border 0]))

    (new plot-container%
         [parent no-similar-sessions-panel]
         [background-message "No routes similar to this one found"])

    (define data-panel
      (new vertical-panel%
           [parent panel]
           [style '(deleted)]
           [spacing 5]))

    (define sessions-lv
      (new (class qresults-list%
             (init)
             (super-new)
             (define/override (on-double-click row row-data)
               (and select-activity-callback
                    (select-activity-callback (vector-ref row-data 0)))))
           [parent data-panel]
           [pref-tag 'al2-inspect-similar-sessions:similar-sessions]
           [get-preference
            (lambda (name fail-thunk)
              (db-get-pref database name (lambda () (get-pref name fail-thunk))))]
           [put-preference
            (lambda (name value)
              (db-put-pref database name value))]
           ))

    (define/public (save-visual-layout)
      (send sessions-lv save-visual-layout))

    (define generation 0)
    (define search-in-progress? #f)
    (define delayed-session-updates null)

    (define/public (set-session session df)
      (define this-session-sid (df-get-property df 'session-id))
      (set! generation (add1 generation))
      (send sessions-lv clear)
      (when this-session-sid
        (send progress-bar set-value 0)
        (send progress-message set-label "Looking for similar sessions...")
        (send panel change-children (lambda (_old) (list progress-bar-panel)))
        (set! search-in-progress? #t)
        (define this-generation generation)
        (thread/dbglog
         (lambda ()
           (define similar-sessions
             (find-similar-sessions
              database this-session-sid
              #:progress-callback
              (lambda (current total)
                (if (equal? this-generation generation)
                    (let ([v (exact-round (* 100 (min 1.0 (/ current total))))])
                      (queue-callback
                       (lambda ()
                         (send progress-message set-label
                               (format "Checking session ~a of ~a possible candidates..." current total))
                         (send progress-bar set-value v)
                         (send progress-bar-panel reflow-container)))
                      #t)
                    #f))))
           (define have-similar-sessions?
             (case (length similar-sessions)
               ((0)
                ;; this case should not happen, since we expect to find at
                ;; least our session in the database...
                #f)
               ((1)
                ;; we expect to find at list this session in the "similar
                ;; session" list, so one session means there are no similar
                ;; sessions, but we still explicitly check...
                (let ([sid (vector-ref (car similar-sessions) 0)])
                  (not (equal? sid this-session-sid))))
               (else #t)))
           (when (equal? this-generation generation)
             (queue-callback
              (lambda ()
                (set! search-in-progress? #f)
                (if have-similar-sessions?
                    (let* ([sport (df-get-property df 'sport)]
                           [column-definitions
                            (cond
                              ((is-runnig? sport)
                               similar-routes-running-columns)
                              ((is-cycling? sport)
                               similar-routes-cycling-columns)
                              (#t
                               similar-routes-cycling-columns))])
                      (send sessions-lv setup-column-defs column-definitions)
                      (send sessions-lv set-data similar-sessions)
                      (send panel change-children (lambda (_old) (list data-panel)))
                      ;; re-process any session updates that happened while we
                      ;; were searching for sessions...
                      (for ([update (in-list delayed-session-updates)])
                        (on-session-changed (car update) (cdr update)))
                      (set! delayed-session-updates null))
                    (send panel change-children (lambda (_old) (list no-similar-sessions-panel)))))))

           ))))

    ;; This is really inefficient, but qresults-list% sorts and reorders the
    ;; data -- I really need to update qresults-list%
    (define/private (row-index-for-sid sid)
      (for/or ([pos (in-range (send sessions-lv get-row-count))])
        (let ([data (send sessions-lv get-data-for-row pos)])
          (if (and data (= sid (vector-ref data 0)))
              pos #f))))

    ;; called by view-session% when sessions in the application are changed
    ;; (see the `change-processing-thread`) we check if the SESSION-ID is
    ;; displayed in our list and update it if necessary.  CHANGE is one of
    ;; 'session-deleted 'session-updated, etc, see docs/gui-consistency.md for
    ;; possible values.
    (define/public (on-session-changed change session-id)
      (if search-in-progress?
          (set! delayed-session-updates (cons (cons change session-id) delayed-session-updates))
          (case change
            ((session-updated session-id)
             (let ([index (row-index-for-sid session-id)])
               (when index
                 (let ([data (fetch-session-summary database session-id)])
                   (send sessions-lv update-row index data)))))
            ((session-deleted session-id)
             (let ([index (row-index-for-sid session-id)])
               (when index
                 (send sessions-lv delete-row index))))
            (else                       ; don't handle anything else
             (void))
            )))

    ))
