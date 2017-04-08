#lang racket/base

;; al-interactive.rkt -- setup a racket session for interactive access to an
;; activitylog database.  this will open the default data base and provide
;; some convenience functions for retrieving data from it.

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

(require db
         plot
         math/statistics
         racket/match
         racket/class
         racket/contract
         "../rkt/al-prefs.rkt"
         "../rkt/data-frame.rkt"
         "../rkt/session-df.rkt"
         "../rkt/dbapp.rkt"
         "../rkt/hrv.rkt"
         "../rkt/fmt-util.rkt"
         "../rkt/database.rkt")

(provide/contract
 (sid->df (-> number? (is-a?/c data-frame%)))
 (hrv->df (-> number? (or/c (is-a?/c data-frame%) #f)))
 (df->csv (-> (is-a?/c data-frame%) path-string? any/c))
 (pp-stops (-> (is-a?/c data-frame%) any/c)))

(provide
 (all-from-out db)
 (all-from-out "../rkt/data-frame.rkt")
 (all-from-out "../rkt/hrv.rkt")
 (all-from-out "../rkt/fmt-util.rkt")
 (all-from-out "../rkt/session-df.rkt")
 (all-from-out "../rkt/dbapp.rkt"))


;;.............................................................. prelude ....

;; Open the default database
(define *db*
  (let ((db-file (al-get-pref 'activity-log:database-file (lambda () #f))))
    (unless db-file (error "No default database, open one in the ActivityLog2 application."))
    (begin0
        (open-activity-log db-file)
      (printf "Opened ~a~%" db-file))))
(when *db* (set-current-database *db*))


;;................................................ convenience functions ....

;; Get a data frame with all recorded series from a session id
(define (sid->df sid)
  (session-df (current-database) sid))
 ;; Get a HRV data frame from a session id
(define (hrv->df sid)
  (make-hrv-data-frame/db (current-database) sid))
;; Write the contents of the data frame DF into FILE in CSV format.

(define (df->csv df file-name)
  (let* ((sn (get-series/ordered df)))
    (call-with-output-file file-name (lambda (port) (apply df-write/csv port df sn))
      #:mode 'text #:exists 'truncate/replace )))


;...................................................... Other functions ....

;; Pretty print the duration of each stop point (can be useful to determine
;; stops that are very long)
(define (pp-stops df)
  (for ((sp (in-list (send df get-property 'stop-points))))
    (let* ((index (send df get-index "timestamp" sp))
           (ts1 (send df ref index "timestamp"))
           (ts2 (send df ref (add1 index) "timestamp")))
      (printf "timestamp ~a, index ~a: ~a~%" sp index (duration->string (- ts2 ts1))))))

;; Export the list of activities to OUT-DIR.  NOTE that ACTIVITY-IDs are
;; specified, *not* SESSION-IDs
(define (bulk-export activity-id-list out-dir)
  (for ([id activity-id-list])
    (let ([fname (build-path out-dir (format "activity-~a.fit" id))])
      (printf "Exporting ~a...~%" fname)
      (db-export-raw-data id (current-database) fname))))

;; A density renderer plotting quantile lines as well.  Needs to be integrated somewhere
(define (df-density-renderer df series)
  (match-define (list q01 q25 q50 q75 q99) (df-quantile df series 0 0.25 0.5 0.75 0.99))
  (define stats (df-statistics df series))
  (list
   (density (send df select series) #:x-min q01 #:x-max q99 #:width 2 #:color "red")
   (tick-grid)
   (vrule q25 #:label "25%" #:color "blue" #:style 'long-dash)
   (vrule q50 #:label "50%" #:color "green" #:style 'long-dash)
   (vrule (statistics-mean stats) #:label "mean" #:color "black" #:style 'long-dash)
   (vrule q75 #:label "75%" #:color "blue" #:style 'long-dash)))
