#lang racket
;; session-inspector-test.rkt 
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2018, 2019, 2020, 2022 Alex Harsányi <AlexHarsanyi@gmail.com>
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


(require al2-test-runner
         rackunit
         db/base
         racket/gui/base
         the-application
         "../rkt/database.rkt"
         "test-util.rkt"
         "../rkt/session-df/session-df.rkt"
         "../rkt/session-inspector/inspect-best-avg.rkt"
         "../rkt/session-inspector/inspect-scatter.rkt"
         "../rkt/session-inspector/inspect-model-parameters.rkt"
         "../rkt/session-inspector/inspect-graphs.rkt"
         "../rkt/session-inspector/inspect-histogram.rkt"
         "../rkt/session-inspector/inspect-aerolab.rkt"
         "../rkt/session-inspector/inspect-quadrant.rkt"
         "../rkt/session-inspector/inspect-overview.rkt"
         "../rkt/session-inspector/inspect-laps.rkt"
         "../rkt/session-inspector/inspect-map.rkt")

(define a1 "test-data/920xt-lap-swim.fit")
(define a2 "test-data/920xt-run.fit")

;; Check that all session inspectors can be created when there is no
;; preferences file...  This is a destructive test and if you run it on your
;; local machine it will remove your preferences file...

(define session-inspector-test-suite
  (test-suite
   "Session Inspector"
   (test-case "No Preferences File"
     (with-fresh-database
       (lambda (db)
         (match-define (cons 'ok aid) (db-import-activity-from-file a2 db))
         (define sid (query-value db "select id from A_SESSION where activity_id = ?" aid))
         (define session (db-fetch-session sid db))
         (define df (session-df db sid))

         (define f (new frame% [label "Test Frame"] [width 1024] [height 768]))
         (check-not-exn
          (lambda ()
            (when (file-exists? (preferences-file))
              (delete-file (preferences-file))) ; this is destructive!
            (define inspector (new map-panel% [parent f]))
            (send inspector set-session session df)
            (send inspector save-visual-layout)))

         (check-not-exn
          (lambda ()
            (when (file-exists? (preferences-file))
              (delete-file (preferences-file))) ; this is destructive!
            (define inspector (new mean-max-plot-panel% [parent f]))
            (send inspector set-session session df)
            (send inspector save-visual-layout)))

         (check-not-exn
          (lambda ()
            (when (file-exists? (preferences-file))
              (delete-file (preferences-file))) ; this is destructive!
            (define inspector (new scatter-plot-panel% [parent f]))
            (send inspector set-session session df)
            (send inspector save-visual-layout)))
         
         (check-not-exn
          (lambda ()
            (when (file-exists? (preferences-file))
              (delete-file (preferences-file))) ; this is destructive!
            (define inspector (new model-parameters-panel% [parent f]))
            (send inspector set-session session df)
            (send inspector save-visual-layout)))

         (check-not-exn
          (lambda ()
            (when (file-exists? (preferences-file))
              (delete-file (preferences-file))) ; this is destructive!
            (define inspector (new graph-panel% [parent f]))
            (send inspector set-session session df)
            (send inspector save-visual-layout)))
         
         (check-not-exn
          (lambda ()
            (when (file-exists? (preferences-file))
              (delete-file (preferences-file))) ; this is destructive!
            (define inspector (new histogram-plot-panel% [parent f]))
            (send inspector set-session session df)
            (send inspector save-visual-layout)))
         
         (check-not-exn
          (lambda ()
            (when (file-exists? (preferences-file))
              (delete-file (preferences-file))) ; this is destructive!
            (define inspector (new aerolab-panel% [parent f]))
            (send inspector set-session session df)
            (send inspector save-visual-layout)))

         (check-not-exn
          (lambda ()
            (when (file-exists? (preferences-file))
              (delete-file (preferences-file))) ; this is destructive!
            (define inspector (new inspect-overview-panel% [parent f] [database db]))
            (send inspector set-session session df)
            (send inspector save-visual-layout)))

         (check-not-exn
          (lambda ()
            (when (file-exists? (preferences-file))
              (delete-file (preferences-file))) ; this is destructive!
            (define inspector (new laps-panel% [parent f]))
            (send inspector set-session session df)
            (send inspector save-visual-layout)))
         
         )))))

(module+ test
  (run-tests #:package "session-inspector-test"
             #:results-file "test-results/session-inspector-test.xml"
             session-inspector-test-suite))
