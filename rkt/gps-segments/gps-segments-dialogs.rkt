#lang racket/base

;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; gps-segments-dialogs -- various GUI dialogs for GPS segments functionality
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2021 Alex Harsányi <AlexHarsanyi@gmail.com>
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
         racket/class
         racket/format
         racket/gui/base
         racket/match
         racket/math
         "../../rkt/fmt-util.rkt"
         "../../rkt/session-df/session-df.rkt"
         "../../rkt/widgets/edit-dialog-base.rkt"
         "../../rkt/widgets/grid-pane.rkt"
         "../../rkt/widgets/icon-resources.rkt"
         "gps-segments.rkt"
         "map-and-elevation-widget.rkt")

(provide get-create-segment-dialog
         get-rename-segment-dialog
         get-rescan-for-matches-dialog)

(define *data-font* (make-object font% 14 'default))


;;............................................... segment-matching-pane% ....

;; A pane to provide a GUI for matching GPS segments against sessions.  This
;; is used by both the "Create Segment" dialog and the "Rescan for Matches"
;; dialog.
(define segment-matching-pane%
  (class object%
    (init parent)
    (super-new)

    ;; Segment matching is done in a separate thread, this variable holds the
    ;; status of that thread.
    (define segment-match-status 'not-started)
    ;; The matches that were found so far, when `segment-match-status` is
    ;; 'complete, it contains all the matches found
    (define segment-matches '())
    ;; When not #f, it contains a reference to the thread used to match the
    ;; segments -- it is here so we can wait on it to terminate when the
    ;; dialog is canceled.
    (define segment-match-thread #f)

    (define segment-match-gb
      (new group-box-panel%
           [parent parent]
           [label "Segment Matches"]
           [border 5]
           [spacing 5]))

    (define status-message
      (new message%
           [parent segment-match-gb]
           [label "Matching segments..."]
           [stretchable-width #t]))
    (define progress-gauge
      (new gauge%
           [parent segment-match-gb]
           [label ""]
           [range 100]
           [style '(horizontal)]))
    (define status-data-panel
      (new grid-pane%
           [columns 2]
           [parent segment-match-gb]
           [stretchable-width #t]))
    (new message%
         [parent status-data-panel]
         [label "Candidate Sessions"]
         [stretchable-width #t])
    (define candidate-sessions
      (new message%
           [parent status-data-panel]
           [label "N/A"]
           [font *data-font*]
           [stretchable-width #t]))
    (new message%
         [parent status-data-panel]
         [label "Matches Found"]
         [stretchable-width #t])
    (define match-count
      (new message%
           [parent status-data-panel]
           [label "N/A"]
           [font *data-font*]
           [stretchable-width #t]))

    ;; Creates a thread which finds matches for SEGMENT in the DATABASE.  The
    ;; thread will add matches to the `segment-matches` variable and will
    ;; monitor `segment-match-status` for aborting, when the user presses
    ;; "Cancel"
    (define/public (start-segment-matching segment database)
      (set! segment-match-status 'not-started)
      (set! segment-matches '())
      (set! segment-match-thread #f)
      (send status-message set-label "Matching segments...")
      (send progress-gauge set-value 0)
      (send candidate-sessions set-label "N/A")
      (send match-count set-label "N/A")
      (set! segment-match-status 'in-progress)
      (set! segment-match-thread
            (thread
             (lambda ()
               (define g (df-ref segment 0 "geoid"))
               (define candidates (find-nearby-sessions database g))
               (send candidate-sessions set-label (~a (length candidates)))
               (define matches-found 0)
               (define waypoints (df-select segment "geoid"))
               (define segment-length (df-get-property segment 'segment-length))
               (define all-matches '())
               (for ([candidate (in-list candidates)]
                     [cnt (in-naturals)]
                     #:unless (equal? segment-match-status 'aborted))
                 (define session (session-df database candidate))
                 (define matches
                   (find-segment-matches session waypoints segment-length))
                 (set! matches-found (+ matches-found (length matches)))
                 (set! all-matches (append all-matches matches))
                 (queue-callback
                  (lambda ()
                    (send match-count set-label (~a matches-found))))
                 (queue-callback
                  (lambda ()
                    (define progress (exact-truncate (* (/ (add1 cnt) (length candidates)) 100.0)))
                    (send progress-gauge set-value progress))))
               (queue-callback
                (lambda ()
                  (send status-message set-label "All matches found")
                  (set! segment-match-status 'complete)
                  (set! segment-matches all-matches)))))))

    ;; Request a stop to segment matching and will wait until the matching
    ;; thread has stopped.
    (define/public (abort-segment-matching)
      (set! segment-match-status 'aborted)
      (and segment-match-thread (thread-wait segment-match-thread)))

    ;; Returns #t if segment matching has completed (either by aborting or by
    ;; completing the task.
    (define/public (complete?)
      (member segment-match-status '(complete aborted)))

    (define/public (get-matches)
      segment-matches)

    ))


;;............................................... create-segment-dialog% ....

;; Dialog to create a new segment.  The name is misleading, as the gps segment
;; itself needs to be passed to the dialog -- this dialog will find the
;; segment matches, and, if the user clicks "Save" it will store the segment
;; and matches in the database. The dialog also displays nice information
;; about the segment, so the user has something to look at while matching is
;; in progress.
(define create-segment-dialog%
  (class edit-dialog-base%
    (init)
    (super-new [title "Create New GPS Segment"]
               [icon (waypoints-icon)]
               [min-width 960]
               [min-height 600])

    (define client-pane (send this get-client-pane))
    (define vp (new horizontal-panel% [parent client-pane]))

    (define segment-details-panel
      (new vertical-pane%
           [parent vp]
           [border 5]
           [spacing 5]))

    (define segment-name-gb
      (new group-box-panel%
           [parent segment-details-panel]
           [label "Segment Name"]
           [border 5]
           [spacing 5]))

    (define segment-name-text
      (new text-field%
           [parent segment-name-gb]
           [label "Name"]))

    (define segment-data-gb
      (new group-box-panel%
           [parent segment-details-panel]
           [label "Segment Details"]
           [border 5]
           [spacing 5]))

    (define segment-data-panel
      (new grid-pane%
           [columns 2]
           [parent segment-data-gb]
           [stretchable-width #t]))

    (define data-fields
      (let ([d->s (lambda (v) (distance->string v #t))]
            [e->s (lambda (v) (vertical-distance->string v #t))]
            [g->s (lambda (v)
                    (string-append (~r v #:precision 1) " %"))])
        (for/hash ([name (in-list '("Length" "Height" "Grade (avg)" "Grade (max)"
                                             "Min Elevation" "Max Elevation"
                                             "Total Ascent" "Total Descent"))]
                   [key (in-list '(segment-length segment-height segment-grade max-grade
                                                  min-elevation max-elevation
                                                  total-ascent total-descent))]
                   [formatter (in-list (list d->s e->s g->s g->s e->s e->s e->s e->s))])
          (new message%
               [parent segment-data-panel]
               [label name]
               [stretchable-width #t])
          (values
           key
           (list
            (new message%
                 [parent segment-data-panel]
                 [label "N/A"]
                 [font *data-font*]
                 [stretchable-width #t])
            formatter)))))

    (define segment-matching
      (new segment-matching-pane% [parent segment-details-panel]))

    (define map-and-elevation-widget
      (new map-and-elevation-widget%
           [parent vp]
           [show-session-controls? #f]
           [pref-tag 'al2-create-segment-dialog:map-and-elevation]))

    (define/override (has-valid-data?)
      (send segment-matching complete?))

    (define/public (show-dialog parent segment db)
      (send map-and-elevation-widget set-segment segment)
      (send segment-name-text set-value
            (df-get-property segment 'name "New Segment"))

      ;; Fill in the Segment Details tab
      (for ([(k v) (in-hash data-fields)])
        (define val (df-get-property segment k #f))
        (match-define (list field formatter) v)
        (send field set-label (if val (formatter val) "N/A")))

      (send segment-matching start-segment-matching segment db)

      (begin0
          (if (send this do-edit parent)
              ;; Save the segment and its matches
              (call-with-transaction
               db
               (lambda ()
                 (df-put-property! segment 'name (send segment-name-text get-value))
                 (define segment-id (put-new-gps-segment db segment))
                 (define segment-matches (send segment-matching get-matches))
                 (for ([m (in-list segment-matches)])
                   (match-define (list df start stop cost) m)
                   (put-new-segment-match db segment-id df start stop cost))
                 segment-id))
              (begin
                (send segment-matching abort-segment-matching)
                #f))

        ;; NOTE: we save these regardless of whether the user hit OK or Cancel
        ;; as these are GUI preferences...
        (send map-and-elevation-widget save-visual-layout)))

    ))


;;............................................... rename-segment-dialog% ....

;; A dialog to rename a GPS segment.
(define rename-segment-dialog%
  (class edit-dialog-base%
    (init)
    (super-new [title "Rename GPS Segment"]
               [icon (waypoints-icon)]
               [min-width 400]
               [min-height 150])

    (define client-pane (send this get-client-pane))
    (define vp (new horizontal-panel% [parent client-pane]))

    (define segment-details-panel
      (new vertical-pane%
           [parent vp]
           [border 5]
           [spacing 5]))

    (define segment-name-gb
      (new group-box-panel%
           [parent segment-details-panel]
           [label "Segment Name"]
           [border 5]
           [spacing 5]))

    (define segment-name-text
      (new text-field%
           [parent segment-name-gb]
           [label "New Name"]))

    (define/public (show-dialog parent segment)
      (send segment-name-text set-value
            (df-get-property segment 'name "New Segment"))
      (if (send this do-edit parent)
          (begin
            (df-put-property! segment 'name (send segment-name-text get-value))
            #t)
          #f))

    ))


;;........................................... rescan-for-matches-dialog% ....

;; A dialog to re-scan for segment matches for a given segment -- if the user
;; clicks "Save" the old matches will be removed from the database and the new
;; matches inserted, so we don't have to worry about finding which matches are
;; new.
(define rescan-for-matches-dialog%
  (class edit-dialog-base%
    (init)
    (super-new [title "Rescan for segment matches"]
               [icon (waypoints-icon)]
               [min-width 400]
               [min-height 150])

    (define client-pane (send this get-client-pane))

    (define segment-name-gb
      (new group-box-panel%
           [parent client-pane]
           [label "Segment Name"]
           [border 5]
           [spacing 5]))

    (define segment-name-text
      (new text-field%
           [parent segment-name-gb]
           [label "Name"]))
    (send segment-name-text enable #f)  ; disable it for now

    (define segment-matching
      (new segment-matching-pane% [parent client-pane]))

    (define/override (has-valid-data?)
      (send segment-matching complete?))

    (define/public (show-dialog parent segment database-id db)
      (send segment-name-text set-value
            (df-get-property segment 'name "New Segment"))

      (send segment-matching start-segment-matching segment db)

      (if (send this do-edit parent)
          ;; Save the segment and its matches
          (call-with-transaction
           db
           (lambda ()
             (define old-matches
               (query-list
                db "select id from GPS_SEGMENT_MATCH where segment_id = ?" database-id))
             (for ([mid (in-list old-matches)])
               (delete-segment-match db mid))
             (define segment-matches (send segment-matching get-matches))
             (for ([m (in-list segment-matches)])
               (match-define (list df start stop cost) m)
               (put-new-segment-match db database-id df start stop cost))
             database-id))
          (begin
            (send segment-matching abort-segment-matching)
            #f)))

  ))


;;................................................................ other ....

(define get-create-segment-dialog
  (let ([the-dialog #f])
    (lambda ()
      (unless the-dialog
        (set! the-dialog (new create-segment-dialog%)))
      the-dialog)))

(define get-rename-segment-dialog
  (let ([the-dialog #f])
    (lambda ()
      (unless the-dialog
        (set! the-dialog (new rename-segment-dialog%)))
      the-dialog)))

(define get-rescan-for-matches-dialog
  (let ([the-dialog #f])
    (lambda ()
      (unless the-dialog
        (set! the-dialog (new rescan-for-matches-dialog%)))
      the-dialog)))
