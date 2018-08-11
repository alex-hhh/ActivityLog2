#lang racket/base
;; edit-cp.rkt -- edit the Critical Power values stored in the database

;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2017 Alex Harsanyi (AlexHarsanyi@gmail.com)
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
         racket/class
         racket/gui/base
         racket/match
         "../widgets/main.rkt"
         "../fmt-util.rkt"
         "../dbutil.rkt"
         "../utilities.rkt")

(provide get-cp-editor)

(define cp-query
  "select VCP.cp_id, VCP.cp, VCP.wprime, VCP.tau, VCP.valid_from, VCP.valid_until, 
          (select count(*) from V_CRITICAL_POWER_FOR_SESSION VCPFS where VCPFS.cp_id = VCP.cp_id) as session_count
     from V_CRITICAL_POWER VCP
    where sport_id = ?
 order by valid_from")

(define (cp-id data) (vector-ref data 0))
(define (cp-cp data) (vector-ref data 1))
(define (cp-wprime data) (vector-ref data 2))
(define (cp-tau data) (sql-column-ref data 3)) ; NOTE: tau can be NULL
(define (cp-valid-from data) (vector-ref data 4))
(define (cp-valid-until data) (vector-ref data 5))
(define (cp-session-count data) (vector-ref data 6))

;; Column definitions for the qresults-list% object used by the critical power
;; editor for swimming (shows CV and D')
(define (make-cp-columns-swim)
  (list
   (qcolumn "Valid From"
            (lambda (row) (date-time->string (cp-valid-from row)))
            cp-valid-from)
   (qcolumn "Valid Until"
            (lambda (row) (date-time->string (cp-valid-until row)))
            cp-valid-from)
   (qcolumn "CV"
            (lambda (row) (swim-pace->string (cp-cp row) #t))
            cp-cp)
   (qcolumn "D'"
            (lambda (row) (short-distance->string (cp-wprime row) #t))
            cp-wprime)
   (qcolumn "Tau"
            (lambda (row)
              (let ((tau (cp-tau row)))
                (if tau (number->string tau) "")))
            (lambda (row) (or  (cp-tau row) 0)))
   (qcolumn "Session Count"
            (lambda (row)
              (let ((sc (cp-session-count row)))
                (if (sql-null? sc) "" (number->string sc))))
            cp-session-count)))

;; Column definitions for the qresults-list% object used by the critical power
;; editor for running (shows CV and D')
(define (make-cp-columns-run)
  (list
   (qcolumn "Valid From"
            (lambda (row) (date-time->string (cp-valid-from row)))
            cp-valid-from)
   (qcolumn "Valid Until"
            (lambda (row) (date-time->string (cp-valid-until row)))
            cp-valid-from)
   (qcolumn "CV"
            (lambda (row) (pace->string (cp-cp row) #t))
            cp-cp)
   (qcolumn "D'"
            (lambda (row) (short-distance->string (cp-wprime row) #t))
            cp-wprime)
   (qcolumn "Tau"
            (lambda (row)
              (let ((tau (cp-tau row)))
                (if tau (number->string tau) "")))
            (lambda (row) (or  (cp-tau row) 0)))
   (qcolumn "Session Count"
            (lambda (row)
              (let ((sc (cp-session-count row)))
                (if (sql-null? sc) "" (number->string sc))))
            cp-session-count)))

;; Column definition for the qresults-list% object used by the critical power
;; editor for cycling (shows CP and W')
(define (make-cp-columns-bike)
  (list
   (qcolumn "Valid From"
            (lambda (row) (date-time->string (cp-valid-from row)))
            cp-valid-from)
   (qcolumn "Valid Until"
            (lambda (row) (date-time->string (cp-valid-until row)))
            cp-valid-from)
   (qcolumn "CP"
            (lambda (row) (power->string (cp-cp row) #t))
            cp-cp)
   (qcolumn "W'"
            (lambda (row) (work->string (cp-wprime row) #t))
            cp-wprime)
   (qcolumn "Tau"
            (lambda (row)
              (let ((tau (cp-tau row)))
                (if tau (number->string tau) "")))
            (lambda (row) (or  (cp-tau row) 0)))
   (qcolumn "Session Count"
            (lambda (row)
              (let ((sc (cp-session-count row)))
                (if (sql-null? sc) "" (number->string sc))))
            cp-session-count)))

(define edit-cp-value%
  (class edit-dialog-base%
    (init)
    (super-new [title "Edit Critical Power"] [icon (edit-icon)] [min-width 300] [min-height 200])

    (define pane
      (let ((p (send this get-client-pane)))
        (new grid-pane% [parent p] [columns 2]
             [border 10]  [spacing 5]
             [alignment '(left center)])))

    (send pane set-alignment 'left 'center)
    (define mode #f)                    ; 'bike, 'run or 'swim

    (define valid-from-message
      (new message% [parent pane] [label "Valid From:"]))
    (define valid-from-field
      (new date-input-field% [parent pane] [label ""] [stretchable-width #f]
           [allow-empty? #f]))
    (define cp-message
      (new message% [parent pane] [label "CP:"]))
    (define cp-field
      (new number-input-field% [parent pane] [label ""]
           [cue-text "watts"] [allow-empty? #f]
           [min-width 100] [stretchable-width #f]))
    (define wprime-message
      (new message% [parent pane] [label "W'"]))
    (define wprime-field
      (new number-input-field% [parent pane]
           [label ""] [cue-text "Joules"]
           [stretchable-width #f] [allow-empty? #f]
           [min-width 100]))
    (define cv-message
      (new message% [parent pane] [label "CV"] [style '(deleted)]))
    (define cv-field
      (new pace-input-field% [parent pane]
           [label ""] [style '(single deleted)]
           [min-width 100] [stretchable-width #f]
           [allow-empty? #f]))
    (define swim-cv-field
      (new swim-pace-input-field% [parent pane]
           [label ""] [style '(single deleted)]
           [min-width 100] [stretchable-width #f]
           [allow-empty? #f]))
    (define dprime-message
      (new message% [parent pane] [label "D'"] [style '(deleted)]))
    (define dprime-field
      (new number-input-field%
           [parent pane] [label ""] [style '(single deleted)]
           [stretchable-width #f] [allow-empty? #f]
           [cue-text "meters"]
           [min-width 100]))
    (define tau-message
      (new message% [parent pane] [label "Tau"] [style '(deleted)]))
    (define tau-field
      (new number-input-field%
           [parent pane] [label ""] [style '(single deleted)]
           [stretchable-width #f] [allow-empty? #t]
           [cue-text "seconds"]
           [min-width 100]
           [min-value 0] [max-value 3600]))

    (define/override (has-valid-data?)
      (cond ((eq? mode 'run)
             (and (send valid-from-field has-valid-value?)
                  (send cv-field has-valid-value?)
                  (send dprime-field has-valid-value?)
                  (send tau-field has-valid-value?)))
            ((eq? mode 'bike)
             (and (send valid-from-field has-valid-value?)
                  (send cp-field has-valid-value?)
                  (send wprime-field has-valid-value?)
                  (send tau-field has-valid-value?)))
            ((eq? mode 'swim)
             (and (send valid-from-field has-valid-value?)
                  (send swim-cv-field has-valid-value?)
                  (send dprime-field has-valid-value?)
                  (send tau-field has-valid-value?)))
            (#t #f)))

    (define/public (show-dialog parent type valid-from cp wprime tau)
      (set! mode type)
      (cond ((eq? type 'run)
             (send pane change-children
                   (lambda (old) (list valid-from-message valid-from-field
                                       cv-message cv-field
                                       dprime-message dprime-field
                                       tau-message tau-field)))
             (send cv-field set-pace-value cp)
             (send dprime-field set-value (if wprime (short-distance->string wprime) ""))
             (send tau-field set-value (if tau (format "~a" tau) ""))
             (if valid-from
                 (send valid-from-field set-date-value valid-from)
                 (send valid-from-field set-value "")))
            ((eq? type 'swim)
             (send pane change-children
                   (lambda (old) (list valid-from-message valid-from-field
                                       cv-message swim-cv-field
                                       dprime-message dprime-field
                                       tau-message tau-field)))
             (send swim-cv-field set-pace-value cp)
             (send dprime-field set-value (if wprime (short-distance->string wprime) ""))
             (send tau-field set-value (if tau (format "~a" tau) ""))
             (if valid-from
                 (send valid-from-field set-date-value valid-from)
                 (send valid-from-field set-value "")))
            ((eq? type 'bike)
             (send pane change-children
                   (lambda (old) (list valid-from-message valid-from-field
                                       cp-message cp-field
                                       wprime-message wprime-field
                                       tau-message tau-field)))
             (send cp-field set-value (if cp (format "~a" cp) ""))
             (send wprime-field set-value (if wprime (format "~a" wprime) ""))
             (send tau-field set-value (if tau (format "~a" tau) ""))
             (if valid-from
                 (send valid-from-field set-date-value valid-from)
                 (send valid-from-field set-value ""))))
      (if (send this do-edit parent)
          (let ((tau (let ((tau (send tau-field get-converted-value)))
                       (if (number? tau) tau #f))))
            (cond ((eq? mode 'run)
                   (list
                    (send valid-from-field get-converted-value)
                    (send cv-field get-converted-value)
                    (send dprime-field get-converted-value)
                    tau))
                  ((eq? mode 'swim)
                   (list
                    (send valid-from-field get-converted-value)
                    (send swim-cv-field get-converted-value)
                    (send dprime-field get-converted-value)
                    tau))
                  ((eq? mode 'bike)
                   (list
                    (send valid-from-field get-converted-value)
                    (send cp-field get-converted-value)
                    (send wprime-field get-converted-value)
                    tau))
                  (#t
                   #f)))
          #f))

    ))

(define edit-cp-dialog%
  (class edit-dialog-base%
    (init)
    (super-new [title "Critical Power"] [icon (edit-icon)] [min-width 600] [min-height 400])

    (define cp-editor (new edit-cp-value%))
    (define database #f)
    (define cplb #f)
    (define sport-choice #f)

    (define cp-columns-run (make-cp-columns-run))
    (define cp-columns-bike (make-cp-columns-bike))
    (define cp-columns-swim (make-cp-columns-swim))

    (let ((p (send this get-client-pane)))
      (let ((p1 (new vertical-pane%
                     [stretchable-height #t]
                     [parent p] [spacing 5]
                     [alignment '(center center)])))
        (let ((p2 (new horizontal-pane%
                       [stretchable-height #f]
                       [border 15]
                       [parent p1] [spacing 5]
                       [alignment '(right center)])))
          (set! sport-choice
                (new choice% [parent p2]
                     [stretchable-width #f]
                     [label "Show Critical Power for: "]
                     [choices '("Cycling" "Running" "Swimming")]
                     [callback (lambda (c e) (on-sport-selected (send c get-selection)))]))
          (new message% [parent p2] [stretchable-width #t] [label ""])
          (new button% [parent p2]
               [stretchable-width #f]
               [label "Add"]
               [callback (lambda (b e) (on-add-cp))])
          (new button% [parent p2]
               [stretchable-width #f]
               [label "Edit"]
               [callback (lambda (b e) (on-edit-cp))])
          (new button% [parent p2] [label "Delete"]
               [stretchable-width #f]
               [callback (lambda (b e) (on-delete-cp))]))

        (set! cplb
              (new qresults-list%
                   [parent p1] [pref-tag 'activity-log:cp-editor]))))

    ;; Called when the sport selection has changed.  Populate the list box
    ;; with CP data for the relevant sport.
    (define (on-sport-selected index)
      
      (define (populate sport cdefs)
        (let ((rows (query-rows database cp-query sport)))
          (send cplb setup-column-defs cdefs)
          (send cplb set-data rows)))
      
      (cond ((eqv? index 1)             ; running
             (populate 1 cp-columns-run))
            ((eqv? index 0)
             (populate 2 cp-columns-bike))
            ((eqv? index 2)
             (populate 5 cp-columns-swim))
            (#t
             (error "cp-edit-dialog%: unknown selection"))))

    ;; Return the selected sport as a symbol (either 'run or 'bike)
    (define (get-selected-sport)
      (let ((selection (send sport-choice get-selection)))
        (cond ((eqv? selection 1)
               'run)
              ((eqv? selection 0)
               'bike)
              ((eqv? selection 2)
               'swim)
              (#t
               (error "cp-edit-dialog%: unknown selection")))))

    ;; Refresh the contents of the list box (called when the contents of the
    ;; database have changed)
    (define (refresh-contents)
      (on-sport-selected (send sport-choice get-selection)))

    ;; Start a database transaction, if we haven't already started one.  This
    ;; is called the first time the contents of the CRITICAL_POWER table are
    ;; about to be modified.
    (define (maybe-start-transaction)
      (unless (in-transaction? database)
        (start-transaction database #:option 'immediate)))

    (define (put-cp sport valid-from cp wprime tau)
      (define qtext
        "insert into CRITICAL_POWER(sport_id, valid_from, cp, wprime, tau)
              values (?, ?, ?, ?, ?)")
      (query-exec database
                  qtext
                  (cond ((eq? sport 'run) 1)
                        ((eq? sport 'swim) 5)
                        ((eq? sport 'bike) 2)
                        (#t (error "Unknown sport")))
                  valid-from cp wprime (or tau sql-null)))

    (define (update-cp id valid-from cp wprime tau)
      (define qtext
        "update CRITICAL_POWER
            set valid_from = ?, cp = ?, wprime = ?, tau = ?
          where id = ?")
      (query-exec database qtext valid-from cp wprime (or tau sql-null) id))

    (define (delete-cp id)
      (define qtext "delete from CRITICAL_POWER where id = ?")
      (query-exec database qtext id))

    ;; Called when the user clicks the "Add" button.  Pops up a dialog to add
    ;; a new critical power value and adds it to the database.  A transaction
    ;; is started if needed, so if the user cancels the dialog, all changes
    ;; are rolled back.
    (define (on-add-cp)
      (define sport (get-selected-sport))
      (define data (send cp-editor show-dialog
                         (send this get-top-level-window)
                         sport #f #f #f #f))
      (when data
        (match-define (list valid-from cp wprime tau) data)
        (maybe-start-transaction)
        (put-cp sport valid-from cp wprime tau)
        (refresh-contents)))

    ;; Called when the user clicks the "Edit" button.  Pops up a dialog to
    ;; edit a new critical power value and updates the database.  A
    ;; transaction is started if needed, so if the user cancels the dialog,
    ;; all changes are rolled back.
    (define (on-edit-cp)
      (let ((index (send cplb get-selected-row-index)))
        (when index
          (let ((sport (get-selected-sport))
                (data (send cplb get-data-for-row index)))
            (let ((ndata (send cp-editor show-dialog
                               (send this get-top-level-window)
                               sport
                               (cp-valid-from data)
                               (cp-cp data)
                               (cp-wprime data)
                               (cp-tau data))))
              (when ndata
                (match-define (list valid-from cp wprime tau) ndata)
                (maybe-start-transaction)
                (update-cp (cp-id data) valid-from cp wprime tau)
                ;; Refresh the entire list
                (refresh-contents)))))))

    ;; Called when the user clicks the "Delete" button.  Deletes the selected
    ;; entry from the database.  A transaction is started if needed, so if the
    ;; user cancels the dialog, all changes are rolled back.
    (define (on-delete-cp)
      (let ((index (send cplb get-selected-row-index)))
        (when index
          (let ((data (send cplb get-data-for-row index)))
            (maybe-start-transaction)
            (delete-cp (cp-id data))
            (refresh-contents)))))

    (define/public (show-dialog parent db)
      (set! database db)
      (refresh-contents)
      (if (send this do-edit parent)
          (when (in-transaction? database)
            (commit-transaction database)
            (log-event 'critical-power-parameters-changed #f))
          (when (in-transaction? database)
            (rollback-transaction database)))
      (set! database #f))

    ))

(define the-cp-editor #f)

(define (get-cp-editor)
  (unless the-cp-editor
    (set! the-cp-editor (new edit-cp-dialog%)))
  the-cp-editor)
