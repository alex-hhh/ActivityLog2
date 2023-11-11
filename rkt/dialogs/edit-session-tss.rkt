#lang racket/base
;; edit-session-tss.rkt -- edit (update) the training stress score for a
;; session
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015, 2018, 2019, 2020, 2021, 2023 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require data-frame
         db/base
         racket/class
         racket/gui/base
         racket/match
         "../dbutil.rkt"
         "../fmt-util-ut.rkt"
         "../fmt-util.rkt"
         "../session-df/session-df.rkt"
         "../models/sport-zone.rkt"
         "../models/tss.rkt"
         "../models/coggan.rkt"
         "../sport-charms.rkt"
         "../widgets/main.rkt")

(provide get-edit-session-tss-dialog)

(define calculation-methods
  '(("RPE" . rpe)
    ("Swim T-Pace" . swim-tpace)
    ;; ("HR Zone" . hr-zone)  ; rough hr based TSS calculation, not in use
    ("HR Zone" . hr-zone-2)
    ("ISO Power" . normalized-power)
    ("Manual" . manual)))

(define edit-session-tss-dialog%
  (class edit-dialog-base%
    (init)
    (super-new [title "Session Effort"] [icon (edit-icon)])

    (define database #f)
    (define session-id #f)
    (define df #f)
    (define effort #f)                  ; as received by `get-session-effort'

    ;; When a session has power data, but no Normalized Power (ISO Power), we
    ;; calculate it here (if the user wants TSS based on ISO Power).  In that
    ;; case, we also save these parameters to the database for the session...
    (define calculated-cg-metrics #f)

    ;; selected method to calculate TSS, one of the symbols in
    ;; `calculation-methods'
    (define current-method #f)

    ;; TSS value computed by `calculate-tss', if it is a number?, it is valid,
    ;; otherwise, it is not.
    (define computed-tss #f)

    (define headline #f)                ; message% containing the session headline
    (define start-time #f)              ; message% containing the session start time
    (define duration #f)                ; message% containing the session duration
    (define original-tss #f)            ; message% containing the session TSS (from the database)

    (define tss-selection-pane #f)

    (define rpe-scale #f)               ; a choice% for selecting the RPE for a session
    (define swim-tpace #f)              ; a number-input-field% for the Swim T-Pace
    (define threshold-power #f)         ; a number-input-field% for the Functional Threshold Power
    (define manual-tss #f)              ; a number-input-field% for manual TSS input

    (define updated-tss #f)             ; a message% to display the computed TSS
    (define notice #f)                  ; a message% to display any errors


    (let ((p (send this get-client-pane))
          (message-font (send the-font-list find-or-create-font 12 'default 'normal 'normal)))
      (let ((hp (make-horizontal-pane p #f)))
        (new message% [parent hp] [label "Activity: "]
             [stretchable-width #f])
        (set! headline (new message% [parent hp] [label "Untitled"]
                            [font message-font]
                            [stretchable-width #t])))
      (let ((hp (make-horizontal-pane p #f)))
        (new message% [parent hp] [label "Start time: "] [stretchable-width #f])
        (set! start-time
              (new message% [parent hp] [label ""] [font message-font]
                   [min-width 150]
                   [stretchable-width #t]))
        (new message% [parent hp] [label "Duration: "] [stretchable-width #f])
        (set! duration
              (new message%
                   [parent hp] [label ""] [font message-font]
                   [min-width 80]
                   [stretchable-width #t]))
        (new message% [parent hp] [label "Original Effort: "])
        (set! original-tss
              (new message% [parent hp] [label "888"]
                   [min-width 80]
                   [font message-font] [stretchable-width #t])))

      (let ((hp (make-horizontal-pane p #f)))
        (new choice% [parent hp] [label "Calculation Method: "]
             [choices (map car calculation-methods)]
             [callback (lambda (c e)
                         (let ((index (send c get-selection)))
                           (on-tss-calculation-method
                            (cdr (list-ref calculation-methods index)))))])
        (set! tss-selection-pane (make-horizontal-pane hp #f)))

      (set! rpe-scale
              (new choice% [parent tss-selection-pane]
                   [label "RPE: "] [style '(deleted)]
                   [choices
                    (for/list ([rpe (in-range 11)])
                      (rpe->string rpe))]
                   [callback (lambda (c e) (calculate-tss))]))

      (set! swim-tpace
            (new swim-pace-input-field% [parent tss-selection-pane]
                 [label "Swim T-Pace: "] [style '(single deleted)]
                 [min-width 100] [stretchable-width #f]
                 [valid-value-cb (lambda (v) (calculate-tss))]))

      (set! threshold-power
            (new number-input-field% [parent tss-selection-pane]
                 [label "FTP: "] [style '(single deleted)]
                 [min-width 100] [stretchable-width #f]
                 [cue-text "watts"]
                 [valid-value-cb (lambda (v) (calculate-tss))]))

      (set! manual-tss
            (new number-input-field% [parent tss-selection-pane]
                 [label "Effort: "] [style '(single deleted)]
                 [min-value 0]
                 [min-width 100]
                 [stretchable-width #f]
                 [cue-text "tss"]
                 [valid-value-cb (lambda (v) (calculate-tss))]))
      (let ((hp (make-horizontal-pane p #f)))
        (set! notice (new message% [parent hp] [label ""]
                                   [stretchable-width #t]
                                   [font message-font])))

      (let ((hp (make-horizontal-pane p #f)))
        (new message% [parent hp] [label "Updated Effort: "])
        (set! updated-tss
              (new message% [parent hp] [label "888"]
                   [stretchable-width #t]
                   [font message-font]))))

    ;; Things to do when the calculation method has changed: setup visible
    ;; fields and update the TSS.
    (define (on-tss-calculation-method m)
      (unless (eq? m current-method)
        (case m
          ((rpe)
           (send tss-selection-pane change-children
                 (lambda (old) (list rpe-scale))))
          ((swim-tpace)
           (send tss-selection-pane change-children
                 (lambda (old) (list swim-tpace))))
          ((normalized-power)
           (send tss-selection-pane change-children
                 (lambda (old) (list threshold-power))))
          ((manual)
           (send tss-selection-pane change-children
                 (lambda (old) (list manual-tss))))
          (else
           (send tss-selection-pane change-children
                 (lambda (old) (list)))))
        (set! current-method m)
        (calculate-tss)))

    ;; Calculate the TSS according to the current method, session data and
    ;; values from the additional input fields.
    (define (calculate-tss)
      (set! computed-tss #f)
      (send notice set-label "")
      (when effort                      ; cannot calculate TSS if effort data is missing
        (if (effort-duration effort)
            (case current-method
              ((rpe)
               (let ((rpe (send rpe-scale get-selection)))
                 (when (> rpe 0)
                   (set! computed-tss (rpe->tss rpe (effort-duration effort))))))
              ((hr-zone-2)
               (let ((hr (effort-avg-hr effort))
                     (zones (sport-zones-for-session session-id 'heart-rate)))
                 (cond ((not hr)
                        (send notice set-label "No heart rate data available"))
                       ((not zones)
                        (send notice set-label "No heart rate zones defined"))
                       (#t
                        (unless df
                          (set! df (session-df database session-id)))
                        (set! computed-tss (compute-session-tss/hr df))))))
              ((swim-tpace)
               (let ((sport (sql-column-ref effort 0 #f))
                     (dist (effort-distance effort))
                     (time (effort-duration effort))
                     (tpace (send swim-tpace get-converted-value)))
                 (cond ((not (eq? sport 5))
                        (send notice set-label "Not a swim activity"))
                       ((not (number? tpace))
                        (send notice set-label "Swim T-Pace is invalid/not set"))
                       (#t
                        (set! computed-tss (swim-speed->tss tpace (/ dist time) time))))))
              ((normalized-power)
               (let ((np (effort-np effort))
                     (ftp (send threshold-power get-converted-value))
                     (duration (effort-duration effort)))
                 (cond ((not (number? ftp))
                        (send notice set-label "FTP value is invalid/not set"))
                       ((not np)
                        ;; If we don't have normalized power, try to calculate
                        ;; it from the power data series in the data frame (if
                        ;; available)
                        (unless df
                          (set! df (session-df database session-id)))
                        (if (df-contains? df "pwr" "timer")
                            (begin
                              (set! calculated-cg-metrics
                                    (cg-metrics df
                                                #:ftp ftp
                                                #:series "pwr"
                                                #:weight-series "timer"))
                              (set! computed-tss (cg-tss calculated-cg-metrics)))
                            (send notice set-label "No power data available")))
                       (#t
                        (set! computed-tss (np->tss ftp np duration))))))
              ((manual)
               (set! computed-tss (send manual-tss get-converted-value))))
            (send notice set-label "Session has no duration")))
      (send updated-tss set-label
            (if (number? computed-tss) (n->string computed-tss) "---")))

    ;; Setup the dialog for editing the TSS of SID, a session id.
    (define (setup-for-session sid db)
      (set! df #f)                      ; will be read in as needed
      (set! effort (get-session-effort sid db))
      (send duration set-label
            (let ((d (effort-duration effort)))
              (if d (duration->string d) "")))
      (send original-tss set-label
            (let ((tss (effort-tss effort)))
              (if tss (n->string tss) "none")))
      (send rpe-scale set-selection
            (let ((rpe (effort-rpe effort)))
              (or rpe 0)))
      (let ((ftp (get-athlete-ftp db)))
        (when ftp
          (send threshold-power set-value (n->string ftp))))
      (let ((tpace (get-athlete-swim-tpace db)))
        (when tpace
          (send swim-tpace set-pace-value tpace)))

      (let ((hl (format "~a (~a)"
                        (sql-column-ref effort 11 "Untitled")
                        (get-sport-name (sql-column-ref effort 0 #f)
                                        (sql-column-ref effort 1 #f)))))
        (send headline set-label hl))
      (let ([ts (sql-column-ref effort 7 0)]
            [tz (sql-column-ref effort 10 #f)])
        (send start-time set-label (date-time->string ts #:time-zone tz)))
      (calculate-tss))

    (define (update-session-tss sid db)
      (when (number? computed-tss)
        (query-exec
         db
         "update A_SESSION set training_stress_score = ? where id = ?"
         computed-tss sid)
        ;; Update any data the user may have set while computing the TSS.  We
        ;; only update the relevant data
        (case current-method
          ((rpe)
           (let ((rpe (send rpe-scale get-selection)))
             (query-exec
              db
              "update A_SESSION set rpe_scale = ? where id = ?"
              (if (> rpe 0) rpe sql-null) sid)))
          ((swim-tpace)
           (let ((tpace (send swim-tpace get-converted-value)))
             (when (number? tpace)
               (put-athlete-swim-tpace tpace db))))
          ((normalized-power)
           (when calculated-cg-metrics
             ;; NOTE: TSS might have changed if FTP has changed, so we need to
             ;; update the cg metrics we have, since `put-session-cg-metrics`
             ;; will write TSS again...
             (define umetrics (struct-copy cg calculated-cg-metrics (tss computed-tss)))
             (put-session-cg-metrics sid umetrics #:database db))
           (let ((ftp (send threshold-power get-converted-value)))
             (when ftp
               (put-athlete-ftp ftp db)))))))

    (define/override (has-valid-data?)
      (number? computed-tss))

    (define/public (run parent db sid)
      (set! session-id sid)
      (set! database db)
      (setup-for-session sid db)
      (let ((result (send this do-edit parent)))
        (when result
          (update-session-tss sid db))
        (set! session-id #f)
        (set! database db)
        result))

    (on-tss-calculation-method 'rpe)

    ))


(define the-edit-session-tss-dialog #f)

(define (get-edit-session-tss-dialog)
  (unless the-edit-session-tss-dialog
    (set! the-edit-session-tss-dialog (new edit-session-tss-dialog%)))
  the-edit-session-tss-dialog)
