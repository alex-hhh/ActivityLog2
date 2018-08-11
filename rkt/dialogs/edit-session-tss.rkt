#lang racket/base
;; edit-session-tss.rkt -- edit (update) the training stress score for a
;; session
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
         racket/class
         racket/gui/base
         racket/match
         "../fmt-util.rkt"
         "../sport-charms.rkt"
         "../dbutil.rkt"
         "../widgets/main.rkt"
         "../session-df.rkt"
         "../data-frame/df.rkt")

(provide get-edit-session-tss-dialog)
(provide maybe-update-session-tss)

(define message-font
  (send the-font-list find-or-create-font 12 'default 'normal 'normal))

;; Return information used to calculate TSS for a session
(define (get-session-effort session-id db)
  (query-maybe-row
   db
   "select S.sport_id, S.sub_sport_id,
       SS.total_timer_time,
       S.rpe_scale,
       SS.avg_heart_rate,
       SS.normalized_power,
       S.training_stress_score,
       S.start_time,
       SS.avg_speed,
       SS.total_distance
 from A_SESSION S, SECTION_SUMMARY SS
 where S.summary_id = SS.id
 and S.id = ?" session-id))

(define (get-session-hr-zones session-id)
  (get-session-sport-zones session-id 1))

(define (effort-np effort-data)
  (sql-column-ref effort-data 5 #f))

(define (effort-avg-hr effort-data)
  (sql-column-ref effort-data 4 #f))

(define (effort-rpe effort-data)
  (sql-column-ref effort-data 3 #f))

(define (effort-duration effort-data)
  (sql-column-ref effort-data 2 #f))

(define (effort-avg-speed effort-data)
  (sql-column-ref effort-data 8 #f))

(define (effort-tss effort-data)
  (sql-column-ref effort-data 6 #f))

(define (effort-distance effort-data)
  (sql-column-ref effort-data 9 #f))

;; Convert a "Rating of Perceived Extertion" value into a TSS/hour value, the
;; result can be multiplied by the activity duration to get the TSS of the
;; activity.
(define (rpe->tss/hour rpe)
  (cond ((<= rpe 1) 20)
        ((<= rpe 2) 30)
        ((<= rpe 3) 40)
        ((<= rpe 4) 50)
        ((<= rpe 5) 60)
        ((<= rpe 6) 70)
        ((<= rpe 7) 80)
        ((<= rpe 8) 100)
        ((<= rpe 9) 120)
        ((<= rpe 10) 140)
        (#t 140)))

;; Convert a (heart rate) zone into a TSS/hour value, the result can be
;; multiplied by the activity duration to get the TSS of the activity.
(define (zone->tss/hour zone)
  (cond ((<= zone 1) 20)
        ((<= zone 2) (+ 20 (* (- zone 1) 30)))
        ((<= zone 3) (+ 50 (* (- zone 2) 20)))
        ((<= zone 4) (+ 70 (* (- zone 3) 10)))
        ((<= zone 5) (+ 80 (* (- zone 4) 20)))
        ((<= zone 6) (+ 100 (* (- zone 5) 40)))
        (#t 140)))

;; Compute TSS based on RPE and duration
(define (rpe->tss rpe duration)
  (* (rpe->tss/hour rpe) (/ duration 3600.0)))

;; Compute TSS based on zone and duration
(define (zone->tss zone duration)
  (* (zone->tss/hour zone) (/  duration 3600.0)))

;; Compute TSS based on NP (normalized power) and duration
(define (np->tss ftp np duration)
  (let ((intensity-factor (/ np ftp)))
    (* intensity-factor intensity-factor (/ duration 3600.0) 100.0)))

;; Compute TSS based on swim FTP pace and duration
(define (swim-speed->tss tpace speed duration)
  (let ((intensity-factor (/ speed tpace)))
    (* intensity-factor intensity-factor intensity-factor (/ duration 3600.0) 100)))

;; Compute the TSS of a session based on heart rate zones using the
;; data-frame% DF.  This is done by computing a fractional TSS for each track
;; point and should provide a better TSS value than simply taking the average
;; HR for the entire session (it is also slower).
(define (compute-session-tss/hr df)
  ;; NOTE: we use the timer series, so we don't count TSS while the recording
  ;; is stopped.  We could use the elapsed series to count TSS while stopped
  ;; as well.
  (if (df-contains? df "hr-zone" "timer")
      (df-fold
       df
       '("timer" "hr-zone")
       0
       (lambda (tss prev next)
         (if prev
             (match-let (((list t0 z0) prev)
                         ((list t1 z1) next))
               (if (and (number? t0) (number? z0) (number? t1) (number? z1))
                   (+ tss (zone->tss (/ (+ z0 z1) 2) (- t1 t0)))
                   tss))
             tss)))
      #f))

(define edit-session-tss-dialog%
  (class edit-dialog-base%
    (init)
    (super-new [title "Session Effort"] [icon (edit-icon)])

    (define database #f)
    (define session-id #f)
    (define session-df #f)
    (define effort #f)                  ; as received by `get-session-effort'

    (define calculation-methods
      '(("RPE" . rpe)
        ("Swim T-Pace" . swim-tpace)
        ;; ("HR Zone" . hr-zone)  ; rough hr based TSS calculation, not in use
        ("HR Zone" . hr-zone-2)
        ("Weighted Power" . normalized-power)
        ("Manual" . manual)))

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

    (let ((p (send this get-client-pane)))
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
                   [choices '("0 -- Not Specified"
                              "1 -- Rest"
                              "2 -- Really Easy"
                              "3 -- Easy"
                              "4 -- Moderate"
                              "5 -- Challenging"
                              "6 -- Hard"
                              "7 -- Hard"
                              "8 -- Really Hard"
                              "9 -- Really, Really Hard"
                              "10 -- Maximal")]
                   [callback (lambda (c e) (calculate-tss))]))
      
      (set! swim-tpace
            (new swim-pace-input-field% [parent tss-selection-pane] 
                 [label "Swim T-Pace: "] [style '(single deleted)]
                 [min-width 100] [stretchable-width #f]
                 [valid-value-cb (lambda (v) (calculate-tss))]))
      
      (set! threshold-power
            (new number-input-field% [parent tss-selection-pane]
                 [label "Threshold Power: "] [style '(single deleted)]
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
              ;; ((hr-zone)
              ;;  (let ((hr (effort-avg-hr effort))
              ;;        (zones (get-session-hr-zones effort))
              ;;        (duration (effort-duration effort)))
              ;;    (cond ((not hr) 
              ;;           (send notice set-label "No heart rate data available"))
              ;;          ((not zones) 
              ;;           (send notice set-label "No heart rate zones defined"))
              ;;          (#t
              ;;           (set! computed-tss (zone->tss (val->zone hr zones) duration))))))
              ((hr-zone-2)
               (let ((hr (effort-avg-hr effort))
                     (zones (get-session-hr-zones session-id)))
                 (cond ((not hr) 
                        (send notice set-label "No heart rate data available"))
                       ((not zones) 
                        (send notice set-label "No heart rate zones defined"))
                       (#t
                        (unless session-df
                          (set! session-df (session-df database session-id)))
                        (set! computed-tss (compute-session-tss/hr session-df))))))
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
                 (cond ((not np)
                        (send notice set-label "No Normalized Power available"))
                       ((not (number? ftp))
                        (send notice set-label "FTP value is invalid/not set"))
                       (#t
                        (set! computed-tss (np->tss ftp np duration))))))
              ((manual)
               (set! computed-tss (send manual-tss get-converted-value))))
            (send notice set-label "Session has no duration")))
      (send updated-tss set-label
            (if (number? computed-tss) (n->string computed-tss) "---")))

    ;; Setup the dialog for editing the TSS of SID, a session id.
    (define (setup-for-session sid db)
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
      
      (let ((row (query-row db "
select name, sport_id, sub_sport_id, start_time from A_SESSION where id = ?"
                            sid)))
        (let ((hl (format "~a (~a)"
                          (sql-column-ref row 0 "Untitled")
                          (get-sport-name (sql-column-ref row 1 #f)
                                          (sql-column-ref row 2 #f)))))
          (send headline set-label hl))
        (let ((timestamp (sql-column-ref row 3 0)))
          (send start-time set-label (date-time->string timestamp))))
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

(define (calculate-session-tss effort df sid db)
  (let ((duration (effort-duration  effort)))
    (if duration
        (or (let ((np (effort-np effort))
                  (ftp (get-athlete-ftp db)))
              (and np ftp (np->tss ftp np duration)))
            (let ((sport (sql-column-ref effort 0 #f))
                  (dist (effort-distance effort))
                  (tpace (get-athlete-swim-tpace db)))
              (and sport (eq? sport 5) dist tpace
                   ;; For swim sessions, we use the normalized speed (total
                   ;; distance/total time), which includes pauses.  The
                   ;; AVG_SPEED stored in the session summary only counts
                   ;; moving time.
                   (swim-speed->tss tpace (/ dist duration) duration)))
            (compute-session-tss/hr df)
            (let ((rpe (effort-rpe effort)))
              (and rpe (rpe->tss rpe duration))))
        #f)))

(define (maybe-update-session-tss session-id df db [force? #f])
  (let ((effort (get-session-effort session-id db)))
    (when (or force? (not (effort-tss effort)))
      (let ((tss (calculate-session-tss effort df session-id db)))
        (when tss
          (query-exec 
           db 
           "update A_SESSION set training_stress_score = ? where id = ?" 
           tss session-id))))))
