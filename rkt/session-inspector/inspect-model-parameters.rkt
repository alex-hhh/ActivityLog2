#lang racket/base
;; inspect-model-parameters.rkt -- show sport zones and CP parameters that
;; apply to the current session.
;;
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

(require racket/class
         racket/gui/base
         db/base
         pict
         pict/snip
         racket/format
         racket/math
         racket/list
         racket/match
         "../color-theme.rkt"
         "../dbapp.rkt"
         "../fmt-util.rkt"
         "../series-meta.rkt"
         "../data-frame/df.rkt")

(provide model-parameters-panel%)

;; Color to use for header styles
(define header-color (make-object color% #x2f #x4f #x4f))

;; The style applied to section headers in the text editor
(define header-style
  (let ([delta (new style-delta%)])
    (send delta set-size-add 4)
    (send delta set-delta-foreground header-color)
    (send delta set-weight-on 'bold)
    delta))

;; The style applied to sub-section headers in the text editor
(define sub-header-style
  (let ([delta (new style-delta%)])
    (send delta set-size-add 2)
    (send delta set-delta-foreground header-color)
    (send delta set-weight-on 'normal)
    delta))

(define bold-style
  (let ([delta (new style-delta%)])
    (send delta set-weight-on 'bold)
    delta))

;; Insert a heading in EDITOR.  The TEXT is inserted and a styling is applied
;; to it (by default header style)
(define (insert-heading editor text (style header-style))
  (insert-newline editor)
  (let ((p (send editor last-position)))
    (send editor insert (make-object string-snip% text))
    (send editor change-style style p (send editor last-position)))
  (insert-newline editor)
  (insert-newline editor))

(define (insert-inline-text editor text (style #f))
  (if style
      (let ((p (send editor last-position)))
        (send editor insert (make-object string-snip% text))
        (send editor change-style style p (send editor last-position)))
      (send editor insert (make-object string-snip% text))))

;; Insert a newline into the editor -- this is surprisingly non-trivial
(define (insert-newline editor)
  (let ((s (make-object string-snip% "\n")))
    (send s set-flags (cons 'hard-newline (send s get-flags)))
    (send editor insert s)))

;; Insert a text paragraph into the editor.  This is a text block followed by
;; a new-line.
(define (insert-paragraph editor text)
  (let ((s (make-object string-snip% text)))
    (send editor insert s)
    (insert-newline editor)))

;; Return the sport zone ID's and their validity ranges that apply for a
;; session identified by SID (a session id)
(define (get-sport-zones-for-session db sid)
  (query-rows db
              "
select VSZFS.zone_id, VSZFS.zone_metric_id, VSZ.valid_from, VSZ.valid_until
  from V_SPORT_ZONE_FOR_SESSION VSZFS,
       V_SPORT_ZONE VSZ
 where VSZFS.session_id = ?
   and VSZ.zone_id = VSZFS.zone_id
order by VSZFS.zone_metric_id" sid))

;; Return the sport zone values for sport zone ZID (a SPORT_ZONE.id)
(define (get-sport-zone-values db zid)
  (query-list
   db
   "select zone_value from SPORT_ZONE_ITEM where sport_zone_id = ? order by zone_number"
   zid))

;; Return the critical power parameters (CP, W'Prime, Tau) and the validity
;; range that apply to a session identified by SID (a session id)
(define (get-critical-power-for-session db sid)
  (query-maybe-row db "
  select VCPFS.cp_id, VCP.cp, VCP.wprime, VCP.tau, VCP.valid_from, VCP.valid_until
  from V_CRITICAL_POWER_FOR_SESSION VCPFS,
       V_CRITICAL_POWER VCP
 where VCPFS.session_id = ?
   and VCP.cp_id = VCPFS.cp_id" sid))

;; Return a PICT with a visual representation of sport ZONES (as returned by
;; `get-sport-zone-values`).  The pict will be WIDTH x HEIGHT pixels.  FMT-FN
;; is used to format zone values (for example when they are pace values)
(define (zones-pict zones [width 600] [height 60]
                    #:fmt-fn (fmt-fn (lambda (z) (~a (exact-round z)))))
  (define start-pad 30)
  (define end-pad 30)
  (define top-height (exact-round (* height 0.25)))
  (define bottom-height (exact-round (* height 0.25)))
  (define middle-height (- height top-height bottom-height))
  (define zone-width (- width end-pad start-pad))
  
  (let ((range (- (last zones) (first zones)))
        (nzones (length zones)))
    (define the-pict
      (vc-append
       (filled-rectangle
        width top-height
        #:draw-border? #f
        #:color "white")
       (apply
        hc-append
        (for/list ([zone zones]
                   [nxt-zone (cdr zones)]
                   [index nzones]
                   #:when (>= zone 0))
          (let ((label (format "Z~a" index))
                (zlength (exact-round (* (/ (- nxt-zone zone) range) zone-width))))
            (cc-superimpose
             (filled-rectangle zlength middle-height
                               #:draw-border? #f
                               #:color (cdr (list-ref (zone-colors) index)))
             (text label)))))
       (filled-rectangle
        width bottom-height
        #:draw-border? #f
        #:color "white")
       ))

    (define ypos start-pad)
    
    (for/list ([zone zones]
               [nxt-zone (cdr zones)]
               [index nzones]
               #:when (>= zone 0))
      (let ((zlength (exact-round (* (/ (- nxt-zone zone) range) zone-width)))
            (label (text (fmt-fn nxt-zone))))
        (set! ypos (+ ypos zlength))
        (set! the-pict
              (pin-over the-pict
                        (exact-round (- ypos (/ (pict-width label) 2)))
                        (if (even? index) 0 (+ top-height middle-height))
                        label))))
    the-pict))

;; Insert a "validity range" message into the editor.  FROM and TO are the
;; timestamps for the range.
(define (insert-validity-range editor from to)
  (insert-paragraph editor
                    (format "Valid from ~a to ~a"
                            (date-time->string from)
                            (date-time->string to))))

;; Insert the sport zone information about the session SID (a session id),
;; SPORT is the sport type for the session.
(define (insert-sport-zone-info editor sid sport)
  (let ((zones (get-sport-zones-for-session (current-database) sid)))
    (if (null? zones)
        (begin
          (insert-heading editor "Sport Zones")
          (insert-paragraph editor "No sport zones are defined for this session."))
        (begin
          (for ((zone zones))
            (match-define (vector zid zmetric valid-from valid-until) zone)
            (define zvals (get-sport-zone-values (current-database) zid))
            (case zmetric
              ((1)
               (insert-heading editor "Heart Rate Zones" header-style)
               (insert-validity-range editor valid-from valid-until)
               (insert-newline editor)
               (send editor insert
                     (new pict-snip% [pict (zones-pict zvals)]))
               (insert-newline editor))
              ((2)
               (insert-heading editor "Speed / Pace Zones" header-style)
               (insert-validity-range editor valid-from valid-until)
               (insert-newline editor)
               (send editor insert
                     (new pict-snip%
                          [pict
                           (zones-pict zvals #:fmt-fn
                                       (cond
                                         ((is-lap-swimming? sport) swim-pace->string)
                                         ((is-runnig? sport) pace->string)
                                         (#t speed->string)))]))
               (insert-newline editor))
              ((3)
               (insert-heading editor "Power Zones" header-style)
               (insert-validity-range editor valid-from valid-until)
               (insert-newline editor)
               (send editor insert
                     (new pict-snip% [pict (zones-pict zvals)]))
               (insert-newline editor))))))))

(define no-cp-text
  "No critical power or velocity parameters are defined for this session.")

;; Insert the critical power (or velocity) information about the session SID
;; (a session id), SPORT is the sport type for the session.
(define (insert-critical-power-info editor sid sport)

  (insert-heading
   editor
   (cond
     ((is-runnig? sport) "Critical Velocity")
     ((is-cycling? sport) "Critical Power")
     ((is-swimming? sport) "Critical Velocity")
     (#t "Critical Power")))
    
  (let ((cp-info (get-critical-power-for-session (current-database) sid)))
    (if cp-info
        (match-let (((vector id cp wprime tau valid-from valid-until) cp-info))
          (let ((actual-tau (if (sql-null? tau) (/ wprime cp) tau))
                (implicit-tau? (sql-null? tau)))
            (cond
              ((is-runnig? sport)
               (insert-paragraph
                editor
                (format "CV: ~a; D': ~a meters; Tau ~a seconds~a"
                        (pace->string cp #t)
                        (short-distance->string wprime)
                        (exact-round actual-tau)
                        (if implicit-tau? " (implicit)" ""))))
              ((is-cycling? sport)
               (insert-paragraph
                editor
                (format "CP: ~a; D': ~a Joules; Tau ~a seconds~a"
                        (power->string cp #t)
                        (exact-round wprime)
                        (exact-round actual-tau)
                        (if implicit-tau? " (implicit)" ""))))
              ((is-swimming? sport)
               (insert-paragraph
                editor
                (format "CV: ~a; D': ~a meters; Tau ~a seconds~a"
                        (swim-pace->string cp #t)
                        (short-distance->string wprime)
                        (exact-round actual-tau)
                        (if implicit-tau? " (implicit)" "")))))))
        (insert-paragraph editor no-cp-text))))

;; Insert a "remarks" section into the editor, instructing the user on how to
;; edit or add sport zones or CP data.
(define (insert-remarks editor)
  (insert-heading editor "Notes" sub-header-style)
  (insert-inline-text editor "You can define or edit sport zones from the ")
  (insert-inline-text editor "\"Athlete/Edit Sport Zones...\"" bold-style)
  (insert-inline-text editor " menu, and the critical power parameters from the ")
  (insert-inline-text editor "\"Athlete/Edit Critical Power...\"" bold-style)
  (insert-inline-text editor " menu."))

;; Display the model parameters that apply to the activity shown in the
;; inspector.  Since sport zones and critical power are defined for date
;; ranges, this page provides some helpful information about what zones and CP
;; are valid for the current session.
(define model-parameters-panel%
  (class object% (init parent) (super-new)

    (define canvas (new editor-canvas%
                        [parent parent]
                        [style '(no-hscroll)]
                        [horizontal-inset 20]))
    (define text (new text%))
    (send canvas set-editor text)
    (send text set-tabs '(8) 8 #f)
    (send text auto-wrap #t)
    (send text lock #t)

    (define/public (save-visual-layout)
      (void))

    (define/public (set-session session df)
      (send text lock #f)
      (send text erase)
      (let ((sid (df-get-property df 'session-id))
            (sport (df-get-property df 'sport)))
        (if sid
            (begin
              (insert-sport-zone-info text sid sport)
              (insert-newline text)
              (insert-critical-power-info text sid sport)
              (insert-newline text)
              (insert-remarks text))
            (insert-heading text "No session id defined for data frame")))
      (send text lock #t))

    ))
