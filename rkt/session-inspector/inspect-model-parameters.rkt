#lang racket/base
;; inspect-model-parameters.rkt -- show sport zones and CP parameters that
;; apply to the current session.
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2017, 2018, 2019 Alex Harsányi <AlexHarsanyi@gmail.com>
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
         pict
         pict/snip
         racket/class
         racket/format
         racket/gui/base
         racket/list
         racket/match
         racket/math
         "../color-theme.rkt"
         (prefix-in ct: "../color-theme.rkt")
         "../dbapp.rkt"
         "../fmt-util-ut.rkt"
         "../fmt-util.rkt"
         "../session-df/native-series.rkt"
         "../sport-charms.rkt")

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

;; style to align elements on a line on the top margin -- used to line up the
;; pict objects.
(define top-align-style
  (let ([delta (new style-delta%)])
    (send delta set-alignment-off 'top)
    (send delta set-alignment-on 'top)
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

(define (get-sport-zones db zone-id)
  (for/list (([name value]
              (in-query db "select zone_name, zone_value
                              from SPORT_ZONE_ITEM
                             where sport_zone_id = ?
                             order by zone_number" zone-id)))
    (list (if (sql-null? name) "" (~a name)) value)))

(define pd-background (make-object color% #xff #xf8 #xdc 0.75))
(define pd-item-color (make-object color% #x2f #x4f #x4f))
(define pd-label-color (make-object color% #x77 #x88 #x99))
(define pd-title-font (send the-font-list find-or-create-font 12 'default 'normal 'normal))
(define pd-item-font (send the-font-list find-or-create-font 12 'default 'normal 'normal))
(define pd-label-font (send the-font-list find-or-create-font 10 'default 'normal 'normal))
(define pd-title-face (cons pd-item-color pd-title-font))
(define pd-item-face (cons pd-item-color pd-item-font))
(define pd-label-face (cons pd-item-color pd-label-font))
(define pd-header-font (send the-font-list find-or-create-font 16 'default 'normal 'bold))
(define pd-header-face (cons header-color pd-header-font))
(define pd-sub-heading-face (cons pd-label-color pd-label-font))

;; Return a PICT with a visual representation of sport ZONES (as returned by
;; `get-sport-zones`).  FMT-FN is used to format zone values (for example when
;; they are pace values)
(define (zones-pict zones #:fmt-fn (fmt-fn (lambda (z) (~a (exact-round z)))))
  (define items
    (for/fold ([result (list (text "Color" pd-label-face)
                             (text "Upper" pd-label-face)
                             (text "Lower" pd-label-face)
                             (text "Zone" pd-label-face))])
              ([(item index) (in-indexed (in-list zones))]
               [nitem (in-list (cdr zones))])
      (match-define (list name start) item)
      (match-define (list _ end) nitem)
      (cons (colorize (disk 20) (cdr (list-ref (ct:zone-colors) index)))
            (cons (text (fmt-fn end) pd-item-face)
                  (cons (text (fmt-fn start) pd-item-face)
                        (cons (text name pd-label-face)
                              result))))))
  (table 4
         (reverse items)
         (list lc-superimpose rc-superimpose rc-superimpose cc-superimpose)
         cc-superimpose
         20
         10))

;; Return the critical power parameters (CP, W'Prime, Tau) and the validity
;; range that apply to a session identified by SID (a session id)
(define (get-critical-power-for-session db sid)
  (query-maybe-row db "
  select VCPFS.cp_id, VCP.cp, VCP.wprime, VCP.tau, VCP.valid_from, VCP.valid_until
  from V_CRITICAL_POWER_FOR_SESSION VCPFS,
       V_CRITICAL_POWER VCP
 where VCPFS.session_id = ?
   and VCP.cp_id = VCPFS.cp_id" sid))

;; Insert a "validity range" message into the editor.  FROM and TO are the
;; timestamps for the range.
(define (insert-validity-range editor from to)
  (if (> to (- (current-seconds) 300))
      ;; The validity SQL code for time zones will always generate a timestamp
      ;; for the "to" part, and the last timezone will have the current time,
      ;; so we can just discard that.
      (insert-paragraph editor
                        (format "Valid from ~a" (calendar-date->string from)))
      (insert-paragraph editor
                        (format "Valid from ~a to ~a"
                                (calendar-date->string from)
                                (calendar-date->string to)))))

(define (validity-range->string from to)
  (if (> to (- (current-seconds) 300))
      ;; The validity SQL code for time zones will always generate a timestamp
      ;; for the "to" part, and the last timezone will have the current time,
      ;; so we can just discard that.
      (format "Valid from ~a" (calendar-date->string from))
      (format "Valid from ~a to ~a" (calendar-date->string from) (calendar-date->string to))))

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
            (define zvals (get-sport-zones (current-database) zid))
            (case zmetric
              ((1)
               (let ((h (text "Heart Rate Zones (BPM)" pd-header-face))
                     (v (text (validity-range->string valid-from valid-until) pd-sub-heading-face))
                     (z (zones-pict zvals)))
                 (define p (inset (vl-append 10 h v z) 30))
                 (let ((start-position (send editor last-position)))
                   (send editor insert (new pict-snip% [pict p]))
                   (send editor change-style top-align-style start-position (send editor last-position)))))
              ((2)
               (let ((h (text (format "Speed / Pace Zones (~a)"
                                      (if (eq? (al-pref-measurement-system) 'metric)
                                          (cond ((is-lap-swimming? sport) "min/100m")
                                                ((is-runnig? sport) "min/km")
                                                (#t "km/h"))
                                          (cond ((is-lap-swimming? sport) "min/100yd")
                                                ((is-runnig? sport) "min/mile")
                                                (#t "miles/h"))))
                              pd-header-face))
                     (v (text (validity-range->string valid-from valid-until) pd-sub-heading-face))
                     (z (zones-pict zvals
                                    #:fmt-fn
                                    (cond
                                      ((is-lap-swimming? sport) swim-pace->string)
                                      ((is-runnig? sport) pace->string)
                                      (#t speed->string)))))
                 (define p (inset (vl-append 10 h v z) 30))
                 (let ((start-position (send editor last-position)))
                   (send editor insert (new pict-snip% [pict p]))
                   (send editor change-style top-align-style start-position (send editor last-position)))))
              ((3)
               (let ((h (text "Power Zones (watts)" pd-header-face))
                     (v (text (validity-range->string valid-from valid-until) pd-sub-heading-face))
                     (z (zones-pict zvals)))
                 (define p (inset (vl-append 10 h v z) 30))
                 (let ((start-position (send editor last-position)))
                   (send editor insert (new pict-snip% [pict p]))
                   (send editor change-style top-align-style start-position (send editor last-position)))))))))))

(define no-cp-text
  "No critical power or velocity parameters are defined for this session.")

;; Insert the critical power (or velocity) information about the session SID
;; (a session id), SPORT is the sport type for the session.
(define (insert-critical-power-info editor sid sport)

  (define title
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
              (define cp-pict
                (cond
                  ((is-runnig? sport)
                   (let ((items (list (text "CV" pd-label-face)
                                      (text (pace->string cp #t) pd-item-face)
                                      (text "D'" pd-label-face)
                                      (text (short-distance->string wprime #t) pd-item-face)
                                      (text (if implicit-tau? "Tau (implicit)" "Tau") pd-label-face)
                                      (text (format "~a seconds" (exact-round actual-tau)) pd-item-face))))
                     (table 2 items (list lc-superimpose lc-superimpose) cc-superimpose 20 10)))
                  
                  ((is-swimming? sport)
                   (let ((items (list (text "CV" pd-label-face)
                                      (text (swim-pace->string cp #t) pd-item-face)
                                      (text "D'" pd-label-face)
                                      (text (short-distance->string wprime #t) pd-item-face)
                                      (text (if implicit-tau? "Tau (implicit)" "Tau") pd-label-face)
                                      (text (format "~a seconds" (exact-round actual-tau)) pd-item-face))))
                     (table 2 items (list lc-superimpose lc-superimpose) cc-superimpose 20 10)))
                  (#t
                   (let ((items (list (text "CP" pd-label-face)
                                      (text (power->string cp #t) pd-item-face)
                                      (text "W'" pd-label-face)
                                      (text (format "~a joules" (exact-round wprime)) pd-item-face)
                                      (text (if implicit-tau? "Tau (implicit)" "Tau") pd-label-face)
                                      (text (format "~a seconds" (exact-round actual-tau)) pd-item-face))))
                     (table 2 items (list lc-superimpose lc-superimpose) cc-superimpose 20 10)))))
              (let ((h (text title pd-header-face))
                    (v (text (validity-range->string valid-from valid-until) pd-sub-heading-face))
                    (z cp-pict))
                (define p (inset (vl-append 10 h v z) 30))
                (let ((start-position (send editor last-position)))
                  (send editor insert (new pict-snip% [pict p]))
                  (send editor change-style top-align-style start-position (send editor last-position))))))
          (insert-paragraph editor no-cp-text))))

;; Insert a "remarks" section into the editor, instructing the user on how to
;; edit or add sport zones or CP data.
(define (insert-remarks editor)
  (insert-heading editor "Notes" sub-header-style)
  (insert-inline-text editor "These are the sport zones and critical power parameters that apply to this activity.  You can define or edit sport zones from the ")
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
              (insert-critical-power-info text sid sport)
              (insert-newline text)
              (insert-remarks text))
            (insert-heading text "No session id defined for data frame")))
      (send text lock #t))

    ))
