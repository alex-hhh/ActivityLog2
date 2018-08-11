#lang racket/base
;; view-equipment.rkt -- equipment and service log management panel
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
         (rename-in srfi/48 (format format-48))
         racket/string
         racket/match
         "utilities.rkt"
         "fmt-util.rkt"
         "dbutil.rkt"
         "widgets/main.rkt")

(provide view-equipment%)


;;...................................................... edit-equipment% ....

(define edit-equipment%
  (class edit-dialog-base%
    (init)
    (super-new [title "Equipment"] [icon (equipment-icon)])

    (define name-text-field #f)
    (define type-text-field #f)
    (define desc-text-field #f)
    (define part-of-choice #f)
    (define part-of-eqids (list sql-null))

    (define retired-check-box #f)

    (let ((p (send this get-client-pane)))
      (set! name-text-field (new text-field% [parent p] [label "Name: "]))
      (let ((p2 (make-horizontal-pane p #f)))
        (set! type-text-field (new text-field% [parent p2] [label "Type: "]))
        (set! retired-check-box
              (new check-box% [parent p2] [label "Retired?"]
                   [value #f])))
      (let ((p3 (make-horizontal-pane p #f)))
        (set! part-of-choice (new choice% [parent p3]
                                  [label "Part of: "]
                                  [choices '("None")]
                                  [min-width 200])))
      (let ((p4 (make-horizontal-pane p #t)))
        (set! desc-text-field
              (new text-field% [parent p4] [label "Description: "]
                   [style '(multiple)]))))

    (define (setup-part-of-choice db exclude)
      (let ((ids (list sql-null))
            (names (list "None")))
        (for (((id name) (in-query db "
select E.id, ifnull(E.name, E.device_name) as ename
  from EQUIPMENT E
 where e.retired = 0 and e.id != ?
 order by ename" (or exclude -1))))
          (set! ids (cons id ids))
          (set! names (cons name names)))
        (set! part-of-eqids (reverse ids))
        (send part-of-choice clear)
        (for ((name (in-list (reverse names))))
          (send part-of-choice append name))))

    (define/override (has-valid-data?)
      ;; The only restriction we have is for a non empty name, although we
      ;; could possibly check for non-duplicate names (in that case we have to
      ;; flag the user that the name is duplicate, otherwise he will not know
      ;; why the save button is disabled.
      (not (equal? (send name-text-field get-value) "")))

    (define (setup-for-equipment db equipment-id)
      (let ((row (query-row db "
select E.name, E.device_name, E.description, E.retired, E.serial_number, E.part_of
from EQUIPMENT E
where E.id = ?" equipment-id)))
        (send name-text-field set-value (sql-column-ref row 0 ""))
        (send type-text-field set-value (sql-column-ref row 1 ""))
        (send desc-text-field set-value (sql-column-ref row 2 ""))
        (send retired-check-box set-value (equal? (sql-column-ref row 3 0) 1))
        ;; Don't allow changing the type for FIT type equipment (that have
        ;; non-zero serial numbers)
        (send type-text-field
              enable
              (equal? (sql-column-ref row 4 0) 0))
        (let ((part-of (vector-ref row 5)))
          (for ((i (in-range (length part-of-eqids)))
                (e (in-list part-of-eqids)))
            (when (equal? e part-of)
              (send part-of-choice set-selection i))))))

    (define (setup-for-new-equipment)
      (send name-text-field set-value "")
      (send type-text-field set-value "")
      (send type-text-field enable  #t)
      (send desc-text-field set-value "")
      (send retired-check-box set-value #f))

    ;; If eqipment-id is #f, a new equipment will be added to the database.
    ;; The equipment id is returned.
    (define (save-to-database db equipment-id)
      (let ((name (send name-text-field get-value))
            (type (send type-text-field get-value))
            (desc (send desc-text-field get-value))
            (part-of (list-ref part-of-eqids (send part-of-choice get-selection)))
            (retired? (send retired-check-box get-value)))
        (if equipment-id
            (begin
              (query-exec db "
update EQUIPMENT set name = ?, device_name = ?, description = ?, retired = ?, part_of = ?
where id = ?" name type desc (if retired? 1 0) part-of equipment-id)
              equipment-id)
            (begin
              (query-exec db "
insert into EQUIPMENT(name, device_name, description, retired, part_of)
values (?, ?, ?, ?, ?)"  name type desc (if retired? 1 0) part-of)
              (query-value db "select max(id) from EQUIPMENT")))))

    (define/public (begin-edit parent database equipment-id)
      (setup-part-of-choice database equipment-id)
      (if equipment-id
          (setup-for-equipment database equipment-id)
          (setup-for-new-equipment))
      (if (send this do-edit parent)
          (save-to-database database equipment-id)
          #f))
    ))

(define the-equipment-editor #f)

(define (get-equipment-editor)
  (unless the-equipment-editor
    (set! the-equipment-editor (new edit-equipment%)))
  the-equipment-editor)


;;............................................... edit-service-log% ....

(define edit-service-log%
  (class edit-dialog-base%
    (init)
    (super-new [title "Service Reminder"] [icon (equipment-icon)])

    (define name-text-field #f)
    (define equipment-choice #f)
    (define tracking-type-choice #f)

    (define tracking-target-panel #f)
    (define target-hours-field #f)
    (define target-mileage-field #f)
    (define target-cal-date-field #f)
    (define target-cal-days-field #f)

    (define start-date-field #f)
    (define end-date-field #f)

    (define equipment-ids '())

    (let ((p (send this get-client-pane)))
      (set! name-text-field (new text-field% [parent p] [label "Description: "]))
      (set! equipment-choice (new choice% [parent p] [label "Equipment: "]
                                  [choices '("None")]
                                  [min-width 200]))
      (let ((p1 (make-horizontal-pane p #f)))
        (set! tracking-target-panel p1)
        (set! tracking-type-choice (new choice% [parent p1] [label "Tracking type: "]
                                        [choices '("Hours" "Mileage" "Calendar date" "Calendar days")]
                                        [callback (lambda (c e) (on-tracking-choice (send c get-selection)))]))
        (set! target-hours-field (new number-input-field% [parent p1] [cue-text "Hours"]))
        (set! target-mileage-field (new number-input-field% [parent p1] [cue-text "Km"] [style '(single deleted)]))
        (set! target-cal-date-field (new date-input-field% [parent p1] [style '(single deleted)]))
        (set! target-cal-days-field (new number-input-field% [parent p1] [cue-text "Days"] [style '(single deleted)])))
      (set! start-date-field (new date-input-field% [parent p] [label "Start tracking on: "]))
      (set! end-date-field (new date-input-field% [parent p] [label "Completion date: "])))

    (define (on-tracking-choice sel)
      (send tracking-target-panel
            change-children
            (lambda (old)
              (list tracking-type-choice
                    (cond ((equal? 0 sel) target-hours-field)
                          ((equal? 1 sel) target-mileage-field)
                          ((equal? 2 sel) target-cal-date-field)
                          ((equal? 3 sel) target-cal-days-field))))))

    (define (setup-equipment-choice db [selected #f])
      (let ((ids '()) (names '()))
        (for (((id name) (in-query db "
select E.id, ifnull(E.name, E.device_name) as ename
  from EQUIPMENT E
 where e.retired = 0
 order by ename")))
          (set! ids (cons id ids))
          (set! names (cons name names)))
        (set! equipment-ids (reverse ids))
        (send equipment-choice clear)
        (for ((name (in-list (reverse names))))
          (send equipment-choice append name)))
      (select-equipment selected))

    (define (select-equipment eqid)
      (let loop ((idx 0) (ids equipment-ids))
        (unless (null? ids)
          (if (equal? (car ids) eqid)
              (send equipment-choice set-selection idx)
              (loop (+ idx 1) (cdr ids))))))

    (define (setup-for-service-log db svid)
      (let ((row (query-row db "
select equipment_id, name, start_date, service_type, target, end_date
from EQUIPMENT_SERVICE_LOG where id = ?" svid)))
        (select-equipment (vector-ref row 0))
        (send name-text-field set-value (vector-ref row 1))
        (send start-date-field set-date-value (vector-ref row 2))
        (set-target (vector-ref row 3) (vector-ref row 4))
        (if (sql-null? (vector-ref row 5))
            (send end-date-field set-value "")
            (send end-date-field set-date-value (vector-ref row 5)))))

    (define (setup-for-new-service-log)
      ;; NOTE: we leave the tracking type choice untouched for now
      (send name-text-field set-value "")
      (send target-hours-field set-value "")
      (send target-mileage-field set-value "")
      (send target-cal-date-field set-value "")
      (send target-cal-days-field set-value "")
      (send start-date-field set-date-value (current-seconds)))

    (define (get-selected-eqid)
      (let ((sel (send equipment-choice get-selection)))
        (list-ref equipment-ids sel)))

    (define (get-target-value)
      (let ((sel (send tracking-type-choice get-selection)))
        (cond ((equal? 0 sel)
               (let ((v (send target-hours-field get-converted-value)))
                 (if (and v (not (equal? v 'empty))) (* v 3600) #f)))
              ((equal? 1 sel)
               (let ((v (send target-mileage-field get-converted-value)))
                 (if (and v (not (equal? v 'empty))) (* v 1000) #f)))
              ((equal? 2 sel)
               (send target-cal-date-field get-converted-value))
              ((equal? 3 sel)
               (let ((v (send target-cal-days-field get-converted-value)))
                 (if (and v (not (equal? v 'empty)))
                     (+ (current-seconds) (* v 24 3600))
                     #f))))))

    (define (get-target-type)
      (let ((sel (send tracking-type-choice get-selection)))
        (cond ((equal? 0 sel) 0)
              ((equal? 1 sel) 1)
              ((equal? 2 sel) 2)
              ((equal? 3 sel) 2))))

    (define (set-target type value)
      (cond ((equal? 0 type)
             (on-tracking-choice 0)
             (send tracking-type-choice set-selection 0)
             (send target-hours-field set-numeric-value (/ value 3600)))
            ((equal? 1 type)
             (on-tracking-choice 1)
             (send tracking-type-choice set-selection 1)
             (send target-mileage-field set-numeric-value (/ value 1000)))
            ((equal? 2 type)
             (on-tracking-choice 2)
             (send tracking-type-choice set-selection 2)
             (send target-cal-date-field set-date-value value))))

    (define (save-to-database db svid)
      (when (has-valid-data?)
        (let ((eqid (get-selected-eqid))
              (name (string-trim (send name-text-field get-value)))
              (start-date (send start-date-field get-converted-value))
              (end-date (let ((v (send end-date-field get-converted-value)))
                          (if (eq? v 'empty) sql-null v)))
              (target-type (get-target-type))
              (target-value (get-target-value)))
          (if svid
              (begin
                (query-exec db "
update EQUIPMENT_SERVICE_LOG
set equipment_id = ?, name = ?, start_date = ?, service_type = ?, target = ?, end_date = ?
where id = ?" eqid name start-date target-type target-value end-date svid)
                svid)
              (begin
                (query-exec db "
insert into EQUIPMENT_SERVICE_LOG(
  equipment_id, name, start_date, service_type, target, end_date)
values(?, ?, ?, ?, ?, ?)" eqid name start-date target-type target-value end-date)
                (query-value db "select max(id) from EQUIPMENT_SERVICE_LOG"))))))

    (define/override (has-valid-data?)
      (let ((name (string-trim (send name-text-field get-value)))
            (start-date (send start-date-field get-converted-value))
            (end-date (send end-date-field get-converted-value))
            (target-value (get-target-value)))
        (and (not (equal? name "")) start-date target-value end-date)))

    (define/public (begin-edit parent database eqid svid [duplicate? #f])
      (setup-equipment-choice database eqid)
      (if svid
          (setup-for-service-log database svid)
          (setup-for-new-service-log))
      (let ((result (send this do-edit parent)))
        (if result (save-to-database database (if duplicate? #f svid)) #f)))

    ))

(define the-service-editor #f)

(define (get-service-editor)
  (unless the-service-editor
    (set! the-service-editor (new edit-service-log%)))
  the-service-editor)


;;........................................... equipment-operations-menu% ....

(define (delete-equipment db eqid)
  (call-with-transaction
   db
   (lambda ()
     (query-exec db "delete from EQUIPMENT_USE where equipment_id = ?" eqid)
     (query-exec db "delete from EQUIPMENT where id = ?" eqid))))

(define (retire-equipment db eqid)
  (query-exec db "update EQUIPMENT set retired = 1 where id = ?" eqid))

(define (get-equipment-use-count db eqid)
  (or (query-maybe-value db "select use_count from V_EQUIPMENT_USE where equipment_id = ?" eqid) 0))

(define (get-equipment-name db eqid)
  (let ((name (query-maybe-value
               db "select ifnull(name, device_name) from EQUIPMENT where id = ?" eqid)))
    (if (or (not name) (sql-null? name))
        ""
        name)))

(define equipment-operations-menu%
  (class object% (init target) (super-new)

    (define the-target target)

    (define (on-demand m)
      (send the-target before-popup)
      ;; Enable/disable appropiate menus
      (let ((have-eqid? (and the-target (send the-target get-selected-eqid))))
        (send edit-menu-item enable have-eqid?)
        (send new-service-item enable have-eqid?)
        (send delete-menu-item enable have-eqid?)))

    (define (on-popdown m e)
      (send the-target after-popdown))

    (define (on-edit m e)
      (let ((eqid (send the-target get-selected-eqid))
            (db (send the-target get-database))
            (toplevel (send the-target get-top-level-window)))
        ;; NOTE: the menu item should be disabled if there is no eqid
        (unless eqid
          (error "equipment-operations-menu%/on-edit: bad eqid" eqid))
        (when (send (get-equipment-editor) begin-edit toplevel db eqid)
          (send the-target after-update eqid))))

    (define (on-new m e)
      (let ((db (send the-target get-database))
            (toplevel (send the-target get-top-level-window)))
        (let ((eqid (send (get-equipment-editor) begin-edit toplevel db #f)))
          (when eqid
            (send the-target after-new-equipment eqid)))))

    (define (on-new-service m e)
      (let ((eqid (send the-target get-selected-eqid))
            (db (send the-target get-database))
            (toplevel (send the-target get-top-level-window)))
        (let ((svid (send (get-service-editor) begin-edit toplevel db eqid #f)))
          (when svid
            (send the-target after-new-service-log svid)))))

    (define (on-delete m e)
      (let ((eqid (send the-target get-selected-eqid))
            (db (send the-target get-database))
            (toplevel (send the-target get-top-level-window)))
        ;; NOTE: the menu item should be disabled if there is no eqid
        (unless eqid
          (error "equipment-operations-menu%/on-delete: bad eqid" eqid))
        (let ((nuses (get-equipment-use-count db eqid)))
          (cond ((> nuses 0) ; ask for confirmation if the equipment is in use
                 (let* ((name (get-equipment-name db eqid))
                        (mresult (message-box/custom
                                  "Confirm delete"
                                  (format "~a is used in ~a sessions, maybe it should be retired. Really delete?" name nuses)
                                  "Retire" "Delete" "Cancel" toplevel '(caution default=3))))
                   (cond ((equal? mresult 1)
                          (begin
                            (retire-equipment db eqid)
                            (send the-target after-update eqid)))
                         ((equal? mresult 2)
                          (begin
                            (delete-equipment db eqid)
                            (send the-target after-delete eqid))))))
                (#t
                 (delete-equipment db eqid)
                 (send the-target after-delete eqid))))))

    (define the-menu
      (new popup-menu% [title "Equipment"]
           [demand-callback on-demand]
           [popdown-callback on-popdown]))
    (define edit-menu-item
      (new menu-item% [parent the-menu]
           [label "Edit equipment ..."] [callback on-edit]))
    (define new-menu-item
      (new menu-item% [parent the-menu]
           [label "New equipment ..."] [callback on-new]))
    (new separator-menu-item% [parent the-menu])
    (define new-service-item
      (new menu-item% [parent the-menu]
           [label "New service reminder ..."] [callback on-new-service]))
    (new separator-menu-item% [parent the-menu])
    (define delete-menu-item
      (new menu-item% [parent the-menu]
           [label "Delete equipment..."] [callback on-delete]))

    (define/public (get-popup-menu) the-menu)

    ))


;;.................................... service-log-operations-menu% ....

(define (mark-service-log-complete db svid complete?)
  (if complete?
      (query-exec db "
update EQUIPMENT_SERVICE_LOG set end_date = strftime('%s', 'now')
where id = ?" svid)
      (query-exec db "
update EQUIPMENT_SERVICE_LOG set end_date = null
where id = ?" svid)))

(define (delete-service-log db svid)
  (query-exec db "
delete from EQUIPMENT_SERVICE_LOG where id = ?" svid))

(define service-log-operations-menu%
  (class object% (init target) (super-new)

    (define the-target target)

    (define (on-demand m)
      (send the-target before-popup)
      ;; Enable/disable appropiate menus
      (let ((have-svid? (and the-target (send the-target get-selected-svid))))
        (send edit-menu-item enable have-svid?)
        (send duplicate-menu-item enable have-svid?)
        (send mark-complete-menu-item enable have-svid?)
        (send delete-menu-item enable have-svid?)
        (when have-svid?
          (let ((complete? (send target is-selected-svid-complete?)))
            (if complete?
                (send mark-complete-menu-item set-label "Remove completion date...")
                (send mark-complete-menu-item set-label "Mark completed ..."))))))

    (define (on-popdown m e)
      (send the-target after-popdown))

    (define (on-edit m e)
      (let ((svid (send the-target get-selected-svid))
            (db (send the-target get-database))
            (toplevel (send the-target get-top-level-window)))
        (let ((svid (send (get-service-editor) begin-edit toplevel db #f svid)))
          (when svid
            (send the-target after-update svid)))))

    (define (on-duplicate m e)
      (let ((svid (send the-target get-selected-svid))
            (db (send the-target get-database))
            (toplevel (send the-target get-top-level-window)))
        (let ((svid (send (get-service-editor) begin-edit toplevel db #f svid #t)))
          (when svid
            (send the-target after-new-service-log svid)))))

    (define (on-new m e)
      (let ((db (send the-target get-database))
            (toplevel (send the-target get-top-level-window)))
        (let ((svid (send (get-service-editor) begin-edit toplevel db #f #f)))
          (when svid
            (send the-target after-new-service-log svid)))))

    (define (on-mark-complete m e)
      (let ((db (send the-target get-database))
            (svid (send the-target get-selected-svid))
            (complete? (send target is-selected-svid-complete?)))
        (mark-service-log-complete db svid (not complete?))
        (send the-target after-update svid)))

    (define (on-delete m e)
      (let ((svid (send the-target get-selected-svid))
            (db (send the-target get-database))
            (toplevel (send the-target get-top-level-window)))
        (let ((mresult (message-box/custom
                        "Confirm delete"
                        "Really delete service log entry? Maybe it should be marked complete?"
                        "Mark Complete" "Delete" "Cancel" toplevel '(caution default=3))))
          (cond ((equal? mresult 1)
                 (begin
                   (mark-service-log-complete db svid #t)
                   (send the-target after-update svid)))
                ((equal? mresult 2)
                 (begin
                   (delete-service-log db svid)
                   (send the-target after-delete svid)))))))

    (define the-menu
      (new popup-menu% [title "Service reminder"]
           [demand-callback on-demand]
           [popdown-callback on-popdown]))
    (define edit-menu-item
      (new menu-item% [parent the-menu]
           [label "Edit ..."] [callback on-edit]))
    (define duplicate-menu-item
      (new menu-item% [parent the-menu]
           [label "Duplicate ..."] [callback on-duplicate]))
    (define new-menu-item
      (new menu-item% [parent the-menu]
           [label "New ..."] [callback on-new]))
    (new separator-menu-item% [parent the-menu])
    (define mark-complete-menu-item
      (new menu-item% [parent the-menu]
           [label "Mark complete ..."] [callback on-mark-complete]))
    (new separator-menu-item% [parent the-menu])
    (define delete-menu-item
      (new menu-item% [parent the-menu]
           [label "Delete ..."] [callback on-delete]))

    (define/public (get-popup-menu) the-menu)

    ))


;;...................................................... equipment-list% ....

(define (get-equipment-list db include-retired?)
  (query-rows
   db
   "select EQ.id,
       ifnull(EQ.name, ''),
       ifnull(EQ.device_name, '') as devname,
       ifnull(EQ.serial_number, 0) as serial,
       EQ.retired as is_retired,
       (select use_count from V_EQUIPMENT_USE where equipment_id = EQ.id) as use_count,
       (select hours_used from V_EQUIPMENT_USE where equipment_id = EQ.id) as hours_used,
       (select kms_used from V_EQUIPMENT_USE where equipment_id = EQ.id) as kms_used,
       (select first_use from V_EQUIPMENT_USE where equipment_id = EQ.id) as first_use,
       (select last_use from V_EQUIPMENT_USE where equipment_id = EQ.id) as last_use,
       (select ifnull(E1.name, E1.device_name) from EQUIPMENT E1 where E1.id = EQ.part_of),
       (select EV.software_version from EQUIPMENT_VER EV where EV.equipment_id = EQ.id) as software_version,
       (select EV.hardware_version from EQUIPMENT_VER EV where EV.equipment_id = EQ.id) as hardware_version,
       (select EV.battery_voltage from EQUIPMENT_VER EV where EV.equipment_id = EQ.id) as battery_voltage,
       (select EBS.name from EQUIPMENT_VER EV, E_BATTERY_STATUS EBS where EV.equipment_id = EQ.id and EV.battery_status = EBS.id) as battery_status
  from EQUIPMENT EQ
  where retired < ?" (if include-retired? 2 1)))

;; NOTE: columns in this query must match the ones returned by get-equipment-list
(define (get-equipment-1 db eqid)
  (query-row
   db
   "select EQ.id,
       ifnull(EQ.name, ''),
       ifnull(EQ.device_name, '') as devname,
       ifnull(EQ.serial_number, 0) as serial,
       EQ.retired as is_retired,
       (select use_count from V_EQUIPMENT_USE where equipment_id = EQ.id) as use_count,
       (select hours_used from V_EQUIPMENT_USE where equipment_id = EQ.id) as hours_used,
       (select kms_used from V_EQUIPMENT_USE where equipment_id = EQ.id) as kms_used,
       (select first_use from V_EQUIPMENT_USE where equipment_id = EQ.id) as first_use,
       (select last_use from V_EQUIPMENT_USE where equipment_id = EQ.id) as last_use,
       (select ifnull(E1.name, E1.device_name) from EQUIPMENT E1 where E1.id = EQ.part_of),
       (select EV.software_version from EQUIPMENT_VER EV where EV.equipment_id = EQ.id) as software_version,
       (select EV.hardware_version from EQUIPMENT_VER EV where EV.equipment_id = EQ.id) as hardware_version,
       (select EV.battery_voltage from EQUIPMENT_VER EV where EV.equipment_id = EQ.id) as battery_voltage,
       (select EBS.name from EQUIPMENT_VER EV, E_BATTERY_STATUS EBS where EV.equipment_id = EQ.id and EV.battery_status = EBS.id) as battery_status
  from EQUIPMENT EQ
where EQ.id = ?" eqid))

(define *equipment-display-columns*
  (list
   (let ((fn (lambda (row) (vector-ref row 1))))
     (qcolumn "Name" fn fn))
   (let ((fn (lambda (row) (vector-ref row 2))))
     (qcolumn "Device Name" fn fn))
   (let ((fn (lambda (row) (vector-ref row 3))))
     (qcolumn "Serial Number"
              (lambda (row)
                (let ((sn (fn row)))
                  (if (zero? sn) "" (number->string (fn row)))))
              fn))
   (let ((fn (lambda (row) (sql-column-ref row 4 0))))
     (qcolumn "Retired?"
              (lambda (row) (if (equal? (fn row) 1) "Yes" ""))
              fn))
   (let ((fn (lambda (row) (sql-column-ref row 10 ""))))
     (qcolumn "Part of" fn fn))
   (let ((fn (lambda (row) (sql-column-ref row 5 0))))
     (qcolumn "Use Count"
              (lambda (row) (number->string (fn row)))
              fn))
   (let ((fn (lambda (row) (sql-column-ref row 6 0))))
     (qcolumn "Hours Used"
              (lambda (row) (duration->string (fn row)))
              fn))
   (let ((fn (lambda (row) (sql-column-ref row 7 0))))
     (qcolumn "Total Distance"
              (lambda (row) (distance->string (fn row) #t))
              fn))
   (let ((fn (lambda (row) (sql-column-ref row 8 #f))))
     (qcolumn "First Use"
              (lambda (row)
                (let ((v (fn row)))
                  (if v (date-time->string v) "")))
              (lambda (row) (or (fn row) 0))))
   (let ((fn (lambda (row) (sql-column-ref row 9 #f))))
     (qcolumn "Last Use"
              (lambda (row)
                (let ((v (fn row)))
                  (if v (date-time->string v) "")))
              (lambda (row) (or (fn row) 0))))
   (let ((fn (lambda (row) (sql-column-ref row 11))))
     (qcolumn "Software Version"
              (lambda (row)
                (let ((v (fn row)))
                  (if v (format "~a" v) "")))
              (lambda (row) (or (fn row) ""))))
   (let ((fn (lambda (row) (sql-column-ref row 12))))
     (qcolumn "Hardware Version"
              (lambda (row)
                (let ((v (fn row)))
                  (if v (format "~a" v) "")))
              (lambda (row) (or (fn row) ""))))
   (let ((fn (lambda (row) (sql-column-ref row 13))))
     (qcolumn "Battery Voltage"
              (lambda (row)
                (let ((v (fn row)))
                  (if v (format-48 "~1,2F" v) "")))
              (lambda (row) (or (fn row) ""))))
   (let ((fn (lambda (row) (sql-column-ref row 14))))
     (qcolumn "Battery Status"
              (lambda (row)
                (let ((v (fn row)))
                  (if v (format "~a" v) "")))
              (lambda (row) (or (fn row) ""))))
   ))


;;.................................................... service reminders ....

(define (get-service-log-list db what-to-show)
  (query-rows
   db
   (format
    "select ESL.id,
       EQ.id,
       ifnull(EQ.name, EQ.device_name),
       ESL.start_date,
       ESL.end_date,
       ESL.name,
       ESL.service_type,
       ESL.target as target,
       VESL.current as current,
       ESL.target - VESL.current as remaining
from EQUIPMENT EQ, EQUIPMENT_SERVICE_LOG ESL, V_EQUIPMENT_SLOG_CURRENT VESL
 where ESL.equipment_id = EQ.id
   and VESL.service_log_id = ESL.id ~a"
    (cond ((eq? what-to-show 'all) "")
          ((eq? what-to-show 'active) "and ESL.end_date is null")
          ((eq? what-to-show 'due) "and ESL.end_date is null and (VESL.current / ESL.target) > 0.9")
          (#t "")))))

(define (get-service-log-1 db svid)
  (query-row
   db
   "select ESL.id,
       EQ.id,
       ifnull(EQ.name, EQ.device_name),
       ESL.start_date,
       ESL.end_date,
       ESL.name,
       ESL.service_type,
       ESL.target as target,
       VESL.current as current,
       ESL.target - VESL.current as remaining
from EQUIPMENT EQ, EQUIPMENT_SERVICE_LOG ESL, V_EQUIPMENT_SLOG_CURRENT VESL
 where ESL.equipment_id = EQ.id
   and VESL.service_log_id = ESL.id
   and ESL.id = ?" svid))

(define (get-low-battery-devices db)
  (query-rows
   db
   "select ifnull(EQ.name, EQ.device_name) as name,
           EV.battery_status as status,
           EV.battery_voltage as voltage,
           EBS.name as status_name
      from EQUIPMENT EQ, EQUIPMENT_VER EV, E_BATTERY_STATUS EBS
     where EV.equipment_id = EQ.id and EV.battery_status = EBS.id
       and EV.battery_status in (4, 5)"))

(define *service-log-display-columns*
  (list
   (let ((fn (lambda (row) (vector-ref row 2))))
     (qcolumn "Equipment" fn fn))
   (let ((fn (lambda (row) (vector-ref row 5))))
     (qcolumn "Description" fn fn))
   (let ((fn (lambda (row) (sql-column-ref row 3  #f))))
     (qcolumn "Start date"
              (lambda (row)
                (let ((v (fn row)))
                  (if v (calendar-date->string v) "")))
              (lambda (row) (or (fn row) 0))))
   (let ((fn (lambda (row) (sql-column-ref row 4  #f))))
     (qcolumn "Completion date"
              (lambda (row)
                (let ((v (fn row)))
                  (if v (calendar-date->string v) "")))
              (lambda (row) (or (fn row) 0))))
   (let ((fn (lambda (row)
               (let ((v (vector-ref row 6)))
                 (cond ((equal? v 0) "Hours used")
                       ((equal? v 1) "Mileage")
                       ((equal? v 2) "Calendar days")
                       (#t (format "Unknown ~a" v)))))))
     (qcolumn "Tracking" fn fn))
   (let ((fn (lambda (row) (sql-column-ref row 7 #f))))
     (qcolumn "Target"
              (lambda (row)
                (let ((t (vector-ref row 6))
                      (v (fn row)))
                  (cond ((eq? v #f) "")
                        ((equal? t 0) (duration->string v))
                        ((equal? t 1) (distance->string v #t))
                        ((equal? v 2) (format "~a days" v))
                        (#t (format "~a" v)))))
              fn))
   (let ((fn (lambda (row) (sql-column-ref row 8 #f))))
     (qcolumn "Current"
              (lambda (row)
                (let ((t (vector-ref row 6))
                      (v (fn row)))
                  (cond ((eq? v #f) "")
                        ((equal? t 0) (duration->string v))
                        ((equal? t 1) (distance->string v #t))
                        ((equal? v 2) (format "~a days" v))
                        (#t (format "~a" v)))))
              fn))
   (let ((fn (lambda (row) (sql-column-ref row 9 #f))))
     (qcolumn "Remaining"
              (lambda (row)
                (let ((t (vector-ref row 6))
                      (v (fn row)))
                  ;; don't dispay a remaining field if this is now overdue
                  (cond ((or (eq? v #f) (< v 0)) "")
                        ((equal? t 0) (duration->string v))
                        ((equal? t 1) (distance->string v #t))
                        ((equal? v 2) (format "~a days" v))
                        (#t (format "~a" v)))))
              fn))
   (let* ((target (lambda (row) (sql-column-ref row 7 #f)))
          (current (lambda (row) (sql-column-ref row 8 #f)))
          (fn (lambda (row)
                (let ((t (target row))
                      (c (current row)))
                  (if (and t c (> t 0)) (/ c t) 0)))))
     (qcolumn "Percent Complete"
              (lambda (row)
                (format-48 "~1,1F%" (* (fn row) 100)))
              fn))
   ))


(define view-equipment%
  (class object%
    (init parent database)
    (super-new)

    (define tag 'activity-log:equipment-list-vl)
    (define show-retired-equipment? #f)
    (define show-service-reminders 'active)

    (let ((prefs (get-pref tag (lambda () (list #f 'active)))))
      (set! show-retired-equipment? (list-ref prefs 0))
      (when (> (length prefs) 1)
        (set! show-service-reminders (list-ref prefs 1))))

    (define the-database database)

    ;; Equipment-operations-menu% target interface
    (define eqop-target
      (new (class object% (init) (super-new)
             (define selected-row-index #f)

             (define/public (before-popup)
               (set! selected-row-index (send lb get-selected-row-index)))
             (define/public (after-popdown)
               (set! selected-row-index #f))
             (define/public (get-selected-eqid)
               (if selected-row-index
                   (let ((data (send lb get-data-for-row selected-row-index)))
                     (if data (vector-ref data 0) #f))
                   #f))
             (define/public (after-update eqid)
               (when selected-row-index
                 (let ((new-data (get-equipment-1 the-database eqid)))
                   (send lb update-row selected-row-index new-data))))
             (define/public (after-new-equipment eqid)
               (let ((new-data (get-equipment-1 the-database eqid)))
                 (send lb add-row new-data)))
             (define/public (after-new-service-log svid)
               (let ((new-data (get-service-log-1 the-database svid)))
                 (send service-log-lb add-row new-data)))
             (define/public (after-delete eqid)
               (when selected-row-index
                 (send lb delete-row selected-row-index)))
             (define/public (get-top-level-window)
               (send pane get-top-level-window))
             (define/public (get-database)
               the-database))))

    (define svop-target
      (new (class object% (init) (super-new)
             (define selected-row-index #f)

             (define/public (before-popup)
               (set! selected-row-index (send service-log-lb get-selected-row-index)))
             (define/public (after-popdown)
               (set! selected-row-index #f))
             (define/public (get-selected-svid)
               (if selected-row-index
                   (let ((data (send service-log-lb get-data-for-row selected-row-index)))
                     (if data (vector-ref data 0) #f))
                   #f))
             (define/public (is-selected-svid-complete?)
               (if selected-row-index
                   (let ((data (send service-log-lb get-data-for-row selected-row-index)))
                     (not (sql-null? (vector-ref data 4))))
                   #f))
             (define/public (after-update svid)
               (when selected-row-index
                 (let ((new-data (get-service-log-1 the-database svid)))
                   (send service-log-lb update-row selected-row-index new-data))))
             (define/public (after-new-service-log svid)
               (let ((new-data (get-service-log-1 the-database svid)))
                 (send service-log-lb add-row new-data)))
             (define/public (after-delete svid)
               (when selected-row-index
                 (send service-log-lb delete-row selected-row-index)))
             (define/public (get-top-level-window)
               (send pane get-top-level-window))
             (define/public (get-database)
               the-database))))

    (define pane (new vertical-pane% [parent parent]))

    (let ((sel-pane (new horizontal-pane% [parent pane]
                         [spacing 20]
                         [border 0]
                         [stretchable-height #f]
                         [stretchable-width #t]
                         [alignment '(left center)])))
      (make-spacer sel-pane)
      (new message% [parent sel-pane] [label (equipment-icon)]))

    (let ((p (new horizontal-pane% [parent pane] 
                  [border 0] [spacing 20] 
                  [alignment '(left center)]
                  [stretchable-height #f])))
      (make-spacer p)
      (new message%
           [parent p]
           [stretchable-height #f]
           [stretchable-width #f]
           [font (send the-font-list find-or-create-font 16 'default 'normal 'normal)]
           [label "Equipment"])
      (new check-box% [parent p]
           [value show-retired-equipment?]
           [label "Show retired"]
           [stretchable-width #f]
           [callback (lambda (b e)
                       (set! show-retired-equipment? (not show-retired-equipment?))
                       (on-filter-changed))]))

    (define lb (new qresults-list% [parent pane]
                    [pref-tag 'activity-log:equipment-list]
                    [right-click-menu
                     (send (new equipment-operations-menu% [target eqop-target]) get-popup-menu)]))

    (send lb set-default-export-file-name "equipment.csv")

    (let ((p (new horizontal-pane% [parent pane] 
                  [border 0] [spacing 20] 
                  [alignment '(left center)]
                  [stretchable-height #f])))
      (make-spacer p)
      (new message%
           [parent p]
           [stretchable-height #f]
           [stretchable-width #f]
           [font (send the-font-list find-or-create-font 16 'default 'normal 'normal)]
           [label "Service Reminders"])
      (new choice% [parent p]
           [stretchable-width #f]
           [label "Show: "]
           [choices '("Active" "Due" "All")]
           [selection (case show-service-reminders
                        ('active 0)
                        ('due 1)
                        ('all 2))]
           [callback (lambda (c e)
                       (let ((sel (send c get-selection)))
                         (set! show-service-reminders (list-ref '(active due all) sel)))
                       (on-filter-changed))])
      )

    (define service-log-lb
      (new qresults-list% [parent pane]
           [pref-tag 'activity-log:service-log]
           [right-click-menu
            (send (new service-log-operations-menu% [target svop-target]) get-popup-menu)]))

    (send service-log-lb set-default-export-file-name "service-log.csv")

    (define (on-filter-changed)
      (refresh))

    (define first-time? #t)
    (define/public (activated)
      (when first-time?
        (send lb setup-column-defs *equipment-display-columns*)
        (send service-log-lb setup-column-defs *service-log-display-columns*)
        (set! first-time? #f))
      (refresh))

    (define/public (refresh)
      (send lb set-data (get-equipment-list the-database show-retired-equipment?))
      (send service-log-lb set-data (get-service-log-list the-database show-service-reminders)))

    (define/public (save-visual-layout)
      (send lb save-visual-layout)
      (send service-log-lb save-visual-layout)
      (put-pref tag (list show-retired-equipment? show-service-reminders)))

    (define/public (log-due-items)
      (let ((items (get-service-log-list the-database 'due)))
        (for ((i (in-list items)))
          (let ((equipment (vector-ref i 2))
                (description (vector-ref i 5)))
            (notify-user 'info "~a: ~a" equipment description))))
      (for ([item (get-low-battery-devices the-database)])
        (match-define (vector name status voltage status-name) item)
        (define msg (format-48 "~a: battery status is ~a (~1,2F V)" name status-name voltage))
        (notify-user (if (> status 4) 'error 'warning) msg)))
    ))
