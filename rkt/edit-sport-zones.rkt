#lang racket/base
;; edit-sport-zones.rkt -- the sport zone editor dialog box
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

(require db
         racket/class
         racket/gui/base
         racket/list
         racket/string
         "fmt-util.rkt"
         "icon-resources.rkt"
         "sport-charms.rkt"
         "widgets.rkt")

(provide get-sport-zone-editor)


;;................................................................. zdef ....

;; Contains definition for each zone field, depending on the zone metric: the
;; cue text, the function to convert the input value into a number and the
;; function to convert a number into the value displayed.
(struct zdef (cue-text str->val val->str))

;; Zone field definitions for different zones
(define heart-rate-zdef (zdef "bpm" string->number number->string))
(define speed-zdef (zdef "speed" string->number number->string))
(define power-zdef (zdef "power" string->number number->string))
(define run-pace-zdef (zdef "mm:ss / km" run-pace-string->mps pace->string))
(define swim-pace-zdef (zdef "mm:ss / 100m" swim-pace-string->mps swim-pace->string))

;; The types of zones we can edit for each sport and zone definitions for each
;; (see zdef).  NOTE: we only support editing zone data for a specific set of
;; sports and metrics.
(define sport-zone-data
  (list 
   ;; NAME, SPORT, SUB-SPORT, LIST-OF ZONE-METRIC
   (list "Running" 1 #f `(("Heart Rate" 1 ,heart-rate-zdef) ("Pace" 2 ,run-pace-zdef)))
   (list "Cycling" 2 #f `(("Heart Rate" 1 ,heart-rate-zdef) ("Speed" 2 ,speed-zdef) ("Power" 3 ,power-zdef)))
   (list "Swimming" 5 #f `(("Pace" 2 ,swim-pace-zdef)))))


;;............................................. edit-sport-zones-dialog% ....


(define edit-sport-zones-dialog%
  (class al-edit-dialog%
    (init)
    (super-new [title "Edit sport zones"] [icon (edit-icon)])

    (define sport-choice #f)
    (define zmetric-choice #f)
    (define zcount-input #f)
    (define zinput-panel #f)
    (define zinputs '())                ; list of zone input text widgets

    (define sport-index #f)
    (define sport #f)
    (define sub-sport #f)
    (define zmetric #f)
    (define zdefinition #f)          ; a zdef strucure for the current zmetric

    ;; list of valid zone definitions for the current sport/sub-sport
    ;; selection
    (define zone-data #f)
    
    (let ((p (send this get-client-pane)))
      (let ((p1 (new vertical-pane% 
                     [parent p] 
                     [spacing 10]
                     [alignment '(center top)])))
        (let ((p2 (make-horizontal-pane p1 #f)))
          (send p2 set-alignment 'center 'center)
          (set! sport-choice 
                (new choice% [parent p2]
                     [label "Sport "] [choices (map first sport-zone-data)]
                     [callback (lambda (c e) (on-sport-selected (send c get-selection)))]))
          (set! zmetric-choice
                (new choice% [parent p2]
                     [label "Zone type "] [choices '("Speed/Pace" "Heart Rate" "Power")]
                     [callback (lambda (c e) (on-zone-metric-selected (send c get-selection)))])))

        (let ((p3 (make-horizontal-pane p1 #f)))
          (send p3 stretchable-width #f)
          (set! zcount-input
                (new number-input-field% [parent p3] 
                     [label "Number of zones "]
                     [min-value 0] [max-value 10]
                     [valid-value-cb (lambda (v) (unless (eq? v 'empty)
                                                   (on-zone-count-changed v)))])))

        (let ((p4 (new vertical-pane% [parent p1])))
          (set! zinput-panel
                (new vertical-panel% [parent p4] [spacing 10] [border 10]
                     [stretchable-width #f] [alignment '(right center)])))))

    ;; Message to display when no zones are defined
    (define no-zones-message 
      (new message% 
           [label "No zones are defined"] [parent zinput-panel] [style '(deleted)]))

    (define (on-sport-selected index (zone-metric-index 0))
      (set! sport-index index)
      (let ((d (list-ref sport-zone-data index)))
        (set! sport (second d))
        (set! sub-sport (third d))
        (set! zone-data (fourth d)))
      (send zmetric-choice clear)
      (for ((x (in-list zone-data)))
        (send zmetric-choice append (first x)))
      (send zmetric-choice set-selection 0)
      (on-zone-metric-selected zone-metric-index))

    (define (on-zone-metric-selected index)
      (let ((d (list-ref zone-data index)))
        (set! zmetric (second d))
        (set! zdefinition (third d)))
      (let ((zones (get-sport-zones sport sub-sport zmetric)))
        (setup-zones (or zones '()))))

    ;; Make a zone input field with LABEL.
    (define (make-zinput label)
      (new validating-input-field% [parent zinput-panel] 
           [label label] [style '(single deleted)]
           [min-width 100] [stretchable-width #f]
           [cue-text (zdef-cue-text zdefinition)]
           [validate-fn (lambda (v) 
                          (or (= (string-length (string-trim v)) 0)
                              ((zdef-str->val zdefinition) v)))]
           [convert-fn (lambda (v) 
                         (if (= (string-length (string-trim v)) 0)
                             'empty
                             ((zdef-str->val zdefinition) v)))]))

    ;; Update the number of zone input fields to NEW-COUNT.  This is done by
    ;; either removing or adding new fields, as needed.  Note that NEW-COUNT
    ;; does not include the MIN and MAX input fields, as such (length zinputs)
    ;; will always be (+ NEW-COUNT 2) after this function runs (unless
    ;; NEW-COUNT is 0, in which case the MIN/MAX fields are removed as well).
    (define (on-zone-count-changed new-count)
      (let ((old-count (max 0 (- (length zinputs) 2))))
        (cond ((= old-count new-count) #t) ; nothing to do
              ((> old-count new-count)
               ;; Remove some inputs.  If NEW-COUNT is 0, remove min/max
               ;; values as well, and display the "No zones defined message.
               (if (= new-count 0)
                   (set! zinputs '())
                   (set! zinputs
                         (append
                          (drop-right zinputs (+ 1 (- old-count new-count)))
                          (take-right zinputs 1))))
               (send zinput-panel change-children 
                     (lambda (old) 
                       (if (> new-count 0) zinputs (list no-zones-message)))))
              ((< old-count new-count)
               ;; Add some inputs. If OLD-COUNT is 0, add the min/max fields
               ;; as well.
               (when (= old-count 0)
                 (set! zinputs (list (make-zinput "Min") (make-zinput "Max"))))
               (set! zinputs
                     (append
                      (drop-right zinputs 1)
                      (for/list ((i (in-range old-count new-count)))
                        (make-zinput (format "Zone ~a" (+ 1 i))))
                      (take-right zinputs 1)))
               (send zinput-panel change-children (lambda (old) zinputs))))))

    (define (setup-zones zones)
      (let ((nzones (max 0 (- (length zones) 2))))
        (send zcount-input set-value (format "~a" nzones))
        (on-zone-count-changed nzones)
        (for ((zval (in-list zones))
              (zinput (in-list zinputs)))
          (send zinput set-cue-text (zdef-cue-text zdefinition))
          (send zinput set-value ((zdef-val->str zdefinition) zval)))))

    ;; Return the zone definition from the values in the zone input fields.
    (define (collect-zones)
      (for/list ((input (in-list zinputs)))
        (send input get-converted-value)))

    ;; Return true if ZONES are valid, they must contain only numbers and be
    ;; in ascending order.
    (define (valid-zones? zones)
      (if (> (length zones) 0)
          (andmap (lambda (a b) (and (number? a) (number? b) (< a b)))
                  (drop-right zones 1) (cdr zones))
          #t))

    ;; Return a list of invalid fields.  These are fields that either contain
    ;; an invalid value, or would define non-ascening zones w.r.t their
    ;; neighbouring fields.
    (define (collect-invalid-fields)
      (define invalid-zinputs '())
      (when (> (length zinputs) 0)
        (for ((a (drop-right zinputs 1))
              (b (cdr zinputs)))
          (let ((v1 (send a get-converted-value))
                (v2 (send b get-converted-value)))
            ;; NOTE: don't mark 'empty filds as invalid
            (when (eq? v1 #f) (set! invalid-zinputs (cons a invalid-zinputs)))
            (when (eq? v2 #f) (set! invalid-zinputs (cons b invalid-zinputs)))
            (when (and (number? v1) (number? v2))
              (when (> v1 v2)
                (set! invalid-zinputs (cons a invalid-zinputs)))))))
      invalid-zinputs)

    (define/override (has-valid-data?)
      (define invalid-zinputs (collect-invalid-fields))
      
      (for ((f (in-list zinputs))) 
        (send f mark-valid (not (member f invalid-zinputs))))
      
      ;; NOTE: the presence of INVALID-ZINPUTS indicates a problem, but the
      ;; absence does not mean all is fine, as we don't mark fields with empty
      ;; values as invalid, yet we don't want to make the entire dialog valid
      ;; if such fields exist.
      (and (= (length invalid-zinputs) 0)
           (valid-zones? (collect-zones))))

    (define/public (show-dialog parent (initial-sport #f) (initial-sub-sport #f) (initial-zone-metric #f))

      (define zone-metric-index 0)
      
      (when initial-sport
        (set! sport-index
              (for/first ([zd sport-zone-data]
                          [index (in-range 0 100)]
                          #:when (and (equal? (second zd) initial-sport)
                                      (equal? (third zd) initial-sub-sport)))
                index)))

      (when initial-zone-metric
        (set! zone-metric-index
              (for/first ([zm (fourth (list-ref sport-zone-data sport-index))]
                          [index (in-range 0 100)]
                          #:when (equal? initial-zone-metric (second zm)))
                index)))

      ;; Re-select last sport (will cause zone values to be read from the
      ;; database.
      (send sport-choice set-selection sport-index)
      (on-sport-selected sport-index zone-metric-index)
      
      (when (send this do-edit parent)
        (put-sport-zones sport sub-sport zmetric (collect-zones))))

    (send sport-choice set-selection 0)
    (on-sport-selected 0)             ; setup the first sport

    ))

(define the-sport-zone-editor #f)

(define (get-sport-zone-editor)
  (unless the-sport-zone-editor
    (set! the-sport-zone-editor (new edit-sport-zones-dialog%)))
  the-sport-zone-editor)

