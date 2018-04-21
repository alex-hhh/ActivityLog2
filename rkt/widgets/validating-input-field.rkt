#lang racket/base
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2018 Alex Harsanyi <AlexHarsanyi@gmail.com>
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

(require racket/gui/base racket/class racket/string)
(provide validating-input-field%)

;; An input field that allows validating its input and converting it to
;; different types.  For example, can be used to read numbers or dates.  You
;; usually want to derive a class from this, for more convenience, but the
;; class can also be used directly.
(define validating-input-field%
  (class text-field%
    (init-field
     ;; Function to validate the text input (-> text (or/c #t #f)).  If the
     ;; function returns #f, the field is invalid.
     validate-fn
     ;; Function to convert a text value into the actual data type entered
     ;; into this field (e.g. to convert the time string "4:00" to 240
     ;; seconds).
     convert-fn
     ;; Callback called when a new valid value is entered in the field.
     [valid-value-cb #f]
     ;; Text to be displayed in the input field when it is empty, can be used
     ;; to provide hints on what valid values look like (e.g for entering
     ;; durations, the cue-text might be "hh:mm:ss")
     [cue-text ""]
     ;; allow-empty? means an empty field is valid.
     [allow-empty? #t]
     [label ""])
    (super-new [label label])

    (inherit get-field-background set-field-background get-editor)

    (define (vfn data)
      (let ((t (string-trim data)))
        (cond ((and (not allow-empty?) (= (string-length t) 0)) ;; Not valid
               #f)
              (#t
               (validate-fn t)))))

    (define old-value #f)
    (define showing-cue? #f)
    (define original-value #f)

    ;; When set to #t, mark this field as invalid, regardless of the VFN
    ;; result, until the next edit is performed.
    (define global-invalid #f)

    ;; The background of the field will change to `bad-bg` if the value it
    ;; contains is invalid.
    (define good-bg (get-field-background))
    (define bad-bg (make-object color% 255 120 124)) ; red

    ;; Text style for cue text
    (define cue-fg
      (let ((grey-text (new style-delta%)))
        (send grey-text set-delta-foreground "gray")
        grey-text))

    ;; Text style for normal text
    (define text-fg
      (let ((black-text (new style-delta%)))
        (send black-text set-delta-foreground "black")
        black-text))

    (define (clear-cue-text)
      (when showing-cue?
        (let ([editor (get-editor)])
          (send editor erase)
          (send editor clear-undos)
          (send editor change-style text-fg 'start 'end #f)
          (set! showing-cue? #f))))

    ;; Insert the cue text if the input field is empty
    (define (maybe-insert-cue-text)
      (unless (or showing-cue? (> (string-length (super get-value)) 0))
        (let ([editor (get-editor)])
          (send editor change-style cue-fg 'start 'end #f)
          (send editor insert cue-text)
          (set! showing-cue? #t))))

    ;; Validate the contents and, if valid, setup a 500ms timer for the
    ;; valid-value-cb (unless report-valid-value? is #f).  The timer is used
    ;; so that we don't fire a sequence of valid values when the user is
    ;; typing.
    ;;
    ;; Note that this is public and it allows the user to write validation
    ;; functions that depend on some external factors.
    (define/public (validate [report-valid-value? #t])
      (let* ((value (send this get-value))
             (valid? (and (vfn value) (not global-invalid))))
        (set-field-background (if valid? good-bg bad-bg))
        (if (and report-valid-value? valid?)
            (send cb-timer start 500 #t)
            (send cb-timer stop))))

    ;; Mark this field as unconditionally invalid or valid (e.g. if the
    ;; contents of this field are invalid due to some external factors).  If
    ;; FLAG is #t the field is marked invalid, if flag is #f the field is
    ;; maker valid or not depending on the result of the validate-fn
    ;;
    ;; This can be used, for example, to link two fields together, where the
    ;; value in the first field must always be "less" than the value in the
    ;; second.
    (define/public (mark-valid flag)
      (set! global-invalid (not flag))
      (validate))

    ;; The user is notified of a valid value via a timer.  This prevents the
    ;; callback to fire multiple times as the user enters the value (e.g, when
    ;; the user types "123", we don't want to fire the callback with 1, 12 and
    ;; 123)
    (define cb-timer
      (new timer% [notify-callback
                   (lambda ()
                     (let ((value (send this get-value)))
                       (unless (equal? old-value value)
                         (when valid-value-cb
                           (valid-value-cb (convert-fn value)))
                         (set! old-value value))))]))

    (define/override (on-focus on?)
      (if on? (clear-cue-text) (maybe-insert-cue-text))
      (super on-focus on?))

    (define/override (on-subwindow-char receiver event)
      ;; Let the parent handle the on-subwindow-char, after it is finished, we
      ;; validate the contents.
      (let ((result (super on-subwindow-char receiver event)))
        (set! global-invalid #f)
        (validate)
        result))

    (define/override (set-value v)
      (clear-cue-text)
      (super set-value v)
      (set! original-value v)
      (validate #f)         ; dont report valid values for externally set data
      (maybe-insert-cue-text))

    (define/override (get-value)
      (if showing-cue? "" (super get-value)))

    (define/public (has-valid-value?)
      ;; NOTE: the empty value might be valid, let vfn decide
      (vfn (send this get-value)))

    (define/public (has-changed?)
      (let ((new-value (send this get-value)))
        (not (string=? new-value original-value))))

    (define/public (get-converted-value)
      (let ((v (send this get-value)))
        (if (vfn v) (convert-fn v) #f)))

    (define/public (set-cue-text text)
      (set! cue-text text)
      (maybe-insert-cue-text))

    (validate #f)
    (maybe-insert-cue-text)))
