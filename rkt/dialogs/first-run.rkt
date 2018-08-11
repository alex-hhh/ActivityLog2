#lang racket/base
;; first-run.rkt -- dialog displayed when the applicaiton is first run
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
         racket/math
         racket/runtime-path
         "../utilities.rkt"
         "../dbapp.rkt")

(provide first-run-dialog%)

(define-runtime-path swimming-icon-file "../../img/swimming-64.png")
(define-runtime-path biking-icon-file "../../img/regular_biking-64.png")
(define-runtime-path running-icon-file "../../img/running-64.png")

;; Display a dialog when the application is run for the first time, presenting
;; the user with the opportunity to create a new database, open an existing
;; one or exit the application.
(define first-run-dialog%
  (class object% (init) (super-new)

    (define button-bar #f)
    (define progress-bar #f)
    (define can-exit? #t)
    (define dialog-result #f)

    (define toplevel
      (new (class dialog% (init) (super-new)
             (define/augment (can-close?) can-exit?)
             (define/augment (on-close) (on-cancel)))
           [label "Create/Open Activity Log Database"]
           [stretchable-width #f]
           [stretchable-height #f]
           [parent #f]))

    (let ((p (new vertical-pane% [parent toplevel]
                  [spacing 10] [border 10]
                  [alignment '(center top)])))
      (let ((icon-bar (new horizontal-pane% [parent p]
                           [spacing 10]
                           [alignment '(center center)])))
        (new message% [parent icon-bar]
             [label (read-bitmap swimming-icon-file)])
        (new message% [parent icon-bar]
             [label (read-bitmap biking-icon-file)])
        (new message% [parent icon-bar]
             [label (read-bitmap running-icon-file)]))

      (set! button-bar (new horizontal-pane% [parent p]
                            [spacing 20]
                            [min-width 400] [stretchable-width #f]
                            [alignment '(center center)]))

      (new button% [parent button-bar]
           [label "New database"]
           [callback (lambda (b e) (on-create-new-database))])

      (new button% [parent button-bar]
           [label "Open existing ..."]
           [callback (lambda (b e) (on-open-database))])

      (new button% [parent button-bar]
           [label "Exit"]
           [callback (lambda (b e) (on-cancel))]))

    (define (get-default-dir)
      (data-directory))

    (define (on-create-new-database)
      (set! can-exit? #f)
      ;; Replace the buttons with a progress bar
      (set! progress-bar
            (new gauge% [parent button-bar]
                 [label ""] [range 100] [style '(horizontal deleted)]))
      (send button-bar change-children (lambda (old) (list progress-bar)))

      (define (progress-callback msg crt max)
        (when (and crt max)
          (let ((completed (exact-round (* 100 (/ crt max)))))
            (send progress-bar set-value completed))))
      
      (let ((db-file (build-path (get-default-dir) "ActivityLog.db")))
        (when (file-exists? db-file)
          (let loop ((try 1))
            (let ((file (build-path (get-default-dir) (format "ActivityLog-~a.db"))))
              (if (file-exists? file)
                  (loop (+ try 1))
                  (set! db-file file)))))
        (thread (lambda ()
                  (let ((db (open-activity-log db-file progress-callback)))
                    (queue-callback
                     (lambda ()
                       (set! dialog-result db-file)
                       ;; Disconnect the database, we only wanted to create it.
                       (disconnect db)
                       (set! can-exit? #t)
                       (send toplevel show #f))))))))

    (define (on-open-database)
      (let ((file (get-file "Open database..." toplevel
                            (get-default-dir))))
        (when (and file (file-exists? file))
          (set! dialog-result file)
          (send toplevel show #f))))

    (define (on-cancel)
      (when can-exit?             ; we cannot exit while a db is being created
        (send toplevel show #f)))

    (define/public (run)
      (set! dialog-result #f)
      (send toplevel show #t)
      dialog-result)

    ))
