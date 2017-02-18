#lang racket/base
;; view-trends.rkt -- trends graphs
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

(require
 racket/class
 racket/gui/base
 racket/match
 racket/list
 "al-prefs.rkt"
 "widgets.rkt"
 "dbglog.rkt"
 "snip-canvas.rkt"
 "icon-resources.rkt"
 "trends-chart.rkt"
 "trends-bw.rkt"
 "trends-vol.rkt"
 "trends-trivol.rkt"
 "trends-tiz.rkt"
 "trends-pmc.rkt"
 "trends-tt.rkt"
 "trends-bavg.rkt"
 "trends-hist.rkt")

(provide view-trends%)


;;......................................................... view-trends% ....

(struct chart-info (name tag class))

(define chart-types
  (list
   (chart-info "Body Weight" 'bw bw-trends-chart%)
   (chart-info "Traning Volume (multisport)" 'trivol trivol-trends-chart%)
   (chart-info "Traning Volume" 'vol vol-trends-chart%)
   (chart-info "Time in Zone" 'tiz tiz-trends-chart%)
   (chart-info "Performance" 'pmc pmc-trends-chart%)
   (chart-info "Training Times" 'tt tt-trends-chart%)
   (chart-info "Best Avg" 'bavg bavg-trends-chart%)
   (chart-info "Histogram" 'hist hist-trends-chart%)))

(define new-trend-chart-dialog%
  (class al-edit-dialog%
    (super-new [title "New Chart"]
               [icon reports-icon]
               [save-button-name "Select"]
               [min-height 10])

    (define chart-choice #f)

    (let ((p (send this get-client-pane)))
      (let ((p0 (make-horizontal-pane p #f)))
        (send p0 spacing 10)
        (set! chart-choice (new choice%
                                [parent p0] [label "Chart Type "]
                                [choices (map chart-info-name chart-types)]))))

    (define/public (show-dialog parent)
      (if (send this do-edit parent)
          (list-ref chart-types (send chart-choice get-selection))
          #f))))

(define trend-chart-pane%
  (class panel%
    (init-field parent info-tag trend-chart)
    (super-new [parent parent] [style '(deleted)])

    (define first-activation? #t)
    (define graph-pb (new snip-canvas% [parent this]))

    (define/public (get-name)
      (send trend-chart get-name))

    (define/public (get-title)
      (send trend-chart get-title))

    (define/public (activate)
      (when first-activation?
        (refresh-chart)))
    
    (define/public (refresh-chart)
      (send trend-chart invalidate-data)
      (send trend-chart put-plot-snip graph-pb)
      (set! first-activation? #f))

    (define/public (interactive-setup parent)
      (when (send trend-chart interactive-setup parent)
        (refresh-chart)))

    (define/public (get-restore-data)
      (list info-tag (send trend-chart get-restore-data)))

    (define/public (export-image-to-file file)
      (send graph-pb export-image-to-file file))

    (define/public (export-data-to-file file formatted?)
      (send trend-chart export-data-to-file file formatted?))

    ))

(define view-trends%
  (class object%
    (init-field parent database)
    (super-new)

    (define tag 'activity-log:view-trends)

    (define pane 
      (new (class vertical-panel%
             (init)
             (super-new)
             (define/public (interactive-export-image)
               (on-interactive-export-image))
             (define/public (interactive-export-data formatted?)
               (on-interactive-export-data formatted?)))
           [parent parent]
           [alignment '(left center)]))

    (define title-field #f)
    (define move-left-button #f)
    (define move-right-button #f)

    (let ((sel-pane (new horizontal-pane% [parent pane] 
                         [spacing 20]
                         [border 0]
                         [stretchable-height #f]
                         [stretchable-width #t]
                         [alignment '(left center)])))

      (make-spacer sel-pane)
      (new message% [parent sel-pane] [label reports-icon])

      (let ([font (send the-font-list find-or-create-font 14 'default 'normal 'normal)])
        (set! title-field (new message% [parent sel-pane] [font font]
                               [label ""] [stretchable-width #t])))

      (set! move-left-button
            (new button% [parent sel-pane] [label "Move left"]
                 [callback (lambda (b e) (on-move-left))]))
      (set! move-right-button
            (new button% [parent sel-pane] [label "Move right"]
                 [callback (lambda (b e) (on-move-right))]))
      (make-spacer sel-pane 30)
      (new button% [parent sel-pane] [label "New..."]
           [callback (lambda (b e) (on-new-chart))])
      (new button% [parent sel-pane] [label "Delete..."]
           [callback (lambda (b e) (on-delete-chart))])
      (new button% [parent sel-pane] [label "Setup..."]
           [callback (lambda (b e) (on-setup-chart))])
      (make-spacer sel-pane))

    (define trend-charts '())

    (define trend-charts-panel
      (new tab-panel%
           [stretchable-height #t]
           [choices '()]
           [callback (lambda (p c)
                       (switch-tabs (send p get-selection)))]
           [parent pane]))

    (define (restore-previous-charts)
      (let ((data (al-get-pref tag (lambda () #f))))

        (define (find-chart-info tag)
          (let loop ((chart-types chart-types))
            (if (null? chart-types)
                #f
                (if (eq? tag (chart-info-tag (car chart-types)))
                    (car chart-types)
                    (loop (cdr chart-types))))))

        (define (make-trends-chart chart-tag restore-data)
          (define ci (find-chart-info chart-tag))
          (when ci
            (let ((pane (let ((tc (new (chart-info-class ci) [database database])))
                          (send tc restore-from restore-data)
                          (new trend-chart-pane%
                               [parent trend-charts-panel]
                               [info-tag (chart-info-tag ci)]
                               [trend-chart tc]))))
              (set! trend-charts (append trend-charts (list pane)))
              (send trend-charts-panel append (send pane get-name)))))
      
        (when (and data (> (length data) 0))
          (let ((first-chart (car data)))
            (match-define (list chart-tag restore-data) first-chart)
            (make-trends-chart chart-tag restore-data))
          (switch-tabs 0)
          (for ([chart-data (cdr data)])
            (match-define (list chart-tag restore-data) chart-data)
            (queue-callback
             (lambda ()
               (make-trends-chart chart-tag restore-data))
             #f)))))
          
    (define (on-new-chart)
      (let ((ct (send (new new-trend-chart-dialog%) show-dialog parent)))
        (when ct
          (let ((pane (let ((tc (new (chart-info-class ct) [database database])))
                        (new trend-chart-pane%
                             [parent trend-charts-panel]
                             [info-tag (chart-info-tag ct)]
                             [trend-chart tc]))))
            (when (send pane interactive-setup parent)
              (set! trend-charts (append trend-charts (list pane)))
              (send trend-charts-panel append (send pane get-name))
              (switch-tabs (- (length trend-charts) 1)))))))
    
    (define (on-delete-chart)
      (let ((n (send trend-charts-panel get-selection)))
        (when n
          (let ((c (list-ref trend-charts n)))
            (let ((mresult (message-box/custom
                            "Confirm delete"
                            (format "Really delete chart \"~a\"?" (send c get-name))
                            #f "Delete" "Cancel"
                            (send parent get-top-level-window)
                            '(caution default=3))))
              (when (equal? mresult 2)
                (send trend-charts-panel delete n)
                (if (eqv? n 0)
                    (set! trend-charts (cdr trend-charts)) ; deletin first item
                    (let-values (([head tail] (split-at trend-charts n)))
                      (set! trend-charts (append head (cdr tail)))))
                (cond ((> (length trend-charts) n)
                       (switch-tabs n))
                      ((> (length trend-charts) 0)
                       (switch-tabs (- (length trend-charts) 1)))
                      (#t
                       (send trend-charts-panel change-children (lambda (old) (list)))))))))))
    
    (define (on-setup-chart)
      (let ((n (send trend-charts-panel get-selection)))
        (when n
          (let ((c (list-ref trend-charts n)))
            (send c interactive-setup parent)
            (let ((nname (send c get-name))
                  (ntitle (send c get-title)))
              (send trend-charts-panel set-item-label n nname)
              (send title-field set-label ntitle))))))

    (define (on-move-left)
      (define sel (send trend-charts-panel get-selection))
      (when (and sel (> sel 0))
        (let-values (((head tail) (split-at trend-charts (sub1 sel))))
          (set! trend-charts (append head (list (car (cdr tail))) (list (car tail)) (cdr (cdr tail)))))
        (after-move (sub1 sel))))
        
    (define (on-move-right)
      (define sel (send trend-charts-panel get-selection))
      (when (and sel (< sel (sub1 (length trend-charts))))
        (let-values (((head tail) (split-at trend-charts sel)))
          (set! trend-charts (append head (list (car (cdr tail))) (list (car tail)) (cdr (cdr tail)))))
        (after-move (add1 sel))))

    (define (after-move new-sel)
      (for (((chart index) (in-indexed (in-list trend-charts))))
        (send trend-charts-panel set-item-label index (send chart get-name)))
      (send trend-charts-panel set-selection new-sel)
      (send move-left-button enable (not (zero? new-sel)))
      (send move-right-button enable (< new-sel (sub1 (length trend-charts)))))
    
    (define (switch-tabs n)
      (send trend-charts-panel set-selection n)
      (let ((chart (list-ref trend-charts n)))
        (send trend-charts-panel change-children
              (lambda (old) (list chart)))
        (queue-callback  ; activate it later, so that the canvas gets its size
         (lambda () (send chart activate))
         #f)
        (send title-field set-label (send chart get-title)))
      (send move-left-button enable (not (zero? n)))
      (send move-right-button enable (< n (sub1 (length trend-charts)))))

    (define (with-exn-handling name thunk)
      (with-handlers
        (((lambda (e) #t)
          (lambda (e)
            ;; NOTE: 'print-error-trace' will only print a stack trace if the
            ;; error trace library is used.  To use it, remove all .zo files
            ;; and run "racket -l errortrace -t run.rkt"
            (let ((message (if (exn? e) (exn-message e) e)))
              (dbglog-exception name e)
              (message-box name message
                           (send parent get-top-level-window)
                           '(ok stop))))))
        (thunk)))
    
    (define (on-interactive-export-image)
      (with-exn-handling
        "on-interactive-export-image"
        (lambda ()
          (let ((n (send trend-charts-panel get-selection)))
            (when n
              (let* ((c (list-ref trend-charts n))
                     (file (put-file "Select file to export to" #f #f
                                     (format "~a.png" (send c get-name))
                                     "png" '()
                                     '(("PNG Files" "*.png") ("Any" "*.*")))))
                (when file
                  (send c export-image-to-file file))))))))
        
    (define (on-interactive-export-data formatted?)
      (with-exn-handling
        "on-interactive-export-data"
        (lambda ()
          (let ((n (send trend-charts-panel get-selection)))
            (when n
              (let* ((c (list-ref trend-charts n))
                     (file (put-file "Select file to export to" #f #f
                                     (format "~a.csv" (send c get-name))
                                     "csv" '()
                                     '(("CSV Files" "*.csv") ("Any" "*.*")))))
                (send c export-data-to-file file formatted?)))))))
        
    (define first-activation #t)

    (define/public (activated)
      (when first-activation
        (restore-previous-charts)
        (set! first-activation #f)))

    (define/public (refresh)
      (let ((n (send trend-charts-panel get-selection)))
        (when n
          (let ((c (list-ref trend-charts n)))
            (send c refresh-chart)))))

    (define/public (save-visual-layout)
      ;; Charts for this view are loaded on first activation (see `activate'),
      ;; if the view was not activated, don't save anything, otherwise we will
      ;; erase all our charts.
      (unless first-activation
        (let ((data (for/list ([tc trend-charts]) (send tc get-restore-data))))
          (al-put-pref tag data))))
    
    ))
