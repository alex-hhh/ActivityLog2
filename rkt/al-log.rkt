#lang racket/base
;; al-log.rkt -- logging facilities
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

(require pict
         racket/class
         racket/gui/base
         racket/string)

(provide log-al-fatal
         log-al-error
         log-al-warning
         log-al-info
         log-al-debug)
(provide make-log-output-window)


;;.......................................................... badge-snip% ....

(define log-snip-class
  (make-object
   (class snip-class% (super-new) (send this set-classname "log-snip"))))

(send (get-the-snip-class-list) add log-snip-class)

(define the-font 
  (send the-font-list find-or-create-font 10 'default 'normal 'normal))

(define (break-into-lines text dc font width)
  (let ((result '())
        (current #f))
    (for-each (lambda (item)
                (if (not current)
                    (set! current item)
                    (let ((candidate (string-append current " " item)))
                      (let-values (((w h x y) (send dc get-text-extent candidate font #t)))
                        (if (> w width)
                            (begin
                              (set! result (cons current result))
                              (set! current item))
                            (set! current candidate))))))
              (string-split text))
    (reverse (cons current result))))

(define (make-log-badge headline color width font)

  (define border 10)
  (define text-width (- width border))
  
  (let* ((dc (new bitmap-dc% [bitmap (make-object bitmap% 100 100)]))
         (lines (break-into-lines headline dc font text-width))
         (text-box (apply vl-append (map
                                     (lambda (line) (text line font))
                                     lines))))
    (let ((text-box-width (pict-width text-box)))
      (set! text-box (inset/clip text-box 0 0 (- text-width text-box-width) 0)))

    (let ((r (cellophane (filled-rounded-rectangle (+ (pict-width text-box) border)
                                                   (+ (pict-height text-box) border)
                                                   #:draw-border? #f)
                         0.3)))
      (let* ((almost-final (cc-superimpose (colorize r color) text-box)))
        almost-final))
      ))

(define log-snip%
  (class snip%
    (init-field level text)
    (super-new)
    (inherit get-admin set-flags get-flags set-snipclass set-count)

    (set-snipclass log-snip-class)
    (set-count 1)

    (define target-width 200)
    (define color
      (case level
        ((fatal error) "red")
        ((warning) "yellow")
        ((info) "blue")
        (else "gray")))

    (define log-pict #f)
    (define draw-fn #f)

    (define (invalidate)
      (set! log-pict #f)
      (set! draw-fn #f))

    (define (make-draw-fn)
      (set! log-pict (make-log-badge text color target-width the-font))
      (set! draw-fn (make-pict-drawer log-pict)))

    (define/override (get-extent dc x y w h descent space lspace rspace)
      (unless draw-fn (make-draw-fn))
      (when w (set-box! w (if log-pict (pict-width log-pict) 0)))
      (when h (set-box! h (if log-pict (pict-height log-pict) 0)))
      (when descent (set-box! descent (if log-pict (pict-descent log-pict) 0)))
      (when space (set-box! space 0.0))
      (when lspace (set-box! lspace 0.0))
      (when rspace (set-box! rspace 0.0)))

    (define/override (draw dc x y . other)
      (unless draw-fn (make-draw-fn))
      (when draw-fn (draw-fn dc x y)))

    (define/public (set-target-width w)
      (invalidate)
      (set! target-width (if w w 200))
      (send (get-admin) resized this #t))

    (define/public (get-actual-width)
      (unless draw-fn (make-draw-fn))
      (if log-pict (pict-width log-pict) 0))

    (define/public (get-actual-height)
      (unless draw-fn (make-draw-fn))
      (if log-pict (pict-height log-pict) 0))

    ))


;;.............................................................. log-pb% ....

(define log-pb%
  (class pasteboard%
    (init)
    (super-new)
    (inherit get-canvas move-to find-first-snip insert delete set-before
             begin-edit-sequence end-edit-sequence erase)

    (define snip-spacing 5)

    (define (get-target-snip-width)
      (let ((c (get-canvas)))
        (if c
            (let ((dc (send (get-canvas) get-dc)))
              (let-values (([w h] (send dc get-size)))
                (- w snip-spacing snip-spacing)))
            #f)))

    (define (with-edit-sequence thunk)
      (begin-edit-sequence)
      (thunk)
      (end-edit-sequence))

    (define/augment (after-insert snip before x y)
      (send snip set-target-width (get-target-snip-width))
      (arrange-badges))

    (define removing-snips? #f)
    (define move-allowed? #f)

    (define/augment (can-delete? snip)
      removing-snips?)

    (define/augment (can-move-to? snip x y dragging?)
      (or dragging? move-allowed?))

    (define/augment (after-delete snip)
      #f)

    (define/augment (after-move-to snip x y dragging?)
      #f)

    (define/augment (on-select snip on?)
      #f)

    (define (remove-all-snips)
      (with-edit-sequence
       (lambda ()
           (set! removing-snips? #t)
           (erase)
           (set! removing-snips? #f))))

    (define (get-snip-list)
      (let ((snip-list (let loop ((snip (find-first-snip)) (snip-list '()))
                         (if snip
                             (loop (send snip next) (cons snip snip-list))
                             snip-list))))
        snip-list))

    (define (arrange-badges)
      (set! move-allowed? #t)
      (let ((canvas (get-canvas)))
        (when canvas
          (let ((dc (send canvas get-dc)))
            (let-values (([w h] (send dc get-size)))
              (let ((badges (get-snip-list)))
                (let loop ((b badges) (y snip-spacing))
                  (unless (null? b)
                    (move-to (car b) snip-spacing y)
                    (loop (cdr b) (+ y (send (car b) get-actual-height) 
                                     snip-spacing)))))))))
      (set! move-allowed? #f))

    (define/augment (on-display-size)
      (with-edit-sequence
       (lambda ()
         (let ((w (get-target-snip-width)))
           (for ((snip (in-list (get-snip-list))))
             (send snip set-target-width w)))
         (arrange-badges))))

    (send this set-selection-visible #f)

    ))


;;.....................................................................  ....

(define-logger al)
(define al-prefix "al: ")

(define (make-log-output-window parent)

  (let ((log-pb (new log-pb%)))
    (new editor-canvas%
         [parent parent]
         [editor log-pb]
         [style '(no-hscroll auto-vscroll)]
         [stretchable-width #t]
         [horizontal-inset 0]
         [vertical-inset 0])
    
    (define (insert-log-messages source)
      (let ((m (sync source)))
        ;; (printf "*** ~a~%" m)
        (let* ((msg (string-trim (vector-ref m 1) al-prefix #:left? #t #:right? #f))
               (snip (new log-snip% [level (vector-ref m 0)] [text msg])))
          (send log-pb insert snip))
        (insert-log-messages source)))

    (thread (lambda () 
              (insert-log-messages
               (make-log-receiver al-logger 'info))))

    log-pb))
