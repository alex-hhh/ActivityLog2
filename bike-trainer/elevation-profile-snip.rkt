#lang racket/gui
(require plot)
(require "../rkt/data-frame.rkt")
(require "../rkt/fmt-util.rkt")
(require "rdp-simplify.rkt")

(provide elevation-profile-snip%)


;;............................................................ resources ....

(define text-color (make-color 130 130 130))
(define text-font (send the-font-list find-or-create-font 12 'default 'normal 'normal))
(define active-color (make-color #x2f #x4f #x4f))
(define outline-pen
  (send the-pen-list find-or-create-pen active-color 1 'solid))
(define transparent-brush
  (send the-brush-list find-or-create-brush "black" 'transparent))


;;............................................................ utilities ....

;; Return the current height on the ELEVATION-PROFILE at DISTANCE from the
;; start.
(define (get-current-height elevation-profile distance)
  (let ((index (bsearch elevation-profile distance #:key (lambda (x) (vector-ref x 0)))))
    (cond ((<= index 0)
           (let ((first (vector-ref elevation-profile 0)))
             (vector-ref first 1)))
          ((>= index (vector-length elevation-profile))
           (let* ((last-index (sub1 (vector-length elevation-profile)))
                  (last (vector-ref elevation-profile last-index)))
             (vector-ref last 1)))
          (#t
           ;; Interpolate the height between two adjacent DISTANCE positions
           ;; in ELEVATION-PROFILE
           (let ((p1 (vector-ref elevation-profile (sub1 index)))
                 (p2 (vector-ref elevation-profile index)))
             (let ((factor (/ (- distance (vector-ref p2 0))
                              (- (vector-ref p1 0) (vector-ref p2 0)))))
               (+ (* factor (vector-ref p1 1))
                  (* (- 1 factor) (vector-ref p2 1)))))))))


;;.............................................. elevation-profile-snip% ....

(define elevation-profile-snip-class
  (make-object
   (class snip-class% (super-new)
     (send this set-classname "elevation-profile-snip"))))

(send (get-the-snip-class-list) add elevation-profile-snip-class)

;; Display an elevation profile plot with a current location marked.  The
;; elevation profile is set from the "alt" or "calt" series of a data frame
;; (see `set-elevation-profile`, while the current position is set via
;; `update-position`
(define elevation-profile-snip%
  (class snip%

    (init-field bgcolor)
    (super-new)
    (send this set-snipclass elevation-profile-snip-class)
    (send this set-count 1)

    (define elevation-profile #f)
    ;; minimum distance that the `current-dst` has to move to trigger a
    ;; refresh.  This is computed to be the minimum change that will result in
    ;; a visible change of the position on the plot.
    (define min-distance-change 0)

    (define plot-x-min 0)
    (define plot-x-max 0)
    (define plot-y-min 0)
    (define plot-y-max 0)

    (define current-distance 0)
    (define current-height 0)

    (define width 180)
    (define height 150)
    (define color (make-object color% 38 127 0))

    ;; Internal implementation for setting elevation profile from a data
    ;; frame.  Assumes DF is not #f.  See `set-elevation-profile`
    (define (set-elevation-profile-1 df)
      (define series
        (cond ((send df contains? "dst" "calt")
               '("dst" "calt"))
              ((send df contains? "dst" "alt")
               '("dst" "alt"))
              (#t
               (error "data frame does not contain required data series"))))
      (define eprofile (send/apply df select* series))
      (define min-elevation #f)
      (define max-elevation #f)
      (for ((p (in-vector eprofile)))
        (define e (vector-ref p 1))
        (set! min-elevation (if min-elevation (min min-elevation e) e))
        (set! max-elevation (if max-elevation (max max-elevation e) e)))
      (define elevation-range (- max-elevation min-elevation))
      (set! plot-y-min (- min-elevation (* 0.05 elevation-range)))
      (set! plot-y-max (+ max-elevation (* 0.05 elevation-range)))
      ;; NOTE: we compute epsilon (used to simplify the plot) as the
      ;; minimum visible change in the height profile based on the
      ;; current height of the snip.  We don't recompute it (or the
      ;; simplified profile) when the size of the snip change.
      (define epsilon (* 0.5 (/ elevation-range height)))
      (set! elevation-profile (rdp-simplify eprofile epsilon))
      ;; NOTE: we compute `min-distance-change` based on the current width of
      ;; the snip and we don't update it if the snip is resized.
      (define last-point (vector-ref eprofile (sub1 (vector-length eprofile))))
      (define max-distance (vector-ref last-point 0))
      (set! plot-x-min (- (* 0.05 max-distance)))
      (set! plot-x-max (+ max-distance (* 0.05 max-distance)))
      (set! min-distance-change (* 0.1 (/ max-distance width)))
      (set! current-distance 0)
      (set! current-height
            (if elevation-profile
                (get-current-height elevation-profile 0)
                0)))

    ;; Set the elevation profile from the data frame DF.  If DF is #f, the
    ;; elevation profile is cleared.
    (define/public (set-elevation-profile df)
      (if df
          (set-elevation-profile-1 df)
          (begin
            (set! elevation-profile #f)
            (set! min-distance-change 0))))

    (define/override (get-extent dc x y w h descent space lspace rspace)
      (when w (set-box! w width))
      (when h (set-box! h height))
      (when descent (set-box! descent 0))
      (when space (set-box! space 10.0))
      (when lspace (set-box! lspace 10.0))
      (when rspace (set-box! rspace 10.0)))

    (define/override (draw dc x y . other)
      (if elevation-profile
          (parameterize ((plot-x-label #f)
                         (plot-y-label #f)
                         ;; (plot-y-axis? #f)
                         ;; (plot-y-far-axis? #f)
                         (plot-x-axis? #f)
                         ;; (plot-x-far-axis? #f)
                         (plot-decorations? #f)
                         (plot-background bgcolor)
                         (plot-background-alpha (if (is-a? bgcolor color%)
                                                    (send bgcolor alpha)
                                                    1)))
            (plot/dc (list (lines elevation-profile #:width 2 #:color color)
                           (point-label
                            (vector current-distance current-height)
                            (vertical-distance->string current-height #t)
                            #:point-color "red"
                            #:point-fill-color "red"
                            #:alpha 0.8
                            #:point-size 10
                            #:point-line-width 2
                            ))
                     #:x-min plot-x-min
                     #:x-max plot-x-max
                     #:y-min plot-y-min
                     #:y-max plot-y-max
                     dc x y width height)
            (send dc set-brush transparent-brush)
            (send dc set-pen outline-pen)
            (send dc draw-rectangle x y width height))
          (let ((msg "No Elevation Profile")
                (brush (send the-brush-list find-or-create-brush bgcolor 'solid))
                (pen outline-pen))
            (send dc set-brush brush)
            (send dc set-pen pen)
            (let-values (([w1 h1 x1 y1] (send dc get-text-extent msg text-font #t)))
              (send dc draw-rectangle x y width height)
              (send dc set-font text-font)
              (send dc set-text-foreground text-color)
              (let ((ox (- (/ width 2) (/ w1 2)))
                    (oy (- (/ height 2) (/ h1 2))))
                (send dc draw-text msg (+ x ox) (+ y oy)))))
          ))

    ;; Return the width of the snip.
    (define/public (get-width) width)

    ;; Set the height of this snip
    (define/public (set-height h)
      (set! height h)
      (let ((admin (send this get-admin)))
        (when admin
          (send admin resized this #t))))

    ;; Update the current position marker on the elevation profile to the
    ;; position for DST meters from the start.
    (define/public (update-position dst)
      (when (> (abs (- dst current-distance)) min-distance-change)
        (set! current-distance dst)
        (set! current-height
              (if elevation-profile
                  (get-current-height elevation-profile dst)
                  0))
        (let ((admin (send this get-admin)))
          (when admin
            (send admin needs-update this 0 0 width height)))))

    ))
