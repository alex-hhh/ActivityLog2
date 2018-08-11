#lang typed/racket/base
;; plot-hack.rkt -- make the typed-racket plot package work sensibly
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

;;; Commentary
;;
;; Staring from Racket 6.2 onwards, the plot package is written in typed
;; racket.  Since most of ActivityLog2 is untyped, the typed plot-snip%
;; objects get into untyped code and the types on the interace are converted
;; to contracts which are evaluated each time a method is invoked.  This
;; causes very poor performance when the plot-snip% is inserted into an
;; editor-canvas% object for display.
;;
;; This code defines a plot-snip/hack function to wrap the plot-snip function
;; and add the canvas so the plot-snip% object never leaves typed-racket.  In
;; doing so, we rely on several undocumented features.  This code fails to
;; compile in Racket 6.2.
;; 
;; The Typed-Racket to Racket interoperation is a bit undercooked.  Hopefully
;; one day this hack can be removed.

(module glue typed-racket/base-env/extra-env-lang
  (module types typed/racket/base
    (require (only-in typed/racket/gui/base Snip% Editor-Canvas%))
    (provide (all-defined-out))
    (define-type Set-Snip
      (-> (Instance Editor-Canvas%) Any Void)))
  (module untyped-functions racket/base
    (require racket/class)
    (define (set-snip canvas snip)
      (send canvas set-snip snip)
      (void))
    (provide set-snip))
  (require 'types
           (for-syntax (submod 'types #%type-decl))
           'untyped-functions)
  (type-environment
   [set-snip (parse-type #'Set-Snip)])
  (provide set-snip))

(require plot
         racket/class
         racket/math
         (only-in typed/racket/gui/base Snip% Editor-Canvas%)
         (only-in typed/racket/draw Bitmap%)
         'glue)

(: plot-snip/hack
   (->* [(Instance Editor-Canvas%)
         (Treeof (U renderer2d nonrenderer))]
        [#:x-min (U Real #f) #:x-max (U Real #f)
         #:y-min (U Real #f) #:y-max (U Real #f)
         #:width Positive-Integer
         #:height Positive-Integer
         #:title (U String #f)
         #:x-label (U String #f)
         #:y-label (U String #f)
         #:legend-anchor Anchor]
        Any))
(define (plot-snip/hack canvas
                        renderer-tree
                        #:x-min [x-min #f] #:x-max [x-max #f]
                        #:y-min [y-min #f] #:y-max [y-max #f]
                        #:width [width (plot-width)]
                        #:height [height (plot-height)]
                        #:title [title (plot-title)]
                        #:x-label [x-label (plot-x-label)]
                        #:y-label [y-label (plot-y-label)]
                        #:legend-anchor [legend-anchor (plot-legend-anchor)])
  ;; Calculate the initial size of the plot such that it fills up the entire
  ;; canvas area.  The #:width and #:height parameters are only used if this
  ;; would result in a 0 sized plot.
  ;;
  ;; The plot snip will be resized automatically as the canvas resizes, but
  ;; this initial sizing ensures there is no flickering when a snip with a
  ;; different size is inserted into the canvas forcing the canvas to resize
  ;; it immediately.
  (let-values (((w h) (send (send canvas get-dc) get-size)))
    (let* ((hinset (send canvas horizontal-inset))
           (vinset (send canvas vertical-inset))
           (iw (exact-round (- w (* 2 hinset))))
           (ih (exact-round (- h (* 2 vinset))))
           [snip (plot-snip renderer-tree
                            #:x-min x-min #:x-max x-max
                            #:y-min y-min #:y-max y-max
                            #:width (if (> iw 0) iw width)
                            #:height (if (> ih 0) ih height)
                            #:title title
                            #:x-label x-label #:y-label y-label
                            #:legend-anchor legend-anchor)])
      (set-snip canvas snip)
      snip)))

(: plot-to-bitmap/hack
   (-> (Treeof (U renderer2d nonrenderer))
       (Instance Bitmap%)
       Void))
(define (plot-to-bitmap/hack renderer-tree bitmap)
  (plot/dc renderer-tree
           (send bitmap make-dc)
           0
           0
           (send bitmap get-width)
           (send bitmap get-height)))

(provide plot-snip/hack
         plot-to-bitmap/hack)

