#lang racket/gui
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; climb-analisys-tool.rkt -- tool to analyze climbs in GPX files
;; see https://alex-hhh.github.io/2021/04/climb-analysis-tool.html
;;
;; This file is part of AL2-Climb-Analysis
;; Copyright (c) 2021, 2022, 2023, 2024 Alex Harsányi <AlexHarsanyi@gmail.com>
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
;;
;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.

(require colormaps
         data-frame
         data-frame/gpx
         data-frame/private/rdp-simplify
         gui-widget-mixins
         map-widget
         math/statistics
         pict
         plot
         plot-container
         plot-container/hover-util
         plot/utils
         racket/cmdline
         racket/draw
         racket/path
         "../rkt/fit-file/course.rkt"
         "../rkt/models/fiets-score.rkt"
         "../rkt/utilities.rkt"
         "../rkt/widgets/dragable-split-panel.rkt"
         "../rkt/widgets/qresults-list.rkt")

;; Install colormaps for showing the gradient on the elevation plot.  See the
;; colormaps package for additional colormaps that can be used
(plot-pen-color-map 'cb-rdbu-11)        ; 'cb-rdylgn-11 is also a nice one
(plot-brush-color-map 'cb-rdbu-11)

(define selected-track-pen
  (send the-pen-list find-or-create-pen "Teal" 6 'solid))
(define selected-climb-color "firebrick")
(define title-font (make-object font% 18 'default))

;; These are default values for various parameters to detect climbs.  The
;; actual values, as modified by the user are saved to a preferences file and
;; restored, so the values below are only used when the application is first
;; run and there are no saved preferences yet.
(define *rdp-epsilon* 1.2)
(define *min-climb-score* 0.25)
(define *min-climb-grade* 1)
(define *max-climb-separation* 1.0)
(define *athlete-weight* 90)            ; includes weight of the bike


;;............................................................... colors ....

;; Construct a function which converts a grade value into an index for a color
;; map.  COLOR-COUNT represents the number of colors in the color map, while
;; INVERT? indicates that the color map is to be inverted (i.e. as the grade
;; goes up, the color map color index goes down).  This function is intended
;; to be used to assign colors to grade values for a diverging color map, for
;; an example, see the colormaps package at:
;; https://docs.racket-lang.org/colormaps/index.html
;;
;; The returned function will convert a grade value such that grade values
;; between -1, 1 fall in the middle of the color map (if there are an odd
;; number of colors, the middle color index will be the range -1, 1, if there
;; is an even number of colors, the middle two colors are the ranges -1, 0 and
;; 0, 1, from there colors are assigned to each side using a log2 base for the
;; grade (i.e. next colors are grades 1 to 2%, followed by 2% to 4% etc and a
;; similar thing happens for negative grades.
;;
;; For example, if there are 7 colors in a color map:
;;
;; color 0 will represent grades less than -4%
;; color 1 between -4 and -2%
;; color 2 between -2 and -1%
;; color 3 (the middle one) between -1, to 1%
;; color 4 between 1% and 2%
;; color 5 between 2 and 4%
;; color 6 grades greater than 4%

(define (make-grade-color-indexer color-count invert?)
  (define offset (exact-floor (/ color-count 2)))
  (lambda (grade)
    (define index0
      (* (if invert? -1 1)
         (sgn grade)
         (exact-floor
          (let ([absolute-grade (abs grade)])
            (if (or (not (rational? absolute-grade))
                    (< absolute-grade 1.0))
                0
                (add1 (log absolute-grade 2)))))))
    (define index1
      (if (odd? color-count)
          (+ offset index0)
          (if (> grade 0)
              (+ offset index0)
              (+ offset -1 index0))))
    (inexact->exact (min (sub1 color-count) (max 0 index1)))))

;; Function to convert a grade into a color index based on the current color
;; map.
(define grade->color-index (make-grade-color-indexer (color-map-size (plot-pen-color-map)) #t))


;;............................................................... climbs ....

;; A climb represents a section of the data frame which goes "up" or "down".
;; Initially, these are sections of constant grade, but such climbs can be
;; joined together forming longer climbs.
(struct climb
  (start             ; distance (in the dst/km series) where this climb starts
   end               ; distance (in the dst/km series) where this climb ends
   bottom            ; altitude at the bottom of the climb
   top               ; altitude at the top of the climb
   grade             ; the grade of the climb (elevation over distance)
   max-grade         ; maximum grade on the climb (see `join-climbs`)
   score             ; FIETS score for the climb (excluding the T - 1000 part), see climbs.md
   energy            ; energy required for the climb to lift 1kg of weight
                     ; (this needs to be multiplied by the cyclist + bike
                     ; weight to find the energy required for the climb
   )
  #:transparent)

(define (climb-distance c)
  (- (climb-end c) (climb-start c)))

(define (climb-elevation c)
  (- (climb-top c) (climb-bottom c)))

;; Construct a climb between two data points, each data point being a vector
;; of distance and altitude.  This is a low level function, which simply
;; constructs a `climb` instance out of points `a` and `b` without assuming
;; they actually form a climb or not.
(define (make-climb a b)
  (match-define (vector dst-a alt-a) a)
  (match-define (vector dst-b alt-b) b)
  (let* ([distance (- dst-b dst-a)]
         [elevation (- alt-b alt-a)]
         [grade (* (/ elevation (* distance 1000.0)) 100.0)]
         ;[score (* grade grade distance)]
         [score (/ (* elevation elevation) (* distance 10000.0))]
         ;; NOTE: don't spend energy on descents
         [energy (if (> elevation 0) (* (sin (atan elevation distance)) elevation) 0)])
    (climb dst-a dst-b alt-a alt-b grade grade score energy)))

;; Construct a list of climb segments from the data frame df -- these will be
;; segments of constant grade, but can go either up or down (thus being
;; descents).  They are in sequence and cover the entire data in the
;; data-frame DF.  `epsilon` represents the "filtering constant" which is used
;; to smooth out the altitude data before segmenting it.  The higher the value
;; the longer the segments will be, smoothing out smaller climbs and descents.
(define (raw-climb-segments df #:epsilon [epsilon *rdp-epsilon*])
  (define simplified-altitude
    (rdp-simplify
     (df-select* df "dst/km" "alt" #:filter valid-only)
     #:epsilon epsilon
     #:destroy-original? #t))
  (for/list ([a (in-vector simplified-altitude 0)]
             [b (in-vector simplified-altitude 1)])
    (make-climb a b)))

;; Return true if climbs A and B are adjacent, that is, climb A ends close to
;; where climb B starts.  MAX-JOIN-DISTANCE defines the maximum acceptable
;; distance for the climbs to be considered adjacent, a value of `#f`
;; indicates that climb A must end exactly where climb B starts.
(define (adjacent-climbs? a b [max-join-distance #f])
  (define diff (- (climb-start b) (climb-end a)))
  (<= diff (or max-join-distance 0)))

;; Join two adjacent climbs A and B producing a new combined climb.
(define (join-climbs a b)
  (define distance (- (climb-end b) (climb-start a)))
  (define elevation (- (climb-top b) (climb-bottom a)))
  (define grade (* (/ elevation (* distance 1000.0)) 100.0))
  (climb
   (climb-start a)
   (climb-end b)
   (climb-bottom a)
   (climb-top b)
   grade
   (max (climb-max-grade a) (climb-max-grade b))
   (+ (climb-score a) (climb-score b))
   (+ (climb-energy a) (climb-energy b))))

;; Find the true climb segments from RAW-CLIMBS (a list of climbs produced by
;; `raw-climb-segments`.  Climbs whose grade is smaller than min-grade are
;; discarded, and remaining climbs are joined together when they are adjacent
;; to form the longest possible climbs.
(define (climb-segments raw-climbs
                        #:min-grade (min-grade *min-climb-grade*)
                        #:min-score (min-score *min-climb-score*)
                        #:max-join-distance (max-join-distance *max-climb-separation*))
  (for/fold ([climbs '()]
             #:result (filter (lambda (c) (>= (climb-score c) min-score)) (reverse climbs)))
            ([current-climb raw-climbs] #:when (> (climb-grade current-climb) min-grade))
    (if (and (not (null? climbs))
             (adjacent-climbs? (first climbs) current-climb max-join-distance))
        (cons (join-climbs (first climbs) current-climb) (rest climbs))
        (cons current-climb climbs))))

;; Find a climb in the list of CLIMBS which is at DISTANCE km from the start.
;; Might return #f if there is no climb at DISTANCE.  Assumes that the climbs
;; are in order (which they are if they are produced by `climb-segments` or
;; `raw-climb-segments`)
(define (find-climb climbs distance)
  (for/or ([c (in-list climbs)])
    (if (and (>= distance (climb-start c))
             (<= distance (climb-end c)))
        c
        #f)))

;; Load a GPX file and return a data frame with its contents.  Will also add a
;; "dst/km" data series to it, which is the distance in kilometers (the "dst"
;; series is in meters).
(define (load-course file-name)
  (with-handlers
    ([exn:fail?
      (lambda (e)
        (message-box
         "Failed to load file"
         (format "Failed to load ~a: ~a" file-name (exn-message e)))
        #f)])
    (let ([type (path-get-extension file-name)])
      (define df
        (cond
          [(equal? type #".gpx")
           (df-read/gpx file-name)]
          [(equal? type #".fit")
           (df-read/course file-name)]
          [#t
           (error "Unknown File Extension")]))
      (df-add-derived!
       df
       "dst/km"
       '("dst")
       (lambda (v)
         (match-define (list dst) v)
         (and dst (/ dst 1000.0))))
      (df-set-sorted! df "dst/km" <)
      df)))

;; Add raw-climbs (as produced by `raw-climb-segments`) to the data frame,
;; attached to the 'raw-climbs property.  Will also create the "grade" and
;; "grade-color" series which can be constructed from the raw climb segments.
;; "grade-color" assigns a color index to each grade value, see
;; `make-grade-color-indexer` and `grade->color-index` for more details.
(define (add-raw-climbs df #:epsilon epsilon)
  (define raw-climbs (raw-climb-segments df #:epsilon epsilon))

  (let ([climbs raw-climbs])
    (df-add-derived!
     df
     "grade"
     '("dst/km")
     (lambda (v)
       (match-define (list dst) v)
       (if (or (null? climbs) (not dst))
           #f
           (let ([current-climb (car climbs)])
             (when (>= dst (climb-end current-climb))
               (set! climbs (cdr climbs))
               (set! current-climb (if (null? climbs) #f (car climbs))))
             (if current-climb
                 (climb-grade current-climb)
                 #f))))))

  (df-add-derived!
   df
   "grade-color"
   '("grade")
   (lambda (v)
     (match-define (list grade) v)
     (and grade (grade->color-index grade))))

  (df-put-property! df 'raw-climbs raw-climbs))

;; Add climbs to the data frame DF as produced by the `climb-segments`
;; function.  The climbs are attached to the 'climbs property on the data
;; frame.  MIN-GRADE and MIN-SCORE are passed to the `climb-segments`
;; function.
;;
;; NOTE: this function assumes that raw climbs (as added by the
;; `add-raw-climbs`) are already present in the data frame.
(define (add-climbs df #:min-grade min-grade #:min-score min-score
                    #:max-join-distance max-join-distance)
  (define raw-climbs (df-get-property df 'raw-climbs))
  (when raw-climbs
    (define the-climbs (climb-segments raw-climbs
                                       #:min-grade min-grade
                                       #:min-score min-score
                                       #:max-join-distance max-join-distance))
    (df-put-property! df 'climbs the-climbs)))

;; Create plot renderers which render the altitude vs distance from the data
;; frame DF.  Line will be shaded underneath with a color based on the grade
;; at that point (using the "grade-color" data series).  The result of this
;; function can be passed to `plot` and related functions to plot altitude
;; shaded by grade.
(define (make-grade-color-renderers df)
  (let ([renderers '()]
        [current-span '()]
        [current-color #f]
        [alt-stats (df-statistics df "alt")])
    (define min-y (- (statistics-min alt-stats) (* (statistics-range alt-stats) 0.05)))
    (for ([(dst alt color) (in-data-frame df "dst/km" "alt" "grade-color")]
          #:when (and dst alt color))
      (set! current-span (cons (vector dst alt) current-span)) ;; always add the current point!
      (unless (equal? color current-color)
        (when current-color
          (let* ([data (reverse current-span)]
                 [start (vector-ref (first data) 0)]
                 [end (vector-ref (last data) 0)]
                 [r (lines-interval
                     data
                     (list (vector start min-y) (vector end min-y))
                     #:line1-color "darkgreen"
                     #:line1-width 2
                     #:line2-style 'transparent
                     #:alpha 0.8
                     #:color current-color)])
            (set! renderers (cons r renderers))))
        (set! current-span (list (vector dst alt)))
        (set! current-color color)))
    (when current-color
      (let* ([data (reverse current-span)]
             [start (vector-ref (first data) 0)]
             [end (vector-ref (last data) 0)]
             [r (lines-interval
                 data
                 (list (vector start min-y) (vector end min-y))
                 #:line1-color "darkgreen"
                 #:line1-width 2
                 #:line2-style 'transparent
                 #:alpha 0.8
                 #:color current-color)])
        (set! renderers (cons r renderers))))
    renderers))

;; Create a set of renderers which highlight the climb segments in the data
;; frame (using the climbs attached to the 'climbs property, see
;; `add-climbs`).
(define (make-climb-renderers df)
  (define the-climbs (df-get-property df 'climbs))
  (if the-climbs
      (rectangles
       (for/list ([c (in-list the-climbs)])
         (vector (ivl (climb-start c) (climb-end c)) (ivl -inf.0 +inf.0)))
       #:line-color "darkgreen"
       #:line-width 0.5
       #:color "ivory")
      '()))

(define additional-renderers '())

;; Convert the FIETS score (climb score) V into a string together with the
;; climb category -- this string is intended to be displayed to the user.
(define (f->s v)
  (let ([cat (fiets-score->climb-category v)]
        [f (~r v #:precision 2)])
    (if (equal? cat "")
        f
        (string-append f " (" cat ")"))))

;; Return a function suitable as a plot call back (to be passed to
;; `set-mouse-event-callback`).  The function will display information about
;; the current location on the plot as well as highlight the current location
;; on the map.  `df` is the data frame which is used for lookups, while `cll`
;; is a `current-location-layer%` instance on which the location is updated.
(define (make-plot-callback df cll)
  (define the-climbs (or (df-get-property df 'climbs) '()))
  (lambda (snip event dst _alt)
    (let/ec return
      (unless (good-hover? snip dst _alt event)
        (send snip set-overlay-renderers additional-renderers)
        (send cll current-location #f)
        (return (void)))

      (define location (df-lookup df "dst/km" '("lat" "lon") dst))
      (if (and (vector-ref location 0) (vector-ref location 1))
          (send cll current-location location)
          (send cll current-location #f))

      (match-define (vector alt grade) (df-lookup df "dst/km" '("alt" "grade") dst))

      (define renderers
        (if (and alt grade)
            (let* ([c (find-climb the-climbs dst)]
                   [climb-badge (and c
                                     (make-hover-badge
                                      `(("Score"  ,(f->s (climb-score c)))
                                        ("Ascent" ,(~r (climb-elevation c) #:precision 0) "meters")
                                        ("Length" ,(~r (climb-distance c) #:precision 1) "km")
                                        ("Climb"))))]
                   [main-badge (make-hover-badge
                                `(("Distance" ,(~r dst #:precision 2) "km")
                                  ("Altitude" ,(~r alt #:precision 1) "meters")
                                  ("Grade" ,(~r grade #:precision 1) "%")))])
            (list (vrule dst #:style 'long-dash)
                  (point-pict
                   (vector dst _alt)
                   (if climb-badge
                       (hc-append 3 main-badge climb-badge)
                       main-badge)
                   #:anchor 'auto)))
            '()))

      (send snip set-overlay-renderers (append additional-renderers renderers)))))


;;............................................ Construct the GUI widgets ....

;; This section constructs all the GUI widgets of the application.  GUI
;; creation is split into 3 sections: first we create the widgets using
;; defaults and values retrieved from the preferences system, next we setup
;; properties by calling various methods (next section) and finally we
;; implement the callbacks, which are functions that react to GUI events and
;; implement the application behavior.

(define toplevel
  (let ([dims (get-pref 'al2-climb-analysis-tool:frame-dimensions (cons 1200 750))])
    (new (class frame%
           (init)
           (super-new)
           (define/augment (on-close)
             (save-preferences)))
         [label "Climb Analysis Tool"]
         [border 10]
         [width (car dims)]
         [height (cdr dims)])))

(define header-pane
  (new horizontal-pane%
       [parent toplevel]
       [border 20]
       [spacing 10]
       [alignment '(left center)]
       [stretchable-height #f]))

(define activity-name-message
  (new message%
       [parent header-pane]
       [font title-font]
       [stretchable-width #t]
       [label "No File Loaded"]))

(define load-gpx-button
  (new button%
       [parent header-pane]
       [label "Load Course..."]
       [callback (lambda (b e)
                   (let ([path (get-file "Load GPX file" toplevel #f #f #f
                                         '()
                                         '(("GPX Files" "*.gpx")
                                           ("FIT Files" "*.fit")
                                           ("Any" "*.*")))])
                     (when (and path (file-exists? path))
                       (on-load-course path))))]))

(define p0
  (new horizontal-dragable-split-panel%
       [parent toplevel]))

(define climb-pane
  (new vertical-pane%
       [parent p0]
       [border 0]
       [spacing 20]))

;; Create a list view of the climbs data with a right click menu attached.
;; This is somewhat complex, requiring a letrec, since the view needs to know
;; about the menu and the menu needs to know about the view.  This approach
;; works for a small menu item, for more complex situations, this would be
;; wrapped in a class...
(define climbs-view
  (letrec ([on-menu-demand
            (lambda (m)
              (define have-selection? (send view get-selected-row-index))
              (send export-menu-item enable have-selection?))]
           [climb-operations-menu
            (new popup-menu%
                 [title "Climb Operations"]
                 [demand-callback on-menu-demand])]
           [export-menu-item
            (new menu-item%
                 [parent climb-operations-menu]
                 [label "Export as GPX..."]
                 [callback (lambda (m e)
                             (define selection (send view get-selected-row-index))
                             (when selection
                               (on-export-climb-segment selection)))])]
           [view (new (class qresults-list%
                        (init)(super-new)
                        (define/override (on-select row-index data)
                          (on-climb-selected data)))
                      [parent climb-pane]
                      [pref-tag 'al2-climb-analysis-tool:climbs-view]
                      [right-click-menu climb-operations-menu])])
    view))

(define climb-controls-pane
  (new vertical-pane%
       [parent climb-pane]
       [border 0]
       [spacing 20]
       [alignment '(left top)]
       [stretchable-height #f]
       [stretchable-width #f]))

(define climb-detection-slider
  (new (tooltip-mixin slider%)
       [parent climb-controls-pane]
       [label "Climb Detection Sensitivity "]
       [style '(horizontal plain)]
       [init-value
        (get-pref 'al2-climb-analysis-tool:elevation-smoothing
                  (exact-round (* *rdp-epsilon* 100)))]
      [min-value 50]
       [max-value 500]
       [callback (lambda (c e) (on-rdp-epsilon-changed (/ (send c get-value) 100)))]))

(define min-climb-grade-field
  (new (decorate-mixin
        (decorate-with " %" #:validate string->number)
        (validate-mixin
         string->number
          (lambda (v) (~r v #:precision 1))
          (tooltip-mixin text-field%)
          #:allow-empty? #f))
       [parent climb-controls-pane]
       [label "Min Climb Grade "]
       [init-value
        (let [(v (get-pref 'al2-climb-analysis-tool:min-grade *min-climb-grade*))]
          (~r (if (rational? v) v *min-climb-grade*) #:precision 1))]
       [tooltip "Minimum Grade for a Segment to be considered a climb"]
       [callback (lambda (b e) (on-climb-parameters-changed))]))

(define min-climb-score-field
  (new (validate-mixin
         string->number
         (lambda (v) (~r v #:precision 0))
         (tooltip-mixin text-field%)
         #:allow-empty? #f)
       [parent climb-controls-pane]
       [label "Min Climb Score "]
       [init-value
        (let [(v (get-pref 'al2-climb-analysis-tool:min-score *min-climb-score*))]
          (~r (if (rational? v) v *min-climb-score*) #:precision 2))]
       [tooltip "Climbs with scores less than this are discarded"]
       [callback (lambda (b e) (on-climb-parameters-changed))]))

(define join-nearby-checkbox
  (new (tooltip-mixin check-box%)
       [parent climb-controls-pane]
       [label "Join Nearby Climbs "]
       [value (get-pref 'al2-climb-analysis-tool:join-nearby? #f)]
       [tooltip "Climbs separated by short flats or descents should be joined"]
       [callback (lambda (c e) (on-join-nearby-climbs (send c get-value)))]))

(define nearby-distance-field
  (new (decorate-mixin
        (decorate-with " km" #:validate string->number)
        (validate-mixin
         string->number
         (lambda (v) (~r v #:precision 0))
         (tooltip-mixin text-field%)
         #:allow-empty? #f))
       [parent climb-controls-pane]
       [label "Nearby Distance "]
       [init-value
        (let [(v (get-pref 'al2-climb-analysis-tool:nearby-distance *max-climb-separation*))]
          (~r (if (rational? v) v *max-climb-separation*) #:precision 2))]
       [enabled (send join-nearby-checkbox get-value)]
       [tooltip "Maximum distance for joining nearby climbs"]
       [callback (lambda (b e) (on-climb-parameters-changed))]))

(define athlete-weight
  (new (decorate-mixin
        (decorate-with " kg" #:validate string->number)
        (validate-mixin
         string->number
         (lambda (v) (~r v #:precision 1))
         (tooltip-mixin text-field%)
         #:allow-empty? #f))
       [parent climb-controls-pane]
       [label "Athlete + Bike Weight "]
       [init-value
        (let [(v (get-pref 'al2-climb-analysis-tool:athlete-weight *athlete-weight*))]
          (~r (if (rational? v) v *athlete-weight*) #:precision 1))]
       [tooltip "Athlete + Bike Weight (total weight)"]
       [callback (lambda (b e) (on-athlete-weight-changed))]))

(define map-and-plot-panel
  (new vertical-dragable-split-panel%
       [parent p0]))

(define map-pane
  (new vertical-pane%
       [parent map-and-plot-panel]
       [border 0]
       [spacing 0]))

(define map-controls-pane
  (new horizontal-pane%
       [parent map-pane]
       [border 5]
       [spacing 20]
       [stretchable-height #f]))

(define track-location-check-box
  (new (tooltip-mixin check-box%)
       [parent map-controls-pane]
       [label "Track Location"]
       [tooltip "Keep selected map location in view"]
       [value (get-pref 'al2-climb-analysis-tool:track-location? #f)]
       [callback (lambda (c e) (on-track-location (send c get-value)))]))

(define zoom-to-selection-check-box
  (new (tooltip-mixin check-box%)
       [parent map-controls-pane]
       [label "Zoom to Selection"]
       [tooltip "Zoom map and plot to selected climb"]
       [value (get-pref 'al2-climb-analysis-tool:zoom-to-selection? #f)]
       [callback (lambda (c e) (on-zoom-to-selection (send c get-value)))]))

(define zoom-slider
  (new (tooltip-mixin slider%)
       [parent map-controls-pane]
       [label "Map Zoom"]
       [tooltip "Select the map zoom level"]
       [style '(horizontal plain)]
       [min-value (min-zoom-level)]
       [max-value (max-zoom-level)]
       [callback (lambda (z e) (on-zoom-level-change (send z get-value)))]))

(define fit-to-window-button
  (new (tooltip-mixin button%)
       [parent map-controls-pane]
       [label "Fit to Window"]
       [tooltip "Center and zoom map so entire track is visible"]
       [callback (lambda (b e) (on-fit-to-window))]))

(define the-map
  (new (class map-widget% (init) (super-new)
         (define/override (on-zoom-level-change zl)
           (send zoom-slider set-value zl)))
       [parent map-pane]))

(define the-plot-container
  (new plot-container%
       [parent map-and-plot-panel]))


;;................................ Further Initialization Of The Widgets ....

;; Setup the columns to be used by the climbs view, along with functions to
;; access the information from the climb structures and format it for display.
(send climbs-view setup-column-defs
      (list
       (qcolumn
        "Start (km)"
        (lambda (c) (~r (climb-start c) #:precision 1))
        climb-start)
       (qcolumn
        "End (km)"
        (lambda (c) (~r (climb-end c) #:precision 1))
        climb-end)
       (qcolumn
        "Length (km)"
        (lambda (c) (~r (climb-distance c) #:precision 1))
        climb-distance)
       (qcolumn
        "Ascent (m)"
        (lambda (c) (~r (climb-elevation c) #:precision 0))
        climb-elevation)
       (qcolumn
        "Avg Grade (%)"
        (lambda (c) (~r (climb-grade c) #:precision 1))
        climb-grade)
       (qcolumn
        "Max Grade (%)"
        (lambda (c) (~r (climb-max-grade c) #:precision 1))
        climb-max-grade)
       (qcolumn
        "Score"
        (lambda (c) (f->s (climb-score c)))
        climb-score)
       (let ([energy
              (lambda (c)
                (define weight (send athlete-weight get-value/validated))
                (if (rational? weight)
                    (/ (* weight (climb-energy c)) 1000.0)
                    ""))])
         (qcolumn
          "Energy (kJ)"
          (lambda (c) (~r (energy c) #:precision 2))
          energy))))

;; Setup the defaults for the map: track location is synchronized with the
;; track location check box value, and the line with and color of the selected
;; track is setup here.  The ZORDER of the selected track is also set up so it
;; is drawn on top of the main track.

(define the-cll
  (current-location-layer
   'curent-location
   #:track-current-location? (send track-location-check-box get-value)))
(send the-map add-layer the-cll)

;; Sync the value of the zoom slider with the current map zoom level.
(send zoom-slider set-value (send the-map zoom-level))

;; Set up a default message for the plot container when no plots are shown
(send the-plot-container set-background-message "No GPX File Loaded")

;; Setup the split percentages of the two panels of the application, either
;; restore previous values or set default ones.
(send p0 set-percentages
      (get-pref 'al2-climb-analysis-tool:climbs-map-split '(1/5 4/5)))
(send map-and-plot-panel set-percentages
      (get-pref 'al2-climb-analysis-tool:map-plot-split '(2/3 1/3)))

;; If the application frame was maximized last time, maximize the fame now.
(define maximized? (get-pref 'al2-climb-analysis-tool:frame-maximized #f))
(when maximized?
  (send toplevel maximize maximized?))

;; Finally, show the application window.
(send toplevel show #t)


;;.......................................... Implement the GUI Callbacks ....

(define df #f)                  ; the data frame for the currently shown route
(define the-plot #f)           ; the elevation plot for the current data frame

;; Create the elevation plot, zooming on the selected climb (if needed), setup
;; the mouse event callback and add it to the plot container to be displayed
;; in the application window.  The resulting plot is also stored in `the-plot`
;; global variable.
(define (construct-the-plot selected-climb zoom?)
  (define-values (w h) (send the-plot-container cell-dimensions 1))
  (set! the-plot
        (parameterize ([plot-x-label "Distance (km)"]
                       [plot-y-label "Elevation (meters)"]
                       [plot-x-transform
                        (if (and selected-climb zoom?)
                            (stretch-transform (climb-start selected-climb)
                                               (climb-end selected-climb)
                                               30)
                            id-transform)])
          (plot-snip (list (make-climb-renderers df)
                           (make-grade-color-renderers df))
                     #:width w #:height h)))
  (send the-plot set-mouse-event-callback (make-plot-callback df the-cll))
  (send the-plot set-overlay-renderers additional-renderers)
  (send the-plot-container set-snips the-plot))

;; Export the selected climb segment as a GPX file, that is export the
;; contents of the data frame for the selected segment.  This is a callback
;; for when the user right-clicks on the climbs list and selects the
;; "export..."  menu.
(define (on-export-climb-segment index)
  (when df
    (let ([path
           (get-file "Save segment as GPX file"
                     toplevel #f #f #f '()
                     '(("GPX Files" "*.gpx")
                       ("Any" "*.*")))])
      (when path
        (let ([climbs (df-get-property df 'climbs)])
          (when (< index (length climbs))
            (let ([c (list-ref climbs index)])
              (match-define (list b e)
                (df-index-of* df "dst/km" (climb-start c) (climb-end c)))
              ;; Also export the last point
              (when (< e (df-row-count df))
                (set! e (add1 e)))
              (df-write/gpx df path #:start b #:stop e #:name "GPX Climb Segment")))))))
  (void))

;; Callback when the user checks/unchecks the join-nearby-checkbox.  Will
;; enable or disable the nearby-distance-field and recalculate the climbs by
;; calling `on-climb-parameters-changed`)
(define (on-join-nearby-climbs flag)
  (send nearby-distance-field enable flag)
  (on-climb-parameters-changed))

;; Callback when the user changes the climb sensitivity.  Calculates new raw
;; climbs with the updated RDP epsilon as well as new climbs.
(define (on-rdp-epsilon-changed new-epsilon)
  (when df
    (add-raw-climbs df #:epsilon new-epsilon)
    (on-climb-parameters-changed)))

;; Called for any change in any of the parameters which affect how climbs are
;; created from raw-climbs.  Will recalculate the new climbs and show them in
;; the climbs view.
(define (on-climb-parameters-changed)
  (when df
    (let ([min-grade (send min-climb-grade-field get-value/validated)]
          [min-score (send min-climb-score-field get-value/validated)]
          [should-join? (send join-nearby-checkbox get-value)]
          [max-join-distance (send nearby-distance-field get-value/validated)])
      (if (and (rational? min-grade) (rational? min-score)
               (or (not should-join?) (rational? max-join-distance)))
          (add-climbs df #:min-grade min-grade #:min-score min-score
                      #:max-join-distance (and should-join? max-join-distance))
          (df-put-property! df 'climbs '())))
    (let ([climbs (df-get-property df 'climbs)])
      (when climbs
        (send climbs-view set-data climbs)))
    (set! additional-renderers '())
    (construct-the-plot #f (send zoom-to-selection-check-box get-value))))

(define (on-athlete-weight-changed)
  (let ([climbs (df-get-property df 'climbs)])
      (when climbs
        (send climbs-view set-data climbs))))

;; Called when the user checks/unchecks the track-location-check-box widget,
;; sends the information to the map widget.
(define (on-track-location track?)
  (send the-cll track-current-location track?))

;; Called when the user changes the zoom level using the zoom-slider.  Sends
;; the new zoom level to the map widget.
(define (on-zoom-level-change zl)
  (send the-map zoom-level zl))

;; Called when the user checks/unchecks the zoom-to-selection-check-box.
;; Constructs a new plot with the selection zoomed and also "focuses" the map
;; view on the selected section.
(define (on-zoom-to-selection zoom?)
  (when df                              ; if a data-frame has been loaded
    (define climb
      (let ([index (send climbs-view get-selected-row-index)])
        (and index (send climbs-view get-data-for-row index))))
    (construct-the-plot climb zoom?)
    (when zoom?
      (send the-map resize-to-fit 'selected-track))))

;; Called when the user presses the fit-to-window-button.  Calls
;; `resize-to-fit` on the map widget
(define (on-fit-to-window)
  (send the-map resize-to-fit)
  (send the-map center-map))

;; Calculate the total corrected ascent and descent in the data frame DF
;; between START-INDEX and END-INDEX using the "alt" series.  Returns a (cons
;; ASCENT DESCENT)
;;
;; NOTE: this is duplicated from "intervals.rkt" - it is here to avoid pulling
;; in most of the modules from the rest of the application.  Perhahs this
;; should be in a common file.
(define (total-ascent-descent df series start-index end-index)

  ;; NOTE: we only accumulate ascent and descent if the elevation gain or loss
  ;; is greater than 1 meter -- this avoids accumulating lots of very small
  ;; elevation changes, which would artificially inflate the total elevation
  ;; gain.

  (match-define
    (list ascent descent _)
    (df-fold df series
             '(0 0 #f)
             (lambda (accum val)
               (match-define (list alt) val)
               (if alt
                   (match-let ([(list ascent descent current) accum])
                     (cond ((equal? current #f)
                            (list ascent descent alt))
                           ((> alt (add1 current))
                            (list (+ ascent (- alt current)) descent alt))
                           ((< alt (sub1 current))
                            (list ascent (+ descent (- current alt)) alt))
                           (#t
                            accum)))
                   accum))
             #:start start-index #:stop end-index))
  (values ascent descent))

;; Called when the user presses the load-gpx-button.  Loads a new data frame
;; from the specified GPX file and creates the climbs + plot.
(define (on-load-course file-name)
  (send the-map remove-layer 'main-track)
  (send the-map remove-layer 'selected-track)
  (set! df (load-course file-name))
  (when df
    (let ([rdp-epsilon (/ (send climb-detection-slider get-value) 100)])
      (add-raw-climbs df #:epsilon rdp-epsilon))
    (on-climb-parameters-changed)

    (let ([waypoints (df-select* df "lat" "lon" #:filter valid-only)])
      (send the-map add-layer (line-layer 'main-track waypoints)))
    (send the-map center-map)
    (send the-map resize-to-fit)

    (define name
      (or (df-get-property df 'name)
          (if (path? file-name)
              (path->string file-name)
              file-name)))
    (define distance
      (let ([dst (df-ref df (sub1 (df-row-count df)) "dst")])
        (if (rational? dst)
            (string-append (~r (/ dst 1000.0) #:precision 1) " km")
            "")))
    (define-values (ascent descent)
      (let-values ([(a d) (total-ascent-descent df "alt" 0 (df-row-count df))])
        (values (string-append "ascent " (~r a #:precision 0) " meters")
                (string-append "descent " (~r a #:precision 0) " meters"))))
    (send activity-name-message set-label
          (format "~a -- ~a, ~a, ~a" name distance ascent descent))
    (send toplevel set-label (format "~a -- Climb Analysis Tool" name))))

;; Called when a new climb is selected in the climbs-view.  Selects the climb
;; on the map and plot and zooms to selection if needed.
(define (on-climb-selected c)
  (define zoom? (send zoom-to-selection-check-box get-value))
  (match-define (list start stop)
    (df-index-of* df "dst/km" (climb-start c) (climb-end c)))
  (set! stop (min (add1 stop) (df-row-count df)))
  (define track (df-select* df "lat" "lon" #:filter valid-only #:start start #:stop stop))
  (send the-map add-layer (line-layer 'selected-track track
                                      #:pen selected-track-pen
                                      #:zorder 0.4))
  (when zoom?
    (send the-map resize-to-fit 'selected-track))
  (define alt (df-select* df "dst/km" "alt" #:filter valid-only #:start start #:stop stop))
  (set! additional-renderers (list (lines alt #:color selected-climb-color #:width 4)))
  (if zoom?
      (construct-the-plot c zoom?)
      ;; NOTE: set-overlay-renderers is already called by `construct-the-plot`
      (send the-plot set-overlay-renderers additional-renderers)))

(define (put-preferences0 names values)
  (for ([n (in-list names)]
        [v (in-list values)])
    (put-pref n v)))

;; Called when the application window is closed.  Saves the state of all GUI
;; widgets to the preferences file, so the same values are used when the
;; application is started again.
(define (save-preferences)
  (send climbs-view save-visual-layout)
  ;; NOTE: we only save the frame dimensions if the frame is not maximized,
  ;; otherwise we will save the maximized dimensions.
  (unless (or (send toplevel is-maximized?) (send toplevel is-fullscreened?))
    (let-values (([w h] (send toplevel get-size)))
      (put-preferences0 '(al2-climb-analysis-tool:frame-dimensions) (list (cons w h)))))
  (put-preferences0
   '(al2-climb-analysis-tool:frame-maximized
     al2-climb-analysis-tool:climbs-map-split
     al2-climb-analysis-tool:map-plot-split

     al2-climb-analysis-tool:track-location?
     al2-climb-analysis-tool:zoom-to-selection?
     al2-climb-analysis-tool:elevation-smoothing
     al2-climb-analysis-tool:min-grade
     al2-climb-analysis-tool:min-score
     al2-climb-analysis-tool:join-nearby?
     al2-climb-analysis-tool:nearby-distance
     al2-climb-analysis-tool:athlete-weight)
   (list
    (send toplevel is-maximized?)
    (send p0 get-percentages)
    (send map-and-plot-panel get-percentages)

    (send track-location-check-box get-value)
    (send zoom-to-selection-check-box get-value)
    (send climb-detection-slider get-value)
    (send min-climb-grade-field get-value/validated)
    (send min-climb-score-field get-value/validated)
    (send join-nearby-checkbox get-value)
    (send nearby-distance-field get-value/validated)
    (send athlete-weight get-value/validated))))

;; Allow the user to specify a filename on the command line.  If present, we
;; load that file.
(command-line
 #:program "Al2-Climb-Analysis"
 #:args filenames
 (cond ((null? filenames)
        ;; Start the application with no course selected
        (void))
       ((> (length filenames) 1)
        (fprintf (current-error-port) "Only one file can be specified in on the command line~%")
        (exit 1))
       (else
       (let ([course-file (car filenames)])
         (unless (file-exists? course-file)
           (fprintf (current-error-port) "File ~a does not exist~%" course-file)
           (exit 1))
         (on-load-course course-file)))))
