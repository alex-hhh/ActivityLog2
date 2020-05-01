;; sport-zone.rkt -- utilities for working with sport zones
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2020 Alex Harsányi <AlexHarsanyi@gmail.com>
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

#lang racket/base
(require racket/contract
         racket/class
         racket/draw
         racket/match
         racket/math
         racket/format
         pict
         db
         "../dbapp.rkt"
         "../color-theme.rkt"
         "../fmt-util.rkt"
         "../fmt-util-ut.rkt")


;;......................................................... sz structure ....

;; Guard function for the sport zone (SZ) structure.  Ensures that the struct
;; is created with reasonable values.
(define/contract (sz-guard sport sub-sport
                           metric
                           boundaries names colors
                           valid-from valid-until
                           id struct-name)
  (-> exact-positive-integer?           ; sport id from E_SPORT
      (or/c exact-positive-integer? #f) ; sub-sport id from E_SUB_SPORT, or #f for no sub-sport
      (or/c 'pace 'heart-rate 'power)   ; one of the metrics
      (vectorof number?)                  ; data
      (vectorof string?)                  ; names (same length as data)
      (vectorof (is-a?/c color%))         ; colors, same length as data
      integer?                            ; valid-from, unix timestamp
      (or/c integer? #f)                  ; #f means no limit on validity
      (or/c exact-positive-integer? #f) ; SPORT_ZONE.id or #f if none yet set.
      any/c
      any)
  (define zone-count (vector-length boundaries))
  (when (< zone-count 2)
    (error (format "sz-guard (~a): no zones defined (must have at least 2 boundaries)" struct-name)))
  (when (and names (not (= zone-count (vector-length names))))
    (error (format "sz-guard (~a): NAMES length does not match BOUNDARIES length" struct-name)))
  (when (and colors (not (= zone-count (vector-length colors))))
    (error (format "sz-guard (~a): COLORS length does not match BOUNDARIES length" struct-name)))
  (when (and valid-until (< valid-until valid-from))
    (error (format "sz-guard (~a): VALID-UNTIL must be after VALID-FROM" struct-name)))
  (values sport sub-sport metric boundaries names colors valid-from valid-until id))

;; Struct to hold information about a sport zone.
(struct sz (sport sub-sport metric boundaries names colors valid-from valid-until id)
  #:transparent #:guard sz-guard)

;; Return the number of sport zones in SZ
(define (sz-count sz)
  (vector-length (sz-boundaries sz)))


;;...................................... reading zones from the database ....

;; Convert a symbolic metric name (heart-rate, pace or power) into the ID used
;; in the database for that metric.
(define (metric->id metric)
  (case metric
    ((heart-rate) 1)
    ((pace speed) 2)
    ((power) 3)))

;; Convert a metric ID (as used in the database) into a symbolic metric name.
(define (id->metric id)
  (case id
    ((1) 'heart-rate)
    ((2) 'pace)
    ((3) 'power)))

;; Return the ZONE-ID for the sport zone that applies to the session SID and
;; the zone METRIC.  Can return #f if no sport zones apply.
(define (get-zone-id-for-session sid metric db)
  (define q1
    "select zone_id from V_SPORT_ZONE_FOR_SESSION where session_id = ? and zone_metric_id = ?")
  (query-maybe-value db q1 sid (metric->id metric)))

;; Return the ZONE-ID which applies to the SPORT/SUB-SPORT combination for the
;; specified METRIC
(define (get-zone-id-for-sport sport sub-sport metric db)
  (define q1 "
select id, max(valid_from) from SPORT_ZONE
 where sport_id = ? and sub_sport_id = ? and zone_metric_id = ?")
  (define q2 "
select id, max(valid_from) from SPORT_ZONE
 where sport_id = ? and sub_sport_id is null and zone_metric_id = ?")
  (define result
    (if sub-sport
        (query-maybe-row db q1 sport sub-sport (metric->id metric))
        (query-maybe-row db q2 sport (metric->id metric))))
  (and result
       (let ([id (vector-ref result 0)])
         (if (sql-null? id) #f id))))

(define fetch-zone-definition-query
  (virtual-statement
   (lambda (dbsys)
     "select coalesce(zone_name, 'Zone ' || zone_number), zone_value
        from SPORT_ZONE_ITEM
       where sport_zone_id = ?
    order by zone_number")))

(define fetch-zone-info-query
  (virtual-statement
   (lambda (dbsys)
     "select sport_id, sub_sport_id, zone_metric_id, valid_from, valid_until
        from V_SPORT_ZONE
      where zone_id = ?")))

;; Read ZONE-ID from the database DB and return a SZ struct containing the
;; data about this sport zone.
(define (read-zone zone-id db)
  (let ([zinfo (query-row db fetch-zone-info-query zone-id)]
        [qresult (query-rows db fetch-zone-definition-query zone-id)])
    (match-define (vector sport sub-sport metric valid-from valid-until) zinfo)
    (define boundaries (for/vector ([item (in-list qresult)]) (vector-ref item 1)))
    (define names (for/vector ([item (in-list qresult)]) (vector-ref item 0)))
    (define colors (for/vector ([color (in-list (zone-colors))]
                                [index (in-range (vector-length boundaries))])
                     (cdr color)))
    (sz sport
        (if (sql-null? sub-sport) #f sub-sport)
        (id->metric metric)
        boundaries
        names
        colors
        valid-from (if (> valid-until (current-seconds)) #f valid-until)
        zone-id)))

;; Return the sport zone corresponding to the session id SID and the
;; METRIC.  Returns #f if there are no such sport zones.
;;
;; NOTE that this function returns the sport zones which were in effect at the
;; time the session was recorded, and may not be the latest sport zones for
;; this metric.
;;
;; DB is the database connection, and default to the current database.
(define (sport-zones-for-session sid metric #:database (db (current-database)))
  (define zone-id (get-zone-id-for-session sid metric db))
  (and zone-id (read-zone zone-id db)))

;; Return a list of all defined sport zones for the session id SID.
;;
;; DB is the database connection, and default to the current database.
(define (all-sport-zones-for-session sid #:database (db (current-database)))
  (filter
   values
   (for/list ([metric (in-list '(pace power heart-rate))])
     (sport-zones-for-session sid metric #:database db))))

;; Return the most recent (currently valid) zones based on METRIC, or return
;; #f if no sport zones are defined.
;;
;; DB is the database connection, and default to the current database.
(define (sport-zones-for-sport sport sub-sport metric #:database (db (current-database)))
  (define zone-id (get-zone-id-for-sport sport sub-sport metric db))
  (and zone-id (read-zone zone-id db)))

(define (value->zone sz value)
  (define boundaries (sz-boundaries sz))
  (define index
    (for/first ([(e idx) (in-indexed (in-vector boundaries))] #:when (>= e value))
      idx))
  (if index
      (if (> index 0)
          (let ([low (vector-ref boundaries (sub1 index))]
                [high (vector-ref boundaries index)])
            (+ (sub1 index) (exact->inexact (/ (- value low) (- high low)))))
          0)
      (vector-length boundaries)))

;; Convert VALUE to a percentage of the maximum value as defined in the sport
;; zone SZ.
(define (value->pct-of-max sz value)
  (define boundaries (sz-boundaries sz))
  (define max (vector-ref boundaries (sub1 (vector-length boundaries))))
  (* (/ value max) 100.0))

;; Return the name of the ZONE (a number), as defined by the sport zones SZ.
;; Note that ZONE can be a fractional number (e.g. 2.5) in which case, the
;; function will return the name of zone 2.
(define (zone->zone-name sz zone)
  (define names (sz-names sz))
  (if names
      (vector-ref names (min (max (exact-truncate zone) 0) (sub1 (vector-length names))))
      (format "Zone ~a" (exact-truncate zone))))

(define zone-labels #(z0 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10))

;; Return a label corresponding to ZONE.  This is deprecated and new code
;; should not use this function
(define (zone->label zone)
  (define index (max 0 (min (sub1 (vector-length zone-labels)) (exact-truncate zone))))
  (vector-ref zone-labels index))


;;................................................. formatting utilities ....

(define (heart-rate->string/bpm bpm)
  (format "~a bpm" (exact-truncate bpm)))

(define (heart-rate->string/pct bpm zones)
  (let ((pct (value->pct-of-max zones bpm)))
    (format "~a% of Max" (exact-truncate pct))))

(define (heart-rate->string/zone bpm zones)
  (let ((zone (value->zone zones bpm)))
    (format "~az" (~r zone #:precision 1))))

(define (heart-rate->string/full bpm [zones #f])
  (if zones
      (format "~a (~a, ~a)"
              (heart-rate->string/bpm bpm)
              (heart-rate->string/pct bpm zones)
              (heart-rate->string/zone bpm zones))
      (heart-rate->string/bpm bpm)))


;;...................................................... pretty-printing ....

;; Return the validity range of the sport zones SZ as a string.  Used for
;; formatting sport zone values.
(define (validity-range->string sz)
  (if (sz-valid-until sz)
      (format "Valid from ~a to ~a"
              (calendar-date->string (sz-valid-from sz))
              (calendar-date->string (sz-valid-until sz)))
      (format "Valid from ~a" (calendar-date->string (sz-valid-from sz)))))

(define (default-format-function v)
  (~a (exact-round v)))

;; Construct a formatting function based on SPORT (a sport-id) and METRIC, or
;; use DEFAULT.  If DEFAULT is the symbol 'default, an appropriate formatting
;; function is constructed (e.g. one that correctly formats pace values taking
;; sport and measurement system into consideration).
(define (format-function sport metric default)
  (if (equal? default 'default)
      (if (equal? metric 'pace)
          (case sport
            ((5) swim-pace->string)
            ((1) pace->string)
            (else default-format-function))
          default-format-function)
      (if default default default-format-function)))

;; Returns a pict object containing a nice graphical representation of ZONES
;; (a SZ struct).  This version shows the zones in a more compact form than
;; `pp-sport-zones/pict`
;;
;; The resulting PICT object can be used in GUI application (usually by
;; drawing it to a canvas%), but it can also be converted to PNG or SVG
;; images.
;;
;; WIDTH and HEIGHT are the dimensions of the picture
;;
;; FMT-FN is a function used to convert zone values to string.  if it is
;; 'default, an appropriate function will be chosen based on the sport and
;; zone metric.
(define (pp-sport-zones/compact-pict
         sz
         #:format-value (fmt-fn 'default)
         #:width (width 600)
         #:height (height 60))

  (define the-fmt-fn (format-function (sz-sport sz) (sz-metric sz) fmt-fn))

  (define start-pad 30)
  (define end-pad 30)
  (define top-border (exact-round (* height 0.25)))
  (define bottom-border (exact-round (* height 0.25)))

  ;; Height of the zone band
  (define zheight (- height top-border bottom-border))
  ;; Total width of the zone band
  (define zwidth (- width end-pad start-pad))
  (define zone-count (sz-count sz))
  (define zslot-width (/ zwidth (sub1 zone-count)))

  (define the-pict
    (vc-append
     (filled-rectangle width top-border #:draw-border? #f #:color "white")
     (apply
      hc-append
      (for/list ((index (in-range (sub1 zone-count)))
                 (color (in-vector (sz-colors sz))))
        (define label
          (let* ((name (zone->zone-name sz index))
                 (candidate (text name)))
            (if (< (pict-width candidate) zslot-width)
                candidate
                (for/or ([text-len (in-range (string-length name) 0 -1)])
                  (let ((ntext (string-append (substring name 0 text-len) "...")))
                    (let ((ncandidate (text ntext)))
                      (if (< (pict-width ncandidate) zslot-width)
                          ncandidate
                          #f)))))))
        (cc-superimpose
         (filled-rectangle zslot-width zheight #:draw-border? #f #:color color)
         label)))
     (filled-rectangle width bottom-border #:draw-border? #f #:color "white")))

  (for/fold ([the-pict the-pict])
            ([(boundary index) (in-indexed (in-vector (sz-boundaries sz)))])
    (define label (text (the-fmt-fn boundary)))
    (pin-over the-pict
              (- (+ start-pad (* index zslot-width)) (/ (pict-width label) 2))
              (if (even? index) 0 (+ top-border zheight))
              label)))

;; Returns a pict object containing a nice graphical representation of ZONES
;; (a SZ struct).  This version shows the zones in a table, with an optional
;; title and validity range.
;;
;; The resulting PICT object can be used in GUI application (usually by
;; drawing it to a canvas%), but it can also be converted to PNG or SVG
;; images.
;;
;; TITLE is an optional title to add to the pict.  If it is #f, no title will
;; be added, the value 'default will add an appropriate title based on sport
;; and zone metric.  Any other string will be displayed as the title.
;;
;; SHOW-VALIDITY-RANGE? determines whether the validity range of the sport
;; zones will be displayed under the title
;;
;; FMT-FN is a function used to convert zone values to string.  if it is
;; 'default, an appropriate function will be chosen based on the sport and
;; zone metric.

(define (pp-sport-zones/pict
         sz
         #:title (title 'default)
         #:show-validity-range? (show-validity-range? #t)
         #:format-value [fmt-fn 'default])

  (define the-title
    (if (equal? title 'default)
        (case (sz-metric sz)
          ((heart-rate) "Heart Rate Zones (BPM)")
          ((power) "Power Zones (Watts)")
          ((pace) (format "Pace Zones (~a)"
                          (if (equal? (sz-sport sz) 5) swim-pace-label pace-label))))
        title))

  (define the-fmt-fn (format-function (sz-sport sz) (sz-metric sz) fmt-fn))

  (define item-color (make-object color% #x2f #x4f #x4f))
  (define label-color (make-object color% #x77 #x88 #x99))

  (define item-font (send the-font-list find-or-create-font 14 'default 'normal 'normal))
  (define label-font (send the-font-list find-or-create-font 12 'default 'italic 'light))

  (define item-face (cons item-color item-font))
  (define label-face (cons label-color label-font))

  (define title-font (send the-font-list find-or-create-font 16 'default 'normal 'normal))
  (define title-face (cons item-color title-font))

  (define items
    (for/fold ([result (list (text "Color" item-face)
                             (text "Upper" item-face)
                             (text "Lower" item-face)
                             (text "Zone" item-face))])
              ([start (in-vector (sz-boundaries sz))]
               [end (in-vector (sz-boundaries sz) 1)]
               [color (in-vector (sz-colors sz))]
               [name (in-vector (sz-names sz))])
      (list* (colorize (disk 20) color)
             (text (the-fmt-fn end) item-face)
             (text (the-fmt-fn start) item-face)
             (text name label-face)
             result)))

  ;; NOTE: the last item in the zone definition is the maximum value, and it
  ;; is usually a bogus one, the zones are "in betweeen" the values, so we
  ;; drop the last value and replace it with the string "max"

  (match-define (list-rest cpict epict rest) items)
  (define nitems (list* cpict (text "max" item-face) rest))

  (define zones-table
    (table 4
           (reverse nitems)
           (list lc-superimpose rc-superimpose rc-superimpose cc-superimpose)
           cc-superimpose
           20
           10))

  (define title-pict (and the-title (text the-title title-face)))
  (define validity-pict (and show-validity-range?
                             (text (validity-range->string sz) label-face)))
  (define title+validity-pict
    (cond ((and title-pict validity-pict)
           (vc-append 5 title-pict validity-pict))
          (title-pict)
          (validity-pict)
          (#t #f)))

  (if title+validity-pict
      (vc-append 15 title+validity-pict zones-table)
      zones-table))

;; Write a pretty-printed version of the sport zones SZ to the current output
;; port.
;;
;; TITLE is an optional title to add to the pict.  If it is #f, no title will
;; be added, the value 'default will add an appropriate title based on sport
;; and zone metric.  Any other string will be displayed as the title.
;;
;; SHOW-VALIDITY-RANGE? determines whether the validity range of the sport
;; zones will be displayed under the title
;;
;; FMT-FN is a function used to convert zone values to string.  if it is
;; 'default, an appropriate function will be chosen based on the sport and
;; zone metric.
(define (pp-sport-zones
         sz
         #:title (title 'default)
         #:show-validity-range? (show-validity-range? #t)
         #:format-value [fmt-fn 'default])

  (define the-title
    (if (equal? title 'default)
        (case (sz-metric sz)
          ((heart-rate) "Heart Rate Zones (BPM)")
          ((power) "Power Zones (Watts)")
          ((pace) (format "Pace Zones (~a)"
                          (if (equal? (sz-sport sz) 5) swim-pace-label pace-label))))
        title))

  (when the-title
    (printf "~a~%" the-title))
  (when show-validity-range?
    (printf "~a~%" (validity-range->string sz)))

  (define the-fmt-fn (format-function (sz-sport sz) (sz-metric sz) fmt-fn))

  (define items
    (for/fold ([result (list (list "Zone" "Lower" "Upper"))])
              ([start (in-vector (sz-boundaries sz))]
               [end (in-vector (sz-boundaries sz) 1)]
               [color (in-vector (sz-colors sz))]
               [name (in-vector (sz-names sz))])
      (cons (list name (the-fmt-fn start) (the-fmt-fn end)) result)))

  ;; NOTE: the last item in the zone definition is the maximum value, and it
  ;; is usually a bogus one, the zones are "in betweeen" the values, so we
  ;; drop the last value and replace it with the string "max"
  (match-define (list name start end) (car items))
  (define nitems (cons (list name start "max") (cdr items)))
  (define-values (cw-name cw-low cw-high)
    (for/fold ([cw-name 0] [cw-low 0] [cw-high 0])
              ([item (in-list items)])
      (match-define (list name low high) item)
      (values (max cw-name (string-length name))
              (max cw-low (string-length low))
              (max cw-high (string-length high)))))

  (for ([item (in-list (reverse items))])
    (match-define (list name low high) item)
    (write-string "\n")
    (write-string (~a name #:width cw-name #:align 'left))
    (write-string " ")
    (write-string (~a low #:width cw-low #:align 'right))
    (write-string " ")
    (write-string (~a high #:width cw-high #:align 'right))))


;;............................................................. provides ....

(provide (struct-out sz))
(provide/contract
 (metric->id (-> symbol? exact-nonnegative-integer?))
 (id->metric (-> exact-nonnegative-integer? symbol?))

 (sport-zones-for-session (->* (exact-nonnegative-integer?
                                (or/c 'heart-rate 'pace 'power))
                               (#:database connection?)
                               (or/c #f sz?)))
 (all-sport-zones-for-session (->* (exact-nonnegative-integer?)
                                   (#:database connection?)
                                   (listof sz?)))
 (sport-zones-for-sport (->* (exact-nonnegative-integer?
                              (or/c #f exact-nonnegative-integer?)
                              (or/c 'heart-rate 'pace 'power))
                             (#:database connection?)
                             (or/c #f sz?)))
 (value->zone (-> sz? real? real?))
 (value->pct-of-max (-> sz? real? real?))
 (zone->zone-name (-> sz? real? string?))
 (zone->label (-> real? symbol?))
 (sz-count (-> sz? exact-integer?))

 (heart-rate->string/bpm (-> real? string?))
 (heart-rate->string/pct (-> real? sz? string?))
 (heart-rate->string/zone (-> real? sz? string?))
 (heart-rate->string/full (-> real? (or/c #f sz?) string?))

 (pp-sport-zones/compact-pict (->* (sz?)
                                 (#:width (and/c real? positive?)
                                  #:height (and/c real? positive?)
                                  #:format-value (or/c (-> real? string?) 'default))
                                 pict?))
 (pp-sport-zones/pict (->* (sz?)
                           (#:format-value (or/c (-> real? string?) 'default)
                            #:title (or/c #f string? 'default)
                            #:show-validity-range? boolean?)
                           pict?))
 (pp-sport-zones (->* (sz?)
                      (#:format-value (or/c (-> real? string?) 'default)
                       #:title (or/c #f string? 'default)
                       #:show-validity-range? boolean?)
                      any/c))
 )
