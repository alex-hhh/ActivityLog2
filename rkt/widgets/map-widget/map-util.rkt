#lang typed/racket/base
;; map-util.rkt -- map utilities
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

(require math/base
         racket/flonum
         racket/math)

(provide map-distance/radians
         map-distance/degrees
         map-bearing/radians
         map-bearing/degrees
         (struct-out map-point)
         lat-lon->map-point
         map-point->lat-lon
         track-bbox
         bbox-center
         bbox-center/ndcs
         bbox-size
         bbox-merge
         (struct-out map-bbox)
         (struct-out map-tile)
         map-tile-equal?
         degrees->wind-rose
         map-widget-logger)

;; Logger used by the map widget to send various notification messages.  The
;; application should define a log-receiver to receive and display the
;; messages.
(define map-widget-logger (make-logger 'map-widget (current-logger)))


;;.................................... distance and bearing calculations ....

;; Formulas from http://www.movable-type.co.uk/scripts/latlong.html

(define earth-radius (->fl 6371000))    ; meters

(: haversin (-> Float Float))
(define (haversin theta)
  (fl/ (fl- 1.0 (flcos theta)) 2.0))

(: inv-haversin (-> Float Float))
(define (inv-haversin h)
  (fl* 2.0 (flasin (flsqrt h))))

;; Calculate the distance in meters between two map coordinates
(: map-distance/radians (-> Float Float Float Float Float))
(define (map-distance/radians lat1 lon1 lat2 lon2)
  (let ((delta-lat (fl- lat2 lat1))
        (delta-lon (fl- lon2 lon1)))
    (let* ((a (fl+ (haversin delta-lat)
                   (fl* (fl* (flcos lat1) (flcos lat2))
                        (haversin delta-lon))))
           (c (inv-haversin a)))
      (fl* c earth-radius))))

(: map-distance/degrees (-> Float Float Float Float Float))
(define (map-distance/degrees lat1 lon1 lat2 lon2)
  (map-distance/radians
   (degrees->radians lat1)
   (degrees->radians lon1)
   (degrees->radians lat2)
   (degrees->radians lon2)))

;; Calculate the initial bearing for traveling between two map coordinates
;; (bearing is returned in radians).  note that the bearing will have to
;; change as one travers towards lat2, lon2 and has to be re-computed
;; periodically.
(: map-bearing/radians (-> Float Float Float Float Float))
(define (map-bearing/radians lat1 lon1 lat2 lon2)
  (let ((delta-lon (fl- lon2 lon1)))
    (let ((y (fl* (flsin delta-lon) (flcos lat2)))
          (x (fl- (fl* (flcos lat1) (flsin lat2))
                  (fl* (fl* (flsin lat1) (flcos lat2)) (flcos delta-lon)))))
      (flatan (/ y x)))))

(: map-bearing/degrees (-> Float Float Float Float Float))
(define (map-bearing/degrees lat1 lon1 lat2 lon2)
  (map-bearing/radians
   (degrees->radians lat1)
   (degrees->radians lon1)
   (degrees->radians lat2)
   (degrees->radians lon2)))

;; Normalized map coordinate point. (0,0) is the top-left corner of the entire
;; world map and (1,1) is the bottom right.
(struct: map-point ([x : Float] [y : Float])
         #:transparent
         #:guard
         (lambda (x y name)
           (if (and (>= x 0.0) (<= x 1.0)
                    (>= y 0.0) (<= y 1.0))
               (values x y)
               (let ((msg (format "~a - bad coordinates: ~a ~a" name x y)))
                 (log-message map-widget-logger 'error #f msg #f #t)
                 (error msg)))))

(: lat-lon->map-point (-> Float Float map-point))
(define (lat-lon->map-point lat lon)
  (let ((r-lat (degrees->radians lat))
        (r-lon (degrees->radians lon)))
    (let ((x r-lon)
          (y (asinh (tan r-lat))))
      (let ((x-norm (/ (+ 1 (/ x pi)) 2))
            (y-norm (/ (- 1 (/ y pi)) 2)))
        (map-point x-norm y-norm)))))

(: map-point->lat-lon (-> map-point (Values Float Float)))
(define (map-point->lat-lon p)
  (let ((x-norm (map-point-x p))
        (y-norm (map-point-y p)))
    (let ((x (* (- (* x-norm 2) 1) pi))
          (y (* (- 1 (* y-norm 2)) pi)))
      (values
       (radians->degrees (atan (sinh y)))
       (radians->degrees x)))))

;; Bounding box defines a rectangular region on the map.
(struct: map-bbox
         ([max-lat : Float] [max-lon : Float] [min-lat : Float] [min-lon : Float])
         #:transparent)

(define-type Coord (Vector Float Float))
(: point-lat (-> Coord Float))
(define (point-lat p) (vector-ref p 0))
(: point-lon (-> Coord Float))
(define (point-lon p) (vector-ref p 1))

;; Return the bounding box of a TRACK (expressed as GPS coordinates, latitude
;; and longitude degrees
(: track-bbox (-> (Sequenceof Coord) (U map-bbox False)))
(define (track-bbox track)
  (let ((min-lat 180.0)
        (min-lon 180.0)
        (max-lat -180.0)
        (max-lon -180.0))
    (for ([point track])
      (let ((lat (point-lat point))
            (lon (point-lon point)))
        (set! min-lat (min lat min-lat))
        (set! max-lat (max lat max-lat))
        (set! min-lon (min lon min-lon))
        (set! max-lon (max lon max-lon))))
    (map-bbox max-lat max-lon min-lat min-lon)))

(: bbox-center (-> map-bbox (Values Float Float)))
(define (bbox-center bbox)
  (let ((lat (/ (+ (map-bbox-max-lat bbox) (map-bbox-min-lat bbox)) 2))
        (lon (/ (+ (map-bbox-max-lon bbox) (map-bbox-min-lon bbox)) 2)))
    (values lat lon)))

;; Return the center point of a bounding box BBOX as a map point (0..1 values)
(: bbox-center/ndcs (-> map-bbox map-point))
(define (bbox-center/ndcs bbox)
  (let ((map1 (lat-lon->map-point (map-bbox-max-lat bbox) (map-bbox-max-lon bbox)))
        (map2 (lat-lon->map-point (map-bbox-min-lat bbox) (map-bbox-min-lon bbox))))
    (let ((cx (/ (+ (map-point-x map1) (map-point-x map2)) 2))
          (cy (/ (+ (map-point-y map1) (map-point-y map2)) 2)))
      (map-point cx cy))))

;; Return the dimensions (width and height) of the BBOX in meters.
(: bbox-size (-> map-bbox (Values Float Float)))
(define (bbox-size bbox)
  (let ((min-lat (map-bbox-min-lat bbox))
        (min-lon (map-bbox-min-lon bbox))
        (max-lat (map-bbox-max-lat bbox))
        (max-lon (map-bbox-max-lon bbox)))
    (values (map-distance/degrees min-lat min-lon min-lat max-lon)
            (map-distance/degrees min-lat min-lon max-lat min-lon))))

(: bbox-merge (-> map-bbox map-bbox map-bbox))
(define (bbox-merge bb1 bb2)
  (map-bbox
   (max (map-bbox-max-lat bb1) (map-bbox-max-lat bb2))
   (max (map-bbox-max-lon bb1) (map-bbox-max-lon bb2))
   (min (map-bbox-min-lat bb1) (map-bbox-min-lat bb2))
   (min (map-bbox-min-lon bb1) (map-bbox-min-lon bb2))))

(: wind-rose (Vectorof String))
(define wind-rose
  (vector "NNE" "NE" "NEE" "E" "ESE" "SE" "SSE" "S" "SSW" "SW" "WSW" "W" "WNW" "NW" "NNW" "N"))

(: degrees->wind-rose (-> Real String))
(define (degrees->wind-rose deg)
  (let* ((nslices (vector-length wind-rose))
         (slice (/ 360.0 nslices))
         (adjusted-deg 
          (modulo (exact-round (- deg (/ slice 2))) 360)))
    (vector-ref wind-rose (exact-truncate (/ adjusted-deg slice)))))


;; Tiles are provided at zoom levels between 1 and 18
(: valid-zoom-level? (-> Integer Boolean))
(define (valid-zoom-level? z) (and (>= z 1) (<= z 18)))

;; A map is drawn as a set of tiles, each tile is a 256x256 pixel image.  See
;; also: http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
(struct: map-tile ((zoom : Integer) (x : Integer) (y : Integer))
  #:transparent
  #:guard
  (lambda (zoom x y name)
    (unless (valid-zoom-level? zoom)
      (define msg (format "invalid zoom level (~a): ~a" name zoom))
      (log-message map-widget-logger 'error #f msg #f #t)
      (error msg))
    (let ((max-val (expt 2 zoom)))
      (unless (and (>= x 0) (< x max-val))
        (define msg (format "~a - bad x: ~a (valid range 0..~a)" name x max-val))
        (log-message map-widget-logger 'error #f msg #f #t)  
        (error msg))
      (unless (and (>= y 0) (< y max-val))
        (define msg (format "~a - bad y: ~a (valid range 0..~a)" name y max-val))
        (log-message map-widget-logger 'error #f msg #f #t)
        (error msg)))
    (values zoom x y)))

(: map-tile-equal? (-> map-tile map-tile Boolean))
(define (map-tile-equal? tile1 tile2)
  (and (equal? (map-tile-zoom tile1) (map-tile-zoom tile2))
       (equal? (map-tile-x tile1) (map-tile-x tile2))
       (equal? (map-tile-y tile1) (map-tile-y tile2))))
