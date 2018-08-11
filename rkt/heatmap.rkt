#lang racket/base
;; heatmap.rkt -- quick hack to export a heat map from a list of activities.
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

(require browser/external
         db/base
         racket/gui/base)

(provide interactive-generate-heatmap)

;; See also: https://developers.google.com/maps/documentation/javascript/examples/layer-heatmap

(define hm-start-fragment
  "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><title>Heatmaps</title>
<style>html,body,#map-canvas {height: 100%;margin: 0px;padding: 0px;}</style>
<script src=\"https://maps.googleapis.com/maps/api/js?v=3.exp&libraries=visualization\"></script>
<script>")

(define hm-end-fragment
  "
var gradient = [
    'rgba(0, 255, 255, 0)',
    'rgba(0, 255, 255, 1)',
    'rgba(0, 191, 255, 1)',
    'rgba(0, 127, 255, 1)',
    'rgba(0, 63, 255, 1)',
    'rgba(0, 0, 255, 1)',
    'rgba(0, 0, 223, 1)',
    'rgba(0, 0, 191, 1)',
    'rgba(0, 0, 159, 1)',
    'rgba(0, 0, 127, 1)',
    'rgba(63, 0, 91, 1)',
    'rgba(127, 0, 63, 1)',
    'rgba(191, 0, 31, 1)',
    'rgba(255, 0, 0, 1)'
  ];

function initialize() {
    var hm = [];
    var min_lat = (data[0])[0];
    var max_lat = min_lat;
    var min_lon = (data[0])[1];
    var max_lon = min_lon;
    var max_weight = (data[0])[2];

    var len = data.length;
    for (var i = 0; i < len; ++i) {
        var entry = data[i];

        var lat = entry[0];
        var lon = entry[1];
        var weight = entry[2];

        if (min_lat > lat) { min_lat = lat; }
        if (max_lat < lat) { max_lat = lat; }
        if (min_lon > lon) { min_lon = lon; }
        if (max_lon < lon) { max_lon = lon; }
        if (max_weight < weight) { max_weight = weight; }

        hm.push({location: new google.maps.LatLng(lat, lon), weight: weight});
    }

    var ptarray = new google.maps.MVCArray(hm);
    data = []; // clear data to save some memory
    hm = [];

    var options = { mapTypeId: google.maps.MapTypeId.SATELLITE };
    map = new google.maps.Map(document.getElementById('map-canvas'), options);

    var bounds = new google.maps.LatLngBounds();
    bounds.extend(new google.maps.LatLng(min_lat, min_lon));
    bounds.extend(new google.maps.LatLng(max_lat, max_lon));
    map.fitBounds(bounds);

    heatmap = new google.maps.visualization.HeatmapLayer({
        data: ptarray,
        dissipating: true,
        maxIntensity: max_weight,
        opacity: 0.8});
    google.maps.event.addListener(map,'zoom_changed',function() {
        var z = map.getZoom();
        heatmap.set('radius', Math.ceil((Math.pow(z, 3))/200));
    });
    heatmap.setMap(map);
    heatmap.set('gradient', gradient);
}

google.maps.event.addDomListener(window,'load',initialize);
</script></head><body><div id=\"map-canvas\"></div></body></html>")


(define (m->scale m)
  (define earth-radius 6371000.0)
  (define pi 3.1415926)
  (let ((earth-circumference (* 2 pi earth-radius)))
    (/ (/ earth-circumference 360.0) m)))

;; Return a list of points representing the heat map for the sessions in the
;; SIDS list.
;;
;; DB is the database connection,
;; SIDS is a list of session ids (A_SESSION.id),
;; GROUP-RADIUS is the radius in meters around which we group adjacent points.
;;
;; Returns a list of (vector LAT LON WEIGHT), where WEIGHT is the number of
;; points at that LAT,LON.
(define (get-heatmap-points db sids group-radius)
  (let ((scale (m->scale group-radius)))
    (call-with-transaction
     db
     (lambda ()
       (query db "delete from TMP_SESSION_LIST")
       (for ((sid sids))
         (query db "insert into TMP_SESSION_LIST(session_id) values(?)" sid))
       (let ((rows (query-rows
                    db
                    (format "
select round(T.position_lat * ~a) as lat,
       round(T.position_long * ~a) as long,
       count(T.id) as weight
from A_TRACKPOINT T, A_LENGTH L, A_LAP P
where P.session_id in (select TSL.session_id from TMP_SESSION_LIST TSL)
  and L.lap_id = P.id
  and T.length_id = L.id
  and position_lat is not null and position_long is not null
 group by lat, long" scale scale))))
         (query db "delete from TMP_SESSION_LIST")
         (for/list ((pt rows))
           (vector
            (/ (vector-ref pt 0) scale)
            (/ (vector-ref pt 1) scale)
            (vector-ref pt 2)))
         )))))
  
(define (emit-heatmap-file port points)
  (write-string hm-start-fragment port)
  ;; We output points in chunks, as the Javascript parser chokes if we declare
  ;; an array with too many elements in one go.
  (write-string (format "var data = []; data = data.concat([~%") port)
  (let loop ((points points)
             (count 0))
    (if (null? points)
        (write-string "]);" port)       ; we're done
        (begin
          (let ((pt (car points)))
            (write-string
             (format "[~a, ~a, ~a],~%"
                     (vector-ref pt 0)
                     (vector-ref pt 1)
                     (vector-ref pt 2))
             port))
          (if (= count 1000)
              (begin
                (write-string (format "]);~%data = data.concat([~%") port)
                (loop (cdr points) 0))
              (loop (cdr points) (+ 1 count))))))
    (write-string hm-end-fragment port))

(define (interactive-generate-heatmap database session-ids)
  (let ((file (put-file "Select Heat Map File Name" #f #f #f "html" '()
                            '(("HTML Files" "*.html") ("Any" "*.*")))))
    (when file
      (let ((points (get-heatmap-points database session-ids 10)))
        (call-with-output-file file
          (lambda (out)
            (emit-heatmap-file out points))
          #:mode 'text #:exists 'truncate))
      (send-url (string-append "file:" (path->string file))))))
