#lang racket/base

;; map-tiles.rkt -- manage map tiles for the map widget.  Tiles are retrieved
;; from the net as needed and stored locally in a cache.
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
 racket/runtime-path
 racket/port
 racket/async-channel
 net/url
 racket/match
 racket/draw
 racket/list
 db
 "dbglog.rkt"
 "al-prefs.rkt"
 "dbutil.rkt"
 "al-log.rkt"
 "map-util.rkt")

(provide
 get-tile-bitmap
 vacuum-tile-cache-database
 al-pref-allow-tile-download)


;........................................................ debug helpers ....

(define print-dbg-messages #f)

(define (dbg-printf . args)
  (when print-dbg-messages
    (apply printf args)))


;.................................................... global parameters ....

;; Name of the tile cache database file.
(define tcache-file-name 
  (al-get-pref 'activity-log:tile-cache-file
               (lambda () (build-path (al-get-pref-dir) "OsmTileCache.db"))))

;; Tiles older than this will be refreshed. Value is in seconds, but the value
;; stored in the preference file is in days.
(define tile-refresh-interval 
  (* 60 60 24
     (al-get-pref 'activity-log:tile-refresh-interval (lambda () 60))))

;; Tiles will not be downloaded if this parameter is #f.
(define al-pref-allow-tile-download
  (let* ((tag 'activity-log:allow-tile-download)
         (val (al-get-pref tag (lambda () #t))))
    (make-parameter
     val
     (lambda (new-val)
       ;; Write the value back to the store
       (al-put-pref tag new-val)
       (if new-val
           (log-al-info "Map tile download enabled")
           (log-al-warning "Map tile download disabled"))
       new-val))))


;..................................................... helper functions ....

;; Read requests from CHANNEL and add them to BACKLOG.  We read without
;; blocking until there are no more requests, however if the backlog is empty,
;; we wait indefinitely.  Requests are added to the backlog in reverse order
;; (the last request added to the channel will end up being the first in the
;; backlog) Returns an updated backlog.
(define (fill-backlog backlog channel)
  (let ([tile ((if (null? backlog) async-channel-get async-channel-try-get) channel)])
    (if tile
        (fill-backlog (cons tile backlog) channel)
        backlog)))


;.................................................. tile cache database ....

(define-runtime-path tcache-schema-file "../sql/osmtc-schema.sql")

(define (open-tcache-database database-file)
  (db-open
   database-file
   #:schema-file tcache-schema-file
   #:expected-version 1))

(define fetch-tile-sql
  (virtual-statement
   (lambda (dbsys) "
select data, timestamp 
  from TILE_CACHE
 where zoom_level = ? and x_coord = ? and y_coord = ?")))
     
;; Fetch a tile from the database. Return a (vector data timestamp) for the
;; TILE, or #f if the tile is not found
(define (db-fetch-tile tile db)
  (let* ((zoom (map-tile-zoom tile))
         (x (map-tile-x tile))
         (y (map-tile-y tile))
         (data (query-maybe-row db fetch-tile-sql zoom x y)))
    data))

(define tile-exists-sql
  (virtual-statement
   (lambda (dbsys) "
select zoom_level
  from TILE_CACHE
 where zoom_level = ? and x_coord = ? and y_coord = ?")))

(define insert-tile-sql
  (virtual-statement
   (lambda (dbsys) "
insert into TILE_CACHE(url, timestamp, data, zoom_level, x_coord, y_coord)
values (?, ?, ?, ?, ?, ?)")))

(define update-tile-sql
  (virtual-statement
   (lambda (dbsys) "
update TILE_CACHE set url = ?, timestamp = ?, data = ?
where zoom_level = ? and x_coord = ? and y_coord = ?")))

;; Store the image (PNG) for a tile in the database.  Does an INSERT or UPDATE
;; as needed.
(define (db-store-tile tile url data db)
  (call-with-transaction
   db
   (lambda ()
     (let ((zoom (map-tile-zoom tile))
           (x (map-tile-x tile))
           (y (map-tile-y tile))
           (timestamp (current-seconds)))
       (if (query-maybe-value db tile-exists-sql zoom x y)
           (query-exec db update-tile-sql url timestamp data zoom x y)
           (query-exec db insert-tile-sql url timestamp data zoom x y))))))

;; Create a thread to service tile fetch and store request for the database.
;; This is done so that the main GUI is not blocked.  Operations are performed
;; against the databse DB, requests are received from REQUEST channel and
;; replies (tile PNG images) are sent to REPLY channel.  If a tile is not
;; found in the database or it is too old, a download request is enqueued on
;; NET-FETCH-REQUEST.
(define (make-tile-fetch-store-thread db request reply net-fetch-request)
  (thread/dbglog
   #:name "tile db fbetch-store thread"
   (lambda ()
     (let loop ((backlog (fill-backlog '() request)))
       (unless backlog
         (dbglog "Tile fetch/store backlog is unexpectedly empty")
         (dbg-printf "Tile fetch/store backlog is unexpectedly empty~%"))
       (when backlog
         (match-define (cons operation data) (car backlog))
         (cond ((eq? operation 'fetch)
                (match-define (list tile) data)
                (dbg-printf "DB Fetching ~a~%" tile)
                (let ([row (db-fetch-tile tile db)])
                  (if row
                      (let ((blob (vector-ref row 0))
                            (timestamp (vector-ref row 1)))
                        (when (> (- (current-seconds) timestamp) tile-refresh-interval)
                          ;; Re-download an old tile
                          (dbg-printf "DB ask re-download old tile ~a~%" tile)
                          (async-channel-put net-fetch-request tile))
                        (async-channel-put reply (list tile blob)))
                      ;; Download it
                      (async-channel-put net-fetch-request tile))))
               ((eq? operation 'store)
                (match-define (list tile url blob) data)
                (dbg-printf "DB Storing ~a ~a~%" tile url)
                (db-store-tile tile url blob db)))
         (loop (fill-backlog (cdr backlog) request)))))))

(define (fetch-tile tile channel)
  (async-channel-put channel (list 'fetch tile)))

(define (store-tile tile url data channel)
  (async-channel-put channel (list 'store tile url data)))


;.................................... downlading tiles from the network ....

(define (random-select l)
  (let ((n (random (length l))))
    (list-ref l n)))

(define (tile->osm-url tile)
  (let ((top (random-select '("a" "b" "c")))
        (zoom (map-tile-zoom tile))
        (x (map-tile-x tile))
        (y (map-tile-y tile)))
    (format "http://~a.tile.openstreetmap.org/~a/~a/~a.png" top zoom x y)))

;; Fetch TILE from the network.  Return a list of the tile, the url it was
;; fetched from and the actual PNG image as a byte-array.  Return #f if
;; there's a problem.  Will not download any tiles if
;; AL-PREF-ALLOW-TILE-DOWNLOAD is #f.
(define (net-fetch-tile tile)
  (with-handlers
   (((lambda (e) #t)
     (lambda (e)
       (dbglog (format "Failed to download tile ~a~%" e))
       (dbg-printf "Failed to download tile ~a~%" e)
       #f)))
   (if (al-pref-allow-tile-download)
       (let* ((url (tile->osm-url tile))
              (data (port->bytes (get-pure-port (string->url url)))))
         (list tile url data))
       #f)))

;; Create a thread to download tiles from the network.  Requests are received
;; on REQUEST channel and replies are sent on REPLY channel.  Every downloaded
;; tile is also stored in the database by adding a request to
;; DB-REQUEST-CHANNEL.
(define (make-tile-download-thread request reply db-request-channel)
  (thread/dbglog
   #:name "tile net fetch thread"
   (lambda ()
     (let loop ((backlog (fill-backlog '() request)))
       (unless backlog
         (dbglog "Tile download backlog is unexpectedly empty")
         (dbg-printf "Tile download backlog is unexpectedly empty~%"))
       (when backlog
         (let* ([tile (car backlog)]
                [data (net-fetch-tile tile)])
           (when data
             (match-define (list tile url blob) data)
             (dbg-printf "NET Downloaded tile ~a~%" tile)
             (store-tile tile url blob db-request-channel)
             (async-channel-put reply (list tile blob)))
           ;; NOTE: a tile may be present in the backlog multiple times,
           ;; remove all such instances, now that we downloaded the data.
           (let ([nbacklog (remove* (list tile) backlog)])
             (loop (fill-backlog nbacklog request)))))))))


;......................................................... main section ....

(define tcache-db #f)                   ; the tile cache database
(define bitmap-cache (make-hash))       ; the bitmap cache

;; The secondary-bitmap-cache is used to keep the max number of tiles, while
;; keeping the most recently used ones.  See GET-TILE-BITMAP for how it is
;; used.
(define secondary-bitmap-cache (make-hash))

;; Number of entries in BITMAP-CACHE before we start discarding bitmaps.
(define cache-threshold 100)

;; Database requests are sent on this channel
(define db-request-channel (make-async-channel #f))

;; Network requests are sent on this channel
(define net-request-channel (make-async-channel #f))

;; Replies (from both the database and network threads) are received on this
;; channel.
(define reply-channel (make-async-channel #f))

;; List of worker threads.  There is currently no mechanism to stop these
;; threads.
;; TODO: stop these threads nicely when the application exits.
(define worker-threads '())

;; Proces some replies from CHANNEL as sent by the database service or the
;; tile download threads.  Replies are tiles and theis corresponding PNG
;; images. We create bitmap% objects and add them to BITMAP-HASH. LIMIT
;; defines the maximum number of replies that we process (#f indicates that we
;; process all available replies in CHANNEL).
(define (process-some-replies channel bitmap-hash [limit #f])
  (let loop ((count 0)
             (reply (async-channel-try-get channel)))
    (when reply
      (match-define (list tile data) reply)
      (let ((bmp (call-with-input-bytes data read-bitmap)))
        (hash-set! bitmap-hash tile bmp))
      (when (or (not limit) (< count limit))
        (loop (+ count 1) (async-channel-try-get channel))))))

;; Open the database and create the worker thread (if not already done so).
(define (maybe-setup)
  (unless tcache-db
    (set! tcache-db (open-tcache-database tcache-file-name))
    (set! worker-threads
          (list
           (make-tile-fetch-store-thread
            tcache-db db-request-channel reply-channel net-request-channel)
           (make-tile-download-thread
            net-request-channel reply-channel db-request-channel)
           (make-tile-download-thread
            net-request-channel reply-channel db-request-channel)
           (make-tile-download-thread
            net-request-channel reply-channel db-request-channel)))))

(define (get-tile-bitmap tile)
  ;; This is called from the map widget paint method, and if we throw an
  ;; exception from that,the whole thing locks up.
  (with-handlers
   (((lambda (e) #t)
     (lambda (e)
       (dbglog (format "get-tile-bitmap(~a): ~a" tile e))
       (dbg-printf "get-tile-bitmap(~a): ~a~%" tile e)
       #f)))

   (maybe-setup)
   (process-some-replies reply-channel bitmap-cache #f)

   ;; If the tile count in the bitmap cache has reached the threshold, discard
   ;; the secondary-bitmap-cache and rotate it. This way, we discard old
   ;; unused tiles, trying to keep the memory down.
   (when (> (hash-count bitmap-cache) cache-threshold)
     (dbg-printf "Rotating tile cache~%")
     (set! secondary-bitmap-cache bitmap-cache)
     (set! bitmap-cache (make-hash)))

   (cond ((hash-ref bitmap-cache tile #f))
         ;; Check if this value is in the secondary bitmap cache.  If it is,
         ;; move it back into the main cache, as it looks like it is in use.
         ((hash-ref secondary-bitmap-cache tile #f)
          => (lambda (v)
               (hash-set! bitmap-cache tile v)
               (hash-remove! secondary-bitmap-cache tile)
               v))
         (#t (fetch-tile tile db-request-channel) #f))))

(define (vacuum-tile-cache-database)
  (maybe-setup)
  (query-exec tcache-db "vacuum"))
