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
 racket/async-channel
 racket/port
 net/url
 net/url-connect
 racket/match
 racket/draw
 racket/list
 racket/class
 db
 "utilities.rkt"
 "dbutil.rkt"
 "map-util.rkt")
(require (for-syntax racket/base))

(provide
 get-tile-bitmap
 vacuum-tile-cache-database
 get-download-backlog
 allow-tile-download
 set-allow-tile-download
 shutdown-map-tile-workers
 tile-copyright-string
 get-tile-provider-names
 current-tile-provider-name
 set-current-tile-provider)


;........................................................ debug helpers ....

(define print-dbg-messages #f)

(define (dbg-printf . args)
  (when print-dbg-messages
    (apply printf args)))


;.................................................... global parameters ....

;; Tiles older than this will be refreshed. Value is in seconds, but the value
;; stored in the preference file is in days.
(define tile-refresh-interval
  (* 60 60 24
     (get-pref 'activity-log:tile-refresh-interval (lambda () 60))))

;; Tiles will not be downloaded if this parameter is #f.
(define allow-tile-download-tag 'activity-log:allow-tile-download)
(define allow-tile-download-val (get-pref allow-tile-download-tag (lambda () #t)))
(define (allow-tile-download) allow-tile-download-val)
(define (set-allow-tile-download new-val)
  (put-pref allow-tile-download-tag new-val)
  (set! allow-tile-download-val new-val)
  (if new-val
      (dbglog "map tile download enabled")
      (dbglog "map tile download disabled")))


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
           (query-exec db update-tile-sql "" timestamp data zoom x y)
           (query-exec db insert-tile-sql "" timestamp data zoom x y))))))


;......................................................... tile backlog ....

;; Keep track of the tiles that need to be downloaded, ensuring that tiles
;; that were requested last will be downloaded first.  This is used by the
;; tile download threads to know which tiles they need to download.
;;
;; This class is essentially a stack, or LIFO queue, with the following
;; modifications:
;;
;; * all access is protected by semaphores, allowing multi-threaded access
;;
;; * PUT will never add a duplicate tile, but it will move an existing tile to
;;   the front.
;;
;; * GET will block until a tile is available, so GET and PUT operations
;;   cannot happen on the same thread
;;
(define tile-backlog-manager%
  (class object% (init) (super-new)

    ;; Contains a list of TILE objects that are waiting to be downloaded,
    ;; unless it is #f, in which case the manager is disabled
    (define backlog #f)

    ;; Contains a list of TILE objects that are being downloaded.
    (define downloading '())
    
    ;; counts number of elements in the backlog list
    (define sem-nitems (make-semaphore 0))

    ;; allows exclusive access to internal structures from multiple threads.
    (define sem-acces (make-semaphore 1))

    ;; Put tile in the backlog if it is not already there, or is not being
    ;; downloaded.  If the tile is already in the backlog it is moved to
    ;; front, so GET will return it next.
    (define/public (put tile)
      (call-with-semaphore
       sem-acces
       (lambda ()
         (when backlog
           (unless (member tile downloading map-tile-equal?)
             (if (member tile backlog map-tile-equal?)
                 ;; Move it to front
                 (begin
                   (set! backlog (cons tile (remove* (list tile) backlog map-tile-equal?))))
                 (begin
                   (set! backlog (cons tile backlog))
                   ;; Tell GET that there are items in the backlog
                   (semaphore-post sem-nitems))))))))
    
    ;; Get the next tile to be downloaded.  Will block until a tile is
    ;; available.  The tile is added to the DOWNLOADING list and will need to
    ;; be cleared.
    (define/public (get)
      (semaphore-wait sem-nitems)  ; wait until there are items in the backlog
      (call-with-semaphore
       sem-acces
       (lambda ()
         (and backlog
              (begin0
                  (car backlog)
                (set! downloading (cons (car backlog) downloading))
                (set! backlog (cdr backlog)))))))

    ;; Mark TILE as being downloaded and remove it from the DOWNLOADING list.
    (define/public (clear tile)
      (call-with-semaphore
       sem-acces
       (lambda ()
         (set! downloading (remove* (list tile) downloading map-tile-equal?)))))

    ;; Remove all items from the backlog.
    (define/public (drain)
      ;; NOTE: we need to ensure that sem-nitems reflects the number of items
      ;; in the backlog!
      (when (semaphore-try-wait? sem-nitems)
        (call-with-semaphore
         sem-acces
         (lambda ()
           (set! backlog (cdr backlog))))
        (drain))
      (set! backlog #f)
      (call-with-semaphore
       sem-acces
       (lambda ()
         (set! downloading '()))))

    (define/public (start)
      (unless backlog
        (set! backlog '())))

    (define/public (num-items)
      (call-with-semaphore
       sem-acces
       (lambda ()
         (+ 
          (if backlog (length backlog) 0)
          (length downloading)))))

    ))

(define tbmanager (new tile-backlog-manager%))
    
(define (get-download-backlog)
  (send tbmanager num-items))

;; Create a thread to service tile fetch and store request for the database.
;; This is done so that the main GUI is not blocked.  Operations are performed
;; against the databse DB, requests are received from REQUEST channel and
;; replies (tile PNG images) are sent to REPLY channel.  If a tile is not
;; found in the database or it is too old, a download request is enqueued on
;; NET-FETCH-REQUEST.
(define (make-tile-fetch-store-thread db request reply)
  (thread/dbglog
   #:name "tile db fbetch-store thread"
   (lambda ()
     (let loop ((work-item (async-channel-get request)))
       (when work-item
         (match-define (cons operation data) work-item)
         (cond ((eq? operation 'fetch)
                (match-define (list tile) data)
                (dbg-printf "DB Fetching ~a~%" tile)
                (let ([row (db-fetch-tile tile db)])
                  (if row
                      (let ((blob (vector-ref row 0))
                            (timestamp (vector-ref row 1)))
                        ;; Maybe request re-download, but still supply the
                        ;; tile
                        (when (> (- (current-seconds) timestamp) tile-refresh-interval)
                          (dbg-printf "DB ask re-download old tile ~a~%" tile)
                          (send tbmanager put tile))
                        (async-channel-put reply (list tile blob)))
                      ;; Download it
                      (send tbmanager put tile))))
               ((eq? operation 'store)
                (match-define (list tile url blob) data)
                (dbg-printf "DB Storing ~a ~a~%" tile url)
                (db-store-tile tile url blob db)))
         (loop (async-channel-get request)))))))

(define (fetch-tile tile channel)
  (async-channel-put channel (list 'fetch tile)))

(define (store-tile tile url data channel)
  (async-channel-put channel (list 'store tile url data)))


;.................................... downlading tiles from the network ....

;; This "magic" macro will embed the Thunderforest API key at compile time,
;; allowing us to ship a built version of the application with the API key
;; already inside it.
(define-syntax (embedded-api-key stx)
  (syntax-case stx ()
    [_ #`(quote #,(getenv "AL2TFAPIKEY"))]))
(define (builtin-api-key) (embedded-api-key))

(define tf-api-key-tag 'activity-log:tf-api-key)
;; the API key value comes either from the preferences file (needs to be
;; manually stored there), from an environment variable, or a built in one (if
;; available)
(define tf-api-key-val
  (or (get-pref tf-api-key-tag (lambda () #f))
      (getenv "AL2TFAPIKEY")
      (builtin-api-key)))
(define (tf-api-key) tf-api-key-val)

(define user-agent "User-Agent: ActivityLog2 http://alex-hhh.github.io/ActivityLog2/")

;; Return the URL string for fetching the bitmap for TILE from OpenStreetMap
(define (tile->osm-url tile)
  (let ((zoom (map-tile-zoom tile))
        (x (map-tile-x tile))
        (y (map-tile-y tile)))
    (format "https://tile.openstreetmap.org/~a/~a/~a.png" zoom x y)))

;; Return the URL string from fetching the bitmap for TILE from Thunderforest.
;; KIND determines the type of the map used (e.g. cycle, outdoors, etc)
(define (tile->tf-url kind tile)
  (let ((zoom (map-tile-zoom tile))
        (x (map-tile-x tile))
        (y (map-tile-y tile)))
    (format "https://tile.thunderforest.com/~a/~a/~a/~a.png?apikey=~a"
            kind zoom x y (tf-api-key))))

;; Hold information about a map tile bitmap provider
(struct tile-provider (name tag cache-file url-fn copyright))

(define tf-outdoors-provider
  (tile-provider
   "Tunderforrest Outdoors"
   'tf-outdoors
   "TfOutdoorsTiles.db"
   (lambda (tile) (tile->tf-url "outdoors" tile))
   "Maps © Thunderforest, Data © OpenStreetMap contributors"))

(define tf-opencycle-provider
  (tile-provider
   "Tunderforrest Cycle"
   'tf-opencycle
   "TfCycleTiles.db"
   (lambda (tile) (tile->tf-url "cycle" tile))
   "Maps © Thunderforest, Data © OpenStreetMap contributors"))

(define tf-landscape-provider
  (tile-provider
   "Tunderforrest Landscape"
   'tf-landscape
   "TfLandscapeTiles.db"
   (lambda (tile) (tile->tf-url "landscape" tile))
   "Maps © Thunderforest, Data © OpenStreetMap contributors"))

(define tf-neighbourhood-provider
  (tile-provider
   "Tunderforrest Neighbourhood"
   'tf-neighbourhood
   "TfNeighbourhoodTiles.db"
   (lambda (tile) (tile->tf-url "neighbourhood" tile))
   "Maps © Thunderforest, Data © OpenStreetMap contributors"))

(define osm-provider
  (tile-provider
   "Open Street Map"
   'osm
   "OsmTiles.db"
   tile->osm-url
   "Copyright © OpenStreetMap contributors"))

;; List all available tile providers.
(define all-provivers
  (let ((providers (list osm-provider)))
    (when (tf-api-key)
      (set! providers (append providers (list tf-outdoors-provider
                                              tf-opencycle-provider
                                              tf-landscape-provider
                                              tf-neighbourhood-provider))))
    (sort providers string<? #:key tile-provider-name)))

;; This is the current tile provider used to paint maps
(define current-provider
  (let ((tag (get-pref 'activity-log:tile-provider (lambda () 'osm))))
    (or (for/first ([tp all-provivers] #:when (equal? tag (tile-provider-tag tp))) tp)
        osm-provider)))

;; Return a list of names for all the tile providers
(define (get-tile-provider-names)
  (map tile-provider-name all-provivers))

;; Return the URL string for TILE.  The CURRENT-PROVIDER is used to obtain the
;; URL.
(define (tile->url tile)
  (let* ((url-fn (tile-provider-url-fn current-provider))
         (url (url-fn tile)))
    (string->url url)))

;; Return the copyright string to be displayed on the map for the current tile
;; provider.
(define (tile-copyright-string)
  (tile-provider-copyright current-provider))

;; Return the name of the cache file for storing tiles for this tile provider
(define (tile-cache-file)
  (build-path (data-directory) (tile-provider-cache-file current-provider)))

;; Return the name of the current tile provider
(define (current-tile-provider-name)
  (tile-provider-name current-provider))

;; Set a new tile provider from NAME.  The tile provider is also stored as a
;; preference.
(define (set-current-tile-provider name)
  (unless (equal? name (tile-provider-name current-provider))
    (define tp
      (or (for/first ([tp all-provivers]
                      #:when (equal? (tile-provider-name tp) name))
            tp)
          osm-provider))
    (put-pref 'activity-log:tile-provider (tile-provider-tag tp))
    (set! current-provider tp)
    ;; Will recreate the workers for the new provider next time a tile will be
    ;; requested.
    (shutdown-map-tile-workers)))

;; Fetch TILE from the network.  Return a list of the tile, the url it was
;; fetched from and the actual PNG image as a byte-array.  Return #f if
;; there's a problem.  Will not download any tiles if
;; AL-PREF-ALLOW-TILE-DOWNLOAD is #f.
(define (net-fetch-tile tile connection)
  (with-handlers
   (((lambda (e) #t)
     (lambda (e)
       (dbglog-exception "net-fetch-tile" e)
       (dbg-printf "Failed to download tile ~a~%" e)
       #f)))
   (if (allow-tile-download)
       (parameterize ((current-https-protocol 'secure))
         (let ((url (tile->url tile)))
           (let-values (((port headers)
                         (get-pure-port/headers url
                                                (list user-agent)
                                                #:connection connection)))
             (let ((data (port->bytes port)))
               (list tile url data)))))
       #f)))

;; Create a thread to download tiles from the network.  Requests are received
;; on REQUEST channel and replies are sent on REPLY channel.  Every downloaded
;; tile is also stored in the database by adding a request to
;; DB-REQUEST-CHANNEL.
(define (make-tile-download-thread reply db-request-channel)
  (thread/dbglog
   #:name "tile net fetch thread"
   (lambda ()
     (let ((connection (make-http-connection)))
       (let loop ((work-item (send tbmanager get)))
         (if work-item
             (let* ([tile work-item]
                    [data (net-fetch-tile tile connection)])
               (send tbmanager clear tile)
               (when data
                 (match-define (list tile url blob) data)
                 (dbg-printf "NET Downloaded tile ~a~%" tile)
                 (store-tile tile url blob db-request-channel)
                 (async-channel-put reply (list tile blob)))
               (loop (send tbmanager get)))
             ;; Finished thread, close the HTTP connection
             (http-connection-close connection)))))))


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
(define db-request-channel #f)

;; Replies (from both the database and network threads) are received on this
;; channel.
(define reply-channel #f)

;; List of worker threads.  There is currently no mechanism to stop these
;; threads.
;; TODO: stop these threads nicely when the application exits.
(define worker-threads '())

;; Shutdown all the worker threads.  We tell the threads to stop and allow
;; them to finish their current task.
(define (shutdown-map-tile-workers)

  (send tbmanager drain)
  
  (for ((worker worker-threads))
    ;; We have several threads servicing the same channel, put a number of #f
    ;; values so that each thread gets one and shuts down.
    (async-channel-put db-request-channel #f))

  (for ((worker worker-threads))
    (sync/timeout 0.1 (thread-dead-evt worker)))

  (for ((worker worker-threads))
    (kill-thread worker))

  ;; Close the database, communication channels and the rest.  If we just
  ;; switched tile providers, a new one will be set up by MAYBE-SETUP.
  (when tcache-db
    (disconnect tcache-db)
    (set! tcache-db #f)
    (set! bitmap-cache (make-hash))
    (set! secondary-bitmap-cache (make-hash))
    (set! db-request-channel #f)
    (set! reply-channel #f)
    (set! worker-threads '())))

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
    (set! tcache-db (open-tcache-database (tile-cache-file)))
    (send tbmanager start)
    (set! db-request-channel (make-async-channel #f))
    (set! reply-channel (make-async-channel #f))
    (set! bitmap-cache (make-hash))
    (set! secondary-bitmap-cache (make-hash))
    (set! worker-threads
          (list
           (make-tile-fetch-store-thread
            tcache-db db-request-channel reply-channel)
           (make-tile-download-thread
            reply-channel db-request-channel)
           (make-tile-download-thread
            reply-channel db-request-channel)
           (make-tile-download-thread
            reply-channel db-request-channel)))))

(define (get-tile-bitmap tile)
  ;; This is called from the map widget paint method, and if we throw an
  ;; exception from that,the whole thing locks up.
  (with-handlers
   (((lambda (e) #t)
     (lambda (e)
       (dbglog-exception (format "get-tile-bitmap(~a)" tile) e)
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
