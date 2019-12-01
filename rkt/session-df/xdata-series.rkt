#lang racket/base

;; xdata.rkt --
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2018, 2019 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require
 db/base
 racket/match
 racket/class
 racket/contract
 racket/async-channel
 racket/format
 racket/runtime-path
 json
 "series-metadata.rkt"
 "native-series.rkt"
 "../utilities.rkt"
 "../data-frame/df.rkt"
 "../data-frame/series.rkt"
 "../data-frame/colors.rkt"
 "../dbutil.rkt")

(provide/contract
 (read-xdata-series (-> data-frame? connection? any/c))
 (get-available-xdata-metadata (->* () (connection?) (listof (is-a?/c series-metadata%)))))


(define-runtime-path xdata-defs-file "../../sql/xdata-defs.json")

(define the-xdata-definitions #f)

(define (xdata-definitions)
  (unless the-xdata-definitions
    (define global (call-with-input-file xdata-defs-file
                     (lambda (in) (read-json in #:null #f))))

    (define user-file (build-path (data-directory) "xdata-defs.json"))

    (define user (if (file-exists? user-file)
                     (with-handlers
                       (((lambda (e) #t)
                         (lambda (e)
                           (dbglog "failed to read ~a" user-file)
                           (dbglog-exception user-file e)
                           '())))
                       (call-with-input-file user-file
                         (lambda (in) (read-json in #:null #f))))
                       '()))
    (if (list? user)
        (set! the-xdata-definitions (append user global))
        (begin
          (dbglog "~a -- unexpected format, discarding" user-file)
          (set! the-xdata-definitions global))))
  the-xdata-definitions)

(define (find-field-definition app-id field-name)
  (define result #f)
  (for ([app (in-list (xdata-definitions))]
        #:when (and (not result)        ; we haven't found it already
                    (hash? app)         ; this is actually a has in the JSON
                    (equal? app-id (hash-ref app 'app_id #f))))
    (for ([fdef (in-list (hash-ref app 'fields))]
          #:when (and (not result)
                      (hash? fdef)
                      (equal? field-name (hash-ref fdef 'name #f))))
      (set! result fdef)))
  result)

(define-runtime-path sql-query-path "../../sql/queries/xdata-field.sql")
(define sql-query (define-sql-statement sql-query-path))

;; Construct a series metadata object from an XDATA field.  This object
;; provides information such as what headline and axis label to use for the
;; plots as well as defaults for some operations
(define (make-xdata-series-metadata db field-id)
  (define row (query-maybe-row db (sql-query) field-id))
  (unless row
    (raise-argument-error 'field-id "valid XDATA_FIELD.id" field-id))
  (match-define (vector app-id nm nf name units) row)

  (define field-label
    (string-append
     "XData "
     (cond ((bytes? name) (bytes->string/utf-8 name))
           ((string? name) name)
           (#t (~a name)))
     " ("
     (cond ((bytes? units) (bytes->string/utf-8 units))
           ((string? units) units)
           (#t (~a units)))
     ")"))

  (define fdef (or (find-field-definition app-id name) (hash)))

  (define field-color (pick-color field-id))

  ;; Determine the parent class for this based on the "parent" metadata or the
  ;; native field member.  This affects if Critical Power or Critical Pace are
  ;; calculated for these fields.
  (define parent-class
    (let ((parent (hash-ref fdef 'parent #f)))
      (cond
        ((or (equal? parent "power")
             (and (eqv? nm 20) (eqv? nf 7)))
             power-series-metadata%)
        ((equal? parent "pace")
         pace-series-metadata%)
        (#t
         series-metadata%))))
  
  ;; Construct a new metadata class.  The parent of our metadata class is
  ;; `series-metadata%` by default, but we also look at the native field value
  ;; and choose a better parent in some cases -- currently we choose
  ;; `power-series-metadata%` for "power" native fields -- this ensures that
  ;; power series collected as XDATA can have CP estimation...
  (define xdata-series-metadata%
    (class parent-class
      (init) (super-new)
      (define/override (should-filter?) (hash-ref fdef 'should_filter #t))
      (define/override (histogram-bucket-slot)
        ;; Be carefull with the hash-ref, as 'histogram_bucket_slot might
        ;; exist and be #f!
        (or (hash-ref fdef 'histogram_bucket_slot #f)
            (super histogram-bucket-slot)))
      (define/override (inverted-mean-max?)
        (hash-ref fdef 'inverted_mean_max #f))
      (define/override (axis-label)
        (or (hash-ref fdef 'axis_label #f) field-label))
      (define/override (headline)
        (or (hash-ref fdef 'headline #f) field-label))
      (define/override (series-name)
        (or (hash-ref fdef 'series #f)
            (format "xdata-~a" field-id)))
      (define/override (plot-color) field-color)
      (define/override (fractional-digits)
        (or (hash-ref fdef 'fractional_digits #f)
            (super fractional-digits)))
      (define/override (missing-value)
        ;; NOTE: missing-value might be defined as #f in the JSON -- this is
        ;; the value that we want...
        (hash-ref fdef 'missing_value 0))))
  (new xdata-series-metadata%))

;; Map a field-id (database id) to the series-metadata% object for this XDATA
;; field.

(define the-xdata-registry (make-hash))
(define the-log-event-source (make-log-event-source))

(define (get-or-make-xdata-series-metadata db field-id)
  (define metadata (hash-ref the-xdata-registry field-id #f))
  (unless metadata
    (set! metadata (make-xdata-series-metadata db field-id))
    ;; Register this for both lap swim and normal activities
    (register-series-metadata metadata #f)
    (register-series-metadata metadata #t)
    (hash-set! the-xdata-registry field-id metadata))
  metadata)

(define-runtime-path xdata-values-query-path "../../sql/queries/xdata-values.sql")
(define xdata-values-query (define-sql-statement xdata-values-query-path))

;; When a new database is opened, remove the XDATA series that were added in
;; the previous one.  This needs to be called from `read-xdata-series` and
;; will do the right thing when the first session from the new database is
;; read.
(define (maybe-flush-xdata-series)
  (let loop ((item (async-channel-try-get the-log-event-source)))
    (when item
      (match-define (list tag data) item)
      ;; unregister our series metadata when the database changes -- new ones
      ;; will be created when session data is read in.
      (when (eq? tag 'database-opened)
        (for (([key value] (in-hash the-xdata-registry)))
          ;; Unregister form both lap swim and other activities.
          (unregister-series-metadata value #t)
          (unregister-series-metadata value #f))
        (set! the-xdata-registry (make-hash)))
      (loop (async-channel-try-get the-log-event-source)))))

;; Read all (or any) XDATA series for the session in the data frame `df` and
;; also create any xdata series metadata objects required for the series in
;; this data frame.
(define (read-xdata-series df db)

  (maybe-flush-xdata-series)

  (define sid (df-get-property df 'session-id))
  (define current-ts #f)
  (define position #f)
  (define xdata-series (make-hash))

  (for (([ts field value] (in-query db (xdata-values-query) sid #:fetch 1000)))
    (unless (equal? current-ts ts)
      (set! current-ts ts)
      (set! position (df-index-of df "timestamp" ts)))
    (define xdata (hash-ref xdata-series field #f))
    (unless xdata
      (set! xdata (make-vector (df-row-count df) #f))
      (hash-set! xdata-series field xdata))
    (vector-set! xdata position value))

  (for ([(key value) (in-hash xdata-series)])
    (define metadata (get-or-make-xdata-series-metadata db key))
    (define series (make-series (send metadata series-name) #:data value))
    (df-add-series df series)))

;; Return all the XDATA metadata objects that we have.  If DB is not #f, XDATA
;; field definitions are read from the database and any missing series
;; metadata objects are created before returning the list.
(define (get-available-xdata-metadata (db #f))
  (maybe-flush-xdata-series)
  ;; Read in any new series that might have been created (if we call this
  ;; function from the trend charts this is the place where the XDATA metadata
  ;; is created.
  (when db
    (for ([id (in-list (query-list db "select id from XDATA_FIELD"))])
      (get-or-make-xdata-series-metadata db id)))
  (define m (for/list ([x (in-hash-values the-xdata-registry)]) x))
  (sort m string<? #:key (lambda (x) (send x headline))))
