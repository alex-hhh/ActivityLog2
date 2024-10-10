#lang racket/base
;; fit-file.rkt -- read and write .FIT files.

;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015, 2018-2024 Alex Harsányi <AlexHarsanyi@gmail.com>
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

;;; Commentary:
;;
;; ACTIVITY .FIT files can be read, WORKOUT, SPORT and SETTINGS FIT files can
;; be written.
;;
;; The structure of a FIT file is described in the FIT SDK which you can
;; download form https://www.thisisant.com/resources/fit/
;;
;; DEVELOPER FIELDS SUPPORT (FIT 2.0)
;;
;; This code supports reading FIT files with developer fields -- these are
;; recorded by 3rd party devices, such as running power and blood oxygen
;; monitors.  Activities containing such fields can be read with the following
;; limitations:
;;
;; An activity will record 3rd party applications in the 'developer-data-id
;; entry, which contains a list defining the application identifier (16 byte
;; value) to an application index.  It looks like this:
;;
;; (developer-data-ids
;;    ((start-time . 1530870414)
;;     (developer-id . "ffffffffffffffffffffffffffffffff")
;;     (application-id . "27dfb7e5900f4c2d80abc57015f42124")
;;     (application-version . 9)
;;     (developer-data-index . 0)))
;;
;; Fields are defined in the 'field-descriptions entry.  For each field, we
;; have the application index, the field number, the native field number (if
;; this field corresponds to a native field in the record), the type, a name
;; and a unit.  In addition, there is a field-key entry (added by this code)
;; which will be used to identify the fields in the various data records
;; (sessions, laps, lengths or track points).  It looks like this:
;;
;; (field-descriptions
;;    ((start-time . 1530870414)
;;     (field-name . "eE")
;;     (field-key . 27dfb7e5900f4c2d80abc57015f42124-1)
;;     (units . #"c/hr")
;;     (native-msg-num . 20)
;;     (developer-data-index . 0)
;;     (field-def-number . 1)
;;     (fit-base-type . 132))
;;    ((start-time . 1530870414)
;;     (field-name . "StrideDistance")
;;     (field-key . 27dfb7e5900f4c2d80abc57015f42124-0)
;;     (units . #"ft or m")
;;     (native-msg-num . 20)
;;     (developer-data-index . 0)
;;     (field-def-number . 0)
;;     (fit-base-type . 136)))
;;
;; From the example above, trackpoints will contain a key of
;; 27dfb7e5900f4c2d80abc57015f42124-0 for the stride distance value of that
;; point.

(require file/gunzip
         json
         racket/class
         racket/dict
         racket/file
         racket/format
         racket/list
         racket/match
         racket/math
         racket/port
         racket/runtime-path
         "../utilities.rkt"
         "activity-util.rkt"
         "fit-defs.rkt")

(provide make-fit-data-stream)
(provide read-fit-records)
(provide fit-event-dispatcher%)
(provide activity-builder%)
(provide read-activity-from-file)
(provide fit-get-device-name)
(provide fit-output-stream%)
(provide fit-output-file%)
(provide mk-fit-string)
(provide fit-workout-file%)
(provide fit-sport-file%)
(provide fit-settings-file%)
;; this is only provided so that unit tests can be written for this.
(provide fit-device-name)


;............................................................... basics ....

(define (raise-error msg . args)
  (apply error 'fit-file msg args))

(define (compute-crc buffer [start 0] [end #f])
  ;; Compute the CRC of the bytes in BUFFER between START and END and return
  ;; it.  The CRC algorithm is the same as described int the FIT file
  ;; documentation.  When verifying CRC, the assumption is that the CRC are
  ;; the last two bytes in the buffer and a return value of 0 indicates that
  ;; the file's CRC is OK.

  (define crc-table #(#x0000 #xCC01 #xD801 #x1400
                             #xF001 #x3C00 #x2800 #xE401
                             #xA001 #x6C00 #x7800 #xB401
                             #x5000 #x9C01 #x8801 #x4400))

  (define limit (or end (bytes-length buffer)))

  (define (update-nibble crc b)
    (let ((tmp (vector-ref crc-table (bitwise-and crc #xf))))
      (let ((crc (bitwise-and (arithmetic-shift crc -4) #x0FFFF)))
        (bitwise-xor (bitwise-xor crc tmp)
                     (vector-ref crc-table (bitwise-and b #xf))))))

  (define (update-byte crc b)
    (update-nibble (update-nibble crc (bitwise-and b #xf))
                   (bitwise-and (arithmetic-shift b -4) #xf)))

  (let loop ((idx start)
             (crc 0))
    (if (= idx limit)
        crc
        (loop (+ idx 1) (update-byte crc (bytes-ref buffer idx))))))


;;...................................................... fit basic types ....

(define (real->bstr val size signed? big-endian? dest-bstr start)
  ;; Write VAL, a floating point value to DEST-BSTR.  This is just a wrapper
  ;; around `real->floating-point-bytes' to add an unused SIGNED? parameter so
  ;; the signature is consistent with what we need from a FIT read function.
  (real->floating-point-bytes val size big-endian? dest-bstr start))

(define (bstr->real bstr signed? big-endian? start end)
  ;; Read a floating point value from a byte string.  This is just a wrapper
  ;; around `floating-point-bytes->real' to add an unused SIGNED? parameter so
  ;; the signature is consistend with what we need from a FIT read function.
  (floating-point-bytes->real bstr big-endian? start end))

(define (integer->bstr val size signed? big-endian? dest-bstr pos)
  ;; Write an integer to a byte string. This is a wrapper for
  ;; `integer->integer-bytes' except that function does not handle 1 byte
  ;; integers.
  (if (> size 1)
      (integer->integer-bytes val size signed? big-endian? dest-bstr pos)
      (bytes-set! dest-bstr pos val)))

(define (bstr->integer bstr signed? big-endian? start end)
  ;; Read an integer from a byte string.  This is a wrapper for
  ;; `integer-bytes->integer' except that function does not handle 1 byte
  ;; integers, so we need to handle it ourselves.
  (if (> (- end start) 1)
      (integer-bytes->integer bstr signed? big-endian? start end)
      ;; we need to handle a size of 1 separately, as integer-bytes->integer
      ;; handles a min 2 byte integer...
      (let ((b (bytes-ref bstr start)))
        (if signed?
            (if (= (bitwise-and b #x80) #x80)
                (- b #x100)
                b)
            b))))

(struct fit-type
  ;; Hold information about a FIT basic type
  (id                                   ; a number as defined in the FIT documentation
   name                                 ; a convenient name for it
   signed?                              ; is this type signed?
   size                                 ; how many bytes does it need?
   ;; a special value whose meaning is "invalid".
   invalid-value
   ;; a function that reads a value this type from a byte string
   read-fn
   ;; a function that writes a value of this type to a byte string
   write-fn)
  #:transparent)

(define fit-types
  ;; Definition of all basic FIT types, as per the FIT file documentation.
  ;; This is a hash with the type ID being the key, see also `get-fit-type'
  (hash
   #x00 (fit-type #x00 'enum #f 1 #xFF bstr->integer integer->bstr)

   #x01 (fit-type #x01 'sint8 #t 1 #x7F bstr->integer integer->bstr)
   #x02 (fit-type #x02 'uint8 #f 1 #xFF bstr->integer integer->bstr)

   #x83 (fit-type #x83 'sint16 #t 2 #x7FFF bstr->integer integer->bstr)
   #x84 (fit-type #x84 'uint16 #f 2 #xFFFF bstr->integer integer->bstr)

   #x85 (fit-type #x85 'sint32 #t 4 #x7FFFFFFF bstr->integer integer->bstr)
   #x86 (fit-type #x86 'uint32 #f 4 #xFFFFFFFF bstr->integer integer->bstr)

   #x07 (fit-type #x07 'string #f 1 #x00 bstr->integer integer->bstr)

   #x88 (fit-type #x88 'float32 #f 4 #xFFFFFFFF bstr->real real->bstr)
   #x89 (fit-type #x89 'float64 #f 8 #xFFFFFFFFFFFFFFFF bstr->real real->bstr)

   #x0a (fit-type #x0a 'uint8z #f 1 #x00 bstr->integer integer->bstr)
   #x8b (fit-type #x8b 'uint16z #f 2 #x00 bstr->integer integer->bstr)
   #x8c (fit-type #x8c 'uint32z #f 4 #x00 bstr->integer integer->bstr)

   #x8e (fit-type #x8e 'sint64 #t 8 #x7FFFFFFFFFFFFFFF bstr->integer integer->bstr)
   #x8f (fit-type #x8f 'uint64 #f 8 #xFFFFFFFFFFFFFFFF bstr->integer integer->bstr)
   #x90 (fit-type #x90 'uint64z #f 8 #x00 bstr->integer integer->bstr)

   #x0d (fit-type #x0d 'byte #f 1 #xFF bstr->integer integer->bstr)))

(define (get-fit-type id)
  ;; Return a fit type based on the ID which can be the actual fit type, its
  ;; number or name.  Return #f if the type is not found or ID is not one of
  ;; the expected values.
  (cond ((fit-type? id) id)
        ((number? id) (hash-ref
                       fit-types id
                       (lambda () (raise-error "bad fit type: ~a" id))))
        ((symbol? id)
         (for/first ([t (in-hash-values fit-types)]
                     #:when (eq? id (fit-type-name t)))
           t))
        (#t #f)))

(define (read-one-fit-value buf pos type big-endian?)
  ;; Read a single value from BUF@POS of the specified TYPE.
  (let ((read-fn (fit-type-read-fn type))
        (size (fit-type-size type))
        (signed? (fit-type-signed? type)))
    (when (> (+ pos size) (bytes-length buf))
      (raise-error "read past end of buffer"))
    (let ((raw-val (bstr->integer buf #f big-endian? pos (+ pos size))))
      (if (equal? raw-val (fit-type-invalid-value type))
          (values #f (+ pos size))
          (values
           (read-fn buf signed? big-endian? pos (+ pos size))
           (+ pos size))))))

(define (read-fit-value buf pos size type big-endian?)
  ;; Read one or more values from BUF@POS of the specified type.  SIZE is the
  ;; total size of the bytes to read (SIZE / SIZEOF(type) determines the
  ;; number of values read).  Returns two values: value or vector of values
  ;; plus the new buffer position.
  ;;
  ;; NOTE: Coros2 has a bug which stores the size as 1 for a 32 bit (4 byte)
  ;; integer.  We allow this here by setting the size to the size of the fit
  ;; type (4 bytes), but it is a bug and the fix works for Coros2 only.  See
  ;; also notes on f0040 and f0041.
  (let* ([fts (fit-type-size type)]
         [nitems (if (< size fts)
                     (if (= size 1)
                         fts
                         (raise-error "bad size" size))
                     (/ size fts))])
    (unless (integer? nitems)
      (raise-error "bad number of items" nitems))
    (if (equal? nitems 1)
        (read-one-fit-value buf pos type big-endian?)
        (let ((result (make-vector nitems #f)))
          (let loop ((pos pos)
                     (i 0))
            (when (< i nitems)
              (let-values (([v p] (read-one-fit-value buf pos type big-endian?)))
                (vector-set! result i v)
                (loop p (+ i 1)))))
          (values result (+ pos size))))))

(define (write-one-fit-value buf pos type value big-endian?)
  ;; Write at BUF@POS the specified VALUE according to TYPE.  If VALUE is #f,
  ;; the type's "invalid" value is written (see the fit-type struct
  ;; definition).  Returns the new buffer position.
  (let ((write-fn (fit-type-write-fn type))
        (size (fit-type-size type))
        (signed? (fit-type-signed? type)))
    (when (> (+ pos size) (bytes-length buf))
      (raise-error "write past end of buffer"))
    ;; NOTE: if value is #f, we write the invalid value to the stream.
    (write-fn (or value (fit-type-invalid-value type))
              size signed? big-endian? buf pos)
    (+ pos size)))

(define (write-fit-value buf pos type value big-endian?)
  ;; Write at BUF@POS the specified VALUE accorting to TYPE.  VALUE can be
  ;; either #f, an individual value or a vector of values.
  (cond ((vector? value)
         (let loop ((index 0)
                    (pos pos))
           (if (< index (vector-length value))
               (loop
                (+ index 1)
                (write-one-fit-value buf pos type (vector-ref value index) big-endian?))
               pos)))
        (#t
         (write-one-fit-value buf pos type value big-endian?))))


;..................................................... fit-data-stream% ....

(define fit-data-stream%
  ;; Helper class to read values from a FIT file.  The object is initialized
  ;; with a byte string, it will validate the header than provide a
  ;; `read-next-value' method to read values.  It will keep track of the
  ;; current buffer position internally.

  (class object% (init data) (super-new)

    (define buffer data)
    (define crtpos 0)
    (define limit (bytes-length buffer)) ; will be updated below

    (define/public (is-eof?) (>= crtpos limit))

    (define/public (position) crtpos)

    (define/public (read-next-value type-id [size #f] [big-endian? #f])
      ;; Read a value of the specified type (TYPE-ID) from the stream at the
      ;; current position (which will be updated).
      (let* ((type (get-fit-type type-id))
             (sz (or size (fit-type-size type))))
        (let-values (([result new-crtpos]
                      (read-fit-value buffer crtpos sz type big-endian?)))
          (set! crtpos new-crtpos)
          (if (and result (eq? (fit-type-name type) 'string))
              ;; convert the result to a string
              (if (equal? size 1)
                  (bytes result)
                  (let ((b '()))
                    (let loop ((idx 0))
                      (when (and (< idx (vector-length result)) (vector-ref result idx))
                        (set! b (cons (vector-ref result idx) b))
                        (loop (+ 1 idx))))
                    (apply bytes (reverse b))))
              result))))

    ;; Start reading the header and performing validation on the FIT
    ;; file. WARNING: we depend on these declaration being in the order below,
    ;; as we are just reading from the buffer now.

    (define header-length (read-next-value 'uint8))

    (unless (>= header-length 12) (raise-error "bad header length"))

    (define protocol-version (read-next-value 'uint8))
    (define profile-version (read-next-value 'uint16))
    (define data-length (read-next-value 'uint32))
    (unless (equal? (read-next-value 'string 4) #".FIT")
      (raise-error "missing .FIT signature"))

    ;; two bytes count the CRC at the end.
    (let ((expected (+ header-length data-length 2))
          (actual (bytes-length buffer)))
      ;; NOTE: fit files can contain chunks, which are effectively multiple
      ;; FIT files concatenated, so we check for actual being greater or equal
      ;; to expected. This FIT file feature is used to record HR data in
      ;; swimming activities (where the strap stores the data and transmits it
      ;; at the end of the activity).  We don't support loading HR data from
      ;; such FIT files yet, but at least we can load the swimming part.
      (unless (>= actual expected)
        (raise-error "bad data-length: ~a, expecting ~a" actual expected))
      ;; Calculate CRC only on the actual chunk used by the header
      (unless (= (compute-crc buffer 0 expected) 0)
        (raise-error "bad file CRC")))

    ;; Now that we have read the header, update crtpos and limit to match the
    ;; actual data content of the buffer.
    (set! crtpos header-length)
    (set! limit (+ header-length data-length))

    ))

(define (make-fit-data-stream source)
  ;; Construct a fit-data-stream% from SOURCE which can be either a byte
  ;; string, a file name or port.
  (let ([data (cond ((bytes? source) source)
                    ((or (path? source) (string? source))
                     (file->bytes source #:mode 'binary))
                    ((input-port? source)
                     (port->bytes source)))])
    (when (and (>= (bytes-length data) 3)
               (= (bytes-ref data 0) #x1f)
               (= (bytes-ref data 1) #x8b)
               (= (bytes-ref data 2) #x08))
      ;; This file is compressed using GZIP, decompress it first
      (let ((in (open-input-bytes data))
            (out (open-output-bytes)))
        (gunzip-through-ports in out)
        (set! data (get-output-bytes out))))
    (new fit-data-stream% [data data])))


;.................................... reading records from a fit stream ....

(define (decode-record-header header)
  ;; Decode the header for a record in a fit file.  The header is a single
  ;; byte.  We return a list of:
  ;; * header type ('normal or 'compressed-timestamp)
  ;; * 'data or 'defintion record
  ;; * local-message-id
  ;; * maybe timestamp offset for a compressed-timestamp header.
  (let ((header-type (if (= (bitwise-and header #x80) 0)
                         'normal 'compressed-timestamp)))
    (if (eq? header-type 'normal)
        (list
         'normal
         (if (= (bitwise-and header #x40) 0) 'data 'definition)
         (bitwise-and header #x0F)      ; local message id
         (if (= (bitwise-and header #x20) 0) 'standard 'custom)
         )
        (list
         'compressed-timestamp
         'data
         (bitwise-bit-field header 5 7) ; local message id, note that the bit field is open ended! ()
         (bitwise-and header #x1F)      ; timestamp
         ))))

(define (read-message-definition fit-stream standard-or-custom)
  ;; Read a message definition from the FIT-STREAM.  A message definition will
  ;; tell us what fields are available in a certain message and what the type
  ;; of these fields is.  STANDARD-OR-CUSTOM is a symbol (either 'standard or
  ;; 'custom) which tells us if we should expect developer fields in the
  ;; message definition.
  (send fit-stream read-next-value 'uint8) ; reserved field, skip it
  (let* ((arhitecture (if (= (send fit-stream read-next-value 'uint8) 0)
                          'little-endian 'big-endian))
         (global-message-number (send fit-stream read-next-value 'uint16 2
                                      (eq? arhitecture 'big-endian)))
         (global-message-name (dict-ref *global-message-number* global-message-number #f))
         (field-count (send fit-stream read-next-value 'uint8))
         (field-names (if global-message-name
                          (dict-ref *field-db* global-message-name #f)
                          #f)))
    (append
     (list arhitecture (or global-message-name global-message-number))
     ;; Standard fields come first
     (for/list ([i (in-range field-count)])
       (let* ((number (send fit-stream read-next-value 'uint8))
              (size (send fit-stream read-next-value 'uint8))
              (type (send fit-stream read-next-value 'uint8))
              (name (if field-names (dict-ref field-names number #f) #f)))
         (list (or name number) size type)))
     ;; Developer specific fields (if any) come last
     (let ((dev-field-count (if (eq? standard-or-custom 'custom)
                                (send fit-stream read-next-value 'uint8)
                                0)))
       (for/list ([i (in-range dev-field-count)])
         (let* ((number (send fit-stream read-next-value 'uint8))
                (size (send fit-stream read-next-value 'uint8))
                (ddi (send fit-stream read-next-value 'uint8))) ; dev data index
           ;; Dev data fields are encoded by adding 1000 to them, so they are
           ;; not confused with FIT types, which are all less than 255.
           (list number size (+ 1000 ddi))))))))

(define (make-message-reader definition dev-field-types)
  ;; Return a function which will read a message from a FIT-STREAM according
  ;; to DEFINITION (as constructed by `read-message-definition')
  ;; DEV-FIELD-TYPES contains a mapping from a DDI to the actual FIT type for
  ;; the field.

  (define (convert-value value field-name conversions)
    ;; Convert VALUE for FIELD-NAME into a more usable format accorting to the
    ;; CONVERSIONS ALIST.  For example, speed is stored multiplied by 1000
    ;; (that is, millimeters per second) so we convert it back to
    ;; meters/second.  Latitude and Longitude are stored as "semirircles", we
    ;; convert them back to degrees.
    (let ((convert-fn (cond ((assq field-name conversions) => cdr)
                            (#t (lambda (x) x)))))
      (if (vector? value)
          (for/vector ((v (in-vector value))) (convert-fn v))
          (convert-fn value))))

  (define conversion-table
    ;; conversion-table to use with `convert-value' for this message id.
    (cond ((assq (second definition) *field-conversion-db*) => cdr)
          (#t '())))

  (define (read-value-fn type size stream)
    (send stream read-next-value type size big-endian?))

  (define big-endian?
    (not (eq? (car definition) 'little-endian)))

  (lambda (stream)
    (for/list ([field (cdr (cdr definition))])
      (match-define (list name size type) field)
      (cond ((>= type 1000)
             ;; this is a DDI, find the actual type and read it.  Don't do any
             ;; conversion on it, but use the specified field name for it, if
             ;; it is available
             (match-let (((list dname dtype)
                          (hash-ref dev-field-types
                                    (cons type name)
                                    (lambda ()
                                      (raise-error "unknown dev field: name ~a index ~a" name (- type 1000))))))
               (cons (or dname name) (read-value-fn dtype size stream))))
            (#t
             (let ((value (read-value-fn type size stream)))
               (cons name (and value (convert-value value name conversion-table)))))))))

(define (read-fit-records fit-stream dispatcher)
  ;; Read all data records from FIT-STREAM (a fit-data-stream%) and send them
  ;; to the DISPATCHER by calling its dispatch method.  This function keeps
  ;; track of message defintions and uses them to decode the actual messages.
  ;; The DISPATCHER is an object with a signle method "dispatch message-num
  ;; data", but see fit-event-dispatcher% for a nicer interface.

  (define message-readers (make-hash))
  ;; Map a dev-data index to the basic FIT field type for that field.
  (define dev-field-types (make-hash))
  (define app-defs (make-hash))

  (define (dev-field-key ddi number name)
    (let ((app-id (hash-ref app-defs ddi #f)))
      (if app-id
          (string->symbol (string-append app-id "-" (~a number)))
          (string->symbol
           (if name
               (bytes->string/utf-8 name)
               (format "dev-field-~a" ddi))))))

  (define (make-string-id id)
    (if (vector? id)                    ; Coros2 uses integers, see f0040
        (apply string-append
               (for/list ([i (in-vector id)])
                 (if (number? i)
                     (~r i #:precision 0 #:base 16 #:min-width 2 #:pad-string "0")
                     "ff")))
        (~a id)))

  (define (read-next-record)
    (let* ((hdr (or (send fit-stream read-next-value 'uint8) 255))
           (header (decode-record-header hdr)))
      ;; (printf "header(~a pos = ~a): ~a~%" hdr (send fit-stream position) header)
      (match-define (list htype def-or-data local-id rest ...) header)
      (cond ((eq? def-or-data 'definition)
             (let ((def (read-message-definition fit-stream (car rest))))
               ;; (display def)(newline)
               ;; (display (format "DEFN local: ~a, global: ~a, ~a field(s)~%"
               ;;                  (third header)
               ;;                  (second def)
               ;;                  (length (cdr (cdr def)))))
               (hash-set! message-readers
                          local-id
                          (cons (second def) (make-message-reader def dev-field-types))))
             #t)
            ((eq? def-or-data 'data)
             (let ((reader (hash-ref message-readers local-id #f)))
               (unless reader
                 (raise-error "no reader for local message id ~a" header))
               ;; (display (format "DATA local: ~a (~a)~%" (third header) (car reader)))
               (let ((message-id (car reader))
                     (message-data ((cdr reader) fit-stream)))
                 ;; (printf "ID: ~a~%    DATA CONTENTS: ~a~%" message-id message-data)
                 (cond ((eq? message-id 'developer-data-id)
                        ;; A developer-data-id message "announces" a new XDATA
                        ;; application.  We convert the developer-id and
                        ;; application-id fields from an array of bytes to
                        ;; string guid and also record this application in
                        ;; APP-DEFS
                        (let ((devid (dict-ref message-data 'developer-id #f))
                              (appid (dict-ref message-data 'application-id #f))
                              (ddi (dict-ref message-data 'developer-data-index #f)))
                          (when appid
                            (define app-key (make-string-id appid))
                            (set! message-data
                                  (cons (cons 'application-id app-key)
                                        (dict-remove message-data 'application-id)))
                            (when ddi
                              (hash-set! app-defs ddi app-key)))
                          (when devid
                            (set! message-data
                                  (cons (cons 'developer-id (make-string-id devid))
                                        (dict-remove message-data 'developer-id))))))
                       ((eq? message-id 'field-description)
                        ;; A field-description message "announces" a new XDATA
                        ;; field.  The field will be present in the records
                        ;; using a unique key (see below). The key is also
                        ;; added to the field as a 'field-key entry for easier
                        ;; processing of XDATA later on.
                        (let* ((ddi (dict-ref message-data 'developer-data-index #f))
                               ;; Have dev fields with missing type and number
                               ;; -- not sure how this works, but Garmin
                               ;; Connect accepts such fit files, so we use
                               ;; some defaults here...
                               (type (dict-ref message-data 'fit-base-type #x0d))
                               (number (dict-ref message-data 'field-def-number 1))
                               (name (dict-ref message-data 'field-name #f))
                               (units (dict-ref message-data 'units #f)))
                          (unless ddi
                            (raise-error "Missing developer-data-index: ~a~%" message-data))
                          (define key (dev-field-key ddi number name))

                          (hash-set! dev-field-types (cons (+ 1000 ddi) number) (list key type))
                          ;; NOTE: we need to store the application id in the
                          ;; field, the developer-data-index is not unique and
                          ;; will be overriden (there is a test that will
                          ;; catch this)
                          (set! message-data `((application-id . ,(hash-ref app-defs ddi #f))
                                               (field-key . ,key)
                                               ,@message-data))
                          (when name
                            (set! message-data
                                  (cons (cons 'field-name (bytes->string/utf-8 name))
                                        (dict-remove message-data 'field-name))))
                          (when units
                            (set! message-data
                                  (cons (cons 'units (bytes->string/utf-8 units))
                                        (dict-remove message-data 'units)))))))
                 ;; Developer data ID and field description messages are also
                 ;; sent to the dispatcher, which will be responsibe for
                 ;; interpreting these fields.  The decoder will use the
                 ;; field-key, not the field ID.
                 (send dispatcher dispatch
                       message-id
                       (if (eq? htype 'compressed-timestamp)
                           (cons (cons 'compressed-timestamp (car rest)) message-data)
                           message-data)))))
            (#t
             (raise-error "bad header: ~a" header)))))

  (define (loop)
    (unless (send fit-stream is-eof?)
      (read-next-record)
      (loop)))

  (loop))


;................................................ fit-event-dispatcher% ....

(define fit-event-dispatcher%
  ;; Decode and dispatch fit-event messsages to different "on-..." methods
  ;; that can be overriden by the user to do something usefull.  Also keeps
  ;; track of the current time in the FIT message and expands
  ;; 'compresset-timestamp' fields.  An object derived from this class can be
  ;; passed to `read-fit-records'
  ;;
  ;; During FIT file parsing, `read-fit-records` will pass the decoded FIT
  ;; file messages to a builder, an object derived from fit-event-dispatcher%
  ;; using various "on-" methods, one for each message type.  The builder
  ;; object will collect the messages and construct a Racket structure that is
  ;; suitable for the application.  Understanding the various messages
  ;; requires familiarity with the FIT file format and its messages.  These
  ;; are available in the FitSDK, in the Profile Excel document in it.

  (class object% (init) (super-new)

    (define start-timestamp #f)
    (define current-timestamp #f)

    (define (update-timestamp record)

      ;; Update start-timestamp, current-timestamp from the current RECORD,
      ;; taking special care not to move the time backwards.
      (let ((ts (dict-ref record 'timestamp current-timestamp)))
        (if (equal? ts *fit-epoch*)    ; somebody just wrote a 0 for the timestamp!
            (when current-timestamp
              (set! record (cons (cons 'timestamp current-timestamp) record)))
            (set! current-timestamp
                  (if current-timestamp (max ts current-timestamp) ts))))

      (unless start-timestamp
        (set! start-timestamp current-timestamp))

      (let ((st (dict-ref record 'start-time #f)))
        (when (and (or (not st) (equal? st *fit-epoch*)) current-timestamp)
          (set! record (cons (cons 'start-time current-timestamp) record))))

      ;; If the record has a compressed-timestamp, add a real timestamp field
      ;; to it.
      (if (and current-timestamp (assq 'compressed-timestamp record))
          ;; NOTE: the compressed timestamp is 5 bits (0-31) and it replaces
          ;; the bottom 5 bits of the current timestamp, we DON'T simply add
          ;; the offset to the current timestamp.  To make things more
          ;; complicated, we also need to account for rollover.
          (let* ((offset (cdr (assq 'compressed-timestamp record)))
                 (bottom-bits (bitwise-and current-timestamp #x1F))
                 (new-ts (if (>= offset bottom-bits)
                             (+ (- current-timestamp bottom-bits) offset)
                             ;; timestamp rolled over, take that into account
                             (+ (- current-timestamp bottom-bits) offset #x20))))
            (cons (cons 'timestamp new-ts) record))
          record))

    (define/public (get-start-timestamp) start-timestamp)
    (define/public (get-current-timestamp) current-timestamp)

    ;; These methods need to be overriden to do something useful
    (define/public (on-file-id file-id) #f)
    (define/public (on-file-creator creator) #f)
    (define/public (on-activity activity) #f)
    (define/public (on-session session) #f)
    (define/public (on-record record) #f)
    (define/public (on-length length) #f)
    (define/public (on-lap lap) #f)
    (define/public (on-device-info device-info) #f)
    (define/public (on-location location) #f)
    (define/public (on-workout workout) #f)
    (define/public (on-workout-step workout-step) #f)
    (define/public (on-sport sport) #f)
    (define/public (on-hrv data) #f)
    (define/public (on-developer-data-id data) #f)
    (define/public (on-field-description data) #f)
    (define/public (on-training-file data) #f)
    (define/public (on-weather-conditions data) #f)
    (define/public (on-weather-report data) #f)
    (define/public (on-course data) #f)
    (define/public (on-course-point data) #f)

    ;; NOTE: on-activity and on-session are also events, so the user could
    ;; call on-event for those as well if needed.  this could be important if
    ;; timer-start/timer-stop events are tracked.
    (define/public (on-event event) #f)
    (define/public (on-other type data) #f)
    ;; ----------------------------------------------------------------------

    (define/public (dispatch message-type record)
      ;; Dispatch RECORD to one of the "on-..." methods depending on the
      ;; MESSAGE-TYPE.
      (let ((record (update-timestamp record)))
        ;; Remove all fields that have no values (they have #f as the value)
        (set! record (filter cdr record))

        (case message-type
          ((file-id) (on-file-id record))
          ((file-creator) (on-file-creator record))
          ((session) (on-session record))
          ((lap) (on-lap record))
          ((length) (on-length record))
          ((record) (on-record record))
          ((activity) (on-activity record))
          ((device-info) (on-device-info record))
          ((event) (on-event record))
          ((location) (on-location record))
          ((workout) (on-workout record))
          ((workout-step) (on-workout-step record))
          ((sport) (on-sport record))
          ((hrv) (on-hrv record))
          ((developer-data-id) (on-developer-data-id record))
          ((field-description) (on-field-description record))
          ((training-file) (on-training-file record))
          ((weather-conditions) (on-weather-conditions record))
          ((weather-report) (on-weather-report record))
          ((course) (on-course record))
          ((course-point) (on-course-point record))
          (else (on-other message-type record)))))

    ))


;;..................................................... activity-builder ....

;; Multiplier to convert angles from the FIT representation to degrees.
(define angle-mult (/ 360 256))

(define (extract-angle record field index)
  (let ((pp (dict-ref record field #f)))
    (if (and pp (vector-ref pp index))
        (* (vector-ref pp index) angle-mult)
        #f)))

;; Some fields in FIT records are inconvenient to use so we process them
;; somewhat.  This is a table containing a field name and a function to obtain
;; a value for that field.
(define mappings
  `(;; Ensure the record has a start-time timestamp, borrow it from the
    ;; 'timestamp' value if needed.
    (start-time . ,(lambda (t) (or (dict-ref t 'start-time #f) (dict-ref t 'timestamp #f))))

    ;; cadences (including AVG and MAX) are stored as an integer plus an
    ;; optional fractional part.  We store it as a real number internally.
    ;; Also swimming candence has a different field name.
    (cadence . ,(lambda (t)
                  (let ((c (dict-ref t 'cadence #f))
                        (f (dict-ref t 'fractional-cadence #f)))
                    (if (and (number? c) (number? f))
                        (+ c f)
                        c))))
    (avg-cadence . ,(lambda (t)
                      (or (dict-ref t 'avg-swimming-cadence #f)
                          (let ((c (dict-ref t 'avg-cadence #f))
                                (f (dict-ref t 'avg-fractional-cadence #f)))
                            (if (and (number? c) (number? f))
                                (+ c f)
                                c)))))
    (max-cadence . ,(lambda (t)
                      (or (dict-ref t 'max-swimming-cadence #f)
                          (let ((c (dict-ref t 'max-cadence #f))
                                (f (dict-ref t 'max-fractional-cadence #f)))
                            (if (and (number? c) (number? f))
                                (+ c f)
                                c)))))

    ;; Swimming activites have a different name for total cycles.
    (total-cycles . ,(lambda (t)
                       (or (dict-ref t 'total-cycles #f) (dict-ref t 'total-strokes #f))))

    ;; Gen2 Running Dynamics introduces GCT balance, we roll it into
    ;; left-right-balance
    (left-right-balance . ,(lambda (t)
                             (or (dict-ref t 'left-right-balance #f)
                                 (dict-ref t 'stance-time-balance #f))))

    ;; Power phase start and end values are stored as a vector of values,
    ;; we store each individual value separately.  Same for peak power
    ;; phase.
    (left-pp-start . ,(lambda (t) (extract-angle t 'left-pp 0)))
    (left-pp-end . ,(lambda (t) (extract-angle t 'left-pp 1)))

    (right-pp-start . ,(lambda (t) (extract-angle t 'right-pp 0)))
    (right-pp-end . ,(lambda (t) (extract-angle t 'right-pp 1)))

    (left-ppp-start . ,(lambda (t) (extract-angle t 'left-peak-pp 0)))
    (left-ppp-end . ,(lambda (t) (extract-angle t 'left-peak-pp 1)))

    (right-ppp-start . ,(lambda (t) (extract-angle t 'right-peak-pp 0)))
    (right-ppp-end . ,(lambda (t) (extract-angle t 'right-peak-pp 1)))

    (avg-left-pp-start . ,(lambda (t) (extract-angle t 'avg-left-pp 0)))
    (avg-left-pp-end . ,(lambda (t) (extract-angle t 'avg-left-pp 1)))

    (avg-right-pp-start . ,(lambda (t) (extract-angle t 'avg-right-pp 0)))
    (avg-right-pp-end . ,(lambda (t) (extract-angle t 'avg-right-pp 1)))

    (avg-left-ppp-start . ,(lambda (t) (extract-angle t 'avg-left-peak-pp 0)))
    (avg-left-ppp-end . ,(lambda (t)(extract-angle t 'avg-left-peak-pp 1)))

    (avg-right-ppp-start . ,(lambda (t) (extract-angle t 'avg-right-peak-pp 0)))
    (avg-right-ppp-end . ,(lambda (t) (extract-angle t 'avg-right-peak-pp 1)))

    ;; Enhanced speed and altitude are just higher resolution fields in
    ;; the FIT file, since we are not constrained by integer sizes, we use
    ;; the highest resolution available
    (speed . ,(lambda (t) (or (dict-ref t 'enhanced-speed #f) (dict-ref t 'speed #f))))
    (max-speed . ,(lambda (t) (or (dict-ref t 'enhanced-max-speed #f) (dict-ref t 'max-speed #f))))
    (avg-speed . ,(lambda (t) (or (dict-ref t 'enhanced-avg-speed #f) (dict-ref t 'avg-speed #f))))
    (altitude . ,(lambda (t) (or (dict-ref t 'enhanced-altitude #f) (dict-ref t 'altitude #f))))
    (max-altitude . ,(lambda (t) (or (dict-ref t 'enhanced-max-altitude #f) (dict-ref t 'max-altitude #f))))
    (min-altitude . ,(lambda (t) (or (dict-ref t 'enhanced-min-altitude #f) (dict-ref t 'min-altitude #f))))

    ))

(define (process-fields record)
  ;; Convert some fields inside RECORD from the FIT representation to the more
  ;; convenient internal representation.
  (define new-fields
    (for/list ([m (in-list mappings)]
               #:do ((define processed ((cdr m) record)))
               #:when processed)
      (cons (car m) processed)))

  ;; Remove fields from RECORD which are already in NEW-FIELDS
  (define record-w/o-new-fields
    (filter
     (lambda (t)
       (not (dict-ref new-fields (car t) #f)))
     record))

  (append new-fields record-w/o-new-fields))

;; Decode a 32 bit word which contains gear change information into 4 values:
;; rear / front gear number (1 is innermost) and front / rear tooth count.
;;
;; Note that the tooth count is likely incorrect.  It needs to be configured
;; explicitly by the user in their Garmin device.  Garmin seems to default to
;; 11-23 for the rear and 53-39 for the front, which will probably be wrong
;; for most modern bikes.
(define (decode-gear-change-data data)
  (let ([rear-gear-num (bitwise-and data #xff)]
        [rear-gear (arithmetic-shift (bitwise-and data #xff00) -8)]
        [front-gear-num (arithmetic-shift (bitwise-and data #xff0000) -16)]
        [front-gear (arithmetic-shift (bitwise-and data #xff000000) -24)])
    (values rear-gear-num rear-gear front-gear-num front-gear)))

;; Create a activity entry from a gear change event -- note that we don't
;; store the event type (front or rear gear change), since we can determine
;; that from what has changed between the last event and the current one.
(define (convert-gear-change-event e)
  (unless (member (dict-ref e 'event #f) '(front-gear-change rear-gear-change))
    (error (format "not a gear change event: ~a" e)))
  (define-values (rgn rg fgn fg) (decode-gear-change-data (dict-ref e 'data 0)))
  `((timestamp . ,(dict-ref e 'timestamp #f))
    (rear-gear-num . ,rgn)
    (rear-gear . ,rg)
    (front-gear-num . ,fgn)
    (front-gear . ,fg)))

(define activity-builder%
  ;; Build an activity from a FIT file.  An instance of this class can be used
  ;; as an event dispatcher to `read-fit-records' and the activity can be
  ;; obtained at the end by calling collect-activity.
  (class fit-event-dispatcher% (init) (super-new)

    (inherit get-start-timestamp)
    (inherit get-current-timestamp)

    (define activity-timestamp #f)
    (define activity-guid #f)

    (define sessions '())
    (define laps '())
    (define lengths '())
    (define records '())
    (define devices '())
    (define sport '())
    (define training-file '())
    (define weather-conditions '())
    (define weather-reports '())
    (define gear-change-events '())

    ;; FIT 2.0 allows "developer" fields, these hold the definitions, for
    ;; referencing the dev fields in trackpoint data.
    (define developer-data-ids '())
    (define field-descriptions '())

    (define timer-stopped #f)
    (define timer-stop-timestamp 0)

    (define/override (on-file-id file-id)
      (unless activity-guid
        ;; Some activitites contain multiple file-id messages, keep the first
        ;; one only.
        (let ((serial-number (dict-ref file-id 'serial-number #f))
              (time-created (dict-ref file-id 'time-created #f))
              (file-type (dict-ref file-id 'type #f)))
          (unless (eq? file-type 'activity)
            (raise-error (format "not an activity: ~a" file-type)))
          ;; We use the device serial and time-created as a unique identifier
          ;; for the activity.
          (set! activity-guid (format "~a-~a" serial-number time-created)))))

    (define/public (get-guid) activity-guid)

    (define/override (on-activity activity)
      (set! activity-timestamp (dict-ref activity 'timestamp #f))
      ;; nothing more to do with this one.  the activity-guid comes from the
      ;; file-id message.
      )

    (define/override (on-session session)
      (define psession (process-fields session))
      ;; Copy sport/sub-sport values from a sport record, if the session does
      ;; not already have them
      (unless (dict-ref session 'sport #f)
        (cond ((dict-ref sport 'sport #f)
               => (lambda (v)
                    (set! psession (cons (cons 'sport v) psession))))))
      (unless (dict-ref session 'sub-sport #f)
        (cond ((dict-ref sport 'sub-sport #f)
               => (lambda (v)
                    (set! psession (cons (cons 'sub-sport v) psession))))))
      (set! sport '())
      (set! sessions (cons psession sessions)))

    (define/override (on-record record)
      (define precord (process-fields record))
      (if (null? records)
          (set! records (cons precord records))
          ;; Merge records that have the same timestamp
          ;;
          ;; Bryton uses records with the same timestamp, but different
          ;; content (f0001), but Wattbike uses sub-second data sampling and
          ;; has distinct records with the same timestamp (f0053).
          ;;
          ;; TODO: we should probably identify Bryton devices the same way as
          ;; we use use-coros2-hack?, and only merge records in that case,
          ;; however, unclear what the bigger impact would be of keeping
          ;; records with the same timestamp.
          (let ((last-record (car records)))
            (if (equal? (dict-ref precord 'timestamp #f)
                        (dict-ref last-record 'timestamp #t))
                ;; Merge the records, as they share timestamps
                (set! records (cons (append precord last-record) (cdr records)))
                (set! records (cons precord records))))))

    (define/override (on-length len)
      (define plen (process-fields len))
      (set! lengths (cons plen lengths)))

    (define/override (on-lap lap)
      (define plap (process-fields lap))

      ;; Wattbike lap start time adjustment.  Wattbike's start-time is
      ;; "seconds since the start of the activitiy", plus they subtract the
      ;; FIT-epoch from it, resulting in a negative number, cast this signed
      ;; value to an unsigned one and write it to the file.  When we read it
      ;; back, this produces a very large 32 bit unsigned value, to which we
      ;; add the *fit-epoch*, since we assume it is a normal FIT date-time
      ;; value.
      ;;
      ;; All these adjustments result in a completely unrealistic timestamp
      ;; and here we attempt to undo this damage...
      (let ([st (dict-ref plap 'start-time #f)])

        ;; If we have a timestamp and it is a negative value in signed-32-bit
        ;; representation...
        (when (and st (> st #x8000000))
          (let ([activity-start (or activity-timestamp (get-start-timestamp))])
            (when activity-start
              ;; ... plus, this timestamp is waaay off from the activtiy start
              ;; -- this check allows us to use correct FIT timestamps beyond
              ;; 2038, failing Wattbike timestamps instead.
              (when (> (- st activity-start) (* 24 3600 10))
                ;; Full "undo damage" formula is:
                ;;
                ;; * t1 = st - fit-epoch   ; undo our fit-epoch adjustment
                ;; * t2 = t1 - #x100000001 ; convert to signed value
                ;; * t3 = t2 + fit-epoch   ; undo Wattbike fit-epoch adjustment
                ;;
                ;; All this simplifies to: (- st #x100000001), since we
                ;; subtract than add fit-epoch.
                (let ([ast (+ (- st #x100000001) activity-start)])
                  (set! plap (dict-set (dict-remove plap 'start-time) 'start-time ast))))))))

      (set! laps (cons plap laps)))

    (define/override (on-device-info device-info)
      ;; (display (format "*** DEVICE-INFO ~a~%" device-info))
      ;; (let ((index (cond ((assq 'device-index device-info) => cdr)
      ;;                    (#t #f))))
      ;;   (when index (hash-set! devices index device-info)))
      (set! devices (cons device-info devices))
      (when (and (equal? 294 (dict-ref device-info 'manufacturer #f))
                 (equal? #"COROS PACE 2" (dict-ref device-info 27 #f)))
        (set! use-coros2-hack? #t))
      #t)

    (define/override (on-training-file tf)
      (set! training-file (cons tf training-file)))

    (define/override (on-weather-conditions w)
      (set! weather-conditions (cons w weather-conditions)))

    (define/override (on-weather-report r)
      (set! weather-reports (cons r weather-reports)))

    (define/override (on-sport data)
      (set! sport data))

    (define/override (on-event event)
      (let ((timestamp (dict-ref event 'timestamp #f))
            (e (dict-ref event 'event #f))
            (type (dict-ref event 'event-type #f)))

        (cond
          ((eq? e 'timer)
           (cond ((eq? type 'stop-all)
                  ;; (when (pair? records)
                  ;;   (display (car records))(newline))
                  (set! timer-stopped #t)
                  (set! timer-stop-timestamp timestamp))
                 ((eq? type 'start)
                  (when timer-stopped
                    ;; (display (format "*** PAUSE ~a seconds~%" (- timestamp timer-stop-timestamp)))
                    (set! timer-stopped #f)))
                 (#t
                  ;; (display (format "*** Unknown timer event ~a~%" event))
                  #t)))
          ((eq? e 'session)              ; not interested in these ones
           #t)
          ((member e '(front-gear-change rear-gear-change))
           (set! gear-change-events (cons event gear-change-events)))
          (#t
           ;; (display (format "*** EVENT: ~a~%" event))
           )))
      #t)

    #;(define/override (on-other type data)
      (printf "*** on-other type = ~a, data = ~a~%" type data))

    (define/override (on-developer-data-id data)
      (set! developer-data-ids (cons data developer-data-ids)))

    (define/override (on-field-description data)
      (set! field-descriptions (cons data field-descriptions)))

    ;; The Coros2 watch writes the timestamp in the start-time field and
    ;; start-time in the timestamp field, however, other FIT files, which use
    ;; these fields correctly, might use a later timestamp from the start time
    ;; causing problems with associating records to fit files, so we only
    ;; apply the "fix" it if we detect a file generated by a Coros2 watch.
    (define use-coros2-hack? #f)

    (define/private (get-start-time record)
      ;; NOTE: start-time is the time when an event occurred, while timestamp
      ;; is the time time when the record of the event was written to the FIT
      ;; file and the two might be unrelated.
      (define st (dict-ref record 'start-time #f))
      (define ts (dict-ref record 'timestamp #f))
      (if (and use-coros2-hack? st ts)
          (min st ts)
          (or st ts)))

    (define/public (display-devices)
      (for ((v (in-list (reverse devices))))
        (display "*** ")(display v)(newline)))

    (define/public (collect-activity)

      #;(printf "*** collect-activity: ~a records, ~a lengths, ~a laps, ~a sessions~%"
              (length records)
              (length lengths)
              (length laps)
              (length sessions))

      (define ((make-start-time-checker start-time) record)
        (define record-timestamp (get-start-time record))
        ;; If the record has no start time (yes it happens), consider it part
        ;; of this selection (i.e. preserve record order as they appear in the
        ;; FIT file)
        (if (real? record-timestamp)
            (>= record-timestamp start-time)
            #t))

      ;; NOTE: session, lap and length and record items can appear in any
      ;; order with respect to each other (e.g. all lap records can be before
      ;; the length records).  However, we assume that for a certain item
      ;; type, those items are in order (e.g. all laps records are in order
      ;; with respect to each other).  Furthermore, since we use `cons`, these
      ;; items are in reverse time order (i.e most recent first).

      (define rsessions
        (let session-loop ([sessions sessions]
                           [laps laps]
                           [lengths lengths]
                           [records records]
                           [devices devices]
                           [weather weather-conditions]
                           [gear-change gear-change-events]
                           [rsessions '()])
          (if (null? sessions)
              (begin
                (unless (null? laps)
                  (printf "*** leftover laps (~a items)~%" (length laps)))
                (unless (null? lengths)
                  (printf "*** leftover lengths (~a items)~%" (length lengths)))
                (unless (null? records)
                  (printf "*** leftover records (1) (~a items)~%" (length records)))
                (unless (null? devices)
                  (printf "*** leftover devices (~a items)~%" (length devices)))
                (unless (null? weather)
                  (printf "*** leftover weather conditions (~a items)~%" (length weather)))
                (unless (null? gear-change)
                  (printf "*** leftover gear change events (~a items)~%" (length gear-change)))
                rsessions)
              (let* ([s (car sessions)]
                     [session-stc (make-start-time-checker (get-start-time s))])

                (define-values (this-session-laps remaining-laps)
                  (splitf-at laps session-stc))
                (define-values (this-session-devices remaining-devices)
                  (splitf-at devices session-stc))
                (define-values (this-session-weather remaining-weather)
                  (splitf-at weather session-stc))
                (define-values (this-session-gear-changes remaining-gear-changes)
                  (splitf-at gear-change session-stc))

                ;; We maybe create a lap for the entire session -- at least
                ;; GoldenCheetah exports FIT files with no lap records...
                (define-values (rlaps remaining-lengths remaining-records)
                  (let laps-loop ([laps (if (null? this-session-laps)
                                            `(((timestamp . ,(dict-ref s 'timestamp #f))
                                               (start-time . ,(dict-ref s 'start-time #f))))
                                            this-session-laps)]
                                  [lengths lengths]
                                  [records records]
                                  [rlaps '()])
                    (if (null? laps)
                        (values
                         ;; Hack: create summary values for the lap we just created
                         (if (and (= 1 (length rlaps)) (null? this-session-laps))
                             (let ([summary (compute-summary-data '() '() rlaps '())])
                               (list (append summary (car rlaps))))
                             rlaps)
                         lengths
                         records)
                        (let* ([l (car laps)]
                               [lap-stc (make-start-time-checker (get-start-time l))])
                          (define-values (this-lap-lengths remaining-lengths)
                            (splitf-at lengths lap-stc))
                          (define-values (this-lap-records remaining-records)
                            (splitf-at records lap-stc))

                          (define rlengths
                            ;; We maybe create a length for the entire lap.
                            ;; Length records are not present, except in lap
                            ;; swim activities, but we always require them, so
                            ;; we first create them if they are not present.
                            (let lengths-loop ([lengths (if (null? this-lap-lengths)
                                                            `(((timestamp . ,(dict-ref l 'timestamp #f))
                                                              (start-time . ,(dict-ref l 'start-time #f))
                                                              (total-timer-time . ,(dict-ref l 'total-timer-time #f))
                                                              (total-elapsed-time . ,(dict-ref l 'total-elapsed-time #f))))
                                                            this-lap-lengths)]
                                               [records this-lap-records]
                                               [rlengths '()])
                              (if (null? lengths)
                                  (begin
                                    (unless (null? records)
                                      (printf "*** leftover records (~a items)~%" (length records)))
                                    rlengths)
                                  (let* ([g (car lengths)]
                                         [length-stc (make-start-time-checker (get-start-time g))])
                                    (define-values (this-length-records remaining-records)
                                      (splitf-at records length-stc))
                                    ;; (printf "~%trackpoints: ~a" (length this-length-records))
                                    (lengths-loop (cdr lengths)
                                                  remaining-records
                                                  (cons `((track ,@(reverse this-length-records)) ,@g) rlengths))))))

                          ;;(printf "~%lengths: ~a" (length rlengths))
                          (laps-loop (cdr laps)
                                     remaining-lengths
                                     remaining-records
                                     (cons `((lengths ,@rlengths) ,@l) rlaps))))))

                (session-loop (cdr sessions)
                              remaining-laps
                              remaining-lengths
                              remaining-records
                              remaining-devices
                              remaining-weather
                              remaining-gear-changes
                              (cons `((laps ,@rlaps)
                                      (devices ,@(reverse this-session-devices))
                                      (weather-conditions ,@(reverse this-session-weather))
                                      (gear-changes ,@(reverse (map convert-gear-change-event this-session-gear-changes)))
                                      ,@s)
                                    rsessions))))))
      
      (list
       (cons 'start-time (or activity-timestamp (get-start-timestamp)))
       (cons 'guid activity-guid)
       (cons 'developer-data-ids developer-data-ids)
       (cons 'field-descriptions field-descriptions)
       (cons 'training-file training-file)
       (cons 'weather-reports weather-reports) ; all of them
       (cons 'sessions rsessions)))

    ))

(define (read-activity-from-file file)
  ;; Convenience function to read an activity from a file.
  (let ((stream (make-fit-data-stream file))
        (builder (new activity-builder%)))
    (read-fit-records stream builder)
    (send builder collect-activity)))

;; This is a database mapping a manufacturer/product ids to product names.  It
;; is kept outside of the code for easier updates.
(define-runtime-path fit-product-defs-file "../../sql/fit-product-defs.json")
(define the-fit-product-defs #f)
(define (fit-product-defs)
  (unless the-fit-product-defs
    (set! the-fit-product-defs
          (call-with-input-file fit-product-defs-file
            (lambda (in) (read-json in #:null #f)))))
  the-fit-product-defs)

;; Return the basic device name corresponding to an ANTDEV device symbol
(define (ant-device-name antdev)
  (cond ((eq? antdev 'stride-speed-distance) "Footpod")
        ((eq? antdev 'bike-speed-cadence) "Bike Speed-Cadence Sensor")
        ((eq? antdev 'bike-cadence) "Bike Cadence Sensor")
        ((eq? antdev 'bike-speed) "Bike Speed Sensor")
        ((eq? antdev 'heart-rate) "Heart Rate Monitor")
        ((eq? antdev 'bike-power) "Power Meter")
        (#t (~a antdev))))

;; Construct a device name from MANUFACTURER/PRODUCT ids and ANTDEV device
;; symbol.  The rules are:
;;
;; * If the MANUFACTURER/PRODUCT pair is found in the `fit-product-defs-file`,
;;   its name is returned
;;
;; * If only the MANUFACTURER id is found or PRODUCT is #f, the manufacturer
;;   name is combined with the ant dev name (producing for example, Wahoo
;;   Heart Rate Monitor)
;;
;; * A string with the manufacturer + product IDs is returned as a last
;;   resort.
;;
(define (fit-device-name manufacturer product antdev)
  (define m (for/first ([m (in-list (fit-product-defs))]
                        #:when (equal? manufacturer (hash-ref m 'manufacturer_id #f)))
              m))
  (cond ((not m)
         (format "~a/~a" manufacturer product))
        ((not product)
         (define mname (hash-ref m 'name (lambda () (~a manufacturer))))
         (if antdev
             (format "~a ~a" mname (ant-device-name antdev))
             mname))
        (#t
         (let ((p (for/first ([p (in-list (hash-ref m 'products))]
                              #:when
                              (let ((id (hash-ref p 'product_id '())))
                                (if (list? id) (member product id) (equal? product id))))
                    p)))
           (define (default-name)
             (let ((mname (hash-ref m 'name (lambda () (~a manufacturer)))))
                 (if antdev
                     (format "~a ~a (~a)" mname (ant-device-name antdev) product)
                     (format "~a (~a)" mname product))))
           (if p
               (hash-ref p 'name default-name)
               (default-name))))))

(define (fit-get-device-name device-info)
  ;; Return a convenient device name from a DEVICE-INFO record.  This function
  ;; is somewhat simplistic and will need to be made more generic, w.r.t
  ;; mapping manufacturer, product to actual product names.
  (let ((manufacturer (dict-ref device-info 'manufacturer #f))
        (product (dict-ref device-info 'product #f))
        (antdev (or (dict-ref device-info 'ant-device-type #f)
                    (dict-ref device-info 'antplus-device-type #f)))
        (source-type (dict-ref device-info 'source-type #f)))
    (cond
      (manufacturer (fit-device-name manufacturer product antdev))
      (antdev (ant-device-name antdev))
      (source-type (format "~a device" source-type))
      (else "unknown device"))))


;;..................................................... fit file writing ....

(define fit-output-stream%
  ;; Helper class to write FIT files.  This is low level stuff, you need to
  ;; handle putting message defintions and messages yourself.  Most likely you
  ;; want to start up with fit-output-file% instead of this class.
  (class object% (init) (super-new)

    (struct mdef (global-id local-id size fields))

    (define header-length 14)
    (define protocol-version 16)
    (define profile-version 1322)

    (define buffer (make-bytes header-length))
    (define mark header-length)

    (define big-endian? #t)

    (define message-definitions (make-hash))

    (define (ensure-space-for n)
      ;; Make sure our buffer has space for at least n bytes
      (let ((available (- (bytes-length buffer) mark)))
        (when (< available n)
          (let ((needed (- n available)))
            (let ((nbuf (make-bytes (+ (bytes-length buffer) needed  0))))
              (bytes-copy! nbuf 0 buffer 0 (bytes-length buffer))
              (set! buffer nbuf))))))

    (define/public (put-message-definition global-id local-id definition)
      ;; Register a new message with GLOBAL-ID that will use LOCAL-ID in the
      ;; FIT file.  DEFINITION contains the list of fields to encode in the
      ;; message.  The message definition is also written to the internal
      ;; buffer.

      ;; Any previous message with the same LOCAL-ID will be overwriten (this
      ;; feature is used frequently).
      (ensure-space-for
       (+ 1 1 1 2 1 (* 3 (length definition))))
      (let ((header (bitwise-ior local-id #x40)))
        (bytes-set! buffer mark header)
        (set! mark (+ 1 mark)))

      (bytes-set! buffer mark 0)        ; reserved byte
      (set! mark (+ 1 mark))

      (bytes-set! buffer mark (if big-endian? 1 0)) ; arhitecture
      (set! mark (+ 1 mark))

      (integer->integer-bytes global-id 2 #f big-endian? buffer mark)
      (set! mark (+ 2 mark))

      (bytes-set! buffer mark (length definition))
      (set! mark (+ 1 mark))

      (let ((message-size 0))

        (for ([def (in-list definition)])
          (let ((type (get-fit-type (list-ref def 3))))
            (unless type
              (raise-error (format "bad type: ~a" (list-ref def 3))))
            (let ((fnum (list-ref def 1))
                  (size (* (list-ref def 2) (fit-type-size type)))
                  (typeid (fit-type-id type)))

              (set! message-size (+ message-size size))

              (bytes-set! buffer mark fnum)
              (bytes-set! buffer (+ mark 1) size)
              (bytes-set! buffer (+ mark 2) typeid)
              (set! mark (+ 3 mark)))))

        (hash-set! message-definitions
                   global-id
                   (mdef global-id local-id message-size definition)))
      #t)

    (define/public (put-message global-id message-data)
      ;; Write a new message with GLOBAL-ID MESSAGE-DATA contains the data for
      ;; the message.  A previous message defintion should have been written
      ;; for this GLOBAL-ID
      (let ((definition (hash-ref message-definitions global-id #f)))
        (unless definition
          (raise-error (format "undefined global message ~a" global-id)))
        (ensure-space-for (+ 1 (mdef-size definition)))
        (bytes-set! buffer mark (mdef-local-id definition))
        (set! mark (+ 1 mark))
        (for ([field (in-list (mdef-fields definition))])
          (let ((value (dict-ref message-data (list-ref field 0) #f))
                (type (get-fit-type (list-ref field 3))))
            (set! mark (write-fit-value buffer mark type value big-endian?)))))
      #t)

    (define/public (get-fit-data)
      ;; Fill in the FIT header, compute the CRC and return the FIT data.
      (bytes-set! buffer 0 header-length)
      (bytes-set! buffer 1 protocol-version)
      (integer->integer-bytes profile-version 2 #f #f buffer 2)
      (let ((data-length (- mark header-length)))
        (integer->integer-bytes data-length 4 #f #f buffer 4))
      (bytes-copy! buffer 8 (string->bytes/utf-8 ".FIT"))
      (integer->integer-bytes
       (compute-crc buffer 0 12)
       2 #f #f buffer 12)
      ;; Put the CRC at the end
      (let ((crc (compute-crc buffer 0 mark)))
        (ensure-space-for 2)
        (integer->integer-bytes crc 2 #f #f buffer mark))
      (subbytes buffer 0 (+ 2 mark)))

    ))



;;..................................................... fit-output-file% ....

(define fit-output-file%
  ;; Create a FIT output stream with the file-id and file-creator messages
  ;; already filled in.  These two messages need to be present in all fit
  ;; files anyway.  Sensible defaults are provided for all fields, but can be
  ;; overriden.  The only parameter that needs a valid value is the FILE-TYPE.

  (class fit-output-stream% (init)
    (init-field
     [file-type #f]
     [manufacturer 1]                   ; Garmin
     [product 65534]                    ; Connect
     [serial-number 1]
     [time-created #f]                  ; if #f, will be updated to current time
     [number #f]
     [hardware-version #f]
     [software-version 1])
    (super-new)
    (inherit put-message-definition put-message)

    (define file-id-message 0)          ; global meessage number
    (define file-id-definition
      '((type 0 1 enum)
        (manufacturer 1 1 uint16)
        (product 2 1 uint16)
        (serial-number 3 1 uint32z)
        (time-created 4 1 uint32)       ; FIT epoch
        (number 5 1 uint16)))

    (define file-creator-message 49)    ; global message number
    (define file-creator-definition
      '((software-version 0 1 uint16)
        (hardware-version 1 1 uint8)))

    ;; Write the file-id and file-creator messages immediately.  Both messages
    ;; use local-id 0.  Local-id 0 should also be available immediately, as
    ;; these messages will not be wrtten again.

    (put-message-definition file-id-message 0 file-id-definition)
    (put-message
     file-id-message
     `((type . ,file-type)
       (manufacturer . ,manufacturer)
       (product . ,product)
       (serial-number . ,serial-number)
       (time-created . ,(unix-time->fit-time (or time-created (current-seconds))))
       (number . ,number)))

    (put-message-definition file-creator-message 0 file-creator-definition)
    (put-message
     file-creator-message
     `((software-version . ,software-version)
       (hardware-version . ,hardware-version)))

    ))



;;.................................................... fit-workout-file% ....

(define (mk-fit-string str size)
  ;; Convert STR, a string into a vector of SIZE bytes, ready to be written
  ;; into a FIT file.  Strings in FIT files have fixed length and are 0
  ;; padded.
  (let ((result (make-vector size 0)))
    (let loop ((idx 0))
      ;; NOTE: we make the string 0 terminated always even though the fit
      ;; standard does not require us
      (when (< idx (min (- size 1) (string-length str)))
        (vector-set! result idx (char->integer (string-ref str idx)))
        (loop (+ idx 1))))
    result))

(define fit-workout-file%
  ;; Create a workout fit file. Note that this is still a fairly low level
  ;; interface for creating workouts.
  (class fit-output-file% (init)
    (init-field name sport)
    (super-new [file-type 5])

    (inherit put-message-definition put-message)

    ;; The index of each workout step.  This is automatically managed by the
    ;; class, but reading it is usefull when adding repeat steps, as they need
    ;; the message index to jump to.
    (define message-index 0)
    (define/public (get-next-message-index) message-index)

    (define workout-message 26)
    (define workout-definition          ; global message 26
      '((name 8 15 string)
        (sport 4 1 enum)
        (capabilities 5 1 uint32z)
        (num-steps 6 1 uint16)))

    (define workout-step-message 27)
    (define workout-step-definition     ; global message 27
      '((message-index 254 1 uint16)
        ;;(name 0 10 string)
        (duration-type 1 1 enum)
        (duration-value 2 1 uint32)
        (target-type 3 1 enum)
        (target-value 4 1 uint32)
        (custom-target-value-low 5 1 uint32)
        (custom-target-value-high 6 1 uint32)
        (intensity 7 1 enum)))

    ;; List of steps in the workout, as added by `add-step'.  Note that they
    ;; are stored in reverse order.
    (define workout-steps '())

    (define (write-workout)
      ;; Write the entire workout to the FIT file.
      (put-message-definition workout-message 0 workout-definition)
      (put-message
       workout-message
       `((sport . ,sport)
         (capabilities . 32)
         (num-steps . ,(length workout-steps))
         (name . ,(mk-fit-string name 15))))
      (put-message-definition workout-step-message 0 workout-step-definition)
      (for ([step (in-list (reverse workout-steps))])
        (put-message workout-step-message step)))

    (define/public (add-step step)
      ;; Add a workout step.  This is an A-LIST corresponding to
      ;; `workout-step-definition' with the step data.
      (set! workout-steps
            (cons (cons (cons 'message-index message-index) step) workout-steps))
      (set! message-index (+ 1 message-index)))

    (define/override (get-fit-data)
      ;; Get the FIT data corresponding to the workout.  This also writes the
      ;; workout to the file.
      (write-workout)
      (super get-fit-data))

    ))


;;...................................................... fit-sport-file% ....

(define fit-sport-file%
  ;; Create a fit sport file (this contains zone definitions for various
  ;; sports).
  (class fit-output-file%
    (init)
    (init-field [sport 0] [sub-sport 0]
                [max-hr #f] [ftp #f]
                [hr-zones #f]
                [speed-zones #f]
                [power-zones #f])
    (super-new [file-type 3])           ; sport file
    (inherit put-message-definition put-message)

    (define zones-target-message 7)
    (define zones-target-definition
      '((max-heart-rate 1 1 uint8)
        (functional-threshold-power 3 1 uint16)
        (hr-calculation-type 5 1 enum)
        (power-calculation-type 7 1 enum)))

    (define sport-message 12)
    (define sport-message-definition
      '((sport 0 1 enum)
        (sub-sport 1 1 enum)))

    (define hr-zone-message 8)
    (define hr-zone-definition
      '((message-index 254 1 uint16)
        (high-bpm 1 1 uint8)
        ;;(name 2 10 string)
        ))

    (define speed-zone-message 53)
    (define speed-zone-definition
      '((message-index 254 1 uint16)
        (high-value 1 1 uint16)
        ;;(name 1 10 string)
        ))

    (define power-zone-message 9)
    (define power-zone-definition
      '((message-index 254 1 uint16)
        (high-value 1 1 uint16)
        ;;(name 2 10 string)
        ))

    (define (write-settings)
      (put-message-definition zones-target-message 0 zones-target-definition)
      (put-message
       zones-target-message
       `((max-heart-rate . ,(if max-hr (exact-round max-hr) #f))
         (functional-threshold-power . ,(if ftp (exact-round ftp) #f))
         (hr-calculation-type . ,(if hr-zones 0 #f))
         (power-calculation-type . ,(if power-zones 0 #f))))
      (put-message-definition sport-message 0 sport-message-definition)
      (put-message
       sport-message
       `((sport . ,sport)
         (sub-sport . ,sub-sport)))
      (when hr-zones
        (put-message-definition hr-zone-message 0 hr-zone-definition)
        (for ([message-index (in-range (length hr-zones))]
              [val (in-list hr-zones)])
          (put-message
           hr-zone-message
           `((message-index . ,message-index)
             (high-bpm . ,(exact-round val))
             (name . ,(mk-fit-string "" 10))))))
      (when power-zones
        (put-message-definition power-zone-message 0 power-zone-definition)
        (for ([message-index (in-range (length power-zones))]
              [val (in-list power-zones)])
          (put-message
           power-zone-message
           `((message-index . ,message-index)
             (high-value . ,(exact-round val))
             (name . ,(mk-fit-string "" 10))))))
      (when speed-zones
        (put-message-definition speed-zone-message 0 speed-zone-definition)
        (for ([message-index (in-range (length speed-zones))]
              [val (in-list speed-zones)])
          (put-message
           speed-zone-message
           `((message-index . ,message-index)
             (high-value . ,(exact-round (* 1000.0 val)))
             (name . ,(mk-fit-string "" 10)))))))

    (define/override (get-fit-data)
      (write-settings)
      (super get-fit-data))

    ))


;;................................................... fit-settings-file% ....

(define fit-settings-file%
  ;; Create a fit settings file (this atthlete info such as heiht, body
  ;; weight, etc)
  (class fit-output-file%
    (init)
    (init-field [date-of-birth #f]
                [gender #f]
                [weight #f]
                [height #f]
                [activity-class #f]
                [collect-hrv-data? #f])
    (super-new [file-type 2])           ; settings
    (inherit put-message-definition put-message)

    (define user-profile-message 3)
    (define user-profile-message-definition
      '((gender 1 1 enum)
        (age 2 1 uint8)
        (height 3 1 uint8)
        (weight 4 1 uint16)
        (activity-class 17 1 enum)
        (birth-year 24 1 uint8)))

    (define hrm-profile-message 4)
    (define hrm-profile-message-definition
      '((log-hrv 2 1 enum)))

    (define (write-settings)

      (define age
        (and date-of-birth
             (exact-round (/ (- (current-seconds) date-of-birth) (* 3600 24 365)))))

      (define birth-year
        (and date-of-birth
             (let ((date (seconds->date date-of-birth)))
               (- (date-year date) 1900))))

      (put-message-definition user-profile-message 0 user-profile-message-definition)
      (put-message
       user-profile-message
       `((gender . ,gender)
         (age . ,age)
         (height . ,(if height (exact-round (* height 100)) #f))
         (weight . ,(if weight (exact-round (* weight 10)) #f))
         (activity-class . ,(if activity-class (exact-round (* activity-class 10)) #f))
         (birth-year . ,birth-year)))

      (put-message-definition hrm-profile-message 0 hrm-profile-message-definition)
      (put-message
       hrm-profile-message
       `((log-hrv . ,(if collect-hrv-data? 1 0)))))

    (define/override (get-fit-data)
      (write-settings)
      (super get-fit-data))

    ))
