#lang racket/base
;; al2-profiler.rkt -- profiling and tracing capabilities
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2016, 2020 Alex Harsányi <AlexHarsanyi@gmail.com>
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

;; This is a helped module used to debug this application.  It contains
;; facilities for profiling and tracing functions.
;;
;; For profiling functions, replace `define` with `define/profile` and timing
;; data will be collected for the function.  This also works for class method
;; names, use 'define/public/profile' or 'define/augment/profile' instead.
;; Timing data for all functions can be displayed with `profile-display`.
;; There are also functions for individually enabling/disabling profiling and
;; resetting the counters.
;;
;; NOTE: in a GUI application `profile-display` should be called on the
;; `on-toplevel-close` method, otherwise it will be called too early.
;; Alternatively, it could be added as a plumber flush function:
;;
;; (plumber-add-flush! (current-plumber) (lambda (h) (profile-display)))
;;
;; For tracing functions, replace `define` with `define/trace` and the
;; function calls with their arguments will be printed out.

(require (for-syntax racket/base
                     racket/syntax)
         math/statistics
         racket/class
         racket/format
         racket/math
         racket/match)

(provide define/profile
         define/private/profile
         define/public/profile
         define/augment/profile

         profile-enable
         profile-enable-all
         profile-reset-all
         profile-display)


;; Profile event information is collected in buffers which contain entries
;; next to each other.  The entries are PD-STRIDE elements apart and each of
;; the constants below define the offset from the start of the record for that
;; entry
(define pd-name 0)                 ; name of the function generating the event
(define pd-event 1)                ; profile event (currently 'start-event or 'end-event)
(define pd-rtime 2)                ; real time at the time of the event
(define pd-ptime 3)                ; process time at the time of the event
(define pd-gctime 4)               ; gc time at the time of the event
(define pd-memory 5)               ; total memory allocated at the time of the event
(define pd-stride 6)

;; Number of entries in a single buffer -- the actual size of the buffer is
;; BUFFER-SLOTS * PD-STRIDE
(define buffer-slots 100000)

;; Current buffer where recording is done
(define current-buffer (make-vector (* buffer-slots pd-stride)))
;; Position to where current buffer is filled (next entry will be recorded at
;; this position
(define current-index 0)
;; List of previous buffers -- once current buffer is full, it will be added
;; to PREVIOUS-BUFFERS and a new one allocated.
(define previous-buffers '())

;; Total memory, real time and process time used by the profiler -- these are
;; used to try to avoid assigning to function calls the time taken by the
;; profiler itself (it is not perfect).
(define profiler-memory-use 0)
(define profiler-real-time-use 0)
(define profiler-process-time-use 0)

;; Record a profile event -- ID is the function being profiled, EVENT is the
;; profile event itself.  A new entry is allocated in the CURRENT-BUFFER
;; recording the times and memory use at the time of the event.
;;
;; NOTE: this is not thread safe.
(define (put-event id event)
  (define rtime (current-inexact-monotonic-milliseconds))
  (define ptime (current-process-milliseconds (current-thread)))
  (define gctime (current-gc-milliseconds))
  (define memory (current-memory-use 'cumulative))
  (when (>= current-index (vector-length current-buffer))
    ;; (printf "new buffer~%")
    (set! previous-buffers (cons current-buffer previous-buffers))
    (set! current-buffer (make-vector (* buffer-slots pd-stride)))
    (set! current-index 0))
  (vector-set! current-buffer (+ current-index pd-name) id)
  (vector-set! current-buffer (+ current-index pd-event) event)
  (vector-set! current-buffer (+ current-index pd-rtime) (- rtime profiler-real-time-use))
  (vector-set! current-buffer (+ current-index pd-ptime) (- ptime profiler-process-time-use))
  (vector-set! current-buffer (+ current-index pd-gctime) gctime)
  (vector-set! current-buffer (+ current-index pd-memory) (- memory profiler-memory-use))
  (set! current-index (+ current-index pd-stride))

  ;; NOTE: we leak some of the profiler time and memory use into the
  ;; application use, but hopefully it is just a small amount.
  (set! profiler-memory-use (+ profiler-memory-use (- (current-memory-use 'cumulative) memory)))
  (set! profiler-real-time-use (+ profiler-real-time-use (- rtime (current-inexact-monotonic-milliseconds))))
  (set! profiler-process-time-use (+ profiler-process-time-use (- ptime (current-process-milliseconds (current-thread))))))

;; Information about a profiled function -- name of the function and whether
;; profiling is enabled for it or not.
(struct profile-data (name (enabled #:mutable)) #:transparent)

;; Hold information about all profiled functions.  Maps a name to a
;; `profile-data` instance.
(define profile-db (make-hash))

;; NOTE: currently, functions with the same name have their data merged into
;; one.  This works OK when we have functions inside a class definition, as
;; they are practically methods, but we also don't make distinctions between a
;; function with the same name defined in separate modules.
;;
;; To help out, we print a warning indicating that profile data will be
;; merged.
(define (make-profile-data name)
  (let ([data (hash-ref profile-db name #f)])
    (if data
        (begin0 data
          (eprintf "*** warning: redefining ~a, will merge profile data~%" name))
        (let ([ndata (profile-data name #t)])
          (hash-set! profile-db name ndata)
          ndata))))

;; Enable/Disable data collection for function NAME.  This must be a function
;; that has been defined with `define/profile'
(define (profile-enable name flag)
  (let ((data (hash-ref profile-db name #f)))
    (when data
      (set-profile-data-enabled! data flag))))

;; Enable/Disable data collection for all instrumented functions.
(define (profile-enable-all flag)
  (for ([item (in-hash-values profile-db)])
    (set-profile-data-enabled! item flag)))

;; Reset collected data for all instrumented functions.
(define (profile-reset-all)
  ;; Don't delete the current buffer if present, setting the index to 0 has
  ;; the same effect, and avoids extra memory allocations
  (set! previous-buffers '())
  (set! profiler-memory-use 0)
  (set! profiler-real-time-use 0)
  (set! profiler-process-time-use 0)
  (set! current-index 0))


;;...................................................... collecting data ....

;; A profile event, as a struct -- while collecting data in buffers is faster,
;; it is more convenient to work with structures when we analyze the data.
(struct pevent (id event rtime ptime gctime memory) #:transparent)

;; Create a PEVENT instance from the profile data in BUFFER at INDEX.
(define (extract-pevent buffer index)
  (pevent
   (vector-ref buffer (+ index pd-name))
   (vector-ref buffer (+ index pd-event))
   (vector-ref buffer (+ index pd-rtime))
   (vector-ref buffer (+ index pd-ptime))
   (vector-ref buffer (+ index pd-gctime))
   (vector-ref buffer (+ index pd-memory))))

;; Calculate the difference in times and memory use between two profile events.
(define (pevent-deltas pevent1 pevent2)
  (match-define (pevent id1 event1 rtime1 ptime1 gctime1 memory1) pevent1)
  (match-define (pevent id2 event2 rtime2 ptime2 gctime2 memory2) pevent2)
  (values
   (- rtime2 rtime1)
   (- ptime2 ptime1)
   (- gctime2 gctime1)
   (- memory2 memory1)))

;; Hold information about the execution time of a function identified by ID.
;; Note that all the time and memory slots are statistics objects, allowing to
;; calculate means and standard deviations of these values.
(struct presult (id
                 ncalls                 ; number of calls
                 own-real-time
                 cumulative-real-time
                 own-duration
                 cumulative-duration
                 own-gc-time
                 cumulative-gc-time
                 own-memory
                 cumulative-memory) #:transparent)

(define (make-presult id)
  (presult
   id
   0
   empty-statistics
   empty-statistics
   empty-statistics
   empty-statistics
   empty-statistics
   empty-statistics
   empty-statistics
   empty-statistics))


;; Update the profile data for a function call.  START-EVENT and END-EVENT are
;; the start and end events of this function call, while
;; OWN-{RTIME,PTIME,GCTIME,MEM} are the time and memory use of the function
;; itself (and other non-profiled functions) -- these times do not include the
;; execution time of other profiled functions called from this one.
;;
;; RESUTS is a hash map of PEVENT structures and the PRESULT of this function
;; is updated with timings from this call.
;;
(define (update-presult results start-event end-event
                        own-rtime own-ptime own-gctime own-mem)
  (define-values (total-rtime total-ptime total-gctime total-memory)
    (pevent-deltas start-event end-event))
  ;; (printf "*** update-presult~%    start: ~a~%    end: ~a~%" start-event end-event)

  (match-define (presult id ncalls
                         own-real-time cumulative-real-time
                         own-duration cumulative-duration
                         own-gc-time cumulative-gc-time
                         own-memory cumulative-memory)
    (or (hash-ref results (pevent-id start-event) #f)
        (make-presult (pevent-id start-event))))
  (define updated-result
    (presult id
             (add1 ncalls)
             (update-statistics own-real-time own-rtime)
             (update-statistics cumulative-real-time total-rtime)
             (update-statistics own-duration own-ptime)
             (update-statistics cumulative-duration total-ptime)
             (update-statistics own-gc-time own-gctime)
             (update-statistics cumulative-gc-time total-gctime)
             (update-statistics own-memory own-mem)
             (update-statistics cumulative-memory total-memory)))
  (hash-set! results id updated-result))

;; Structure to hold data while parsing the results buffer -- holds the start
;; event of a function call and the own timings and memory uses for the
;; function.
(struct centry (start-event own-rtime own-ptime own-gctime own-memory) #:transparent)

;; Collect the profile results from the all the profile buffers (current and
;; previous).  Returns a hash map of PRESULT structures with timings for each
;; function call.
(define (collect-results)
  (define results (make-hash))
  (define call-stack '())
  (define last-event #f)

  (define (process buffer index)
    (define current (extract-pevent buffer index))
    #;(printf "*** current: ~a~%" current)
    (define-values (drt dpt dgct dm)
      (if last-event
          (pevent-deltas last-event current)
          (values 0 0 0 0)))
    #;(printf "+++ drt ~a, dpt ~a, dgct ~a, dm ~a~%" drt dpt dgct dm)
    (unless (null? call-stack)
      (match-define (centry start-event own-rtime own-ptime own-gctime own-memory)
        (car call-stack))
      (if (and (equal? (pevent-id start-event) (pevent-id current))
               (equal? (pevent-event current) 'end-event))
          (begin
            (update-presult results start-event current
                            (+ own-rtime drt)
                            (+ own-ptime dpt)
                            (+ own-gctime dgct)
                            (+ own-memory dm))
            (set! call-stack (cdr call-stack)))
          (let ([new-top (centry start-event
                                 (+ own-rtime drt)
                                 (+ own-ptime dpt)
                                 (+ own-gctime dgct)
                                 (+ own-memory dm))])
            (set! call-stack (cons new-top (cdr call-stack))))))
    (when (equal? (pevent-event current) 'start-event)
      (set! call-stack (cons (centry current 0 0 0 0) call-stack)))
    (set! last-event current))

  (for ([buffer (reverse previous-buffers)])
    (for ([index (in-range 0 (vector-length buffer) pd-stride)])
      (process buffer index)))
  (for ([index (in-range 0 current-index pd-stride)])
    (process current-buffer index))

  results)

;; Display the current profile information to PORT -- defaults to the current
;; output port, usually the terminal.  The data is intended to be human
;; readable.
(define (profile-display (port (current-output-port)))

  (define names
    (sort
     (for/list ([k (in-hash-keys profile-db)])
       (symbol->string k))
     string<?))

  (define results (collect-results))

  (define (pval val)
    (if (or (string? val) (symbol? val) (nan? val) (infinite? val))
        (~a val)
        (~r val #:precision '(= 2))))

  (define rdata
    (for/list ([name (in-list names)])
      (match-define (presult
                     _id
                     ncalls
                     own-real-time
                     cumulative-real-time
                     own-duration
                     cumulative-duration
                     own-gc-time
                     cumulative-gc-time
                     own-memory
                     cumulative-memory)
        (hash-ref results (string->symbol name) (lambda () (make-presult name))))
      (list
       name
       ncalls

       (* ncalls (statistics-mean own-real-time))
       (statistics-min own-real-time)
       (statistics-max own-real-time)
       (statistics-mean own-real-time)
       (statistics-stddev own-real-time)

       (* ncalls (statistics-mean cumulative-real-time))
       (statistics-min cumulative-real-time)
       (statistics-max cumulative-real-time)
       (statistics-mean cumulative-real-time)
       (statistics-stddev cumulative-real-time)

       (* ncalls (statistics-mean own-duration))
       (statistics-min own-duration)
       (statistics-max own-duration)
       (statistics-mean own-duration)
       (statistics-stddev own-duration)

       (* ncalls (statistics-mean cumulative-duration))
       (statistics-min cumulative-duration)
       (statistics-max cumulative-duration)
       (statistics-mean cumulative-duration)
       (statistics-stddev cumulative-duration)

       (* ncalls (statistics-mean own-gc-time))
       (statistics-min own-gc-time)
       (statistics-max own-gc-time)
       (statistics-mean own-gc-time)
       (statistics-stddev own-gc-time)

       (* ncalls (statistics-mean cumulative-gc-time))
       (statistics-min cumulative-gc-time)
       (statistics-max cumulative-gc-time)
       (statistics-mean cumulative-gc-time)
       (statistics-stddev cumulative-gc-time)

       (* ncalls (statistics-mean own-memory))
       (statistics-min own-memory)
       (statistics-max own-memory)
       (statistics-mean own-memory)
       (statistics-stddev own-memory)

       (* ncalls (statistics-mean cumulative-memory))
       (statistics-min cumulative-memory)
       (statistics-max cumulative-memory)
       (statistics-mean cumulative-memory)
       (statistics-stddev cumulative-memory)

       )))

  (define sdata
    (for/list ([entry (in-list rdata)])
      (map pval entry)))

  (define max-width
    (for/fold ([max-width 0])
              ([entry (in-list sdata)])
      (max max-width
           (for/fold ([max-width 0])
                     ([item (in-list entry)])
             (max max-width (string-length item))))))

  (define (~p v) (~a v #:min-width max-width))

  (for ([entry (in-list sdata)])
    (match-define
      (list name ncalls
            total-own-real-time min-own-real-time max-own-real-time avg-own-real-time stdev-own-real-time
            total-cumulative-real-time min-cumulative-real-time max-cumulative-real-time avg-cumulative-real-time stdev-cumulative-real-time
            total-own-duration min-own-duration max-own-duration avg-own-duration stdev-own-duration
            total-cumulative-duration min-cumulative-duration max-cumulative-duration avg-cumulative-duration stdev-cumulative-duration
            total-own-gc-time min-own-gc-time max-own-gc-time avg-own-gc-time stdev-own-gc-time
            total-cumulative-gc-time min-cumulative-gc-time max-cumulative-gc-time avg-cumulative-gc-time stdev-cumulative-gc-time
            total-own-memory min-own-memory max-own-memory avg-own-memory stdev-own-memory
            total-cumulative-memory min-cumulative-memory max-cumulative-memory avg-cumulative-memory stdev-cumulative-memory)
      entry)
    (fprintf port "*** Function ~a: ~a calls~%" name ncalls)
    (fprintf port "                      ~a ~a ~a ~a ~a~%" (~p "Total") (~p "Min") (~p "Max") (~p "Avg") (~p "Stdev"))
    (fprintf port "Own Real Time:        ~a ~a ~a ~a ~a~%"
             (~p total-own-real-time)
             (~p min-own-real-time)
             (~p max-own-real-time)
             (~p avg-own-real-time)
             (~p stdev-own-real-time))
    (fprintf port "Cumulative Real Time: ~a ~a ~a ~a ~a~%"
             (~p total-cumulative-real-time)
             (~p min-cumulative-real-time)
             (~p max-cumulative-real-time)
             (~p avg-cumulative-real-time)
             (~p stdev-cumulative-real-time))
    (fprintf port "Own Duration:         ~a ~a ~a ~a ~a~%"
             (~p total-own-duration)
             (~p min-own-duration)
             (~p max-own-duration)
             (~p avg-own-duration)
             (~p stdev-own-duration))
    (fprintf port "Cumulative Duration:  ~a ~a ~a ~a ~a~%"
             (~p total-cumulative-duration)
             (~p min-cumulative-duration)
             (~p max-cumulative-duration)
             (~p avg-cumulative-duration)
             (~p stdev-cumulative-duration))
    (fprintf port "Own GC Time:          ~a ~a ~a ~a ~a~%"
             (~p total-own-gc-time)
             (~p min-own-gc-time)
             (~p max-own-gc-time)
             (~p avg-own-gc-time)
             (~p stdev-own-gc-time))
    (fprintf port "Cumulative GC Time:   ~a ~a ~a ~a ~a~%"
             (~p total-cumulative-gc-time)
             (~p min-cumulative-gc-time)
             (~p max-cumulative-gc-time)
             (~p avg-cumulative-gc-time)
             (~p stdev-cumulative-gc-time))
    (fprintf port "Own Memory:           ~a ~a ~a ~a ~a~%"
             (~p total-own-memory)
             (~p min-own-memory)
             (~p max-own-memory)
             (~p avg-own-memory)
             (~p stdev-own-memory))
    (fprintf port "Cumulative Memory:    ~a ~a ~a ~a ~a~%"
             (~p total-cumulative-memory)
             (~p min-cumulative-memory)
             (~p max-cumulative-memory)
             (~p avg-cumulative-memory)
             (~p stdev-cumulative-memory))))

;; Define an instrumented function.  The body of the function will be wrapped
;; around code that collects timing information about the function: when the
;; function is called, data about the duration of the body will be collected.
;; Profiling data for all functions can be displayed using `profile-display'
(define-syntax define/profile
  (syntax-rules ()
    [(_ (name . args) body ...)
     (define name
       (let* ([pdata (make-profile-data (quote name))])
         (lambda args
           (define enabled? (profile-data-enabled pdata))
           (dynamic-wind
             (lambda ()
               (when enabled?
                 (put-event (quote name) 'start-event)))
             (lambda () body ...)
             (lambda ()
               (when enabled?
                 (put-event (quote name) 'end-event)))))))]))

;; Same as define/profile, but for public methods of a class.
;;
;; LIMITATION: the class name is not recorded and the function shows up in the
;; profile output with the "-profiled" string appended.
(define-syntax (define/public/profile stx)
  (syntax-case stx ()
    [(_ (name . args) body ...)
     (with-syntax*
         ([fname (format-id stx "~a-profiled" (syntax->datum #'name))]
          [fname+args (datum->syntax stx (list* #'fname #'args))]
          [name+args (datum->syntax stx (list* #'name #'args))])
       #'(begin
           (define/profile fname+args body ...)
           (public name)
           (define name+args fname+args)))]))

;; Same as define/public/profile but for private methods.  Same limitations too
(define-syntax (define/private/profile stx)
  (syntax-case stx ()
    [(_ (name . args) body ...)
     (with-syntax*
         ([fname (format-id stx "~a-profiled" (syntax->datum #'name))]
          [fname+args (datum->syntax stx (list* #'fname #'args))]
          [name+args (datum->syntax stx (list* #'name #'args))])
       #'(begin
           (define/profile fname+args body ...)
           (private name)
           (define name+args fname+args)))]))

;; Same as define/public/profile but for augmented methods of a class.  Same
;; limitations too.
(define-syntax (define/augment/profile stx)
  (syntax-case stx ()
    [(_ (name . args) body ...)
     (with-syntax*
         ([fname (format-id stx "~a-profiled" (syntax->datum #'name))]
          [fname+args (datum->syntax stx (list* #'fname #'args))]
          [name+args (datum->syntax stx (list* #'name #'args))])
       #'(begin
           (define/profile fname+args body ...)
           (augment name)
           (define name+args fname+args)))]))

