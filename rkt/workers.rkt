#lang racket/base
;; workers.rkt -- worker threads

;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2016 Alex Harsanyi (AlexHarsanyi@gmail.com)
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
 racket/async-channel
 racket/contract
 "dbglog.rkt")

(provide/contract
 [queue-task (-> string? procedure? any/c)])

;; Requests to the worker threads are sent on this channel
(define *request-channel* (make-async-channel #f))

;; This is a task sent to a worker thread.  It has a name so any unhandled
;; exceptions can be reported.
(struct task (name thunk))

;; Queue a task to one of the worker threads. NOTE: since there are multiple
;; threads, tasks might not be executed in the same order they are queued.
(define (queue-task name thunk)
  (async-channel-put *request-channel* (task name thunk)))

;; Create a worker thread to execute tasks.  Uncaught exceptions from the
;; tasks are logged.
(define (make-worker-thread id request-channel)
  (thread/dbglog
   #:name (format "worker-~a" id) #:log-start #t #:log-finish #t
   (lambda ()
     (let loop ((task (async-channel-get request-channel)))
       (when (task? task)               ; anything else stops the thread
         (with-handlers
           (((lambda (e) #t)
             (lambda (e) (dbglog-exception (task-name task) e))))
           ((task-thunk task)))
         (loop (async-channel-get request-channel)))))))

;; Our worker threads.  NOTE: since racket threads don't really execute in
;; parallel, it is not worth starting too many of them.  Theis main purpose is
;; to free up the GUI thread from long operations.
(define workers
  (for/list ([id (in-range 3)])
    (make-worker-thread id *request-channel*)))
