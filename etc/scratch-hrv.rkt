#lang racket

(require "../rkt/al-prefs.rkt")
(require "../rkt/database.rkt")
(require "../rkt/dbapp.rkt")
(require "../rkt/fit-file.rkt")

(define session-id 1621)

;; Open the default database
(define *db*
  (let ((db-file (al-get-pref 'activity-log:database-file (lambda () #f))))
    (unless db-file
      (error "No default database"))
    (open-activity-log db-file)))
(when *db* (current-database *db*))

(define extract-hrv% 
  (class fit-event-dispatcher% (init) (super-new)
    
    (define/override (on-file-id file-id) 
      (printf "file-id: ~a~%" file-id))
    
    (define/override (on-file-creator creator) 
      (printf "file-creator: ~a~%" creator))
    
    (define/override (on-hrv data)
      (printf "hrv data: ~a~%" data))
      
    ))

(define (hrv-data/1 file)
  (let ((stream (make-fit-data-stream file))
        (consumer (new extract-hrv%)))
    (read-fit-records stream consumer)
    'done))

(define (hrv-data guid db)
  (let* ([aid (db-get-activity-id guid db)]
         [data (db-extract-activity-raw-data aid db)]
         [stream (make-fit-data-stream data)]
         [consumer (new extract-hrv%)])
    (read-fit-records stream consumer)
    'done))


