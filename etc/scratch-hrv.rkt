#lang racket

(require plot)
(require db)
(require math/statistics)
(require "../rkt/al-prefs.rkt")
(require "../rkt/database.rkt")
(require "../rkt/dbapp.rkt")
(require "../rkt/data-frame.rkt")
(require "../rkt/hrv.rkt")


;;.............................................................. prelude ....

;; Open the default database
(define *db*
  (let ((db-file (al-get-pref 'activity-log:database-file (lambda () #f))))
    (unless db-file (error "No default database"))
    (open-activity-log db-file)))
(when *db* (current-database *db*))



;;.......................................... update-hrv-for-all-sessions ....

(define (update-hrv-for-all-sessions db)
  (define sids (query-list db "select id from A_SESSION"))
  (for ([sid sids])
    (with-handlers
      (((lambda (e) #t)
        (lambda (e) (printf "sid: ~a: ~a~%" sid e) (raise e))))
      (define hrv (make-hrv-data-frame/db db sid))
      (when hrv
        (define metrics (compute-hrv-metrics hrv))
        (when metrics
          (put-hrv-metrics metrics sid db))))))


;;................................................................. rest ....

(define (df-density-renderer df series)
  (match-define (list q01 q25 q50 q75 q99) (df-quantile df series 0 0.25 0.5 0.75 0.99))
  (define stats (df-statistics df series))
  (list
   (density (send df select series) #:x-min q01 #:x-max q99 #:width 2 #:color "red")
   (tick-grid)
   (vrule q25 #:label "25%" #:color "blue" #:style 'long-dash)
   (vrule q50 #:label "50%" #:color "green" #:style 'long-dash)
   (vrule (statistics-mean stats) #:label "mean" #:color "black" #:style 'long-dash)
   (vrule q75 #:label "75%" #:color "blue" #:style 'long-dash)))

(define session-id 1717)                ; skiing
;;(define session-id 1709)                ; running

