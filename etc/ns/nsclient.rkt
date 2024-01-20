#lang racket
(require "../al-interactive.rkt"
         "../../rkt/sport-charms.rkt"
         net/http-easy)

(define two-ppl-bay 3582)
(define cl60 3588)
(define millford 3444)

(define (make-track df)
  (for/list ([(lat lon) (in-data-frame df "lat" "lon")]
             #:when (and (rational? lat) (rational? lon)))
    (list lat lon)))

(define app-key "abc")
(define route-name-endpoint (format "http://localhost:8080/app/~a/route-name" app-key))

(define (sport-name df)
  (define sport (df-get-property df 'sport))
  (and sport (get-sport-name (vector-ref sport 0) (vector-ref sport 1))))

(define (naming-service-opinion df)
  (and (df-contains? df "lat" "lon")
       (with-handlers ((exn? (lambda () #f)))
         (define r (post route-name-endpoint #:json (make-track df)))
         (and (equal? (response-status-code r) 200) (response-json r)))))

;; (time (response-json (post "http://localhost:8080/app/abc/route-name" #:json (make-track (sid->df 3604)))))

