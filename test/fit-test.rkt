#lang racket/base

;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2018 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require racket/format
         rackunit
         "test-util.rkt"
         "../rkt/data-frame/df.rkt"
         "../rkt/session-df.rkt"
         "../rkt/series-meta.rkt"
         "../rkt/weather.rkt")

(set-allow-weather-download #f)        ; don't download weather for unit tests

(define (do-basic-checks file series-count row-count
                         #:extra-df-checks (extra-df-checks #f))
  (when (file-exists? file)
    (define start (current-milliseconds))
    (printf "File ~a, ~a data-points ..." file row-count)(flush-output)
    (with-database
      (lambda (db)
        (db-import-activity-from-file/check
         file db
         #:expected-row-count row-count
         #:expected-series-count series-count
         #:extra-df-checks extra-df-checks)))
    (define elapsed (/ (- (current-milliseconds) start) 1000.0))
    (printf " done in ~a seconds ~%" (~r elapsed #:precision 2))(flush-output)))

(define fit-files-test-suite
  (test-suite
   "FIT files test suite"
   (test-case "FIT files test case"
     (do-basic-checks "./test-fit/f0001.fit" 18 14035)
     (do-basic-checks "./test-fit/f0002.fit" 16 500)
     (do-basic-checks "./test-fit/f0003.fit" 14 47)
     (do-basic-checks "./test-fit/f0004.fit" 18 138294)
     (do-basic-checks "./test-fit/f0005.fit" 13 227)
     (do-basic-checks "./test-fit/f0006.fit" 13 1297)
     (do-basic-checks "./test-fit/f0007.fit" 13 1452)
     (do-basic-checks "./test-fit/f0008.fit" 13 2331)
     (do-basic-checks "./test-fit/f0009.fit" 6 57)
     (do-basic-checks "./test-fit/f0010.fit" 19 8078)
     (do-basic-checks "./test-fit/f0011.fit" 12 39
                      #:extra-df-checks
                      (lambda (df)
                        (define stop-points (df-get-property df 'stop-points '()))
                        (check = (length stop-points) 2)
                        ;; Since this is a small data set, stop points are
                        ;; added to the data, instead of clearing existing
                        ;; points, we could also check that stop points are
                        ;; correctly added...
                        (define data (extract-data df axis-elapsed-time axis-speed 0 #f))
                        (check = (vector-length data) (+ (df-row-count df)
                                                         (* 2 (length stop-points))))
                        ;; The timer time axis does not mark stop points.
                        (define data2 (extract-data df axis-timer-time axis-speed 0 #f))
                        (check = (vector-length data2) (df-row-count df))))
     (do-basic-checks "./test-fit/f0012.fit" 6 47)
     (do-basic-checks "./test-fit/f0013.fit" 18 8253)
     (do-basic-checks "./test-fit/f0014.fit" 19 155)
     (do-basic-checks "./test-fit/f0015.fit" 19 4057)
     (do-basic-checks "./test-fit/f0016.fit" 23 2119)
     (do-basic-checks "./test-fit/f0017.fit" 16 3211)
     
     )))

(module+ test
  (require rackunit/text-ui)
  (run-tests fit-files-test-suite 'verbose))
