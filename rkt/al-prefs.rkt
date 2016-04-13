#lang racket/base
;; al-prefs.rkt -- store and retrieve preferences
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

(require racket/file)
(provide al-get-pref-dir al-get-pref-file al-put-pref al-get-pref)

(define (get-pref-dir)
  (if (eq? 'windows (system-type 'os))
      (let ([pref-dir (getenv "LOCALAPPDATA")])
        (if pref-dir
            (string->path pref-dir)
            (find-system-path 'pref-dir)))
      (find-system-path 'pref-dir)))

;;(: al-get-pref-dir (-> Path))
(define (al-get-pref-dir)
  (let ((dir (get-pref-dir)))
    ;; dir might not exist, but make-directory* never fails
    (let ((pref-dir (build-path dir "ActivityLog")))
      (make-directory* pref-dir)
      pref-dir)))

;;(: al-get-pref-file (-> Path))
(define (al-get-pref-file)
  (build-path (al-get-pref-dir) "ActivityLogPrefs.rktd"))

;;(: al-put-pref (-> Symbol Any Void))
(define (al-put-pref name value)
  (put-preferences (list name) (list value) 
                   (lambda (p) (error 'lock-fail "Failed to get the pref file lock" p))
                   (al-get-pref-file)))

;;(: al-get-pref (-> Symbol (-> Any) Any))
(define (al-get-pref name fail-thunk)
  (get-preference name fail-thunk 'timestamp (al-get-pref-file)))

