#lang racket/base
;; utilities.rkt -- various utilities
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015, 2019, 2020 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require racket/contract
         racket/file)

(provide/contract
 (data-directory (-> path-string?))
 (preferences-file (-> path-string?))
 (put-pref (-> symbol? any/c any/c))
 (get-pref (-> symbol? any/c any/c)))

;; Return the default place where data files are stored on this system.
(define (get-pref-dir)
  (if (eq? 'windows (system-type 'os))
      (let ([pref-dir (getenv "LOCALAPPDATA")])
        (if pref-dir
            (string->path pref-dir)
            (find-system-path 'pref-dir)))
      (find-system-path 'pref-dir)))

(define the-data-directory #f)

;; Return the default directory where the application will store its data
;; files.  This directory will be created if it does not exist.
(define (data-directory)
  (unless the-data-directory
    (let ((dir (get-pref-dir)))
      ;; dir might not exist, but make-directory* never fails
      (let ((pref-dir (build-path dir "ActivityLog")))
        (make-directory* pref-dir)
        (set! the-data-directory pref-dir))))
  the-data-directory)

(define the-preferences-file #f)

;; Return the name of the file used to store preferences.
(define (preferences-file)
  (unless the-preferences-file
    (set! the-preferences-file
          (build-path (data-directory) "ActivityLogPrefs.rktd")))
  the-preferences-file)

;; Store VALUE under NAME in the preferences file
(define (put-pref name value)
  (put-preferences (list name)
                   (list value)
                   (lambda (p) (error 'lock-fail "Failed to get the pref file lock" p))
                   (preferences-file)))

;; Retrieve the value for NAME from the preferences file, or return the value
;; of FAIL-THUNK if it does not exist.
(define (get-pref name fail-thunk)
  (get-preference name fail-thunk 'timestamp (preferences-file)))
