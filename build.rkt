#lang racket/base
;; build.rkt -- utilities for building the ActivityLog2 application

;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015, 2018, 2019, 2020, 2021, 2022, 2023, 2024 Alex Harsányi <AlexHarsanyi@gmail.com>
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

;; NOTE: Gracket instance used to run this might have to have "debugging"
;; disabled in the Language menu

(require compiler/cm
         compiler/distribute
         compiler/embed
         launcher/launcher
         racket/file
         racket/port
         racket/string
         racket/system)

(define app-icon-file "./img/logo/ActivityLog2.ico")
(define app-exe-file
  (if (eq? (system-type 'os) 'windows)
      "ActivityLog2.exe"
      "ActivityLog2"))
(define app-icon-file-ca "./img/logo/AL2-Climb-Analysis.ico")
(define app-exe-file-ca
  (if (eq? (system-type 'os) 'windows)
      "AL2-Climb-Analysis.exe"
      "AL2-Climb-Analysis"))

(define app-exe-file-ai
  (if (eq? (system-type 'os) 'windows)
      "AL2-Activity-Import.exe"
      "AL2-Activity-Import"))

(define (app-revision)
  (parameterize ((current-error-port (open-output-bytes)))
    (let ((rev (string-trim (with-output-to-string
                              (lambda () (system "git rev-parse HEAD")))))
          (status (with-output-to-string
                    (lambda () (system "git status --porcelain")))))
      (if (string=? status "")
          rev
          (format "~a (modified)" rev)))))

(define (compile-app)
  (define revision (app-revision))

  ;; Write the application revision, version.rkt will read it in and store it
  ;; in the compiled .zo file.
  (with-output-to-file "./build-id.txt"
    (lambda () (printf "~a~%" revision))
    #:mode 'text #:exists 'truncate/replace)

  (managed-compile-zo "./rkt/main.rkt")
  (managed-compile-zo "./run.rkt"))

(define (build-app)

  (parameterize
      ([use-compiled-file-paths (list "compiled")])
    (create-embedding-executable
     app-exe-file
     #:modules '((#f "run.rkt"))
     #:mred? #t
     #:configure-via-first-module? #t
     ;; NOTE: these command line options are processed by racket, the "--"
     ;; flag allows the application to see its own command line options.
     #:cmdline '("--no-user-path" "--no-init-file" "--")
     #:expand-namespace (make-base-namespace)
     #:literal-expression
     (parameterize ([current-namespace (make-base-namespace)])
       (compile `(namespace-require ''run)))
     #:aux
     (cons (cons 'single-instance? #f)
           (build-aux-from-path app-icon-file))))

    (parameterize
        ([use-compiled-file-paths (list "compiled")])
      (create-embedding-executable
       app-exe-file-ca
       #:modules '((#f "apps/al2-climb-analysis.rkt"))
       #:mred? #t
       #:configure-via-first-module? #t
       ;; NOTE: these command line options are processed by racket, the "--"
       ;; flag allows the application to see its own command line options.
       #:cmdline '("--no-user-path" "--no-init-file" "--")
       #:expand-namespace (make-base-namespace)
       #:literal-expression
       (parameterize ([current-namespace (make-base-namespace)])
         (compile `(namespace-require ''al2-climb-analysis)))
       #:aux
       (cons (cons 'single-instance? #f)
             (build-aux-from-path app-icon-file-ca))))

    (parameterize
        ([use-compiled-file-paths (list "compiled")])
      (create-embedding-executable
       app-exe-file-ai
       #:modules '((#f "apps/al2-activity-import.rkt"))
       #:mred? #f
       #:configure-via-first-module? #t
       ;; NOTE: these command line options are processed by racket, the "--"
       ;; flag allows the application to see its own command line options.
       #:cmdline '("--no-user-path" "--no-init-file" "--")
       #:expand-namespace (make-base-namespace)
       #:literal-expression
       (parameterize ([current-namespace (make-base-namespace)])
         (compile `(namespace-require ''al2-activity-import))))))

(define (mkdist)
  (assemble-distribution
   "dist"
   (list app-exe-file app-exe-file-ca app-exe-file-ai)))

(define issc-program "C:/Program Files (x86)/Inno Setup 6/iscc.exe")

(define (mkinstaller)
  (define version (string-trim (file->string "./version.txt" #:mode 'text)))
  ;; Azure Builds store a unique incrementing build id in the BUILD_BUILDID
  ;; environment variable.  Append it to the version number if we have it.
  (define build-id (getenv "BUILD_BUILDID"))
  (define v (if (and build-id (string->number build-id))
                (string-append version "." build-id)
                version))
  (system (format "\"~a\" /DMyAppVersion=~a etc/scripts/install.iss" issc-program v)))

(module+ main

  (require "rkt/check-missing-modules.rkt")

  ;; Check and inform the user that these packages need to be installed...
  (check-missing-modules
   the-application
   tzinfo
   tzgeolookup
   data-frame
   plot-container
   gui-widget-mixins
   map-widget
   colormaps
   geoid)

  ;; If this file is invoked from the command line, the application will be
  ;; built automatically (including the installer)
  (printf "Compiling .zo files...")(flush-output (current-output-port))
  (compile-app)
  (printf " done.")(newline)(flush-output (current-output-port))
  (printf "Building application executable...")(flush-output (current-output-port))
  (build-app)
  (printf " done.")(newline)(flush-output (current-output-port))
  (printf "Assembling distribution...")(flush-output (current-output-port))
  (mkdist)
  (printf " done.")(newline)(flush-output (current-output-port)))
