#lang racket/base
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

;; Build a racket distribution from the command line.  To use it run it as:
;;
;; racket --require auto-build.rkt
;;

(require "build.rkt")
(printf "Compiling .zo files...")(flush-output (current-output-port))
(compile-app)
(printf " done.")(newline)(flush-output (current-output-port))
(printf "Building application executable...")(flush-output (current-output-port))
(build-app)
(printf " done.")(newline)(flush-output (current-output-port))
(printf "Assembling distribution...")(flush-output (current-output-port))
(mkdist)
(printf " done.")(newline)(flush-output (current-output-port))
(printf "Creating installer...")(flush-output (current-output-port))
(mkinstaller)
(printf " done.")(newline)(flush-output (current-output-port))
