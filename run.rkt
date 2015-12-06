#lang racket/base
;; run.rkt -- toplevel file to run the ActivityLog2 application

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


;; for some reason, Dr-Racket wants this require when building a
;; distribution.  Commenting it out seems to produce better error
;; messages in interactive mode.

(require errortrace
         "rkt/activity-log-main.rkt")

(main)
