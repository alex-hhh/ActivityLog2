#lang racket/gui
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

;;; Comentary:
;;
;; Fixup swim drill laps for all sessions in the database.  Some garmin
;; watches will record the same timestamp for every length in a 'drill' lap
;; and will not record any distance or speed for these lenghts.  This function
;; fixes up the start time for such lengths and also adds in distance and
;; speed information.
;;
;; Swim activities are fixed on import.  This can be used to fix swim
;; activities that have already been imported.

(require "al-interactive.rkt")
(require "../rkt/import.rkt")           ; fixup-swim-drills
(require db)

(define q
  "select distinct P.session_id
     from A_LAP P, SECTION_SUMMARY SS 
    where P.summary_id = SS.id 
      and SS.swim_stroke_id = 4")

(define (fixup-all-swim-drills db)
  (for ([id (query-list db q)])
    (fixup-swim-drills db id)))

;; (fixup-all-swim-drills (current-database))
