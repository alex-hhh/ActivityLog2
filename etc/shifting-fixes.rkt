#lang racket
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; shifting-fixes.rkt -- example racket program to fix gear shifting in sessions
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2024 Alex Harsányi <AlexHarsanyi@gmail.com>
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
;;
;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.

(require "al-interactive.rkt")
(require "../rkt/session-df/shifting.rkt")

;; This program shows how to update gear teeth information for a session, if
;; the device that recorded it was mis-configured.  ActivityLog2 reads gear
;; teeth count information from the FIT file, and assumes that this is
;; correctly configured for the device.

;; Defines the front and rear teeth counts to use
(define front-gears (vector 35 46))
(define rear-gears (vector 36 32 28 24 21 19 17 15 13 12 11 10))

;; replace with the session id to fix -- you can get the session id from the
;; "Activity/Copy Session ID to clipboard" menu in ActivityLog2
(define session-id -1)

(update-gear-rations-for-session!
 (current-database)
 session-id
 front-gears
 rear-gears)
