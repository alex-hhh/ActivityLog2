#lang racket/base
;; cp-util.rkt -- critical power utilities
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2020 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require
 racket/contract
 "critical-power.rkt"
 db
 racket/match
 "../dbapp.rkt"
 "../dbutil.rkt")

(struct cp-validity (id sport sub-sport valid-from valid-until) #:transparent)

(define scp-query
  (virtual-statement
   (lambda (dbsys)
     "select VCP.cp_id,
             VCP.sport_id,
             VCP.sub_sport_id,
             VCP.cp,
             VCP.wprime,
             VCP.pmax,
             VCP.valid_from,
             VCP.valid_until
     from V_CRITICAL_POWER_FOR_SESSION VCPFS,
          V_CRITICAL_POWER VCP
    where VCPFS.session_id = ?
      and VCPFS.cp_id = VCP.cp_id")))

(define (critical-power-for-session session-id #:database (db (current-database)))
  (match (query-maybe-row db scp-query session-id)
    (#f (values #f #f))
    ((vector id sport sub-sport cp wprime pmax valid-from valid-until)
     (if (sql-null? pmax)
         (values (cp2 cp wprime)
                 (cp-validity
                  id
                  (if (sql-null? sport) #f sport)
                  (if (sql-null? sub-sport) #f sub-sport)
                  (if (sql-null? valid-from) #f valid-from)
                  (if (sql-null? valid-until) #f valid-until)))
         (values (cp3 cp wprime (- (/ wprime (- pmax cp))))
                 (cp-validity
                  id
                  (if (sql-null? sport) #f sport)
                  (if (sql-null? sub-sport) #f sub-sport)
                  (if (sql-null? valid-from) #f valid-from)
                  (if (sql-null? valid-until) #f valid-until)))))))

(provide (struct-out cp-validity))

(provide/contract
 (critical-power-for-session (->* (exact-nonnegative-integer?)
                                  (#:database connection?)
                                  (values (or/c #f cp2? cp3?) (or/c #f cp-validity?)))))
;; critical-power-for-sport
;; delete-critical-power
;; put-critical-power

#|
 (sport-zones-for-session (->* (exact-nonnegative-integer? zone-metric/c)
                               (#:database connection?)
                               (or/c #f sz?)))

 (sport-zones-for-sport (->* (exact-nonnegative-integer?
                              (or/c #f exact-nonnegative-integer?)
                              zone-metric/c)
                             (#:database connection?)
                             (or/c #f sz?)))

 (delete-sport-zones (->* ((or/c sz? exact-nonnegative-integer?))
                          (#:database connection?)
                          any/c))
 (put-sport-zones (->* (sz?)
                       (#:database connection?
                        #:exists (or/c 'replace 'error))
                       exact-nonnegative-integer?))
|#



