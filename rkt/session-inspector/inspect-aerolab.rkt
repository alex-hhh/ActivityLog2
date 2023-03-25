#lang racket/base

;; inspect-aerolab.rkt -- Calculate air drag coefficient and rolling resistance
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2022, 2023 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require racket/class
         racket/gui/base
         "../aerolab/aerolab-widget.rkt")

(provide aerolab-panel%)

;;....................................................... aerolab-panel% ....

(define aerolab-panel%
  (class object%
    (init parent)
    (init-field
     [get-aerolab-parameters (lambda () (hash))]
     [put-aerolab-parameters (lambda (p) (void))])
    (super-new)

    (define awidget
      (new aerolab-widget%
           [parent parent]
           [save-parameters put-aerolab-parameters]))

    (define/public (save-visual-layout)
      (send awidget save-visual-layout))

    (define/public (unsaved-edits?)
      (send awidget unsaved-edits?))

    (define/public (set-session session df)
      (send awidget setup session df (get-aerolab-parameters)))

    (define/public (clear)
      (send awidget clear))

    ))
