#lang racket/base

;; make-checksums.rkt -- produce checksums of a distribution, this is used
;; from the build pipeline.
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2022 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require file/sha1
         racket/string
         racket/match)

;; Calculate SHA256 checksums for all files in DIR and write them out to
;; OUTPUT-FILE -- the output file is in the same format that `sha25sum`
;; program would produce this.
(define (make-checksums dir output-file)
  ;; Collect all entries first, and write them out in one go at the end.  This
  ;; avoids confusing `in-directory` if the output file is inside the
  ;; directory `dir` and would be picked up by the scan.
  (define checksums
    (parameterize ([current-directory dir])
      (for/list ([d (in-directory ".")] #:when (file-exists? d))
        (define sha256 (call-with-input-file d sha256-bytes))
        ;; This is technically a hack, but I have no idea how to use the
        ;; Racket path's utilities to make the file name "uniform" across
        ;; systems
        (define file-name (string-replace (path->string d) "\\" "/"))
        (cons file-name sha256))))
  (call-with-output-file output-file
    (lambda (out)
      (for ([entry (in-list checksums)])
        (match-define (cons file-name sha256) entry)
        (write-string (format "~a *~a~%" (bytes->hex-string sha256) file-name) out)))
    #:exists 'replace
    ;; Use binary mode to force Unix line endings, so the sha256sum can verify
    ;; the file.
    #:mode 'binary))

(module+ main
  (require racket/cmdline)

  (define-values (dist-dir checksum-file)
    (command-line
     #:program "make-checksums"
     #:args (dist-dir checksum-file)
     (values dist-dir checksum-file)))

  (make-checksums dist-dir checksum-file))
