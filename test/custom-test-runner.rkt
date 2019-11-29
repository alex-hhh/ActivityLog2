#lang racket/base
;; custom-test-runner.rkt -- run tests and report the results in JUnit format
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2019 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require racket/os
         racket/class
         racket/format
         racket/match
         racket/contract
         racket/math
         racket/port
         rackunit
         rackunit/log
         rackunit/private/check-info
         rackunit/private/format
         (only-in xml
                  write-xml
                  document
                  prolog
                  p-i
                  xexpr->xml))

(provide/contract
 (run-tests (->* () (#:package (or/c #f string?)
                     #:results-file (or/c #f path-string?)
                     #:only (or/c #f (listof (listof string?)))
                     #:exclude (or/c #f (listof (listof string?))))
                 #:rest (listof test-suite?) any/c))
 (skip-test (-> any/c)))


;;.................................................. test-result helpers ....

;; A skipped test result.  The test collector has control over which tests to
;; run, and will record a test-skipped test result for skipped tests.
(struct test-skipped test-result () #:transparent)

(struct exn:test:skip exn:test (name) #:transparent)

;; Skip the current test.  This will abort the current running test and record
;; it as skipped.  Note that the test will actually error out when we use this
;; method and the collector will convert an error that contains an
;; exn:test:skip exception into a `test-skipped` result.
(define (skip-test)
  (raise (exn:test:skip "test skipped"
                        (current-continuation-marks)
                        (current-test-name))))

;; Return one of 'ok, 'fail or 'error depending on the test RESULT.  We need
;; this, since each result is a different structure type
(define (test-result-tag result)
  (cond
    ((test-skipped? result) 'skipped)
    ((test-success? result) 'ok)
    ((test-failure? result) 'fail)
    ((test-error? result) 'error)
    (#t 'unknown)))

;; Return the failure or error message for the test RESULT.  Will return an
;; empty string when RESULT is a success result.
(define (test-result-message result)
  (define value (cond ((test-failure? result) (test-failure-result result))
                      ((test-error? result) (test-error-result result))
                      (#t #f)))
  (if value
      (cond ((exn:test:check? value)
             (or (for/first ([item (in-list (exn:test:check-stack value))]
                             #:when (and (check-info? item)
                                         (equal? 'expression (check-info-name item))))
                   (let ([value (check-info-value item)])
                     (if (verbose-info? value)
                         (~a (verbose-info-value value))
                         (~a value))))
                 ""))
            ((exn? value) (exn-message value))
            (#t (~a value)))
      ""))


;; Display detailed information about the failure RESULT, does nothing if the
;; result is a success.  This is the same information the rackunit's run-tests
;; function prints out.  Note that the output is written to the standard error
;; port.
(define (maybe-display-result result)
  (define name (test-result-test-case-name result))
  (define value (cond ((test-failure? result)
                       (test-failure-result result))
                      ((test-error? result)
                       (test-error-result result))
                      (#t #f)))
  (when value
    (display-test-failure/error value name)))


;;............................................................ tsr + tcr ....

;; A test case result contains the result of the test and the duration it took
;; to execute.  Note that the name of the test case is part of the result
;; object and can be retrieved using `test-result-test-case-name`
(struct tcr (result duration) #:transparent)

;; A test suite result has a name, a timestamp when running the test suite
;; started, a total duration of all the test cases, number of test cases,
;; number of failures and errors and a list of test case results, one for each
;; test case.
;;
;; NOTE: tcrs will be in reverse order from how the tests are run.
(struct tsr (name timestamp duration total failures errors skipped tcrs) #:transparent)

;; Produce a fresh tsr structure for the test suite NAME.  All members of the
;; struct are setup to sensible initial values.
(define (fresh-tsr name)
  (tsr name (current-inexact-milliseconds) 0 0 0 0 0 '()))

;; Update a test suite result struct with a new test case result and duration.
;; This does a functional update: a new tsr structure is returned containing
;; the data from TS-RESULT updated with the new TC-RESULT + TC-DURATION.  The
;; total duration, test cases, failures and error counts are updated and the
;; test case result is also added to the structure.
(define (update-tsr ts-result tc-result [tc-duration 0])
  (match-define (tsr name timestamp duration total failures errors skipped tcrs) ts-result)
  (struct-copy
   tsr ts-result
   ;; Add the current duration to the current duration
   [duration (+ duration tc-duration)]
   ;; Increment the total number of test cases
   [total (add1 total)]
   ;; If this was a failure, increment the number of failures
   [failures (if (test-failure? tc-result) (add1 failures) failures)]
   ;; If this was an error, increment the number of errors
   [errors (if (test-error? tc-result) (add1 errors) errors)]
   ;; If this test was skipped, increment the skips
   [skipped (if (test-skipped? tc-result) (add1 skipped) skipped)]
   ;; Add the test case result to the list of results for this test
   ;; suite
   [tcrs (cons (tcr tc-result tc-duration) tcrs)]))


;;.......................................................... junit-xexpr ....

;; https://github.com/windyroad/JUnit-Schema/blob/master/JUnit.xsd

;; Convert a UTC timestamp (in milliseconds) into a string suitable for
;; writing to the XML file.
(define (->timestamp utc-milliseconds)
  (define (fmt num width)
    (~a #:width width #:align 'right #:pad-string "0" num))
  (let ((d (seconds->date (exact-truncate (/ utc-milliseconds 1000.0)) #f)))
    (string-append
     (fmt (date-year d) 4) "-" (fmt (date-month d) 2) "-" (fmt (date-day d) 2)
     "T"
     (fmt (date-hour d) 2) ":" (fmt (date-minute d) 2) ":" (fmt (date-second d) 2) "Z")))

;; Convert a list of test suite results (tsr struct instances) to an XEXPR in
;; JUnit test result format.
(define (tsrs->junit-xepr tsrs package)
  `(testsuites
    ()
    ,@(for/list ([(tsr id) (in-indexed (reverse tsrs))])
        (tsr->junit-xexpr tsr package id))))

;; Convert a single test suite result into an XEXPR corresponding to a JUnit
;; testsuite XML element.
(define (tsr->junit-xexpr result package id)
  (match-define (tsr name timestamp duration total failures errors skipped results) result)
  `(testsuite
    ((name ,name)
     (package ,package)
     (id ,(~a id))
     (timestamp ,(->timestamp timestamp))
     (hostname ,(gethostname))
     (tests ,(~a total))
     (failures ,(~a failures))
     (errors ,(~a errors))
     (skipped ,(~a skipped))
     ;; NOTE: time needs to be in seconds
     (time ,(~a (/ duration 1000.0))))
    ,@(map tcr->junit-xexpr (reverse results))))

;; Produce a string representing the detailed error information in the test
;; case RESULT.  This runs `maybe-display-result` and captures the output to a
;; string.
(define (error->string result)
  (call-with-output-string
   (lambda (out)
     (parameterize ([current-error-port out])
       (maybe-display-result result)))))

;; Convert a single test case result into an XEXPR corresponding to a JUnit
;; testcase XML element.
(define (tcr->junit-xexpr data)
  (match-define (tcr result duration) data)
  `(testcase
    ((name ,(test-result-test-case-name result))
     ;; NOTE: time needs to be in seconds
     (time ,(~a (/ duration 1000.0))))
    ,(cond ((test-failure? result)
            `(failure
              ((message ,(test-result-message result)) (type "failure"))
              ,(error->string result)))
           ((test-error? result)
            `(error
              ((message ,(test-result-message result)) (type "error"))
              ,(error->string result)))
           ((test-skipped? result)
            `(skipped ()))
           (#t ""))))

;; Save the XERPR as an XML document into FILE-NAME.  The file is replaced if
;; it already exists.
(define (save-xexpr-to-file xexpr file-name)
  (call-with-output-file file-name
    (lambda (out)
      (write-xml
       (document
        (prolog
         (list (p-i #f #f 'xml "version=\"1.0\" encoding=\"utf-8\""))
         #f '())
        (xexpr->xml xexpr)
        '())
       out))
    #:exists 'replace))


;;............................................... test-result-collector% ....

;; Collect test results from a test-suite run.  This class has methods for
;; each of the "events" tracked by `foldts-test-suite` and keeps track of the
;; current test suite and the result of the test cases encountered.
(define test-result-collector%
  (class object% (init [only #f] [exclude exclude]) (super-new)

    (define completed '())              ; completed TSRs
    (define aside '()) ; TSRs that we set aside while running nested test suites
    (define current #f)                 ; the current TSR

    ;; If not #f, contains a list of tests to run, each element is a list with
    ;; a test suite name followed by the tests to run.  If #f, all tests are
    ;; run, but `excluded-tests` is also consulted.
    (define tests-to-run only)
    ;; If not #f, contains a list of tests to exclude from the run.  Same
    ;; format as tests-to-run.  If #f, no tests are excluded.
    (define excluded-tests exclude)

    (define/public (get-test-suite-results) completed)

    ;; Invoked when a new test suite beings to run.
    (define/public (on-test-suite-start name)
      (printf "*** Testsuite ~a~%" name)
      ;; If there is a current test suite running, set it aside.  Than start a
      ;; fresh one.
      (when current
        (set! aside (cons current aside)))
      (set! current (fresh-tsr name)))

    ;; Invoked when a test suite has completed all the test cases.
    (define/public (on-test-suite-completed name)
      ;; Print detailed information about the current test suite
      (match-define (tsr name timestamp duration total failures errors skipped tsrs) current)
      (printf "*** Testsuite ~a completed in ~a ms~%*** Total tests: ~a, failures: ~a, errors: ~a, skipped: ~a~%"
              name (~r duration #:precision 2) total failures errors skipped)
      ;; Add it to the completed test suite runs
      (set! completed (cons current completed))
      ;; If there is a test suite that was set aside, restore it.
      (if (null? aside)
          (set! current #f)
          (begin
            (set! current (car aside))
            (set! aside (cdr aside)))))

    ;; Determine if the test named NAME should be run.  The `tests-to-run` and
    ;; `excluded-tests` lists are consulted.  Tests that are not run will be
    ;; marked as "skipped".
    (define/private (should-run-test? name)
      (if current
          (cond (tests-to-run
                 (for/first ([item (in-list tests-to-run)]
                             #:when (equal? (car item) (tsr-name current)))
                   (for/first ([test (in-list (cdr item))]
                               #:when (equal? test name))
                     #t)))
                (excluded-tests
                 (not
                  (for/first ([item (in-list excluded-tests)]
                              #:when (equal? (car item) (tsr-name current)))
                    (for/first ([test (in-list (cdr item))]
                                #:when (equal? test name))
                      #t))))
                (#t #t))
          #t))

    ;; Called before a test is run and if it returns #f, the test will be
    ;; skipped.  If we have a lists of tests to run, allow only those test
    ;; which are in the list, and record all other tests as skipped.  If we
    ;; don't have a list of tests to run, just run all tests.
    (define/public (before-test-case-start name)
      (if (should-run-test? name)
          (begin0 #t
            (printf "\t~a: " name))
          (begin0 #f
            ;; Display the fact that we skip this test
            (printf "\t~a: ~a~%" name 'skipped)
            (set! current (update-tsr
                           (or current (fresh-tsr "unnamed test suite"))
                           (test-skipped name))))))

    ;; Invoked after each completed test case with the test case RESULT and
    ;; the DURATION (in milliseconds) it took to run the test.
    (define/public (on-test-case-completed result duration)
      ;; If we raised the skipped exception, convert this test to a skipped
      ;; one
      (when (and (test-error? result) (exn:test:skip? (test-error-result result)))
        (set! result (test-skipped (test-result-test-case-name result))))
      ;; Display the result of running this test
      (printf "~a (~a ms)~%" (test-result-tag result) (~r duration #:precision 2))
      ;; Mark this test as succeed or failed, so that "raco test" will return
      ;; a non-zero exit code.
      (test-log! (member (test-result-tag result) '(ok skipped)))
      ;; Display any error related to this test case
      (maybe-display-result result)
      ;; Add the current test case result to the test suite result.
      (set! current (update-tsr (or current (fresh-tsr "unnamed test suite")) result duration)))

    ))

;; Run the tests in TEST-SUITE, publishing the results to the COLLECTOR, an
;; instance of test-result-collector%
(define (fold-test-results test-suite collector)
  (parameterize ([current-output-port (current-error-port)])
    (foldts-test-suite
     ;; Called before a test suite is run
     (lambda (suite name before after seed)
       (before)
       (send collector on-test-suite-start name))
     ;; Called after a test suite completed
     (lambda (suite name before after seed kid-seed)
       (after)
       (send collector on-test-suite-completed name))
     ;; Called once for each test case in a suite
     (lambda (case name action seed)
       (when (send collector before-test-case-start name)
         (define start (current-inexact-milliseconds))
         (define result (run-test-case name action))
         (define end (current-inexact-milliseconds))
         (send collector on-test-case-completed result (- end start))))
     ;; The seed (unused by our code)
     (void)
     ;; The test suite we run
     test-suite)))

;; Run all the tests in TEST-SUITES and write the report to RESULTS-FILE (if
;; it is a string).  The report will be written in JUnit xml test report
;; format.
(define (run-tests #:package [package "unnamed package"]
                   #:results-file [results-file #f]
                   #:only [only #f]
                   #:exclude [exclude #f]
                   . test-suites)
  (define collector (new test-result-collector% [only only] [exclude exclude]))
  (for ([ts (in-list test-suites)])
    (fold-test-results ts collector))
  (when results-file
    (printf "*** Writing results to ~a~%" results-file)
    (define xexpr (tsrs->junit-xepr (send collector get-test-suite-results) package))
    (save-xexpr-to-file xexpr results-file)))
