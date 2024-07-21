;;;; t/test.lisp - basic tests and test utilities/fixtures/etc for the cl-alsa-midi test suite.

(defpackage #:cl-alsa-midi/tests
  (:use #:cl
        #:cl-alsa-midi
        #:alexandria
        #:mutility
        #:fiveam))

(in-package #:cl-alsa-midi/tests)

(def-suite cl-alsa-midi-tests
  :description "cl-alsa-midi tests suite.")

(in-suite cl-alsa-midi-tests)

;; (defmacro skip-unless (condition &body body)
;;   "Generate a `fiveam::test-skipped' result if CONDITION is not true; otherwise, run BODY."
;;   `(if (not ,condition) (skip "Skipped due to null ~S" ',condition) ,@body))

;; (defmacro skips-unless (condition &body body)
;;   "Generate a `fiveam::test-skipped' result for each item in BODY if CONDITION is not true; otherwise, run BODY.

;; See also: `skip-unless'"
;;   `(if (not ,condition)
;;        (progn ,@(loop :repeat (length body) :collect `(skip "Skipped due to null ~S" ',condition)))
;;        (progn ,@body)))

(test system-attributes
      "Check that the system has all the standard attributes"
      (let ((missing (system-missing-attributes '#:cl-alsa-midi)))
        (is-false missing
                  "The system definition is missing attributes: ~S" missing)))

(test undocumented-symbols
      "Check for any undocumented exported symbols"
      (let ((undocumented (package-undocumented-symbols '#:cl-alsa-midi)))
        (is-false undocumented
                  "Some exported symbols do not have docstrings: ~S" undocumented)))

(test docstrings-broken-links
      "Check for any broken links in docstrings of exported symbols"
      (let ((symbols (package-docstrings-with-broken-links '#:cl-alsa-midi)))
        (is-false symbols
                  "Some exported symbols have docstrings that contain broken links: ~S" symbols)))
