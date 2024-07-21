;;;; cl-alsa-midi.asd - The cl-alsa-midi ASDF system definitions.

(asdf:defsystem #:cl-alsa-midi
  :name "cl-alsa-midi"
  :version "0.2"
  :description "A Common Lisp library for MIDI in Linux via ALSA."
  :license "GPL"
  :author "modula t."
  :mailto "modula-t at pm dot me"
  :homepage "https://github.com/defaultxr/cl-alsa-midi/"
  :bug-tracker "https://github.com/defaultxr/cl-alsa-midi/issues"
  :source-control (:git "git@github.com:defaultxr/cl-alsa-midi.git")
  :depends-on (#:cffi
               #:calispel
               #:optima
               #:let-over-lambda)
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:module driver
                :components ((:file "bindings")
                             (:file "event-lookup")
                             (:file "cl-alsa-midi")))
               (:module midihelper
                :components ((:file "reader")
                             (:file "writer")
                             (:file "clock")
                             (:file "midihelper")))
               (:file "easy-api"))
  :in-order-to ((test-op (test-op "cl-alsa-midi/tests"))))

(asdf:defsystem #:cl-alsa-midi/tests
  :name "cl-alsa-midi tests"
  :description "FiveAM-based test suite for cl-alsa-midi."
  :author "modula t."
  :license "GPL"
  :depends-on (#:cl-alsa-midi
               #:fiveam
               #:mutility/test-helpers)
  :pathname "t/"
  :serial t
  :components ((:file "test")
               (:file "midi"))
  :perform (test-op (op c)
                    (uiop:symbol-call :fiveam :run!
                                      (uiop:find-symbol* '#:cl-alsa-midi-tests
                                                         :cl-alsa-midi/tests))))

