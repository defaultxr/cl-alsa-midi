;;;; t/midi.lisp - Test MIDI sending and receiving.
;; FIX: we should check that aseqdump is actually installed before trying to run these

(in-package #:cl-alsa-midi/tests)

(in-suite cl-alsa-midi-tests)

;;; helper functions + their tests

(defun split-by-multiple-spaces (string)
  "Split STRING up by multiple consecutive spaces."
  (labels ((position-of-non-space (string)
             (position-if-not (fn (char= _ #\space)) string)))
    (let* ((first-non-space (position-of-non-space string))
           (multiple-spaces (search "  " string :start2 first-non-space)))
      (cons (subseq string first-non-space multiple-spaces)
            (when multiple-spaces
              (split-by-multiple-spaces (subseq string multiple-spaces)))))))

(test split-by-multiple-spaces
  "Test the `split-by-multiple-spaces' aseqdump parser helper function"
  (is (length= 3 (split-by-multiple-spaces "  0:1   Port subscribed            130:1 -> 132:0")))
  (is (length= 2 (split-by-multiple-spaces "130:0   Clock")))
  (is (length= 3 (split-by-multiple-spaces "132:0   Note on                 0, note 127, velocity 127")))
  (is (length= 3 (split-by-multiple-spaces "132:0   Note off                0, note 0, velocity 127")))
  (is (length= 3 (split-by-multiple-spaces "130:0   Song position pointer      value 0")))
  (is (length= 3 (split-by-multiple-spaces "130:1   Control change          0, controller 1, value 65"))))

(defun plists-match-p (plist-1 plist-2 &key (test 'eql))
  "True if the keys and values of PLIST-1 exist and are equal (as per TEST) to the keys in PLIST-2. Any extra keys/values in PLIST-2 are ignored. Returns a list of the non-matching keys and their values as the second value."
  (loop :for (key value) :on plist-1 :by #'cddr
        :for value-2 := (getf plist-2 key)
        :unless (funcall test value value-2)
          :collect (list key (list value value-2)) :into res
        :finally (return (values (null res) res))))

(test plists-match-p
  "Test the `plists-match-p' helper function"
  (is (plists-match-p (list :foo 1 :bar 2) (list :foo 1 :bar 2))
      "plists-match-p doesn't see that the same list matches")
  (is (plists-match-p (list :foo 1 :bar 2) (list :foo 1 :bar 2 :baz 3))
      "plists-match-p doesn't ignore keys missing from plist-1")
  (is-false (plists-match-p (list :foo 1 :bar 2) (list :foo 1 :bar 3))
            "plists-match-p doesn't notice non-matching values")
  (is (plists-match-p (list :foo 1 :bar 2 :baz 3) (list :bar 2 :baz 3 :foo 1))
      "plists-match-p seems to check the order when it shouldn't")
  (is (plists-match-p (list :foo 1 :bar 2 :abcd 99) (list :foo 1 :abcd 99 :bar 2 :quux 43))
      "plists-match-p doesn't ignore missing keys or doesn't ignore ordering"))

;;; aseqdump functions

(defun aseqdump (&optional (port "CL"))
  "Launch the \"aseqdump\" program with it listening to the MIDI port named PORT."
  (uiop:launch-program (list "aseqdump" (concat "--port=" port)) :ignore-error-status t :output :stream))

(defun aseqdump-lines (process)
  "Given PROCESS, a `uiop:process-info' for an aseqdump process, get its output lines."
  (subseq (uiop:slurp-stream-lines (uiop:process-info-output process)) 2))

(defun aseqdump-parse-client-port (string)
  "Parse STRING, a client:port specification, into a list of the client and port numbers."
  (let ((client-port (string-split string :char-bag #\colon)))
    (list (parse-integer (first client-port) :junk-allowed t)
          (parse-integer (second client-port) :junk-allowed t))))

(test aseqdump-parse-client-port
  "Test the `aseqdump-parse-client-port' aseqdump parser helper function"
  (is (equal (list 130 0) (aseqdump-parse-client-port "130:0")))
  (is (equal (list 0 1) (aseqdump-parse-client-port "0:1"))))

(defun aseqdump-parse-line (line)
  "Parse a line of output from aseqdump into a plist of the MIDI data."
  (destructuring-bind (client-port event-type &optional event-params) (split-by-multiple-spaces line)
    (let ((client-port (aseqdump-parse-client-port client-port))
          (event-type (friendly-symbol event-type)))
      (list* :client (first client-port)
             :port (second client-port)
             :event-type event-type
             (case event-type
               (:port-subscribed
                (let* ((split (string-split event-params))
                       (from-client-port (aseqdump-parse-client-port (first split)))
                       (to-client-port (aseqdump-parse-client-port (third split))))
                  (list :from-client (first from-client-port)
                        :from-port (second from-client-port)
                        :to-client (first to-client-port)
                        :to-port (second to-client-port))))
               (:clock
                nil)
               ((:note-on :note-off)
                (let ((split (string-split event-params)))
                  (list :channel (parse-integer (first split) :junk-allowed t)
                        :note (parse-integer (third split) :junk-allowed t)
                        :velocity (parse-integer (fifth split) :junk-allowed t))))
               (:song-position-pointer
                (list :position (parse-integer (second (string-split event-params)) :junk-allowed t)))
               (:control-change
                (let ((split (string-split event-params)))
                  (list :channel (parse-integer (first split) :junk-allowed t)
                        :controller (parse-integer (third split) :junk-allowed t)
                        :value (parse-integer (fifth split) :junk-allowed t))))
               (otherwise
                (error "Unknown event type in line ~S" line)))))))

(test aseqdump-parse-line
  "Test the `aseqdump-parse-line' aseqdump parser helper function"
  (is (equal (list :client 0 :port 1 :event-type :port-subscribed :from-client 130 :from-port 1 :to-client 132 :to-port 0)
             (aseqdump-parse-line "  0:1   Port subscribed            130:1 -> 132:0")))
  (is (equal (list :client 130 :port 0 :event-type :clock)
             (aseqdump-parse-line "130:0   Clock")))
  (is (equal (list :client 132 :port 0 :event-type :note-on :channel 0 :note 127 :velocity 127)
             (aseqdump-parse-line "132:0   Note on                 0, note 127, velocity 127")))
  (is (equal (list :client 132 :port 0 :event-type :note-off :channel 0 :note 0 :velocity 127)
             (aseqdump-parse-line "132:0   Note off                0, note 0, velocity 127")))
  (is (equal (list :client 130 :port 0 :event-type :song-position-pointer :position 0)
             (aseqdump-parse-line "130:0   Song position pointer      value 0")))
  (is (equal (list :client 130 :port 1 :event-type :control-change :channel 0 :controller 1 :value 65)
             (aseqdump-parse-line "130:1   Control change          0, controller 1, value 65"))))

(defmacro with-aseqdump (&body body)
  "Start the MIDI threads and aseqdump, run BODY, then stop aseqdump and the MIDI threads, returning the parsed output of aseqdump as a list of plists."
  `(let ((reader-thread (getf (cl-alsa-midi/midihelper:midihelper-threads) :reader)))
     (unless reader-thread
       (cl-alsa-midi/midihelper:midihelper-start)
       (sleep 0.5))
     (unwind-protect
          (let ((process (aseqdump)))
            ,@body
            (sleep 0.5)
            (uiop:terminate-process process)
            (values (mapcar 'aseqdump-parse-line (aseqdump-lines process)) process))
       (unless reader-thread
         (cl-alsa-midi/midihelper:midihelper-stop)))))

(test midihelper-send-event
  "Test sending events with the midihelper subsystem"
  (let ((results (with-aseqdump
                   (cl-alsa-midi/midihelper:send-event (cl-alsa-midi/midihelper:ev-noteon 0 0 127))
                   (cl-alsa-midi/midihelper:send-event (cl-alsa-midi/midihelper:ev-noteon 0 69 127))
                   (cl-alsa-midi/midihelper:send-event (cl-alsa-midi/midihelper:ev-noteon 0 127 127))
                   (cl-alsa-midi/midihelper:send-event (cl-alsa-midi/midihelper:ev-noteoff 0 0 127))
                   (cl-alsa-midi/midihelper:send-event (cl-alsa-midi/midihelper:ev-noteoff 0 69 127))
                   (cl-alsa-midi/midihelper:send-event (cl-alsa-midi/midihelper:ev-noteoff 0 127 127)))))
    (is (plists-match-p (list :port 0 :event-type :note-on :channel 0 :note 0 :velocity 127)
                        (first results)))
    (is (plists-match-p (list :port 0 :event-type :note-on :channel 0 :note 69 :velocity 127)
                        (second results)))
    (is (plists-match-p (list :port 0 :event-type :note-on :channel 0 :note 127 :velocity 127)
                        (third results)))
    (is (plists-match-p (list :port 0 :event-type :note-off :channel 0 :note 0 :velocity 127)
                        (fourth results)))
    (is (plists-match-p (list :port 0 :event-type :note-off :channel 0 :note 69 :velocity 127)
                        (fifth results)))
    (is (plists-match-p (list :port 0 :event-type :note-off :channel 0 :note 127 :velocity 127)
                        (sixth results)))))
