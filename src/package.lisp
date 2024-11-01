;;;; package.lisp

(defpackage #:cl-alsa-midi
  (:use #:cl #:cffi)
  (:import-from #:let-over-lambda
                #:g!-symbol-p
                #:defmacro/g!
                #:o!-symbol-p
                #:o!-symbol-to-g!-symbol
                #:defmacro!)
  (:export #:open-port
           #:close-port
           #:open-seq
           #:close-seq
           #:with-seq
           #:send-queue-ctrl
           #:send-ctrl
           #:send-note
           #:recv
           #:channel))

(defpackage #:cl-alsa-midi/quick
  (:use #:cl #:cl-alsa-midi)
  (:export #:send-note-on
           #:send-note-off
           #:send-pgmchange
           #:send-chanpress
           #:send-pitchbend
           #:send-control))

(defpackage #:cl-alsa-midi/midihelper
  (:use #:cl #:cffi #:cl-alsa-midi)
  (:export #:*clock-ochan*
           #:*clock-ctrl-chan*
           #:*reader-ichan*
           #:*reader-ochan*
           #:set-master-bpm
           #:midihelper-threads
           #:midihelper-start
           #:midihelper-stop
           #:if-gesture
           #:if-clock
           #:macromatch
           #:drain-channel
           #:send-event
           #:ev-noteon
           #:ev-noteoff
           #:ev-pgmchange
           #:ev-tick
           #:ev-microtick
           #:ev-start
           #:ev-stop
           #:ev-continue
           #:ev-songpos
           #:ev-cc
	   #:ev-pitchbend))
