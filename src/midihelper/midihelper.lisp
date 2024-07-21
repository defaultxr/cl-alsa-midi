(in-package :cl-alsa-midi/midihelper)

(defun midihelper-threads ()
  "Get a plist of the current midihelper clock, reader, and writer threads."
  (list :clock *clock-thread* :reader *reader-thread* :writer *writer-thread*))

(defun midihelper-start (&optional (master-slave :master) (ppqn 96) (reader-map #'identity))
  "Start the midihelper clock, reader, and writer threads."
  (check-type ppqn (member 24 96))
  (check-type master-slave (member :master :slave))
  (alexandria:doplist (key val (midihelper-threads))
    (when val
      (warn "At least one midihelper thread appears to already be running; to restart, run ~S first" 'midihelper-stop)
      (return-from midihelper-start nil)))
  (drain-channel *clock-ochan*)
  (drain-channel *clock-ctrl-chan*)
  (start-reader *clock-ctrl-chan* reader-map)
  (start-clock *clock-ctrl-chan* master-slave ppqn)
  (start-writer-thread))

(defun midihelper-stop ()
  "Stop the midihelper clock, reader, and writer threads."
  (ignore-errors (stop-reader))
  (setf *reader-thread* nil)
  (ignore-errors (stop-writer-thread))
  (setf *writer-thread* nil)
  (ignore-errors (stop-clock))
  (setf *clock-thread* nil)
  (midihelper-threads))
