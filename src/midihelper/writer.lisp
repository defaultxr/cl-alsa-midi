(in-package :cl-alsa-midi/midihelper)

(defvar *seq* nil) ; sequence struct
(defvar **seq nil) ; pointer to sequence struct (for memory deallocation)
(defvar *my-ports* nil)

(defun %send-event (description &optional (port (car *my-ports*)) (seq *seq*))
  (optima:match description
    ((optima.extra:plist :EVENT-TYPE (optima:guard event-type (or (equal event-type :snd_seq_event_noteoff)
                                                                  (equal event-type :snd_seq_event_noteon)))
                         :EVENT-DATA (optima.extra:plist ; DURATION duration OFF_VELOCITY off_velocity
                                      VELOCITY velocity NOTE note CHANNEL channel))
     (send-note velocity note channel event-type seq port))
    ((optima.extra:plist :EVENT-TYPE (optima:guard event-type (or (equal event-type :snd_seq_event_sysex)
                                                                  (equal event-type :snd_seq_event_controller)
                                                                  (equal event-type :snd_seq_event_songpos)
                                                                  (equal event-type :snd_seq_event_pgmchange)
                                                                  (equal event-type :snd_seq_event_chanpress)
                                                                  (equal event-type :snd_seq_event_pitchbend)
                                                                  (equal event-type :snd_seq_event_control14)
                                                                  (equal event-type :snd_seq_event_nonregparam)
                                                                  (equal event-type :snd_seq_event_regparam)))
                         :EVENT-DATA (optima.extra:plist VALUE value PARAM param CHANNEL channel))
     (send-ctrl channel param value event-type seq port))
    ((optima.extra:plist :EVENT-TYPE (optima:guard event-type (or (equal event-type :snd_seq_event_clock)
                                                                  (equal event-type :snd_seq_event_start)
                                                                  (equal event-type :snd_seq_event_stop)
                                                                  (equal event-type :snd_seq_event_continue))))
     (send-queue-ctrl 0 event-type seq port))
    (_ (format t "Unknown event ~S~%" description))))

(defun start-writer ()
  (check-type **seq null)
  (check-type *seq* null)
  (check-type *my-ports* null)
  (setf **seq (open-seq "CL"))
  (setf *seq* (mem-ref **seq :pointer))
  (setf *my-ports* (list (open-port (format nil "Output")
                                    *seq*
                                    :output))))

(defun stop-writer ()
  (check-type **seq (not null))
  (close-seq **seq)
  (setf *seq* nil)
  (setf **seq nil)
  (setf *my-ports* nil))

(defvar *writer-thread* nil)
(defvar *writer-ichan* (make-nonblock-buf-channel))

(defun start-writer-thread ()
  (check-type *writer-thread* null)
  (setf *writer-thread* (bt:make-thread (lambda ()
                                          (declare (optimize (debug 3)))
                                          (with-seq (thread-seq :name "CL")
                                            (unwind-protect
                                                 (let ((port (open-port "port0" thread-seq :output)))
                                                   (handler-case
                                                       (loop
                                                         (let ((message (calispel:? *writer-ichan*)))
                                                           (restart-case
                                                               (%send-event message
                                                                            port
                                                                            thread-seq)
                                                             (carry-on-writing ()))))
                                                     (stop-thread ())))
                                              (setf *writer-thread* nil))))
                                        :name "cl-alsa-midi midihelper writer")))

(defun stop-writer-thread ()
  (bt:interrupt-thread *writer-thread* (lambda ()
                                         (error 'stop-thread))))

(defun send-event (event)
  (calispel:! *writer-ichan* event))
