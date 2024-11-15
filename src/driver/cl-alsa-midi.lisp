(in-package #:cl-alsa-midi)

(load-foreign-library '(:or "libasound.so.2" "libasound.so"))

(defun open-seq (client-name)
  "Get a new ALSA sequencer object named CLIENT-NAME."
  (let ((seq (foreign-alloc :pointer)))
    (snd_seq_open seq "default" SND_SEQ_OPEN_DUPLEX 0)
    (snd_seq_set_client_name (mem-ref seq :pointer) client-name)
    seq))

(defun close-seq (seq)
  "Close the ALSA sequencer object SEQ."
  (snd_seq_close (mem-ref seq :pointer))
  (foreign-free seq)
  (setf seq nil))

(defun open-port (name seq &optional (direction :duplex))
  "Create a new port on the ALSA sequencer object SEQ named NAME."
  (snd_seq_create_simple_port seq name
                              (apply #'logior (append (optima:match direction
                                                        ((or :duplex :input)
                                                         (list SND_SEQ_PORT_CAP_WRITE
                                                               SND_SEQ_PORT_CAP_SUBS_WRITE)))
                                                      (optima:match direction
                                                        ((or :duplex :output)
                                                         (list SND_SEQ_PORT_CAP_READ
                                                               SND_SEQ_PORT_CAP_SUBS_READ)))))
                              (logior SND_SEQ_PORT_TYPE_MIDI_GENERIC
                                      SND_SEQ_PORT_TYPE_APPLICATION)))
(defun close-port (seq port)
  "Close port PORT on ALSA sequencer object SEQ."
  (snd_seq_delete_simple_port seq port))

(defmacro! with-seq ((seq &key (name "Common Lisp")) &body body)
  "Open an ALSA sequencer connection SEQ named NAME with lexical scope in BODY."
  `(let* ((,g!seq (open-seq ,name))
          (,seq (mem-ref ,g!seq :pointer)))
     (unwind-protect
          (progn ,@body)
       (close-seq ,g!seq))))

(defun ev-key-int (key)
  "Convert an ALSA event-type keyword to a CFFI int."
  (foreign-enum-value 'snd_seq_event_type key))

(defun ev-int-key (int)
  "Convert a CFFI int to an ALSA event-type keyword."
  (foreign-enum-keyword 'snd_seq_event_type int))

(defun cond-lookup-test (event-type-key)
  "Helper function for debugging cond-lookup macro, i.e. (cond-lookup-test :snd_seq_event_noteon)."
  (let ((event-type (ev-key-int event-type-key)))
    (cond-lookup)))

(defun midi-data (*data event-type)
  "Mapping event-type and data pointer to lisp-readable MIDI data."
  (let ((type (cond-lookup)))
    (if type
        (cffi:mem-ref *data type)
        (warn "Received unknown data of type ~A" (ev-int-key type)))))

(defun describe-event (event)
  "Get a plist representation of a raw CFFI MIDI event."
  (with-foreign-slots ((type (:pointer data) queue (:pointer source) (:pointer dest)) event (:struct snd_seq_event_t))
    (list ; :pointer event
     :event-type (ev-int-key type)
     :event-data (midi-data data type)
     :source (list (mem-ref source '(:struct snd_seq_addr_t)))
     :dest (list (mem-ref dest '(:struct snd_seq_addr_t))))))

(defcvar "errno" :int)

(defun recv (*seq)
  "Poll the ALSA MIDI port at *SEQ and my-port; block until there is a MIDI event to read, then return that event."
  (cffi:with-foreign-object (*event '(:struct snd_seq_event_t))
    (snd_seq_event_input *seq *event)
    (describe-event (mem-ref *event :pointer))))

(defcfun "memset" :pointer
  (*dest :pointer)
  (val :int)
  (*n :uchar))

(defun clear-foreign-type (*ev type)
  (memset *ev 0 (foreign-type-size type)))

;; (defcvar "errno" :int)

(defun set-addr-slots (addr *port *client)
  (with-foreign-slots (((:pointer port) (:pointer client)) addr (:struct snd_seq_addr_t))
    (setf (mem-ref port :uchar) *port)
    (setf (mem-ref client :uchar) *client)))

(defmacro with-midi-event ((var data type &key (queue snd_seq_queue_direct) (flags 0)) &body body)
  `(let ((,var (convert-to-foreign (list 'type ,type
                                         'queue ,queue
                                         'flags ,flags)
                                   '(:struct snd_seq_event_t))))
     (with-foreign-slots (((:pointer data)) ,var (:struct snd_seq_event_t))
       (let ((,data data))
         ,@body))))

(defmacro! with-midi-ctrl-event ((var type channel param value) &body body)
  `(with-midi-event (,var ,g!data (ev-key-int ,type))
     (let ((,g!channel ,channel)
           (,g!param ,param)
           (,g!value ,value))
       (with-foreign-slots (((:pointer channel)
                             (:pointer param)
                             (:pointer value))
                            ,g!data (:struct snd_seq_ev_ctrl_t))
         (setf (mem-ref channel :uchar) ,g!channel)
         (setf (mem-ref param :uint) ,g!param)
         (setf (mem-ref value :int) ,g!value)))
     ,@body))

(defmacro! with-midi-note-event ((var type channel note velocity off_velocity duration) &body body)
  `(with-midi-event (,var ,g!data (ev-key-int ,type))
     (let ((,g!channel ,channel)
           (,g!note ,note)
           (,g!velocity ,velocity)
           (,g!off_velocity ,off_velocity)
           (,g!duration ,duration))
       (with-foreign-slots (((:pointer channel)
                             (:pointer note)
                             (:pointer velocity)
                             (:pointer off_velocity)
                             (:pointer duration))
                            ,g!data (:struct snd_seq_ev_note_t))
         (setf (mem-ref channel :uchar) ,g!channel)
         (setf (mem-ref note :uchar) ,g!note)
         (setf (mem-ref velocity :uchar) ,g!velocity)
         (setf (mem-ref off_velocity :uchar) ,g!off_velocity)
         (setf (mem-ref duration :uint) ,g!duration)))
     ,@body))

(defmacro! with-midi-queue-event ((var type queue unused param) &body body)
  `(with-midi-event (,var ,g!data (ev-key-int ,type))
     (let ((,g!queue ,queue)
           (,g!unused ,unused)
           (,g!param ,param))
       (with-foreign-slots (((:pointer queue)
                             (:pointer unused)
                             (:pointer param))
                            ,g!data (:struct snd_seq_ev_queue_control_t))
         (setf (mem-ref queue :uchar) ,g!queue)
         (setf (mem-ref unused :pointer) ,g!unused)
         (setf (mem-ref param :pointer) ,g!param)))
     ,@body))

(defun send-midi (*seq my-port event)
  (with-foreign-slots (((:pointer source) (:pointer dest)) event (:struct snd_seq_event_t))
    (set-addr-slots source my-port 0)
    (set-addr-slots dest SND_SEQ_ADDRESS_UNKNOWN SND_SEQ_ADDRESS_SUBSCRIBERS)
    (snd_seq_event_output *seq event)
    (snd_seq_drain_output *seq)))

(defun event-type-assert (type type-min type-max)
  "Check that TYPE is a valid event type keyword whose enum value is TYPE-MIN or above, and below TYPE-MAX."
  (let ((type-value (ev-key-int type)))
    (assert (and (>= type-value (ev-key-int type-min))
                 (< type-value (ev-key-int type-max))))))

(defun send-note (velocity note channel note-type *seq my-port)
  "Send a MIDI note."
  (event-type-assert note-type :SND_SEQ_EVENT_NOTE :SND_SEQ_EVENT_CONTROLLER)
  (with-midi-note-event (event note-type channel note velocity 0 0)
    (send-midi *seq my-port event)))

(defun send-ctrl (channel param value ctrl-type *seq my-port)
  "Send a MIDI control change (CC) message."
  (event-type-assert ctrl-type :SND_SEQ_EVENT_CONTROLLER :SND_SEQ_EVENT_SONGPOS)
  (with-midi-ctrl-event (event ctrl-type channel param value)
    (send-midi *seq my-port event)))

(defun send-queue-ctrl (queue queue-ctrl-type *seq my-port)
  "Send a MIDI queue control event."
  (event-type-assert queue-ctrl-type :SND_SEQ_EVENT_START :SND_SEQ_EVENT_TUNE_REQUEST)
  (with-midi-queue-event (event queue-ctrl-type queue (null-pointer) (null-pointer))
    (send-midi *seq my-port event)))
