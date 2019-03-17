(in-package #:jupyter)

#|

# The Heartbeat channel #

|#

(defclass hb-channel (channel)
  ((thread-id :initarg :thread-id
              :initform nil
              :accessor hb-thread-id))
  (:documentation "Heartbeat channel class."))

#-cmucl
(defmethod start :after ((hb hb-channel))
  (with-slots (socket thread-id) hb
    (setf thread-id
          (bordeaux-threads:make-thread
            (lambda ()
              (inform :info hb "Starting thread")
              (pzmq:proxy socket socket (cffi:null-pointer)))))))

#-cmucl
(defmethod stop :before ((hb hb-channel))
  (inform :info hb "Stopping thread")
  (bordeaux-threads:destroy-thread (hb-thread-id hb)))
