(in-package #:jupyter)

#|

# The Heartbeat channel #

|#

(defclass hb-channel (channel)
  ((thread-id :initarg :thread-id
              :initform nil
              :accessor hb-thread-id))
  (:documentation "Heartbeat channel class."))

(defmethod start ((hb hb-channel))
  (start-channel hb)
  (let ((socket (channel-socket hb)))
    (setf (hb-thread-id hb)
          (bordeaux-threads:make-thread
            (lambda ()
              (info "[hb-channel] Thread starting...~%")
              (pzmq:proxy socket socket (cffi:null-pointer)))))))

(defmethod stop ((hb hb-channel))
  (info "[hb-channel] Thread stopped.~%")
  (bordeaux-threads:destroy-thread (hb-thread-id hb))
  (stop-channel hb))
