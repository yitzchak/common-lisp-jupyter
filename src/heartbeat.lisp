(in-package #:jupyter)

#|

# The Heartbeat channel #

|#

(defclass hb-channel (channel)
  ()
  (:documentation "Heartbeat channel class."))

#-cmucl
(defmethod start :after ((hb hb-channel))
  (with-slots (socket thread) hb
    (setf thread
          (bordeaux-threads:make-thread
            (lambda ()
              (inform :info hb "Starting thread")
              (pzmq:proxy socket socket (cffi:null-pointer)))))))

