(in-package #:jupyter)

#|

# The Heartbeat channel #

|#

(defclass hb-channel (channel)
  ()
  (:documentation "Heartbeat channel class."))


(defmethod start :after ((hb hb-channel))
  (with-slots (socket thread) hb
    (setf thread
          (bordeaux-threads:make-thread
            (lambda ()
              (inform :info hb "Starting thread")
              #-cmucl (pzmq:proxy socket socket (cffi:null-pointer))
              #+cmucl
              (pzmq:with-poll-items items ((socket :pollin))
                (prog ()
                 poll
                  (unless (zerop (pzmq:poll items +zmq-poll-timeout+))
                    (send-heartbeat hb (recv-heartbeat hb)))
                  (bordeaux-threads:thread-yield)
                  (go poll))))))))

