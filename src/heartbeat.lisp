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
             (prog (m)
              poll
                (j:inform :info hb "in ~s" (setf m (nilmq:receive socket)))
                (when m
                  (j:inform :info hb "out ~s" m)
                  (nilmq:send socket m))
                (go poll)))))))
