(in-package #:jupyter)

#|

# The Heartbeat channel #

|#

(defclass hb-channel (channel)
  ()
  (:documentation "Heartbeat channel class.")
  (:default-initargs :socket (nilmq:make-socket :rep)))

(defmethod start :after ((hb hb-channel))
  (with-slots (socket thread) hb
    (setf thread
          (bordeaux-threads:make-thread
           (lambda ()
             (inform :info hb "Starting thread")
             (prog (m)
              poll
                (when (nilmq:input-available-p socket)
                  (j:inform :info hb "in ~s" (setf m (nilmq:receive socket)))
                  (nilmq:send socket m))
                (go poll)))))))
