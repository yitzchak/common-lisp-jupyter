(in-package #:cl-jupyter)

#|

# The Heartbeat channel #

|#

(defclass hb-channel (channel)
  ((thread-id :initarg :thread-id
              :initform nil
              :accessor hb-thread-id)))

(defun make-hb-channel (config ctx)
  (make-channel config
                (pzmq:socket ctx :rep)
                (config-hb-port config)
                :class 'hb-channel))

(defmethod start ((hb hb-channel))
  (start-channel hb)
  (let ((socket (channel-socket hb)))
    (setf (hb-thread-id hb)
          (bordeaux-threads:make-thread
            (lambda ()
              (info "[Heartbeat] thread started~%")
              (pzmq:proxy socket socket (cffi:null-pointer)))))))

(defmethod stop ((hb hb-channel))
  (info "[Heartbeat] thread stopped~%")
  (bordeaux-threads:destroy-thread (hb-thread-id hb))
  (stop-channel hb))
