(in-package #:cl-jupyter)

#|

# The Heartbeat channel #

|#

(defclass hb-channel (channel)
  ((thread-id :initarg :thread-id
              :initform nil
              :accessor hb-thread-id)))

(defun make-hb-channel (config ctx)
  (let* ((hb (make-channel config
                           (pzmq:socket ctx :rep)
                           (config-hb-port config)
                           :class 'hb-channel))
         (socket (channel-socket hb)))
    (setf (hb-thread-id hb)
          (bordeaux-threads:make-thread
            (lambda ()
              (info "[Heartbeat] thread started~%")
              (pzmq:proxy socket socket (cffi:null-pointer)))))
    hb))

(defmethod stop ((hb hb-channel))
  (info "[Heartbeat] thread stopped~%")
  (bordeaux-threads:destroy-thread (hb-thread-id hb))
  (pzmq:close (channel-socket hb)))
