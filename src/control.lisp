(in-package #:jupyter)

#|

# The Control channel #

|#

(defclass control-channel (request-channel)
  ()
  (:documentation "Control channel class."))

(defmethod start :after ((instance control-channel))
  (setf (channel-thread instance)
        (bordeaux-threads:make-thread
          (lambda ()
            (inform :info instance "Starting thread")
            (do ((msg (message-recv instance) (message-recv instance)))
                (nil)
              (inform :info instance "Received ~A message" (json-getf (message-header msg) "msg_type"))
              (enqueue-high (channel-request-queue instance) msg))))))

#|

# Message sending functions

|#

(defun send-shutdown-reply (ch parent-msg restart)
  (message-send ch
                (make-message parent-msg "shutdown_reply"
                              (jsown:new-js
                                ("restart" (if restart t :f))))))

              
