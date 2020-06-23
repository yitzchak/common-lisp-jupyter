(in-package #:jupyter)

#|

# The Control channel #

|#

(defclass control-channel (request-channel)
  ()
  (:documentation "Control channel class."))

#|

# Message sending functions

|#

(defun send-shutdown-reply (ch parent-msg restart)
  (message-send ch
                (make-message (channel-session ch) "shutdown_reply"
                              `(:object
                                 ("restart" ,restart))
                              :parent parent-msg)))

(defun send-interrupt-reply (ch parent-msg)
  (message-send ch
                (make-message (channel-session ch) "interrupt_reply"
                              :empty-object
                              :parent parent-msg)))

                            
