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
                              (json-new-obj
                                ("restart" (if restart t :f)))
                              :parent parent-msg)))

              
