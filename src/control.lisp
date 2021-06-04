(in-package #:jupyter)

#|

# The Control channel #

|#

(defclass control-channel (channel)
  ()
  (:documentation "Control channel class."))

#|

# Message sending functions

|#

(defun send-shutdown-reply (ch restart)
  (message-send ch
                (make-message (channel-session ch) "shutdown_reply"
                              `(:object-alist
                                 ("restart" ,restart)))))

(defun send-interrupt-reply (ch)
  (message-send ch
                (make-message (channel-session ch) "interrupt_reply"
                              :empty-object)))
                            
