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
                            
(defun send-debug-reply (ch parent-msg &optional body)
  (inform :info ch "Sending debug_reply ~S" body)
  (message-send ch
                (make-message (channel-session ch) "debug_reply"
                              `(:object-alist
                                 ("type" . "response")
                                 ("request_seq" . ,(gethash "seq" (message-content parent-msg)))
                                 ("success" . :true)
                                 ("command" . ,(gethash "command" (message-content parent-msg) "command"))
                                 ("body" . ,(or body :empty-object)))
                              :parent parent-msg)))

(defun send-debug-reply-failure (ch parent-msg message)
  (inform :info ch "Sending debug_reply ~S" message)
  (message-send ch
                (make-message (channel-session ch) "debug_reply"
                              `(:object-alist
                                 ("type" . "response")
                                 ("request_seq" . ,(gethash "seq" (message-content parent-msg)))
                                 ("success" . :false)
                                 ("command" . ,(gethash "command" (message-content parent-msg)))
                                 ("message" . ,message))
                              :parent parent-msg)))

