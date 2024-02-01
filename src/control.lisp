(in-package #:jupyter)

#|

# The Control channel #

|#

(defclass control-channel (channel)
  ()
  (:documentation "Control channel class.")
  (:default-initargs :socket (nilmq:make-socket :router)))

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
                            

(defun send-debug-reply (&optional body &aux (control (kernel-control *kernel*))
                                             (content (message-content *message*)))
  (message-send control
                (make-message (channel-session control) "debug_reply"
                              (list :object-plist
                                    "type" "response"
                                    "request_seq" (gethash "seq" content)
                                    "success" :true
                                    "command" (gethash "command" content "command")
                                    "body" (or body :empty-object)))))


(defun send-debug-reply-failure (message &aux (control (kernel-control *kernel*))
                                             (content (message-content *message*)))
  (message-send control
                (make-message (channel-session control) "debug_reply"
                              (list :object-plist
                                    "type" "response"
                                    "request_seq" (gethash "seq" content)
                                    "success" :false
                                    "command" (gethash "command" content)
                                    "message" message))))

