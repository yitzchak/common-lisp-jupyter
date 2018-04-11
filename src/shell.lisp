(in-package #:maxima-jupyter)

#|

# The shell router socket #

|#

(defclass shell-channel (channel)
  ()
  (:documentation "SHELL channel class."))

(defun make-shell-channel (config ctx)
  (make-channel 'shell-channel
                config
                (pzmq:socket ctx :router)
                (config-shell-port config)))

#|

# Message sending functions

|#

(defun send-shutdown-reply (shell parent-msg restart)
  (message-send shell
                (make-message parent-msg "shutdown_reply"
                              (jsown:new-js
                                ("restart" (if restart t :f))))))

(defun send-is-complete-reply (shell parent-msg status)
  (message-send shell
                (make-message parent-msg "is_complete_reply"
                              (jsown:new-js
                                ("status" status)
                                ("indent" "")))))

(defun send-execute-reply-ok (shell parent-msg execution-count)
  (message-send shell
                (make-message parent-msg "execute_reply"
                              (jsown:new-js
                                ("status" "ok")
                                ("execution_count" execution-count)
                                ("user_expressions" (jsown:new-js))
                                ("payload" '())))))

(defun send-execute-reply-error (shell parent-msg execution-count ename evalue)
  (message-send shell
                (make-message parent-msg "execute_reply"
                              (jsown:new-js
                                ("status" "error")
                                ("execution_count" execution-count)
                                ("ename" ename)
                                ("evalue" evalue)
                                ("traceback" nil)))))
