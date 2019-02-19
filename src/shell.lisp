(in-package #:jupyter)

#|

# The shell router socket #

|#

(defclass shell-channel (channel)
  ()
  (:documentation "SHELL channel class."))

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

(defun send-execute-reply-ok (shell parent-msg execution-count payload)
  (message-send shell
                (make-message parent-msg "execute_reply"
                              (jsown:new-js
                                ("status" "ok")
                                ("execution_count" execution-count)
                                ("user_expressions" (jsown:new-js))
                                ("payload" payload)))))

(defun send-execute-reply-error (shell parent-msg execution-count ename evalue)
  (message-send shell
                (make-message parent-msg "execute_reply"
                              (jsown:new-js
                                ("status" "error")
                                ("execution_count" execution-count)
                                ("ename" ename)
                                ("evalue" evalue)
                                ("traceback" nil)))))

(defun send-inspect-reply-error (shell parent-msg ename evalue)
  (message-send shell
                (make-message parent-msg "inspect_reply"
                              (jsown:new-js
                                ("status" "error")
                                ("ename" ename)
                                ("evalue" evalue)
                                ("traceback" nil)))))

(defun send-inspect-reply-ok (shell parent-msg data)
  (message-send shell
                (make-message parent-msg "inspect_reply"
                              (jsown:new-js
                                ("status" "ok")
                                ("found" (if data t nil))
                                ("data" data)
                                ("metadata" (jsown:new-js))))))

(defun send-complete-reply-error (shell parent-msg ename evalue)
  (message-send shell
                (make-message parent-msg "complete_reply"
                              (jsown:new-js
                                ("status" "error")
                                ("ename" ename)
                                ("evalue" evalue)
                                ("traceback" nil)))))

(defun send-complete-reply-ok (shell parent-msg matches start end)
  (message-send shell
                (make-message parent-msg "complete_reply"
                              (jsown:new-js
                                ("status" "ok")
                                ("matches" matches)
                                ("cursor_start" start)
                                ("cursor_end" end)
                                ("metadata" (jsown:new-js))))))

(defun send-comm-info-reply (shell parent-msg comms)
  (message-send shell
                (make-message parent-msg "comm_info_reply"
                              (jsown:new-js
                                ("comms" (mapcar (lambda (p)
                                                   (cons
                                                     (car p)
                                                     (jsown:new-js
                                                       ("target_name" (cdr p)))))
                                           comms))))))
