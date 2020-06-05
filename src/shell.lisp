(in-package #:jupyter)

#|

# The shell router socket #

|#

(defclass shell-channel (request-channel)
  ()
  (:documentation "SHELL channel class."))

(defmethod start :after ((instance shell-channel))
  (setf (channel-thread instance)
        (bordeaux-threads:make-thread
          (lambda ()
            (inform :info instance "Starting thread")
            (do ((msg (message-recv instance) (message-recv instance)))
                (nil)
              (inform :info instance "Received ~A message" (json-getf (message-header msg) "msg_type"))
              (enqueue (channel-request-queue instance) msg))))))

#|

# Message sending functions

|#

(defun send-is-complete-reply (shell parent-msg status)
  (message-send shell
                (make-message parent-msg "is_complete_reply"
                              (json-new-obj
                                ("status" status)
                                ("indent" "")))))

(defun send-execute-reply-ok (shell parent-msg execution-count payload)
  (message-send shell
                (make-message parent-msg "execute_reply"
                              (json-new-obj
                                ("status" "ok")
                                ("execution_count" execution-count)
                                ("user_expressions" (json-empty-obj))
                                ("payload" payload)))))

(defun send-execute-reply-error (shell parent-msg execution-count ename evalue)
  (message-send shell
                (make-message parent-msg "execute_reply"
                              (json-new-obj
                                ("status" "error")
                                ("execution_count" execution-count)
                                ("ename" ename)
                                ("evalue" evalue)
                                ("traceback" nil)))))

(defun send-inspect-reply-error (shell parent-msg ename evalue)
  (message-send shell
                (make-message parent-msg "inspect_reply"
                              (json-new-obj
                                ("status" "error")
                                ("ename" ename)
                                ("evalue" evalue)
                                ("traceback" nil)))))

(defun send-inspect-reply-ok (shell parent-msg data)
  (message-send shell
                (make-message parent-msg "inspect_reply"
                              (json-new-obj
                                ("status" "ok")
                                ("found" (if data t nil))
                                ("data" data)
                                ("metadata" (json-empty-obj))))))

(defun send-complete-reply-error (shell parent-msg ename evalue)
  (message-send shell
                (make-message parent-msg "complete_reply"
                              (json-new-obj
                                ("status" "error")
                                ("ename" ename)
                                ("evalue" evalue)
                                ("traceback" nil)))))

(defun send-complete-reply-ok (shell parent-msg matches start end)
  (message-send shell
                (make-message parent-msg "complete_reply"
                              (json-new-obj
                                ("status" "ok")
                                ("matches" matches)
                                ("cursor_start" start)
                                ("cursor_end" end)
                                ("metadata" (json-empty-obj))))))

(defun send-comm-info-reply (shell parent-msg comms)
  (message-send shell
                (make-message parent-msg "comm_info_reply"
                              (json-new-obj
                                ("comms" (mapcar (lambda (p)
                                                   (cons
                                                     (car p)
                                                     (json-new-obj
                                                       ("target_name" (cdr p)))))
                                           comms))))))

(defun send-history-reply (shell parent-msg history)
  (message-send shell
                (make-message parent-msg "history_reply"
                              (json-new-obj
                                ("history" history)))))
