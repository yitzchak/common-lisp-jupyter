(in-package #:jupyter)

#|

# The shell router socket #

|#

(defclass shell-channel (channel)
  ()
  (:documentation "SHELL channel class.")
  (:default-initargs :socket (nilmq:make-socket :router)))

#|

# Message sending functions

|#

(defun send-is-complete-reply (status &aux (shell (kernel-shell *kernel*)))
  (message-send shell
                (make-message (channel-session shell) "is_complete_reply"
                              (if (equal status "incomplete")
		                            (list :object-plist
		                                  "status" status
		                                  "indent" "")
		                            (list :object-plist
		                                  "status" status)))))


(defun send-execute-reply-ok (execution-count payload &aux (shell (kernel-shell *kernel*)))
  (message-send shell
                (make-message (channel-session shell) "execute_reply"
                              (list :object-plist
                                    "status" "ok"
                                    "execution_count" execution-count
                                    "user_expressions" :empty-object
                                    "payload" payload))))


(defun send-execute-reply-error (execution-count ename evalue &optional traceback &aux (shell (kernel-shell *kernel*)))
  (message-send shell
                (make-message (channel-session shell) "execute_reply"
                              (list :object-plist
                                    "status" "error"
                                    "execution_count" execution-count
                                    "ename" ename
                                    "evalue" evalue
                                    "traceback" (or traceback :empty-array)))))


(defun send-inspect-reply-error (ename evalue &optional traceback &aux (shell (kernel-shell *kernel*)))
  (message-send shell
                (make-message (channel-session shell) "inspect_reply"
                              (list :object-plist
                                    "status" "error"
                                    "ename" ename
                                    "evalue" evalue
                                    "traceback" (or traceback :empty-array)))))


(defun send-inspect-reply-ok (data &optional metadata &aux (shell (kernel-shell *kernel*)))
  (message-send shell
                (make-message (channel-session shell) "inspect_reply"
                              (list :object-plist
                                    "status" "ok"
                                    "found" (if data :true :false)
                                    "data" (or data :empty-object)
                                    "metadata" (or metadata :empty-object)))))


(defun send-complete-reply-error (ename evalue &optional traceback &aux (shell (kernel-shell *kernel*)))
  (message-send shell
                (make-message (channel-session shell) "complete_reply"
                              (list :object-plist
                                    "status" "error"
                                    "ename" ename
                                    "evalue" evalue
                                    "traceback" (or traceback :empty-array)))))


(defun send-complete-reply-ok (matches start end &optional metadata &aux (shell (kernel-shell *kernel*)))
  (message-send shell
                (make-message (channel-session shell) "complete_reply"
                              (list :object-plist
                                    "status" "ok"
                                    "matches" (or matches :empty-array)
                                    "cursor_start" start
                                    "cursor_end" end
                                    "metadata" (or metadata :empty-object)))))


(defun send-comm-info-reply (comms &aux (shell (kernel-shell *kernel*)))
  (message-send shell
                (make-message (channel-session shell) "comm_info_reply"
                              (list :object-plist
                                    "status" "ok"
                                    "comms" (cons :object-alist
                                                  (mapcar (lambda (p)
                                                            (cons (car p)
                                                                  (list :object-plist
                                                                        "target_name" (cdr p))))
                                                          comms))))))


(defun send-history-reply (history &aux (shell (kernel-shell *kernel*)))
  (message-send shell
                (make-message (channel-session shell) "history_reply"
                              (list :object-plist
                                    "status" "ok"
                                    "history" (or history :empty-array)))))

