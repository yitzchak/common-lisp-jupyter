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

(defun send-is-complete-reply (shell parent-msg status)
  (message-send shell
                (make-message (channel-session shell) "is_complete_reply"
                              (list :object-plist
                                    "status" status
                                    "indent" "")
                              :parent parent-msg)))


(defun send-execute-reply-ok (shell parent-msg execution-count payload)
  (message-send shell
                (make-message (channel-session shell) "execute_reply"
                              (list :object-plist
                                    "status" "ok"
                                    "execution_count" execution-count
                                    "user_expressions" :empty-object
                                    "payload" payload)
                              :parent parent-msg)))


(defun send-execute-reply-error (shell parent-msg execution-count ename evalue &optional traceback)
  (message-send shell
                (make-message (channel-session shell) "execute_reply"
                              (list :object-plist
                                    "status" "error"
                                    "execution_count" execution-count
                                    "ename" ename
                                    "evalue" evalue
                                    "traceback" (or traceback :empty-array))
                              :parent parent-msg)))


(defun send-inspect-reply-error (shell parent-msg ename evalue &optional traceback)
  (message-send shell
                (make-message (channel-session shell) "inspect_reply"
                              (list :object-plist
                                    "status" "error"
                                    "ename" ename
                                    "evalue" evalue
                                    "traceback" (or traceback :empty-array))
                              :parent parent-msg)))


(defun send-inspect-reply-ok (shell parent-msg data &optional metadata)
  (message-send shell
                (make-message (channel-session shell) "inspect_reply"
                              (list :object-plist
                                    "status" "ok"
                                    "found" (if data :true :false)
                                    "data" (or data :empty-object)
                                    "metadata" (or metadata :empty-object))
                              :parent parent-msg)))


(defun send-complete-reply-error (shell parent-msg ename evalue &optional traceback)
  (message-send shell
                (make-message (channel-session shell) "complete_reply"
                              (list :object-plist
                                    "status" "error"
                                    "ename" ename
                                    "evalue" evalue
                                    "traceback" (or traceback :empty-array))
                              :parent parent-msg)))


(defun send-complete-reply-ok (shell parent-msg matches start end &optional metadata)
  (message-send shell
                (make-message (channel-session shell) "complete_reply"
                              (list :object-plist
                                    "status" "ok"
                                    "matches" (or matches :empty-list)
                                    "cursor_start" start
                                    "cursor_end" end
                                    "metadata" (or metadata :empty-object))
                              :parent parent-msg)))


(defun send-comm-info-reply (shell parent-msg comms)
  (message-send shell
                (make-message (channel-session shell) "comm_info_reply"
                              (list :object-plist
                                    "status" "ok"
                                    "comms" (cons :object-alist
                                                  (mapcar (lambda (p)
                                                            (cons (car p)
                                                                  (list :object-plist
                                                                        "target_name" (cdr p))))
                                                          comms)))
                              :parent parent-msg)))


(defun send-history-reply (shell parent-msg history)
  (message-send shell
                (make-message (channel-session shell) "history_reply"
                              (list :object-plist
                                    "status" "ok"
                                    "history" history)
                              :parent parent-msg)))
