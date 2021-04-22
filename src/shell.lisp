(in-package #:jupyter)

#|

# The shell router socket #

|#

(defclass shell-channel (request-channel)
  ()
  (:documentation "SHELL channel class."))

#|

# Message sending functions

|#

(defun send-is-complete-reply (shell parent-msg status)
  (message-send shell
                (make-message (channel-session shell) "is_complete_reply"
                              `(:object-alist
                                 ("status" . ,status)
                                 ("indent" . ""))
                              :parent parent-msg)))

(defun send-execute-reply-ok (shell parent-msg execution-count payload)
  (message-send shell
                (make-message (channel-session shell) "execute_reply"
                              `(:object-alist
                                 ("status" . "ok")
                                 ("execution_count" . ,execution-count)
                                 ("user_expressions" . ,:empty-object)
                                 ("payload" . ,payload))
                              :parent parent-msg)))

(defun send-execute-reply-error (shell parent-msg execution-count ename evalue &optional traceback)
  (message-send shell
                (make-message (channel-session shell) "execute_reply"
                              `(:object-alist
                                 ("status" . "error")
                                 ("execution_count" . ,execution-count)
                                 ("ename" . ,ename)
                                 ("evalue" . ,evalue)
                                 ("traceback" . ,(or traceback :empty-array)))
                              :parent parent-msg)))

(defun send-inspect-reply-error (shell parent-msg ename evalue &optional traceback)
  (message-send shell
                (make-message (channel-session shell) "inspect_reply"
                              `(:object-alist
                                 ("status" . "error")
                                 ("ename" . ,ename)
                                 ("evalue" . ,evalue)
                                 ("traceback" . ,(or traceback :empty-array)))
                              :parent parent-msg)))

(defun send-inspect-reply-ok (shell parent-msg data)
  (message-send shell
                (make-message (channel-session shell) "inspect_reply"
                              `(:object-alist
                                 ("status" . "ok")
                                 ("found" . ,(if data :true :false))
                                 ("data" . ,(or data :empty-object))
                                 ("metadata" . :empty-object))
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
                              `(:object-alist
                                 ("status" . "ok")
                                 ("matches" . ,(or matches :empty-list))
                                 ("cursor_start" . ,start)
                                 ("cursor_end" . ,end)
                                 ("metadata" . ,(or metadata :empty-object)))
                              :parent parent-msg)))

(defun send-comm-info-reply (shell parent-msg comms)
  (message-send shell
                (make-message (channel-session shell) "comm_info_reply"
                              (list :object-alist
                                    (cons "comms" (or (mapcar (lambda (p)
                                                                (cons
                                                                  (car p)
                                                                  (list :object-alist
                                                                        (cons "target_name" (cdr p)))))
                                                              comms)
                                                      :empty-array)))
                              :parent parent-msg)))

(defun send-history-reply (shell parent-msg history)
  (message-send shell
                (make-message (channel-session shell) "history_reply"
                              `(:object-alist
                                 ("history" . ,history))
                              :parent parent-msg)))
