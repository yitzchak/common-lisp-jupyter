(in-package #:cl-jupyter)

#|

# The IOPUB publish/subscribe channel #

|#

(defun make-iopub-channel (config ctx)
  (make-channel config
                (pzmq:socket ctx :pub)
                (config-iopub-port config)))

#|

# Message sending functions

|#

(defun send-status (iopub session status)
  (message-send iopub
                (make-orphan-message session "status" '("status")
                                     (jsown:new-js
                                       ("execution_state" status)))))

(defun send-status-update (iopub parent-msg status)
  (message-send iopub
                (make-message parent-msg "status"
                              (jsown:new-js
                                ("execution_state" status)))))

(defun send-execute-code (iopub parent-msg execution-count code)
  (message-send iopub
                (make-message parent-msg "execute_input"
                              (jsown:new-js
                                ("code" code)
                                ("execution_count" execution-count)))))

(defun send-execute-result (iopub parent-msg execution-count result)
  (message-send iopub
                (make-message parent-msg "execute_result"
                              (jsown:new-js
                                ("execution_count" execution-count)
                                ("data" (display-object-data (display result)))
                                ("metadata" (jsown:new-js))))))

(defun send-execute-error (iopub parent-msg execution-count ename evalue)
  (message-send iopub
                (make-message parent-msg "error"
                              (jsown:new-js
                                ("execution_count" execution-count)
                                ("ename" ename)
                                ("evalue" evalue)
                                ("traceback" nil)))))

(defun send-stream (iopub parent-msg stream-name data)
  (message-send iopub
                (make-message parent-msg "stream"
                              (jsown:new-js
                                ("name" stream-name)
                                ("text" data)))))
