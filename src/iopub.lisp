(in-package #:cl-jupyter)

#|

# The IOPUB publish/subscribe channel #

|#

(defclass iopub-channel ()
  ((kernel :initarg :kernel
           :reader iopub-kernel)
   (socket :initarg :socket
           :initform nil
           :accessor iopub-socket)))

(defun make-iopub-channel (kernel)
  (let* ((socket (pzmq:socket (kernel-ctx kernel) :pub))
         (iopub (make-instance 'iopub-channel
                               :kernel kernel
                               :socket socket))
         (config (slot-value kernel 'config))
         (endpoint (format nil "~A://~A:~A"
                           (config-transport config)
                           (config-ip config)
                           (config-iopub-port config))))
    ; (format t "[IOPUB] iopub endpoint is: ~A~%" endpoint)
    (pzmq:bind socket endpoint)
	  (setf (slot-value kernel 'iopub) iopub)
    iopub))

(defun send-status-starting (iopub session &key (key nil))
  (let ((status-msg (make-orphan-message session "status" '("status")
                                         (jsown:new-js
                                           ("execution_state" "starting")))))
    (message-send (iopub-socket iopub) status-msg :key key)))

(defun send-status-update (iopub parent-msg status &key (key nil))
  (let ((status-msg (make-message parent-msg "status"
                                  (jsown:new-js
                                    ("execution_state" status)))))
      (message-send (iopub-socket iopub) status-msg :key key)))

(defun send-execute-code (iopub parent-msg execution-count code &key (key nil))
  (let ((code-msg (make-message parent-msg "execute_input"
                                (jsown:new-js
                                  ("code" code)
                                  ("execution_count" execution-count)))))
    (message-send (iopub-socket iopub) code-msg :key key)))

(defun send-execute-result (iopub parent-msg execution-count result &key (key nil))
  (let* ((display-obj (display result))
         (result-msg (make-message parent-msg "execute_result"
                                   (jsown:new-js
                                     ("execution_count" execution-count)
                                     ("data" (display-object-data display-obj))
                                     ("metadata" (jsown:new-js))))))
    (message-send (iopub-socket iopub) result-msg :key key)))

(defun send-execute-error (iopub parent-msg execution-count ename evalue &key (key nil))
  (let* ((result-msg (make-message parent-msg "error"
                                   (jsown:new-js
                                     ("execution_count" execution-count)
                                     ("ename" ename)
                                     ("evalue" evalue)
                                     ("traceback" nil)))))
    (message-send (iopub-socket iopub) result-msg :key key)))

(defun send-stream (iopub parent-msg stream-name data &key (key nil))
  (let ((stream-msg (make-message parent-msg "stream"
                                  (jsown:new-js
                                    ("name" stream-name)
                                    ("text" data)))))
    (message-send (iopub-socket iopub) stream-msg :key key)))
