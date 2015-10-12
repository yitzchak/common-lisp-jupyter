
(in-package #:cl-jupyter)

#|

# The IOPUB publish/subscribe channel #

|#

(defclass iopub-channel ()
  ((kernel :initarg :kernel :reader iopub-kernel)
   (socket :initarg :socket :initform nil :accessor iopub-socket)))

(defun make-iopub-channel (kernel)
  (let ((socket (pzmq:socket (kernel-ctx kernel) :pub)))  
    (let ((iopub (make-instance 'iopub-channel
                                :kernel kernel
                                :socket socket)))
      (let ((config (slot-value kernel 'config)))
        (let ((endpoint (format nil "~A://~A:~A"
                                  (config-transport config)
                                  (config-ip config)
                                  (config-iopub-port config))))
          ;;(format t "[IOPUB] iopub endpoint is: ~A~%" endpoint)
          (pzmq:bind socket endpoint)
	  (setf (slot-value kernel 'iopub) iopub)
          iopub)))))

(defun send-status-starting (iopub session &key (key nil))
  (let ((status-msg (make-orphan-message session "status" nil
					  `(("execution_state" . "starting")))))
    (message-send (iopub-socket iopub) status-msg :identities '("status") :key key)))

(defun send-status-update (iopub parent-msg status &key (key nil))
  (let ((status-content `((:execution--state . ,status))))
    (let ((status-msg (make-message parent-msg "status" nil
				    `(("execution_state" . ,status)))))
      (message-send (iopub-socket iopub) status-msg :identities '("status") :key key))))

(defun send-execute-code (iopub parent-msg execution-count code &key (key nil))
  (let ((code-msg (make-message  parent-msg "execute_input" nil
				 `(("code" . ,code)
				   ("execution_count" . ,execution-count)))))
    ;;(format t "content to send = ~W~%" (encode-json-to-string (message-content code-msg)))
    (message-send (iopub-socket iopub) code-msg :identities '("execute_input") :key key)))


(defun send-execute-result (iopub parent-msg execution-count result &key (key nil))
  (let ((display-obj (display result)))
    (let ((result-msg (make-message parent-msg "execute_result" nil
				    `(("execution_count" . ,execution-count)
				      ("data" . ,(display-object-data display-obj))
				      ("metadata" . ())))))
      (message-send (iopub-socket iopub) result-msg :identities '("execute_result") :key key))))

(defun send-stream (iopub parent-msg stream-name data &key (key nil))
  (let ((stream-msg (make-message parent-msg "stream" nil
				  `(("name" . ,stream-name)
				    ("text" . ,data)))))
    (message-send (iopub-socket iopub) stream-msg :identities `(,(format nil "stream.~W" stream-name)) :key key)))
