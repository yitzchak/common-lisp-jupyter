
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

(defun send-status-update (iopub parent-msg sig status)
  (let ((status-content `((:execution--state . ,status))))
    (let ((status-msg (make-message-from-parent parent-msg "status" nil
						`(("execution_state" . ,status)))))
      ;;(message-send (iopub-socket iopub) status-msg :identities '("status")))))
      (message-send (iopub-socket iopub) status-msg))))

;; (json:encode-json-to-string '((:execution--state . :busy) (:text--plain . "toto")))

(defun send-execute-code (iopub parent-msg sig execution-count code)
  (let ((code-msg (make-message-from-parent parent-msg "pyin" nil
                                            `(("code" . ,code)
                                              ("execution_count" . ,execution-count)))))
    ;;(format t "content to send = ~W~%" (encode-json-to-string (message-content code-msg)))
    ;; (message-send (iopub-socket iopub) result-msg :identities '("pyin") :raw-content t))))
    (message-send (iopub-socket iopub) code-msg)))


(defun send-execute-result (iopub parent-msg sig execution-count result)
  (let ((display-obj (display result)))
    (let ((result-msg (make-message-from-parent parent-msg "pyout" nil
						`(("execution_count" . ,execution-count)
						  ("data" . ,(display-object-data display-obj))
						  ("metadata" . ())))))
    ;; (message-send (iopub-socket iopub) result-msg :identities '("pyout") :raw-content t))))
      (message-send (iopub-socket iopub) result-msg))))

(defun send-stream (iopub parent-msg sig stream-name data)
  (let ((stream-msg (make-message-from-parent parent-msg "stream" nil
					      `(("name" . ,stream-name)
						("data" . ,data)))))
    ;; (message-send (iopub-socket iopub) stream-msg :identities '("stream") :raw-content t))))
    (message-send (iopub-socket iopub) stream-msg)))
