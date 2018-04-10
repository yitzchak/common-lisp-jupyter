(in-package #:maxima-jupyter)

(defvar *iopub-execute* nil)

#|

# The IOPUB publish/subscribe channel #

|#

(defclass iopub-channel (channel)
  ()
  (:documentation "IOPUB channel class."))

(defun make-iopub-channel (config ctx)
  (make-channel 'iopub-channel
                config
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

(defun send-execute-result (iopub parent-msg execution-count data)
  (message-send iopub
                (make-message parent-msg "execute_result"
                              (jsown:new-js
                                ("execution_count" execution-count)
                                ("data" data)
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

(defclass iopub-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((channel :initarg :channel
            :reader iopub-stream-channel)
   (parent-msg :initarg :parent-msg
               :reader iopub-stream-parent-msg)
   (name :initarg :name
         :reader iopub-stream-name)
   (value :initarg :value
          :initform (make-array 0
                                :fill-pointer 0
                                :adjustable t
                                :element-type 'character)
          :accessor iopub-stream-value)))

(defun make-iopub-stream (iopub parent-msg name)
  (make-instance 'iopub-stream :channel iopub
                               :parent-msg parent-msg
                               :name name))

(defmethod trivial-gray-streams:stream-write-char ((stream iopub-stream) char)
  (vector-push-extend char (iopub-stream-value stream)))

; (defmethod trivial-gray-streams:stream-write-string ((stream iopub-stream) string &optional start end)
  ; (format *debug-io* "string: ~A~%" string)
  ; t)

(defmethod trivial-gray-streams:stream-finish-output ((stream iopub-stream))
  (unless (zerop (length (iopub-stream-value stream)))
    (send-stream (iopub-stream-channel stream)
                 (iopub-stream-parent-msg stream)
                 (iopub-stream-name stream)
                 (iopub-stream-value stream))
    (setf (iopub-stream-value stream)
          (make-array 0
                      :fill-pointer 0
                      :adjustable t
                      :element-type 'character))))
