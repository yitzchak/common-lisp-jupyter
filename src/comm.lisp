(in-package #:jupyter)

(defclass comm ()
  ((id :initarg :id
       :initform (make-uuid)
       :reader comm-id)
   (target-name :initarg :target-name
                :reader comm-target-name)
   (kernel :initarg :kernel
           :initform *kernel*
           :reader comm-kernel)))

(defun get-comm (id)
  (gethash id (kernel-comms *kernel*)))

(defgeneric create-comm (target-name id data metadata))

(defmethod create-comm (target-name id data metadata))

(defgeneric on-comm-open (comm data metadata))

(defmethod on-comm-open (comm data metadata))

(defgeneric on-comm-message (comm data metadata))

(defmethod on-comm-message (comm data metadata))

(defgeneric on-comm-close (comm data metadata))

(defmethod on-comm-close (comm data metadata))

(defun send-comm-open (comm &optional data metadata)
  (with-slots (id kernel target-name) comm
    (when kernel
      (with-slots (iopub session comms) kernel
        (setf (gethash id comms) comm)
        (message-send iopub
          (make-orphan-message session "comm_open" '("comm_open")
                               (jsown:new-js
                                 ("comm_id" id)
                                 ("target_name" target-name)
                                 ("data" (or data (jsown:new-js))))
                               metadata))))))

(defun send-comm-message (comm &optional data metadata)
  (with-slots (id kernel) comm
    (when kernel
      (with-slots (iopub session) kernel
        (message-send iopub
          (make-orphan-message session "comm_msg" '("comm_msg")
                               (jsown:new-js
                                 ("comm_id" id)
                                 ("data" (or data (jsown:new-js))))
                               metadata))))))

(defun send-comm-close (comm &optional data metadata)
  (with-slots (id kernel) comm
    (when kernel
      (with-slots (iopub session comms) kernel
        (remhash id comms)
        (message-send iopub
          (make-orphan-message session "comm_close" '("comm_close")
                               (jsown:new-js
                                 ("comm_id" id)
                                 ("data" (or data (jsown:new-js))))
                               metadata))))))
