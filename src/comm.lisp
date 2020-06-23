(in-package #:jupyter)

(defclass comm (source)
  ((id
     :initarg :id
     :initform (make-uuid)
     :reader comm-id)
   (target-name
     :initarg :target-name
     :reader comm-target-name)
   (kernel
     :initarg :kernel
     :initform *kernel*
     :reader comm-kernel))
  (:default-initargs
    :sink (when *kernel*
            (source-sink *kernel*))))

(defun get-comm (id)
  (gethash id (kernel-comms *kernel*)))

(defgeneric create-comm (target-name id data metadata buffers))

(defmethod create-comm (target-name id data metadata buffers))

(defgeneric on-comm-open (comm data metadata buffers))

(defmethod on-comm-open (comm data metadata buffers))

(defgeneric on-comm-message (comm data metadata buffers))

(defmethod on-comm-message (comm data metadata buffers))

(defgeneric on-comm-close (comm data metadata buffers))

(defmethod on-comm-close (comm data metadata buffers))

(defun send-comm-open (comm &optional data metadata buffers)
  (with-slots (id kernel target-name) comm
    (when kernel
      (inform :info comm "~A ~A ~A" data metadata buffers)
      (with-slots (iopub session comms) kernel
        (setf (gethash id comms) comm)
        (message-send iopub
          (make-message session "comm_open"
                        `(:object
                           ("comm_id" . ,id)
                           ("target_name" . ,target-name)
                           ("data" . ,(or data :empty-object)))
                        :metadata (or metadata :empty-object)
                        :buffers buffers))))))

(defun send-comm-message (comm &optional data metadata buffers)
  (with-slots (id kernel) comm
    (when kernel
      (with-slots (iopub session) kernel
        (message-send iopub
          (make-message session "comm_msg"
                        `(:object
                           ("comm_id" . ,id)
                           ("data" . ,(or data :empty-object)))
                        :metadata (or metadata :empty-object)
                        :buffers buffers))))))

(defun send-comm-close (comm &optional data metadata buffers)
  (with-slots (id kernel) comm
    (when kernel
      (with-slots (iopub session comms) kernel
        (remhash id comms)
        (message-send iopub
          (make-message session "comm_close"
                        `(:object
                           ("comm_id" . ,id)
                           ("data" . ,(or data :empty-object)))
                        :metadata (or metadata :empty-object)
                        :buffers buffers))))))
