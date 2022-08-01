(in-package #:jupyter)

(defclass comm (source)
  ((comm-id
     :initarg :comm-id
     :initform (make-uuid)
     :reader comm-id)
   (target-name
     :initarg :target-name
     :reader comm-target-name)
   (kernel
     :initarg :kernel
     :initform *kernel*
     :reader comm-kernel)
   (on-close
     :initarg :on-close
     :initform nil
     :accessor comm-on-close
     :documentation "Instance specific close notification"))
  (:default-initargs
    :sink (when *kernel*
            (source-sink *kernel*))))

(defun get-comm (id)
  (gethash id (kernel-comms *kernel*)))

(defgeneric create-comm (target-name id data metadata buffers)
  (:method (target-name id data metadata buffers)
    (declare (ignore target-name id data metadata buffers))
    nil))

(defgeneric on-comm-open (comm data metadata buffers))

(defmethod on-comm-open (comm data metadata buffers)
  (declare (ignore comm data metadata buffers)))

(defgeneric on-comm-message (comm data metadata buffers))

(defmethod on-comm-message (comm data metadata buffers)
  (declare (ignore comm data metadata buffers)))

(defgeneric on-comm-close (comm data metadata buffers))

(defmethod on-comm-close (comm data metadata buffers)
  (declare (ignore comm data metadata buffers)))

(defmethod on-comm-close :after ((comm comm) data metadata buffers)
  (dolist (func (comm-on-close comm))
    (funcall func comm data metadata buffers)))

(defun send-comm-open (comm &optional data metadata buffers)
  (with-slots (comm-id kernel target-name) comm
    (when kernel
      (with-slots (iopub session comms) kernel
        (setf (gethash comm-id comms) comm)
        (message-send iopub
          (make-message session "comm_open"
                        `(:object-alist
                           ("comm_id" . ,comm-id)
                           ("target_name" . ,target-name)
                           ("data" . ,(or data :empty-object)))
                        :metadata (or metadata :empty-object)
                        :buffers buffers))))))


(defun send-comm-message (comm &optional data metadata buffers)
  (with-slots (comm-id kernel) comm
    (when kernel
      (with-slots (iopub session) kernel
        (message-send iopub
          (make-message session "comm_msg"
                        `(:object-alist
                           ("comm_id" . ,comm-id)
                           ("data" . ,(or data :empty-object)))
                        :metadata (or metadata :empty-object)
                        :buffers buffers))))))


(defun send-comm-close (comm &optional data metadata buffers)
  (with-slots (comm-id kernel) comm
    (when kernel
      (with-slots (iopub session comms) kernel
        (remhash comm-id comms)
        (message-send iopub
          (make-message session "comm_close"
                        `(:object-alist
                           ("comm_id" . ,comm-id)
                           ("data" . ,(or data :empty-object)))
                        :metadata (or metadata :empty-object)
                        :buffers buffers))))))


(defun send-comm-close-orphan (comm-id &optional data metadata buffers
                               &aux (iopub (kernel-iopub *kernel*)))
  (message-send iopub
                (make-message (channel-session iopub) "comm_close"
                              (list :object-plist
                                    "comm_id" comm-id
                                    "data" (or data :empty-object))
                              :metadata (or metadata :empty-object)
                              :buffers buffers)))

