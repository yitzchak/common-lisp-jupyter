(in-package #:jupyter-widgets)

(defclass output (dom-widget)
  ((msg-id
    :initarg :msg-id
    :initform ""
    :accessor widget-msg-id
    :documentation "Parent message id of messages to capture"
    :trait :unicode)
   (outputs
    :initarg :outputs
    :initform nil
    :accessor widget-outputs
    :documentation "The output messages synced from the frontend."
    :trait :list))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "OutputModel"
    :%view-name "OutputView"))

(register-widget output)


(defmacro with-output (o &body body)
  `(with-slots (msg-id) ,o
    (setf msg-id (jsown:val (jupyter::message-header jupyter::*message*) "msg_id"))
    (unwind-protect
      (progn ,@body)
      (setf msg-id ""))))
