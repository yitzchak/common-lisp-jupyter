(in-package #:jupyter-widgets)

(defclass button (styled-widget button-style-slot disabled-slot icon-slot
                  tooltip-slot)
  ((description
     :initarg :description
     :accessor widget-description
     :documentation "Button label."
     :trait :unicode)
   (on-click
     :initarg :on-click
     :initform nil
     :accessor widget-on-click))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "ButtonModel"
    :%view-name "ButtonView"
    :style (make-instance 'button-style))
  (:documentation "Button widget.
This widget has an `on-button-click` method that allows you to listen for the
user clicking on the button.  The click event itself is stateless."))

(register-widget button)

(defun on-button-click (widget handler)
  (push handler (widget-on-click widget)))

(defmethod on-custom-message ((w button) content buffers)
  (declare (ignore buffers))
  (if (equal (gethash "event" content) "click")
    (dolist (handler (widget-on-click w))
            ()
      (funcall handler w))
    (call-next-method)))
