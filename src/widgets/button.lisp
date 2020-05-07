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

(defgeneric on-button-click (w)
  (:documentation
    "This method is called when the button receives a click message."))

(defmethod on-button-click (w))

(defmethod on-custom-message ((w button) content buffers)
  (declare (ignore buffers))
  (if (and (equal (jupyter:json-getf content "event") "click")
           (slot-boundp w 'on-click))
    (funcall (widget-on-click w) w)
    (call-next-method)))
