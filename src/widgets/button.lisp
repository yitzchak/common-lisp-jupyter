(in-package #:jupyter-widgets)

(defclass button (styled-widget button-style-slot disabled-slot icon-slot
                  tooltip-slot)
  ((description
     :initarg :description
     :accessor widget-description
     :documentation "Button label."
     :trait :unicode))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "ButtonModel"
    :%view-name "ButtonView"
    :style (make-widget 'button-style))
  (:documentation "Button widget.
This widget has an `on-button-click` method that allows you to listen for the
user clicking on the button.  The click event itself is stateless."))

(register-widget button)

(defgeneric on-button-click (w)
  (:documentation
    "This method is called when the button receives a click message."))

(defmethod on-button-click (w))

(defmethod jupyter:on-comm-message ((w button) data metadata buffers)
  (declare (ignore metadata buffers))
  (if (and (equal (jupyter:json-getf data "method") "custom")
           (equal (jupyter:json-getf (jupyter:json-getf data "content") "event") "click"))
    (on-button-click w)
    (call-next-method)))
