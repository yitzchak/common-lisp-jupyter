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
    :style (make-widget 'button-style)))

(register-widget button)

(defgeneric on-button-click (w))

(defmethod on-button-click (w))

(defmethod jupyter:on-comm-message ((w button) data metadata)
  (declare (ignore metadata))
  (if (and (equal (jsown:val data "method") "custom")
           (equal (jsown:val (jsown:val data "content") "event") "click"))
    (on-button-click w)
    (call-next-method)))
