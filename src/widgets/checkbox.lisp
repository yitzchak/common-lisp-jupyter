(in-package #:jupyter-widgets)

(defclass checkbox (description-widget)
  ((disabled
    :initarg :disabled
    :initform nil
    :accessor widget-disabled
    :documentation "Enable or disable user changes."
    :trait :bool)
   (indent
    :initarg :indent
    :initform t
    :accessor widget-indent
    :documentation "Indent the control to align with other controls with a description."
    :trait :bool)
   (value
    :initarg :value
    :initform nil
    :accessor widget-value
    :documentation "Bool value"
    :trait :bool))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "CheckboxModel"
    :%view-name "CheckboxView"))

(register-widget checkbox)
