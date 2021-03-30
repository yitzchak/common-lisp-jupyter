(in-package #:jupyter-widgets)

(defclass checkbox (description-widget disabled-slot bool-value-slot)
  ((indent
     :initarg :indent
     :initform t
     :accessor widget-indent
     :documentation "Indent the control to align with other controls with a description."
     :trait :bool))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "CheckboxModel"
    :%view-name "CheckboxView")
  (:documentation "Displays a boolean `value` in the form of a checkbox."))


