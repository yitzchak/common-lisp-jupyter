(in-package #:jupyter-widgets)

(defclass date-picker (description-widget)
  ((disabled
    :initarg :disabled
    :initform nil
    :accessor widget-disabled
    :documentation "Enable or disable user changes."
    :trait :bool)
   (value
    :initarg :value
    :initform nil
    :accessor widget-value
    :documentation "The date value."
    :trait :date))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "DatePickerModel"
    :%view-name "DatePickerView"))

(register-widget date-picker)
