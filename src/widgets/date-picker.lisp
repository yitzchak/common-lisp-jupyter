(in-package #:jupyter-widgets)

(defclass date-picker (description-widget disabled-slot)
  ((value
     :initarg :value
     :initform nil
     :accessor widget-value
     :documentation "The date value."
     :trait :date))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "DatePickerModel"
    :%view-name "DatePickerView")
  (:documentation "Date picker widget"))


