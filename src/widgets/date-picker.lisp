(in-package #:jupyter/widgets)

(defwidget date-picker (description-widget disabled-slot)
  ((value
     :initarg :value
     :initform nil
     :accessor widget-value
     :documentation "The date value."
     :trait :date))
  (:default-initargs
    :%model-name "DatePickerModel"
    :%view-name "DatePickerView")
  (:documentation "Date picker widget"))


