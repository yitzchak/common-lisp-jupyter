(in-package #:jupyter-widgets)

(defclass color-picker (description-widget disabled-slot)
  ((concise
     :initarg :concise
     :initform nil
     :accessor widget-concise
     :documentation "Display short version with just a color selector."
     :trait :bool)
   (value
     :initarg :value
     :initform "black"
     :accessor widget-value
     :documentation "The color value."
     :trait :color))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "ColorPickerModel"
    :%view-name "ColorPickerView")
  (:documentation "Color picker widget"))

(register-widget color-picker)
