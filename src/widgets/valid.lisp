(in-package #:jupyter-widgets)

(defclass valid (description-widget disabled-slot bool-value-slot)
  ((readout
     :initarg :readout
     :initform "Invalid"
     :accessor widget-readout
     :documentation "Message displayed when the value is False"
     :trait :unicode))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "ValidModel"
    :%view-name "ValidView")
  (:documentation
"Displays a boolean `value` in the form of a green check (True / valid) or a red
cross (False / invalid)."))


