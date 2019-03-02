(in-package #:jupyter-widgets)


(defclass base-progress (description-widget orientation-slot)
  ((bar-style
    :initarg :bar-style
    :initform ""
    :accessor widget-bar-style
    :documentation "Use a predefined styling for the progess bar."
    :trait :bool))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%view-name "ProgressView"
    :style (make-widget 'progress-style)))


(defclass float-progress (base-progress float-min-max-slots float-value-slot)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "FloatProgressModel")
  (:documentation "Displays a progress bar."))

(register-widget float-progress)


(defclass int-progress (base-progress int-min-max-slots int-value-slot)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "IntProgressModel")
  (:documentation
    "Progress bar that represents an integer bounded from above and below."))

(register-widget int-progress)
