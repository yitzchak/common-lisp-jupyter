(in-package #:jupyter/widgets)


(defwidget base-progress (description-widget orientation-slot)
  ((bar-style
     :initarg :bar-style
     :initform ""
     :accessor widget-bar-style
     :documentation "Use a predefined styling for the progess bar."
     :trait :bool))
  (:default-initargs
    :%view-name "ProgressView"
    :style (make-instance 'progress-style)))


(defwidget float-progress (base-progress float-min-max-slots float-value-slot)
  ()
  (:default-initargs
    :%model-name "FloatProgressModel")
  (:documentation "Displays a progress bar."))




(defwidget int-progress (base-progress int-min-max-slots int-value-slot)
  ()
  (:default-initargs
    :%model-name "IntProgressModel")
  (:documentation
    "Progress bar that represents an integer bounded from above and below."))


