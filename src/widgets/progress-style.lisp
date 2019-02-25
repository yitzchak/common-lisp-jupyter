(in-package #:jupyter-widgets)

(defclass progress-style (widget)
  ((bar-color
    :initarg :bar-color
    :accessor widget-bar-color
    :documentation "Color of the slider handle."
    :trait :color)
   (description-width
    :initarg :description-width
    :initform nil
    :accessor widget-description-width
    :documentation "Width of the description to the side of the control."
    :trait :unicode))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "ProgressStyleModel"
    :%model-module +controls-module+
    :%model-module-version +controls-module-version+
    :%view-name "StyleView"
    :%view-module +base-module+
    :%view-module-version +base-module-version+))

(register-widget progress-style)
