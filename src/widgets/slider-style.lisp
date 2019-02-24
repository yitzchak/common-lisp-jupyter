(in-package #:jupyter-widgets)

(defclass slider-style (widget)
  ((description-width
    :initarg :description-width
    :initform nil
    :accessor widget-description-width
    :documentation "Width of the description to the side of the control."
    :sync t)
   (handle-color
    :initarg :handle-color
    :accessor widget-handle-color
    :documentation "Color of the slider handle."
    :sync t))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "SliderStyleModel"
    :%model-module +controls-module+
    :%model-module-version +controls-module-version+
    :%view-name "StyleView"
    :%view-module +base-module+
    :%view-module-version +base-module-version+))
