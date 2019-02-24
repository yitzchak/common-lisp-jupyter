(in-package #:jupyter-widgets)

(defclass button-style (widget)
  ((button-color
    :initarg :button-color
    :initform nil
    :accessor widget-button-color
    :documentation "Color of the button"
    :trait :unicode)
   (font-weight
    :initarg :font-weight
    :accessor widget-font-weight
    :documentation "Button text font weight."
    :trait :unicode))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "ButtonStyleModel"
    :%model-module +controls-module+
    :%model-module-version +controls-module-version+
    :%view-name "StyleView"
    :%view-module +base-module+
    :%view-module-version +base-module-version+))
