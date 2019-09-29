(in-package #:jupyter-widgets)

(defclass style (widget)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-module +controls-module+
    :%model-module-version +controls-module-version+
    :%view-name "StyleView"
    :%view-module +base-module+
    :%view-module-version +base-module-version+))


(defclass button-style (style)
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
    :%model-name "ButtonStyleModel")
  (:documentation "Button style widget"))

(register-widget button-style)


(defclass description-style (style)
  ((description-width
     :initarg :description-width
     :initform nil
     :accessor widget-description-width
     :documentation "Width of the description to the side of the control."
     :trait :unicode))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "DescriptionStyleModel"))

(register-widget description-style)


(defclass progress-style (description-style)
  ((bar-color
     :initarg :bar-color
     :accessor widget-bar-color
     :documentation "Color of the slider handle."
     :trait :color))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "ProgressStyleModel")
  (:documentation "Progress style widget."))

(register-widget progress-style)


(defclass slider-style (description-style)
  ((handle-color
     :initarg :handle-color
     :accessor widget-handle-color
     :documentation "Color of the slider handle."
     :trait :color))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "SliderStyleModel"))

(register-widget slider-style)


(defclass toggle-buttons-style (style)
  ((button-width
     :initarg :button-width
     :initform nil
     :accessor widget-button-width
     :documentation "The width of each button."
     :trait :unicode)
   (description-width
     :initarg :description-width
     :initform nil
     :accessor widget-description-width
     :documentation "Width of the description to the side of the control."
     :trait :unicode)
   (font-weight
     :initarg :font-weight
     :accessor widget-font-weight
     :documentation "Text font weight of each button."
     :trait :unicode))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "ToggleButtonsStyleModel")
  (:documentation "Toggle Button style widget."))

(register-widget toggle-buttons-style)


(defclass styled-widget (dom-widget)
  ((style
     :initarg :style
     :accessor widget-style
     :documentation "Reference to style widget."
     :trait :widget))
  (:metaclass trait-metaclass))


(defclass description-widget (styled-widget)
  ((description
     :initarg :description
     :accessor widget-description
     :documentation "Description of the control."
     :trait :unicode)
   (description-tooltip
     :initarg :description-tooltip
     :accessor widget-description-tooltip
     :documentation "Tooltip for the description (defaults to description)."
     :trait :unicode))
  (:metaclass trait-metaclass)
  (:default-initargs
    :style (make-instance 'description-style)))
