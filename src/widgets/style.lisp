(in-package #:jupyter/widgets)

(defwidget style (widget)
  ()
  (:default-initargs
    :%model-module +controls-module+
    :%model-module-version +controls-module-version+
    :%view-name "StyleView"
    :%view-module +base-module+
    :%view-module-version +base-module-version+))


(defwidget button-style (style)
  ((button-color
     :initarg :button-color
     :initform nil
     :accessor widget-button-color
     :documentation "Color of the button"
     :trait :string)
   (font-weight
     :initarg :font-weight
     :accessor widget-font-weight
     :documentation "Button text font weight."
     :trait :string))
  (:default-initargs
    :%model-name "ButtonStyleModel")
  (:documentation "Button style widget"))




(defwidget description-style (style)
  ((description-width
     :initarg :description-width
     :initform nil
     :accessor widget-description-width
     :documentation "Width of the description to the side of the control."
     :trait :string))
  (:default-initargs
    :%model-name "DescriptionStyleModel"))




(defwidget progress-style (description-style)
  ((bar-color
     :initarg :bar-color
     :accessor widget-bar-color
     :documentation "Color of the slider handle."
     :trait :color))
  (:default-initargs
    :%model-name "ProgressStyleModel")
  (:documentation "Progress style widget."))




(defwidget slider-style (description-style)
  ((handle-color
     :initarg :handle-color
     :accessor widget-handle-color
     :documentation "Color of the slider handle."
     :trait :color))
  (:default-initargs
    :%model-name "SliderStyleModel"))




(defwidget toggle-buttons-style (style)
  ((button-width
     :initarg :button-width
     :initform nil
     :accessor widget-button-width
     :documentation "The width of each button."
     :trait :string)
   (description-width
     :initarg :description-width
     :initform nil
     :accessor widget-description-width
     :documentation "Width of the description to the side of the control."
     :trait :string)
   (font-weight
     :initarg :font-weight
     :accessor widget-font-weight
     :documentation "Text font weight of each button."
     :trait :string))
  (:default-initargs
    :%model-name "ToggleButtonsStyleModel")
  (:documentation "Toggle Button style widget."))




(defwidget styled-widget (dom-widget)
  ((style
     :initarg :style
     :accessor widget-style
     :documentation "Reference to style widget."
     :trait :widget)))


(defwidget description-widget (styled-widget)
  ((description
     :initarg :description
     :accessor widget-description
     :documentation "Description of the control."
     :trait :string)
   (description-tooltip
     :initarg :description-tooltip
     :accessor widget-description-tooltip
     :documentation "Tooltip for the description (defaults to description)."
     :trait :string))
  (:default-initargs
    :style (make-instance 'description-style)))
