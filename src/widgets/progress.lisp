(in-package #:jupyter-widgets)


(defclass base-progress (dom-widget)
  ((bar-style
    :initarg :bar-style
    :initform ""
    :accessor widget-bar-style
    :documentation "Use a predefined styling for the progess bar."
    :trait :bool)
   (description
    :initarg :description
    :initform ""
    :accessor widget-description
    :documentation "Description of the control."
    :trait :unicode)
   (description-tooltip
    :initarg :description-tooltip
    :accessor widget-description-tooltip
    :documentation "Tooltip for the description (defaults to description)."
    :trait :unicode)
   (orientation
    :initarg :orientation
    :initform "horizontal"
    :accessor widget-orientation
    :documentation "Vertical or horizontal."
    :trait :unicode)
   (style
    :initarg :style
    :initform (make-widget 'progress-style)
    :accessor widget-style
    :documentation "Reference to progress style widget."
    :trait :widget))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-module +controls-module+
    :%model-module-version +controls-module-version+
    :%view-name "ProgressView"
    :%view-module +controls-module+
    :%view-module-version +controls-module-version+))


(defclass float-progress (base-progress)
  ((max
    :initarg :max
    :initform 100.0d0
    :accessor widget-max
    :documentation "Max value"
    :trait :float)
   (min
    :initarg :min
    :initform 0.0d0
    :accessor widget-min
    :documentation "Min value"
    :trait :float)
   (value
    :initarg :value
    :initform 0.0d0
    :accessor widget-value
    :documentation "Float value"
    :trait :float))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "FloatProgressModel"))

(register-widget float-progress)


(defclass int-progress (base-progress)
  ((max
    :initarg :max
    :initform 100
    :accessor widget-max
    :documentation "Max value"
    :trait :int)
   (min
    :initarg :min
    :initform 0
    :accessor widget-min
    :documentation "Min value"
    :trait :int)
   (value
    :initarg :value
    :initform 0
    :accessor widget-value
    :documentation "Int value"
    :trait :int))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "IntProgressModel"))

(register-widget int-progress)
