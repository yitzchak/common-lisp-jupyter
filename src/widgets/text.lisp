(in-package #:jupyter-widgets)


(defclass base-text (dom-widget)
  ((continuous-update
    :initarg :continuous-update
    :initform t
    :accessor widget-continuous-update
    :documentation "Update the value of the widget as the user is holding the slider."
    :trait :boolean)
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
   (disabled
    :initarg :disabled
    :initform nil
    :accessor widget-disabled
    :documentation "Enable or disable user changes."
    :trait :boolean)
   (style
    :initarg :style
    :initform (make-widget 'description-style)
    :accessor widget-style
    :documentation "Reference to description style widget."
    :trait :widget))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-module +controls-module+
    :%model-module-version +controls-module-version+
    :%view-module +controls-module+
    :%view-module-version +controls-module-version+))


(defclass text (base-text)
  ((placeholder
    :initarg :placeholder
    :initform (coerce '(#\U200B) 'string)
    :accessor widget-placeholder
    :documentation "Placeholder text to display when nothing has been typed."
    :trait :unicode)
   (value
    :initarg :value
    :initform ""
    :accessor widget-value
    :documentation "string value"
    :trait :unicode))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "TextModel"
    :%view-name "TextView"))


(defclass text-area (text)
  ((rows
    :initarg :rows
    :initform nil
    :accessor widget-rows
    :documentation "The number of rows to display."
    :trait :integer))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "TextareaModel"
    :%view-name "TextareaView"))


(defclass float-text (base-text)
  ((step
    :initarg :step
    :initform nil
    :accessor widget-step
    :documentation "Minimum step to increment the value"
    :trait :float)
   (value
    :initarg :value
    :initform 0.0d0
    :accessor widget-value
    :documentation "Float value"
    :trait :float))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "FloatTextModel"
    :%view-name "FloatTextView"))


(defclass bounded-float-text (float-text)
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
    :trait :float))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "BoundedFloatTextModel"
    :%view-name "BoundedFloatTextView"))


(defclass int-text (base-text)
  ((step
    :initarg :step
    :initform nil
    :accessor widget-step
    :documentation "Minimum step to increment the value"
    :trait :integer)
   (value
    :initarg :value
    :initform 0.0d0
    :accessor widget-value
    :documentation "Int value"
    :trait :integer))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "IntTextModel"
    :%view-name "IntTextView"))


(defclass bounded-int-text (int-text)
  ((max
    :initarg :max
    :initform 100.0d0
    :accessor widget-max
    :documentation "Max value"
    :trait :integer)
   (min
    :initarg :min
    :initform 0.0d0
    :accessor widget-min
    :documentation "Min value"
    :trait :integer))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "BoundedIntTextModel"
    :%view-name "BoundedIntTextView"))
