(in-package #:jupyter-widgets)


(defclass %options-labels-slot ()
  ((%options-labels
    :initarg :%options-labels
    :initform nil
    :accessor widget-%options-labels
    :documentation "The labels for the options."
    :trait :unicode-list))
  (:metaclass trait-metaclass))


(defclass bool-value-slot ()
  ((value
    :initarg :value
    :initform nil
    :accessor widget-value
    :documentation "Bool value"
    :trait :bool))
  (:metaclass trait-metaclass))


(defclass button-style-slot ()
  ((button-style
    :initarg :button-style
    :initform ""
    :accessor widget-button-style
    :documentation "Use a predefined styling for the button."
    :trait :unicode))
  (:metaclass trait-metaclass))


(defclass byte-value-slot ()
  ((value
    :initarg :value
    :initform nil
    :accessor widget-value
    :documentation "The value as a byte string."
    :trait :byte))
  (:metaclass trait-metaclass))


(defclass continuous-update-slot ()
  ((continuous-update
    :initarg :continuous-update
    :initform t
    :accessor widget-continuous-update
    :documentation "Update the value of the widget as the user is holding the slider."
    :trait :bool))
  (:metaclass trait-metaclass))


(defclass disabled-slot ()
  ((disabled
    :initarg :disabled
    :initform nil
    :accessor widget-disabled
    :documentation "Enable or disable user changes."
    :trait :bool))
  (:metaclass trait-metaclass))


(defclass float-min-max-slots ()
  ((max
    :initarg :max
    :initform 100d0
    :accessor widget-max
    :documentation "Max value"
    :trait :float)
   (min
    :initarg :min
    :initform 0d0
    :accessor widget-min
    :documentation "Min value"
    :trait :float))
  (:metaclass trait-metaclass))


(defclass float-step-slot ()
  ((step
    :initarg :step
    :initform 0.1d0
    :accessor widget-step
    :documentation "Minimum step to increment the value"
    :trait :float))
  (:metaclass trait-metaclass))


(defclass float-value-slot ()
  ((value
    :initarg :value
    :initform 0d0
    :accessor widget-value
    :documentation "Float value"
    :trait :float))
  (:metaclass trait-metaclass))


(defclass format-slot ()
  ((format
    :initarg :format
    :accessor widget-format
    :documentation "The format of the media."
    :trait :string))
  (:metaclass trait-metaclass))


(defclass height-slot ()
  ((height
    :initarg :height
    :initform ""
    :accessor widget-height
    :documentation "Height of the media in pixels."
    :trait :string))
  (:metaclass trait-metaclass))


(defclass icon-slot ()
  ((icon
    :initarg :icon
    :initform ""
    :accessor widget-icon
    :documentation "Font-awesome icon name, without the 'fa-' prefix."
    :trait :unicode))
  (:metaclass trait-metaclass))


(defclass index-slot ()
  ((index
    :initarg :index
    :initform nil
    :accessor widget-index
    :documentation "Selected index"
    :trait :int))
  (:metaclass trait-metaclass))


(defclass int-min-max-slots ()
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
    :trait :int))
  (:metaclass trait-metaclass))


(defclass int-step-slot ()
  ((step
    :initarg :step
    :initform 1
    :accessor widget-step
    :documentation "Minimum step to increment the value"
    :trait :int))
  (:metaclass trait-metaclass))


(defclass int-value-slot ()
  ((value
    :initarg :value
    :initform 0
    :accessor widget-value
    :documentation "Int value"
    :trait :int))
  (:metaclass trait-metaclass))


(defclass orientation-slot ()
  ((orientation
    :initarg :orientation
    :initform "horizontal"
    :accessor widget-orientation
    :documentation "Vertical or horizontal."
    :trait :unicode))
  (:metaclass trait-metaclass))


(defclass placeholder-slot ()
  ((placeholder
    :initarg :placeholder
    :initform (coerce '(#\U200B) 'string)
    :accessor widget-placeholder
    :documentation "Placeholder text to display when nothing has been typed."
    :trait :unicode))
  (:metaclass trait-metaclass))


(defclass string-value-slot ()
  ((value
    :initarg :value
    :initform ""
    :accessor widget-value
    :documentation "String value"
    :trait :unicode))
  (:metaclass trait-metaclass))


(defclass tooltip-slot ()
  ((tooltip
    :initarg :tooltip
    :initform nil
    :accessor widget-tooltip
    :documentation "Tooltip caption."
    :trait :unicode))
  (:metaclass trait-metaclass))


(defclass width-slot ()
  ((width
    :initarg :width
    :initform ""
    :accessor widget-width
    :documentation "Width of the media in pixels."
    :trait :string))
  (:metaclass trait-metaclass))
