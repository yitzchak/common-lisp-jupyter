(in-package #:jupyter-widgets)


(defclass base-slider (dom-widget)
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
   (orientation
    :initarg :orientation
    :initform "horizontal"
    :accessor widget-orientation
    :documentation "Vertical or horizontal."
    :trait :unicode)
   (readout
    :initarg :readout
    :initform t
    :accessor widget-readout
    :documentation "Display the current value of the slider next to it."
    :trait :boolean)
   (readout-format
    :initarg :readout-format
    :accessor widget-readout-format
    :documentation "Format for the readout"
    :trait :unicode)
   (style
    :initarg :style
    :initform (make-widget 'slider-style)
    :accessor widget-style
    :documentation "Reference to slider style widget."
    :trait :widget))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-module +controls-module+
    :%model-module-version +controls-module-version+
    :%view-module +controls-module+
    :%view-module-version +controls-module-version+))


(defclass number-slider (base-slider)
  ((readout-format
    :initarg :readout-format
    :accessor widget-readout-format
    :documentation "Format for the readout"
    :trait :unicode))
  (:metaclass trait-metaclass))


(defclass float-log-slider (number-slider)
  ((base
    :initarg :base
    :initform 10.0d0
    :accessor widget-base
    :documentation "Base for the logarithm"
    :trait :float)
   (max
    :initarg :max
    :initform 4.0d0
    :accessor widget-max
    :documentation "Max value for the exponent"
    :trait :float)
   (min
    :initarg :min
    :initform 0.0d0
    :accessor widget-min
    :documentation "Min value for the exponent"
    :trait :float)
   (step
    :initarg :step
    :initform 0.1d0
    :accessor widget-step
    :documentation "Minimum step in the exponent to increment the value"
    :trait :float)
   (value
    :initarg :value
    :initform 1.0d0
    :accessor widget-value
    :documentation "Float value"
    :trait :float))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "FloatLogSliderModel"
    :%view-name "FloatLogSliderView"
    :readout-format ".3g"))


(defclass float-range-slider (number-slider)
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
   (step
    :initarg :step
    :initform 0.1d0
    :accessor widget-step
    :documentation "Minimum step to increment the value"
    :trait :float)
   (value
    :initarg :value
    :initform '(0.0d0 1.0d0)
    :accessor widget-value
    :documentation "Float range"
    :trait :float-list))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "FloatRangeSliderModel"
    :%view-name "FloatRangeSliderView"
    :readout-format ".2f"))


(defclass float-slider (number-slider)
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
   (step
    :initarg :step
    :initform 0.1d0
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
    :%model-name "FloatSliderModel"
    :%view-name "FloatSliderView"
    :readout-format ".2f"))


(defclass int-range-slider (number-slider)
  ((max
    :initarg :max
    :initform 100
    :accessor widget-max
    :documentation "Max value"
    :trait :integer)
   (min
    :initarg :min
    :initform 0
    :accessor widget-min
    :documentation "Min value"
    :trait :integer)
   (step
    :initarg :step
    :initform 1
    :accessor widget-step
    :documentation "Minimum step to increment the value"
    :trait :integer)
   (value
    :initarg :value
    :initform '(0 1)
    :accessor widget-value
    :documentation "Int range value"
    :trait :integer-list))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "IntRangeSliderModel"
    :%view-name "IntRangeSliderView"
    :readout-format "d"))


(defclass int-slider (number-slider)
  ((max
    :initarg :max
    :initform 100
    :accessor widget-max
    :documentation "Max value"
    :trait :integer)
   (min
    :initarg :min
    :initform 0
    :accessor widget-min
    :documentation "Min value"
    :trait :integer)
   (step
    :initarg :step
    :initform 1
    :accessor widget-step
    :documentation "Minimum step to increment the value"
    :trait :integer)
   (value
    :initarg :value
    :initform 0
    :accessor widget-value
    :documentation "Int value"
    :trait :integer))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "IntSliderModel"
    :%view-name "IntSliderView"
    :readout-format "d"))


(defclass label-slider (base-slider)
  ((%options-labels
    :initarg :%options-labels
    :initform nil
    :accessor widget-%options-labels
    :documentation "The labels for the options."
    :trait :unicode-list))
  (:metaclass trait-metaclass))


(defclass selection-range-slider (label-slider)
  ((index
    :initarg :index
    :initform '(0 0)
    :accessor widget-index
    :documentation "Min and max selected indices"
    :trait :integer-list))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "SelectionRangeSliderModel"
    :%view-name "SelectionRangeSliderView"))


(defclass selection-slider (label-slider)
  ((index
    :initarg :index
    :initform 0
    :accessor widget-index
    :documentation "Selected index"
    :trait :integer))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "SelectionSliderModel"
    :%view-name "SelectionSliderView"))
