(in-package #:jupyter-widgets)


(defclass base-slider (description-widget disabled-slot orientation-slot
                       continuous-update-slot)
  ((readout
     :initarg :readout
     :initform t
     :accessor widget-readout
     :documentation "Display the current value of the slider next to it."
     :trait :bool)
   (readout-format
     :initarg :readout-format
     :accessor widget-readout-format
     :documentation "Format for the readout"
     :trait :unicode))
  (:metaclass trait-metaclass)
  (:default-initargs
    :style (make-instance 'slider-style)))


(defclass number-slider (base-slider)
  ((readout-format
     :initarg :readout-format
     :accessor widget-readout-format
     :documentation "Format for the readout"
     :trait :unicode))
  (:metaclass trait-metaclass))


(defclass float-log-slider (number-slider float-min-max-slots float-step-slot
                            float-value-slot)
  ((base
     :initarg :base
     :initform 10.0d0
     :accessor widget-base
     :documentation "Base for the logarithm"
     :trait :float))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "FloatLogSliderModel"
    :%view-name "FloatLogSliderView"
    :readout-format ".3g"
    :max 4.0d0)
  (:documentation
    "Slider/trackbar of logarithmic floating values with the specified range."))

(register-widget float-log-slider)


(defclass float-range-slider (number-slider float-min-max-slots float-step-slot)
  ((value
     :initarg :value
     :initform '(0.0d0 1.0d0)
     :accessor widget-value
     :documentation "Float range"
     :trait :float-list))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "FloatRangeSliderModel"
    :%view-name "FloatRangeSliderView"
    :readout-format ".2f")
  (:documentation
"Slider/trackbar that represents a pair of floats bounded by minimum and maximum
value."))

(register-widget float-range-slider)


(defclass float-slider (number-slider float-min-max-slots float-step-slot
                        float-value-slot)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "FloatSliderModel"
    :%view-name "FloatSliderView"
    :readout-format ".2f")
  (:documentation "Slider/trackbar of floating values with the specified range."))

(register-widget float-slider)


(defclass int-range-slider (number-slider int-min-max-slots int-step-slot)
  ((value
     :initarg :value
     :initform '(0 1)
     :accessor widget-value
     :documentation "Int range value"
     :trait :int-list))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "IntRangeSliderModel"
    :%view-name "IntRangeSliderView"
    :readout-format "d")
  (:documentation
"Slider/trackbar that represents a pair of ints bounded by minimum and maximum
value."))

(register-widget int-range-slider)


(defclass int-slider (number-slider int-min-max-slots int-step-slot
                      int-value-slot)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "IntSliderModel"
    :%view-name "IntSliderView"
    :readout-format "d")
  (:documentation
    "Slider widget that represents an integer bounded from above and below."))

(register-widget int-slider)


(defclass label-slider (base-slider %options-labels-slot)
  ()
  (:metaclass trait-metaclass))


(defclass selection-range-slider (label-slider)
  ((index
     :initarg :index
     :initform '(0 0)
     :accessor widget-index
     :documentation "Min and max selected indices"
     :trait :int-list))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "SelectionRangeSliderModel"
    :%view-name "SelectionRangeSliderView")
  (:documentation "Slider to select multiple contiguous items from a list."))

(register-widget selection-range-slider)


(defclass selection-slider (label-slider index-slot)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "SelectionSliderModel"
    :%view-name "SelectionSliderView")
  (:documentation "Slider to select a single item from a list or dictionary."))

(register-widget selection-slider)
