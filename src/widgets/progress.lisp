(in-package #:jupyter-widgets)


(defclass base-progress (description-widget)
  ((bar-style
    :initarg :bar-style
    :initform ""
    :accessor widget-bar-style
    :documentation "Use a predefined styling for the progess bar."
    :trait :bool)
   (orientation
    :initarg :orientation
    :initform "horizontal"
    :accessor widget-orientation
    :documentation "Vertical or horizontal."
    :trait :unicode))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%view-name "ProgressView"
    :style (make-widget 'progress-style)))


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
