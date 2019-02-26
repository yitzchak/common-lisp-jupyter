(in-package #:jupyter-widgets)


(defclass base-text (description-widget disabled-slot continuous-update-slot)
  ()
  (:metaclass trait-metaclass))


(defclass text (base-text placeholder-slot string-value-slot)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "TextModel"
    :%view-name "TextView"))

(register-widget text)


(defclass text-area (text)
  ((rows
    :initarg :rows
    :initform nil
    :accessor widget-rows
    :documentation "The number of rows to display."
    :trait :int))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "TextareaModel"
    :%view-name "TextareaView"))

(register-widget text-area)


(defclass password (text)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "PasswordModel"
    :%view-name "PasswordView"))

(register-widget password)


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

(register-widget float-text)


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

(register-widget bounded-float-text)


(defclass int-text (base-text)
  ((step
    :initarg :step
    :initform nil
    :accessor widget-step
    :documentation "Minimum step to increment the value"
    :trait :int)
   (value
    :initarg :value
    :initform 0.0d0
    :accessor widget-value
    :documentation "Int value"
    :trait :int))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "IntTextModel"
    :%view-name "IntTextView"))

(register-widget int-text)


(defclass bounded-int-text (int-text)
  ((max
    :initarg :max
    :initform 100.0d0
    :accessor widget-max
    :documentation "Max value"
    :trait :int)
   (min
    :initarg :min
    :initform 0.0d0
    :accessor widget-min
    :documentation "Min value"
    :trait :int))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "BoundedIntTextModel"
    :%view-name "BoundedIntTextView"))

(register-widget bounded-int-text)
