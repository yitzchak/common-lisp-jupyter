(in-package #:jupyter-widgets)


(defclass base-select (description-widget)
  ((%options-labels
    :initarg :%options-labels
    :initform nil
    :accessor widget-%options-labels
    :documentation "The labels for the options."
    :trait :unicode-list)
   (disabled
    :initarg :disabled
    :initform nil
    :accessor widget-disabled
    :documentation "Enable or disable user changes."
    :trait :bool)
   (rows
    :initarg :rows
    :initform 5
    :accessor widget-rows
    :documentation "The number of rows to display."
    :trait :int))
  (:metaclass trait-metaclass))


(defclass select (base-select)
  ((index
    :initarg :index
    :initform nil
    :accessor widget-index
    :documentation "Selected index"
    :trait :int))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "SelectModel"
    :%view-name "SelectView"))

(register-widget select)


(defclass select-multiple (base-select)
  ((index
    :initarg :index
    :initform nil
    :accessor widget-index
    :documentation "Selected indicies"
    :trait :int-list))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "SelectMultipleModel"
    :%view-name "SelectMultipleView"))

(register-widget select-multiple)
