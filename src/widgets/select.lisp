(in-package #:jupyter-widgets)


(defclass base-select (description-widget %options-labels-slot disabled-slot)
  ((rows
    :initarg :rows
    :initform 5
    :accessor widget-rows
    :documentation "The number of rows to display."
    :trait :int))
  (:metaclass trait-metaclass))


(defclass select (base-select index-slot)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "SelectModel"
    :%view-name "SelectView")
  (:documentation
    "Listbox that only allows one item to be selected at any given time."))

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
    :%view-name "SelectMultipleView")
  (:documentation
    "Listbox that allows many items to be selected at any given time."))

(register-widget select-multiple)
