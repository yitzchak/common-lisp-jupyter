(in-package #:jupyter-widgets)


(defclass radio-buttons (description-widget)
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
   (index
    :initarg :index
    :initform nil
    :accessor widget-index
    :documentation "Selected index"
    :trait :int))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "RadioButtonsModel"
    :%view-name "RadioButtonsView"))


(register-widget radio-buttons)


(defclass dropdown (radio-buttons)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "DropdownModel"
    :%view-name "DropdownView"))

(register-widget dropdown)
