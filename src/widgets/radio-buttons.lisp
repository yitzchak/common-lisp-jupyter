(in-package #:jupyter-widgets)


(defclass radio-buttons (description-widget %options-labels-slot disabled-slot
                         index-slot)
  ()
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
