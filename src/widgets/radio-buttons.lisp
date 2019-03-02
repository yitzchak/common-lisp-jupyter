(in-package #:jupyter-widgets)


(defclass radio-buttons (description-widget %options-labels-slot disabled-slot
                         index-slot)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "RadioButtonsModel"
    :%view-name "RadioButtonsView")
  (:documentation
"Group of radio buttons that represent an enumeration. Only one radio button can
be toggled at any point in time."))

(register-widget radio-buttons)


(defclass dropdown (radio-buttons)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "DropdownModel"
    :%view-name "DropdownView")
  (:documentation "Allows you to select a single item from a dropdown."))

(register-widget dropdown)
