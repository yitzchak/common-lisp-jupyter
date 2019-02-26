(in-package #:jupyter-widgets)

(defclass toggle-button (description-widget button-style-slot disabled-slot
                         icon-slot tooltip-slot bool-value-slot)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "ToggleButtonModel"
    :%view-name "ToggleButtonView"))

(register-widget toggle-button)


(defclass toggle-buttons (description-widget %options-labels-slot
                          button-style-slot disabled-slot index-slot)
  ((icons
    :initarg :icons
    :initform ""
    :accessor widget-icons
    :documentation "Icons names for each button (FontAwesome names without the fa- prefix)."
    :trait :unicode-list)
   (tooltips
    :initarg :tooltips
    :accessor widget-tooltips
    :documentation "Tooltips for each button."
    :trait :unicode-list))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "ToggleButtonsModel"
    :%view-name "ToggleButtonsView"))

(register-widget toggle-buttons)
