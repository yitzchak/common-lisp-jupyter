(in-package #:jupyter-widgets)

(defclass toggle-button (description-widget)
  ((button-style
    :initarg :button-style
    :initform ""
    :accessor widget-button-style
    :documentation "Use a predefined styling for the button."
    :trait :unicode)
   (disabled
    :initarg :disabled
    :initform nil
    :accessor widget-disabled
    :documentation "Enable or disable user changes."
    :trait :bool)
   (icon
    :initarg :icon
    :initform ""
    :accessor widget-icon
    :documentation "Font-awesome icon name, without the 'fa-' prefix."
    :trait :unicode)
   (tooltip
    :initarg :tooltip
    :accessor widget-tooltip
    :documentation "Tooltip caption of the button."
    :trait :unicode)
   (value
    :initarg :value
    :initform nil
    :accessor widget-value
    :documentation "Bool value"
    :trait :bool))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "ToggleButtonModel"
    :%view-name "ToggleButtonView"))

(register-widget toggle-button)


(defclass toggle-buttons (description-widget)
  ((%options-labels
    :initarg :%options-labels
    :initform nil
    :accessor widget-%options-labels
    :documentation "The labels for the options."
    :trait :unicode-list)
   (button-style
    :initarg :button-style
    :initform ""
    :accessor widget-button-style
    :documentation "Use a predefined styling for the button."
    :trait :unicode)
   (disabled
    :initarg :disabled
    :initform nil
    :accessor widget-disabled
    :documentation "Enable or disable user changes."
    :trait :bool)
   (icons
    :initarg :icons
    :initform ""
    :accessor widget-icons
    :documentation "Icons names for each button (FontAwesome names without the fa- prefix)."
    :trait :unicode-list)
   (index
    :initarg :index
    :initform nil
    :accessor widget-index
    :documentation "Selected index"
    :trait :int)
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
