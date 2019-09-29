(in-package #:jupyter-widgets)


(defclass combobox (description-widget continuous-update-slot disabled-slot placeholder-slot
                    string-value-slot)
  ((ensure-option
     :initarg :ensure-option
     :initform t
     :accessor widget-ensure-option
     :documentation "If set, ensure value is in options. Implies continuous_update=False."
     :trait :bool)
   (options
     :initarg :options
     :initform nil
     :accessor widget-options
     :documentation "Dropdown options for the combobox"
     :trait :unicode-list))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "ComboboxModel"
    :%view-name "ComboboxView"))

(register-widget combobox)

