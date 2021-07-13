(in-package #:jupyter/widgets)


(defwidget combobox (description-widget continuous-update-slot disabled-slot placeholder-slot
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
     :trait :string-list))
  (:default-initargs
    :%model-name "ComboboxModel"
    :%view-name "ComboboxView"))



