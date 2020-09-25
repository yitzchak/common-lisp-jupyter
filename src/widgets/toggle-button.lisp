(in-package #:jupyter-widgets)

(defclass toggle-button (description-widget button-style-slot disabled-slot
                         icon-slot tooltip-slot bool-value-slot)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "ToggleButtonModel"
    :%view-name "ToggleButtonView")
  (:documentation "Displays a boolean `value` in the form of a toggle button."))

(register-widget toggle-button)


(defclass toggle-buttons (description-widget %options-labels-slot
                          button-style-slot disabled-slot index-slot)
  ((icons
     :initarg :icons
     :initform ""
     :accessor widget-icons
     :documentation "Icons names for each button (FontAwesome names without the fa- prefix)."
     :trait :unicode-list)
   (options
     :accessor widget-options
     :initarg :options
     :documentation "The option values that correspond to the labels")
   (tooltips
     :initarg :tooltips
     :accessor widget-tooltips
     :documentation "Tooltips for each button."
     :trait :unicode-list))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "ToggleButtonsModel"
    :%view-name "ToggleButtonsView")
  (:documentation
"Group of toggle buttons that represent an enumeration. Only one toggle button
can be toggled at any point in time."))

(register-widget toggle-buttons)

(defmethod widget-value ((instance toggle-buttons))
  (select-value instance (widget-index instance)))

(defmethod (setf widget-value) (new-value (instance toggle-buttons))
  (setf (widget-index instance)
        (position new-value
                  (if (slot-boundp instance 'options)
                    (widget-options instance)
                    (widget-%options-labels instance))
                  :test #'equal)))

(defmethod on-trait-change :after ((instance toggle-buttons) type (name (eql :index)) old-value new-value source)
  (jupyter::enqueue *trait-notifications*
    (list instance :any :value (select-value instance old-value) (select-value instance new-value) source)))


(defmethod initialize-instance :after ((instance toggle-buttons) &rest initargs &key &allow-other-keys)
  (let ((value (getf initargs :value)))
    (when value
      (setf (widget-value instance) value))))

