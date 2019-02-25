(in-package #:jupyter-widgets)


(defclass controller-axis (dom-widget)
  ((value
    :initarg :value
    :accessor widget-value
    :documentation "The value of the axis."
    :trait :float))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "ControllerAxisModel"
    :%model-module +controls-module+
    :%model-module-version +controls-module-version+
    :%view-name "ControllerAxisView"
    :%view-module +controls-module+
    :%view-module-version +controls-module-version+))

(register-widget controller-axis)


(defclass controller-button (dom-widget)
  ((value
    :initarg :value
    :accessor widget-value
    :documentation "The value of the button."
    :trait :float)
   (pressed
    :initarg :pressed
    :initform nil
    :accessor widget-pressed
    :documentation "Whether the button is pressed."
    :trait :boolean))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "ControllerButtonModel"
    :%model-module +controls-module+
    :%model-module-version +controls-module-version+
    :%view-name "ControllerButtonView"
    :%view-module +controls-module+
    :%view-module-version +controls-module-version+))

(register-widget controller-button)


(defclass controller (dom-widget)
  ((axes
    :initarg :axes
    :accessor widget-axes
    :documentation "The axes on the gamepad."
    :trait :widget-list)
   (buttons
    :initarg :buttons
    :accessor widget-buttons
    :documentation "The buttons on the gamepad."
    :trait :widget-list)
   (connected
    :initarg :connected
    :initform nil
    :accessor widget-connected
    :documentation "Whether the gamepad is connected."
    :trait :boolean)
   (index
    :initarg :index
    :initform 0
    :accessor widget-index
    :documentation "The id number of the controller."
    :trait :integer)
   (mapping
    :initarg :mapping
    :initform ""
    :accessor widget-mapping
    :documentation "The name of the control mapping."
    :trait :unicode)
   (name
    :initarg :name
    :initform ""
    :accessor widget-name
    :documentation "The name of the controller."
    :trait :unicode)
   (timestamp
    :initarg :timestamp
    :initform 0.0d0
    :accessor widget-timestamp
    :documentation "The last time the data from this gamepad was updated."
    :trait :float))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "ControllerModel"
    :%model-module +controls-module+
    :%model-module-version +controls-module-version+
    :%view-name "ControllerView"
    :%view-module +controls-module+
    :%view-module-version +controls-module-version+))
