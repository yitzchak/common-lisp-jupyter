(in-package #:jupyter-widgets)


(defclass controller-axis (dom-widget float-value-slot)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "ControllerAxisModel"
    :%view-name "ControllerAxisView")
  (:documentation "Represents a gamepad or joystick axis."))




(defclass controller-button (dom-widget float-value-slot)
  ((pressed
     :initarg :pressed
     :initform nil
     :accessor widget-pressed
     :documentation "Whether the button is pressed."
     :trait :bool))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "ControllerButtonModel"
    :%view-name "ControllerButtonView")
  (:documentation "Represents a gamepad or joystick button."))




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
     :trait :bool)
   (index
     :initarg :index
     :initform 0
     :accessor widget-index
     :documentation "The id number of the controller."
     :trait :int)
   (mapping
     :initarg :mapping
     :initform ""
     :accessor widget-mapping
     :documentation "The name of the control mapping."
     :trait :string)
   (name
     :initarg :name
     :initform ""
     :accessor widget-name
     :documentation "The name of the controller."
     :trait :string)
   (timestamp
     :initarg :timestamp
     :initform 0.0d0
     :accessor widget-timestamp
     :documentation "The last time the data from this gamepad was updated."
     :trait :float))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "ControllerModel"
    :%view-name "ControllerView")
  (:documentation "Represents a game controller."))


