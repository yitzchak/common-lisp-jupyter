(in-package #:jupyter-widgets)

(defclass int-slider (dom-widget)
  ((continuous-update
    :initarg :continuous-update
    :initform t
    :accessor widget-continuous-update
    :documentation "Update the value of the widget as the user is holding the slider."
    :sync t)
   (description
    :initarg :description
    :initform ""
    :accessor widget-description
    :documentation "Description of the control."
    :sync t)
   (description-tooltip
    :initarg :description-tooltip
    :accessor widget-description-tooltip
    :documentation "Tooltip for the description (defaults to description)."
    :sync t)
   (disabled
    :initarg :disabled
    :initform :false
    :accessor widget-disabled
    :documentation "Enable or disable user changes."
    :sync t)
   (max
    :initarg :max
    :initform 100
    :accessor widget-max
    :documentation "Max value"
    :sync t)
   (min
    :initarg :min
    :initform 0
    :accessor widget-min
    :documentation "Min value"
    :sync t)
   (orientation
    :initarg :orientation
    :initform "horizontal"
    :accessor widget-orientation
    :documentation "Vertical or horizontal."
    :sync t)
   (readout
    :initarg :readout
    :initform t
    :accessor widget-readout
    :documentation "Display the current value of the slider next to it."
    :sync t)
   (readout-format
    :initarg :readout-format
    :initform "d"
    :accessor widget-readout-format
    :documentation "Format for the readout"
    :sync t)
   (step
    :initarg :step
    :initform 1
    :accessor widget-step
    :documentation "Minimum step to increment the value"
    :sync t)
   (value
    :initarg :value
    :initform 0
    :accessor widget-value
    :documentation "Int value"
    :sync t))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "IntSliderModel"
    :%model-module +model-module+
    :%model-module-version +model-module-version+
    :%view-name "IntSliderView"
    :%view-module +view-module+
    :%view-module-version +view-module-version+))

(defun make-int-slider ()
  (with-trait-silence
    (let* ((inst (make-instance 'int-slider))
           (state (to-json-state inst))
           (data (jsown:new-js
                  ("state" state)
                  ("buffer_paths" nil))))
      (jupyter:send-comm-open inst data
        (jsown:new-js ("version" +protocol-version+)))
      inst)))
