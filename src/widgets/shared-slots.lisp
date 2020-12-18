(in-package #:jupyter-widgets)

(defun outside-closed-interval (value left right)
  (not (<= left value right)))

(defun outside-left-closed-interval (value left right)
  (or (< value left) (>= value right)))

(defclass %options-labels-slot ()
  ((%options-labels
     :initarg :%options-labels
     :initform nil
     :accessor widget-%options-labels
     :documentation "The labels for the options."
     :trait :unicode-list))
  (:metaclass trait-metaclass))

(defmethod validate-trait ((w %options-labels-slot) (type (eql :int)) name value)
  (if (and (integerp value)
           (equal name 'index)
           (outside-left-closed-interval
             value
              0
              (if (slot-boundp w '%options-labels)
                (length (widget-%options-labels w))
                0)))
    (error 'trait-error :format-control "Invalid selection: index out of bounds")
    (call-next-method)))

(defmethod validate-trait ((w %options-labels-slot) (type (eql :int-list)) name value)
  (if (equal name 'index)
    (let ((len (if (slot-boundp w '%options-labels)
                 (length (widget-%options-labels w))
                 0)))
      (if (and (listp value) (some (lambda (v) (outside-left-closed-interval v 0 len)) value))
        (error 'trait-error :format-control "Invalid selection: index out of bounds")
        (call-next-method)))
    (call-next-method)))


(defclass bool-value-slot ()
  ((value
     :initarg :value
     :initform nil
     :accessor widget-value
     :documentation "Bool value"
     :trait :bool))
  (:metaclass trait-metaclass))


(defclass button-style-slot ()
  ((button-style
     :initarg :button-style
     :initform ""
     :accessor widget-button-style
     :documentation "Use a predefined styling for the button."
     :trait :unicode))
  (:metaclass trait-metaclass))


(defclass byte-value-slot ()
  ((value
     :initarg :value
     :initform nil
     :accessor widget-value
     :documentation "The value as a byte string."
     :trait :buffer))
  (:metaclass trait-metaclass))


(defclass continuous-update-slot ()
  ((continuous-update
     :initarg :continuous-update
     :initform t
     :accessor widget-continuous-update
     :documentation "Update the value of the widget as the user is holding the slider."
     :trait :bool))
  (:metaclass trait-metaclass))


(defclass disabled-slot ()
  ((disabled
     :initarg :disabled
     :initform nil
     :accessor widget-disabled
     :documentation "Enable or disable user changes."
     :trait :bool))
  (:metaclass trait-metaclass))


(defclass float-min-max-slots ()
  ((max
     :initarg :max
     :initform 100d0
     :accessor widget-max
     :documentation "Max value"
     :trait :float)
   (min
     :initarg :min
     :initform 0d0
     :accessor widget-min
     :documentation "Min value"
     :trait :float))
  (:metaclass trait-metaclass))

(defmethod validate-trait ((w float-min-max-slots) (type (eql :float)) name value)
  (if (and (numberp value) (equal name 'value))
    (let* ((min (when (slot-boundp w 'min) (widget-min w)))
           (max (when (slot-boundp w 'max) (widget-max w)))
           (v (if (numberp max) (min max value) value)))
      (call-next-method w type name (if (numberp min) (max min v) v)))
    (call-next-method)))

(defmethod validate-trait ((w float-min-max-slots) (type (eql :float-list)) name value)
  (if (equal name 'value)
    (mapcar (lambda (v) (validate-trait w :float name v)) value)
    (call-next-method)))

(defclass float-step-slot ()
  ((step
     :initarg :step
     :initform 0.1d0
     :accessor widget-step
     :documentation "Minimum step to increment the value"
     :trait :float))
  (:metaclass trait-metaclass))


(defclass float-value-slot ()
  ((value
     :initarg :value
     :initform 0d0
     :accessor widget-value
     :documentation "Float value"
     :trait :float))
  (:metaclass trait-metaclass))


(defclass format-slot ()
  ((format
     :initarg :format
     :accessor widget-format
     :documentation "The format of the media."
     :trait :unicode))
  (:metaclass trait-metaclass))


(defclass height-slot ()
  ((height
     :initarg :height
     :initform ""
     :accessor widget-height
     :documentation "Height of the media in pixels."
     :trait :unicode))
  (:metaclass trait-metaclass))


(defclass icon-slot ()
  ((icon
     :initarg :icon
     :initform ""
     :accessor widget-icon
     :documentation "Font-awesome icon name, without the 'fa-' prefix."
     :trait :unicode))
  (:metaclass trait-metaclass))


(defclass index-slot ()
  ((index
     :initarg :index
     :initform nil
     :accessor widget-index
     :documentation "Selected index"
     :trait :int))
  (:metaclass trait-metaclass))


(defclass int-min-max-slots ()
  ((max
     :initarg :max
     :initform 100
     :accessor widget-max
     :documentation "Max value"
     :trait :int)
   (min
     :initarg :min
     :initform 0
     :accessor widget-min
     :documentation "Min value"
     :trait :int))
  (:metaclass trait-metaclass))

(defmethod validate-trait ((w int-min-max-slots) (type (eql :int)) name value)
  (if (and (numberp value) (equal name 'value))
    (let* ((min (when (slot-boundp w 'min) (widget-min w)))
           (max (when (slot-boundp w 'max) (widget-max w)))
           (v (if (numberp max) (min max value) value)))
      (call-next-method w type name (if (numberp min) (max min v) v)))
    (call-next-method)))

(defmethod validate-trait ((w int-min-max-slots) (type (eql :int-list)) name value)
  (if (equal name 'value)
    (mapcar (lambda (v) (validate-trait w :int name v)) value)
    (call-next-method)))

(defclass int-step-slot ()
  ((step
     :initarg :step
     :initform 1
     :accessor widget-step
     :documentation "Minimum step to increment the value"
     :trait :int))
  (:metaclass trait-metaclass))


(defclass int-value-slot ()
  ((value
     :initarg :value
     :initform 0
     :accessor widget-value
     :documentation "Int value"
     :trait :int))
  (:metaclass trait-metaclass))


(defclass orientation-slot ()
  ((orientation
     :initarg :orientation
     :initform "horizontal"
     :accessor widget-orientation
     :documentation "Vertical or horizontal."
     :trait :unicode))
  (:metaclass trait-metaclass))


(defclass placeholder-slot ()
  ((placeholder
     :initarg :placeholder
     :initform (coerce #-cmucl '(#\U200B) #+cmucl '(#\U+200B) 'string)
     :accessor widget-placeholder
     :documentation "Placeholder text to display when nothing has been typed."
     :trait :unicode))
  (:metaclass trait-metaclass))


(defclass string-value-slot ()
  ((value
     :initarg :value
     :initform ""
     :accessor widget-value
     :documentation "String value"
     :trait :unicode))
  (:metaclass trait-metaclass))


(defclass tooltip-slot ()
  ((tooltip
     :initarg :tooltip
     :initform nil
     :accessor widget-tooltip
     :documentation "Tooltip caption."
     :trait :unicode))
  (:metaclass trait-metaclass))


(defclass width-slot ()
  ((width
     :initarg :width
     :initform ""
     :accessor widget-width
     :documentation "Width of the media in pixels."
     :trait :unicode))
  (:metaclass trait-metaclass))
