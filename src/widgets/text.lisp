(in-package #:jupyter-widgets)


(defclass base-text (description-widget disabled-slot continuous-update-slot)
  ()
  (:metaclass trait-metaclass))


(defclass text (base-text placeholder-slot string-value-slot)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "TextModel"
    :%view-name "TextView")
  (:documentation "Single line textbox widget."))




(defclass text-area (text)
  ((rows
     :initarg :rows
     :initform nil
     :accessor widget-rows
     :documentation "The number of rows to display."
     :trait :int))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "TextareaModel"
    :%view-name "TextareaView")
  (:documentation "Multiline text area widget."))




(defclass password (text)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "PasswordModel"
    :%view-name "PasswordView")
  (:documentation "Single line textbox widget."))




(defclass float-text (base-text float-step-slot float-value-slot)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "FloatTextModel"
    :%view-name "FloatTextView")
  (:documentation
"Displays a float value within a textbox. For a textbox in which the value must
be within a specific range, use BoundedFloatText."))




(defclass bounded-float-text (float-text float-min-max-slots)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "BoundedFloatTextModel"
    :%view-name "FloatTextView")
  (:documentation
"Displays a float value within a textbox. Value must be within the range
specified. For a textbox in which the value doesn't need to be within a specific
range, use float-text."))




(defclass int-text (base-text int-step-slot int-value-slot)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "IntTextModel"
    :%view-name "IntTextView")
  (:documentation "Textbox widget that represents an integer."))




(defclass bounded-int-text (int-text int-min-max-slots)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "BoundedIntTextModel")
  (:documentation
    "Textbox widget that represents an integer bounded from above and below."))


