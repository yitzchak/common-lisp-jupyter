(in-package #:jupyter/widgets)

(defwidget base-text (description-widget disabled-slot continuous-update-slot)
  ())

(defwidget text (base-text placeholder-slot string-value-slot)
  ()
  (:default-initargs :%model-name "TextModel"
		     :%view-name "TextView"
		     :style (make-instance 'text-style))
  (:documentation "Single line textbox widget."))

(defwidget text-area (text)
  ((rows :initarg :rows
	 :initform nil
	 :accessor widget-rows
	 :documentation "The number of rows to display."
	 :trait :int))
  (:default-initargs :%model-name "TextareaModel"
		     :%view-name "TextareaView")
  (:documentation "Multiline text area widget."))

(defwidget password (text)
  ()
  (:default-initargs :%model-name "PasswordModel"
		     :%view-name "PasswordView")
  (:documentation "Single line textbox widget."))

(defwidget float-text (base-text float-step-slot float-value-slot)
  ()
  (:default-initargs :%model-name "FloatTextModel"
		     :%view-name "FloatTextView")
  (:documentation "Displays a float value within a textbox. For a textbox in which the value must
be within a specific range, use BoundedFloatText."))

(defwidget bounded-float-text (float-text float-min-max-slots)
  ()
  (:default-initargs :%model-name "BoundedFloatTextModel"
		     :%view-name "FloatTextView")
  (:documentation
"Displays a float value within a textbox. Value must be within the range
specified. For a textbox in which the value doesn't need to be within a specific
range, use float-text."))

(defwidget int-text (base-text int-step-slot int-value-slot)
  ()
  (:default-initargs :%model-name "IntTextModel"
		     :%view-name "IntTextView")
  (:documentation "Textbox widget that represents an integer."))

(defwidget bounded-int-text (int-text int-min-max-slots)
  ()
  (:default-initargs :%model-name "BoundedIntTextModel")
  (:documentation
    "Textbox widget that represents an integer bounded from above and below."))
