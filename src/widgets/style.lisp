(in-package #:jupyter/widgets)

(defwidget style (widget)
  ()
  (:default-initargs :%model-module +controls-module+
		     :%model-module-version +controls-module-version+
		     :%view-name "StyleView"
		     :%view-module +base-module+
		     :%view-module-version +base-module-version+))

(defwidget background-style (style)
  ((widget-background :initarg :background
		      :initform nil
		      :accessor widget-background
		      :documentation "Background specifications."
		      :trait :string)))

(defwidget base-text-style (style)
  ((font-size :initarg :font-size
	      :initform nil
	      :accessor widget-font-size
	      :documentation "Button text font size."
	      :trait :string)
   (text-color :initarg :text-color
	       :initform nil
	       :accessor widget-text-color
	       :documentation "text color."
	       :trait :string)))

(defwidget weight-text-style (style)
  ((font-weight :initarg :font-weight
		:initform nil
		:accessor widget-font-weight
		:documentation "Button text font weight."
		:trait :string)))

(defwidget extended-text-style (base-text-style weight-text-style)
  ((font-family :initarg :font-family
		:initform nil
		:accessor widget-font-family
		:documentation "Button text font family."
		:trait :string)
   (font-style :initarg :font-style
	       :initform nil
	       :accessor widget-font-style
	       :documentation "Button text font style."
	       :trait :string)
   (font-variant :initarg :font-variant
		 :initform nil
		 :accessor widget-font-variant
		 :documentation "Button text font variant."
		 :trait :string)
   (font-decoration :initarg :font-decoration
		    :initform nil
		    :accessor widget-font-decoration
		    :documentation "Button text font decoration."
		    :trait :string)))

(defwidget button-style (extended-text-style)
  ((button-color :initarg :button-color
		 :initform nil
		 :accessor widget-button-color
		 :documentation "Color of the button"
		 :trait :string))
  (:default-initargs :%model-name "ButtonStyleModel")
  (:documentation "Button style widget"))

(defwidget description-style (style)
  ((description-width
     :initarg :description-width
     :initform nil
     :accessor widget-description-width
     :documentation "Width of the description to the side of the control."
     :trait :string))
  (:default-initargs
    :%model-name "DescriptionStyleModel"))

(defwidget label-style (extended-text-style description-style)
  ()
  (:default-initargs :%model-name "LabelStyleModel")
  (:documentation "Label style widget"))

(defwidget checkbox-style (background-style)
  ()
  (:default-initargs :%model-name "CheckboxStyleModel")
  (:documentation "Button style widget"))

(defwidget text-style (base-text-style description-style background-style)
  ()
  (:default-initargs :%model-name "TextStyleModel")
  (:documentation "Text style widget"))

(defwidget html-style (text-style)
  ()
  (:default-initargs :%model-name "HTMLStyleModel")
  (:documentation "HTML style widget"))

(defwidget html-math-style (text-style)
  ()
  (:default-initargs :%model-name "HTMLMathStyleModel")
  (:documentation "HTML math style widget"))

(defwidget progress-style (description-style)
  ((bar-color
     :initarg :bar-color
     :accessor widget-bar-color
     :documentation "Color of the slider handle."
     :trait :color))
  (:default-initargs
    :%model-name "ProgressStyleModel")
  (:documentation "Progress style widget."))

(defwidget slider-style (description-style)
  ((handle-color
     :initarg :handle-color
     :accessor widget-handle-color
     :documentation "Color of the slider handle."
     :trait :color))
  (:default-initargs
    :%model-name "SliderStyleModel"))

(defwidget toggle-button-style (extended-text-style description-style)
  ()
  (:default-initargs
    :%model-name "ToggleButtonStyleModel")
  (:documentation "Toggle Button style widget."))

(defwidget toggle-buttons-style (weight-text-style description-style)
  ((button-width
     :initarg :button-width
     :initform nil
     :accessor widget-button-width
     :documentation "The width of each button."
     :trait :string))
  (:default-initargs
    :%model-name "ToggleButtonsStyleModel")
  (:documentation "Toggle Buttons style widget."))

(defwidget styled-widget (dom-widget)
  ((style
     :initarg :style
     :accessor widget-style
     :documentation "Reference to style widget."
     :trait :widget)))

(defwidget description-widget (styled-widget)
  ((description
     :initarg :description
     :accessor widget-description
     :documentation "Description of the control."
     :trait :string)
   (description-tooltip
     :initarg :description-tooltip
     :accessor widget-description-tooltip
     :documentation "Tooltip for the description (defaults to description)."
     :trait :string))
  (:default-initargs :style (make-instance 'description-style)))
