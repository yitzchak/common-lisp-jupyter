(in-package #:jupyter-widgets)

(defclass layout (widget)
  ((align-content
     :initarg :align-content
     :initform nil
     :accessor widget-align-content
     :documentation "The align-content CSS attribute."
     :trait :string)
   (align-items
     :initarg :align-items
     :initform nil
     :accessor widget-align-items
     :documentation "The align-items CSS attribute."
     :trait :string)
   (align-self
     :initarg :align-self
     :initform nil
     :accessor widget-align-self
     :documentation "The align-self CSS attribute."
     :trait :string)
   (border
     :initarg :border
     :initform nil
     :accessor widget-border
     :documentation "The border CSS attribute."
     :trait :string)
   (bottom
     :initarg :bottom
     :initform nil
     :accessor widget-bottom
     :documentation "The bottom CSS attribute."
     :trait :string)
   (display
     :initarg :display
     :initform nil
     :accessor widget-display
     :documentation "The display CSS attribute."
     :trait :string)
   (flex
     :initarg :flex
     :initform nil
     :accessor widget-flex
     :documentation "The flex CSS attribute."
     :trait :string)
   (flex-flow
     :initarg :flex-flow
     :initform nil
     :accessor widget-flex-flow
     :documentation "The flex-flow CSS attribute."
     :trait :string)
   (grid-area
     :initarg :grid-area
     :initform nil
     :accessor widget-grid-area
     :documentation "The grid-area CSS attribute."
     :trait :string)
   (grid-auto-columns
     :initarg :grid-auto-columns
     :initform nil
     :accessor widget-grid-auto-columns
     :documentation "The grid-auto-columns CSS attribute."
     :trait :string)
   (grid-auto-flow
     :initarg :grid-auto-flow
     :initform nil
     :accessor widget-grid-auto-flow
     :documentation "The grid-auto-flow CSS attribute."
     :trait :string)
   (grid-auto-rows
     :initarg :grid-auto-rows
     :initform nil
     :accessor widget-grid-auto-rows
     :documentation "The grid-auto-rows CSS attribute."
     :trait :string)
   (grid-column
     :initarg :grid-column
     :initform nil
     :accessor widget-grid-column
     :documentation "The grid-column CSS attribute."
     :trait :string)
   (grid-gap
     :initarg :grid-gap
     :initform nil
     :accessor widget-grid-gap
     :documentation "The grid-gap CSS attribute."
     :trait :string)
   (grid-row
     :initarg :grid-row
     :initform nil
     :accessor widget-grid-row
     :documentation "The grid-row CSS attribute."
     :trait :string)
   (grid-template-areas
     :initarg :grid-template-areas
     :initform nil
     :accessor widget-grid-template-areas
     :documentation "The grid-template-areas CSS attribute."
     :trait :string)
   (grid-template-columns
     :initarg :grid-template-columns
     :initform nil
     :accessor widget-grid-template-columns
     :documentation "The grid-template-columns CSS attribute."
     :trait :string)
   (grid-template-rows
     :initarg :grid-template-rows
     :initform nil
     :accessor widget-grid-template-rows
     :documentation "The grid-template-rows CSS attribute."
     :trait :string)
   (height
     :initarg :height
     :initform nil
     :accessor widget-height
     :documentation "The height CSS attribute."
     :trait :string)
   (justify-content
     :initarg :justify-content
     :initform nil
     :accessor widget-justify-content
     :documentation "The justify-content CSS attribute."
     :trait :string)
   (justify-items
     :initarg :justify-items
     :initform nil
     :accessor widget-justify-items
     :documentation "The justify-items CSS attribute."
     :trait :string)
   (left
     :initarg :left
     :initform nil
     :accessor widget-left
     :documentation "The left CSS attribute."
     :trait :string)
   (margin
     :initarg :margin
     :initform nil
     :accessor widget-margin
     :documentation "The margin CSS attribute."
     :trait :string)
   (max-height
     :initarg :max-height
     :initform nil
     :accessor widget-max-height
     :documentation "The max-height CSS attribute."
     :trait :string)
   (max-width
     :initarg :max-width
     :initform nil
     :accessor widget-max-width
     :documentation "The max-width CSS attribute."
     :trait :string)
   (min-height
     :initarg :min-height
     :initform nil
     :accessor widget-min-height
     :documentation "The min-height CSS attribute."
     :trait :string)
   (min-width
     :initarg :min-width
     :initform nil
     :accessor widget-min-width
     :documentation "The min-width CSS attribute."
     :trait :string)
   (object-fit
     :initarg :object-fit
     :initform nil
     :accessor widget-object-fit
     :documentation "The object-fit CSS attribute."
     :trait :string)
   (object-position
     :initarg :object-position
     :initform nil
     :accessor widget-object-position
     :documentation "The object-position CSS attribute."
     :trait :string)
   (order
     :initarg :order
     :initform nil
     :accessor widget-order
     :documentation "The order CSS attribute."
     :trait :string)
   (overflow
     :initarg :overflow
     :initform nil
     :accessor widget-overflow
     :documentation "The overflow CSS attribute."
     :trait :string)
   (overflow-x
     :initarg :overflow-x
     :initform nil
     :accessor widget-overflow-x
     :documentation "The overflow-x CSS attribute."
     :trait :string)
   (overflow-y
     :initarg :overflow-y
     :initform nil
     :accessor widget-overflow-y
     :documentation "The overflow-y CSS attribute."
     :trait :string)
   (padding
     :initarg :padding
     :initform nil
     :accessor widget-padding
     :documentation "The padding CSS attribute."
     :trait :string)
   (right
     :initarg :right
     :initform nil
     :accessor widget-right
     :documentation "The right CSS attribute."
     :trait :string)
   (top
     :initarg :top
     :initform nil
     :accessor widget-top
     :documentation "The top CSS attribute."
     :trait :string)
   (visibility
     :initarg :visibility
     :initform nil
     :accessor widget-visibility
     :documentation "The visibility CSS attribute."
     :trait :string)
   (width
     :initarg :width
     :initform nil
     :accessor widget-width
     :documentation "The width CSS attribute."
     :trait :string))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "LayoutModel"
    :%model-module +base-module+
    :%model-module-version +base-module-version+
    :%view-name "LayoutView"
    :%view-module +base-module+
    :%view-module-version +base-module-version+)
  (:documentation
"Layout specification

Defines a layout that can be expressed using CSS.  Supports a subset of
https://developer.mozilla.org/en-US/docs/Web/CSS/Reference

When a property is also accessible via a shorthand property, we only
expose the shorthand."))




(defclass dom-widget (widget)
  ((%dom-classes
     :initarg :%dom-classes
     :accessor widget-%dom-classes
     :documentation "CSS classes applied to widget DOM element"
     :trait :string-list)
   (layout
     :initarg :layout
     :initform (make-instance 'layout)
     :accessor widget-layout
     :documentation "Reference to layout widget."
     :trait :widget))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "DOMWidgetModel"
    :%model-module +controls-module+
    :%model-module-version +controls-module-version+
    :%view-name ""
    :%view-module +controls-module+
    :%view-module-version +controls-module-version+)
(:documentation "Base class for all Jupyter widgets which have DOM view."))


(defun focus (widget)
  (send-custom widget '(:object-plist "do" "focus")))


(defun blur (widget)
  (send-custom widget '(:object-plist "do" "blur")))
