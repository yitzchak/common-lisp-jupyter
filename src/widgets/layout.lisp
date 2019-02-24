(in-package #:jupyter-widgets)

(defclass layout (widget)
  ((align-content
    :initarg :align-content
    :initform nil
    :accessor widget-align-content
    :documentation "The align-content CSS attribute."
    :sync t)
   (align-items
    :initarg :align-items
    :initform nil
    :accessor widget-align-items
    :documentation "The align-items CSS attribute."
    :sync t)
   (align-self
    :initarg :align-self
    :initform nil
    :accessor widget-align-self
    :documentation "The align-self CSS attribute."
    :sync t)
   (border
    :initarg :border
    :initform nil
    :accessor widget-border
    :documentation "The border CSS attribute."
    :sync t)
   (bottom
    :initarg :bottom
    :initform nil
    :accessor widget-bottom
    :documentation "The bottom CSS attribute."
    :sync t)
   (display
    :initarg :display
    :initform nil
    :accessor widget-display
    :documentation "The display CSS attribute."
    :sync t)
   (flex
    :initarg :flex
    :initform nil
    :accessor widget-flex
    :documentation "The flex CSS attribute."
    :sync t)
   (flex-flow
    :initarg :flex-flow
    :initform nil
    :accessor widget-flex-flow
    :documentation "The flex-flow CSS attribute."
    :sync t)
   (grid-area
    :initarg :grid-area
    :initform nil
    :accessor widget-grid-area
    :documentation "The grid-area CSS attribute."
    :sync t)
   (grid-auto-columns
    :initarg :grid-auto-columns
    :initform nil
    :accessor widget-grid-auto-columns
    :documentation "The grid-auto-columns CSS attribute."
    :sync t)
   (grid-auto-flow
    :initarg :grid-auto-flow
    :initform nil
    :accessor widget-grid-auto-flow
    :documentation "The grid-auto-flow CSS attribute."
    :sync t)
   (grid-column
    :initarg :grid-column
    :initform nil
    :accessor widget-grid-column
    :documentation "The grid-column CSS attribute."
    :sync t)
   (grid-gap
    :initarg :grid-gap
    :initform nil
    :accessor widget-grid-gap
    :documentation "The grid-gap CSS attribute."
    :sync t)
   (grid-template-areas
    :initarg :grid-template-areas
    :initform nil
    :accessor widget-grid-template-areas
    :documentation "The grid-template-areas CSS attribute."
    :sync t)
   (grid-template-columns
    :initarg :grid-template-columns
    :initform nil
    :accessor widget-grid-template-columns
    :documentation "The grid-template-columns CSS attribute."
    :sync t)
   (grid-template-rows
    :initarg :grid-template-rows
    :initform nil
    :accessor widget-grid-template-rows
    :documentation "The grid-template-rows CSS attribute."
    :sync t)
   (height
    :initarg :height
    :initform nil
    :accessor widget-height
    :documentation "The height CSS attribute."
    :sync t)
   (justify-content
    :initarg :justify-content
    :initform nil
    :accessor widget-justify-content
    :documentation "The justify-content CSS attribute."
    :sync t)
   (left
    :initarg :left
    :initform nil
    :accessor widget-left
    :documentation "The left CSS attribute."
    :sync t)
   (margin
    :initarg :margin
    :initform nil
    :accessor widget-margin
    :documentation "The margin CSS attribute."
    :sync t)
   (max-height
    :initarg :max-height
    :initform nil
    :accessor widget-max-height
    :documentation "The max-height CSS attribute."
    :sync t)
   (max-width
    :initarg :max-width
    :initform nil
    :accessor widget-max-width
    :documentation "The max-width CSS attribute."
    :sync t)
   (min-height
    :initarg :min-height
    :initform nil
    :accessor widget-min-height
    :documentation "The min-height CSS attribute."
    :sync t)
   (min-width
    :initarg :min-width
    :initform nil
    :accessor widget-min-width
    :documentation "The min-width CSS attribute."
    :sync t)
   (order
    :initarg :order
    :initform nil
    :accessor widget-order
    :documentation "The order CSS attribute."
    :sync t)
   (overflow
    :initarg :overflow
    :initform nil
    :accessor widget-overflow
    :documentation "The overflow CSS attribute."
    :sync t)
   (overflow-x
    :initarg :overflow-x
    :initform nil
    :accessor widget-overflow-x
    :documentation "The overflow-x CSS attribute."
    :sync t)
   (overflow-y
    :initarg :overflow-y
    :initform nil
    :accessor widget-overflow-y
    :documentation "The overflow-y CSS attribute."
    :sync t)
   (padding
    :initarg :padding
    :initform nil
    :accessor widget-padding
    :documentation "The padding CSS attribute."
    :sync t)
   (right
    :initarg :right
    :initform nil
    :accessor widget-right
    :documentation "The right CSS attribute."
    :sync t)
   (top
    :initarg :top
    :initform nil
    :accessor widget-top
    :documentation "The top CSS attribute."
    :sync t)
   (visibility
    :initarg :visibility
    :initform nil
    :accessor widget-visibility
    :documentation "The visibility CSS attribute."
    :sync t)
   (width
    :initarg :width
    :initform nil
    :accessor widget-width
    :documentation "The width CSS attribute."
    :sync t))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "LayoutModel"
    :%model-module +base-module+
    :%model-module-version +base-module-version+
    :%view-name "LayoutView"
    :%view-module +base-module+
    :%view-module-version +base-module-version+))
