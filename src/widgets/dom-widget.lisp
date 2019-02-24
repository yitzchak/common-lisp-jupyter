(in-package #:jupyter-widgets)

(defclass dom-widget (widget)
  ((%dom-classes
    :initarg :%dom-classes
    :accessor widget-%dom-classes
    :documentation "CSS classes applied to widget DOM element"
    :sync t)
   (layout
    :initarg :layout
    :initform (make-widget 'layout)
    :accessor widget-layout
    :documentation "Reference to layout widget."
    :sync t))
  (:metaclass trait-metaclass)
  (:default-initargs :%model-name "DOMWidgetModel"))
