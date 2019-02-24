(in-package #:jupyter-widgets)

(defclass dom-widget (widget)
  ((%dom-classes
    :initarg :%dom-classes
    :accessor widget-%dom-classes
    :documentation "CSS classes applied to widget DOM element"
    :trait :unicode-list)
   (layout
    :initarg :layout
    :initform (make-widget 'layout)
    :accessor widget-layout
    :documentation "Reference to layout widget."
    :trait :widget))
  (:metaclass trait-metaclass)
  (:default-initargs :%model-name "DOMWidgetModel"))
