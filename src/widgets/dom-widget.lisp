(in-package #:jupyter-widgets)

(defclass dom-widget (widget)
  ((%dom-classes :initarg :%dom-classes
                 :reader dom-widget-%dom-classes
                 :documentation "CSS classes applied to widget DOM element"
                 :sync t)
   (layout :initarg :layout
           :reader dom-widget-%dom-classes
           :sync t))
  (:metaclass trait-metaclass)
  (:default-initargs :%model-name "DOMWidgetModel"))
