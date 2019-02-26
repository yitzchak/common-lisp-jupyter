(in-package #:jupyter-widgets)


(defclass base-text (description-widget disabled-slot continuous-update-slot)
  ()
  (:metaclass trait-metaclass))


(defclass label (description-widget placeholder-slot string-value-slot)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "LabelModel"
    :%view-name "LabelView"))

(register-widget label)


(defclass html (label)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "HTMLModel"
    :%view-name "HTMLView"))

(register-widget html)


(defclass html-math (label)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "HTMLMathModel"
    :%view-name "HTMLMathView"))

(register-widget html-math)
