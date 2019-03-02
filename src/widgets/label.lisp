(in-package #:jupyter-widgets)


(defclass base-text (description-widget disabled-slot continuous-update-slot)
  ()
  (:metaclass trait-metaclass))


(defclass label (description-widget placeholder-slot string-value-slot)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "LabelModel"
    :%view-name "LabelView")
  (:documentation "Label widget.

It also renders math inside the string `value` as Latex (requires $ $ or
$$ $$ and similar latex tags)."))

(register-widget label)


(defclass html (label)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "HTMLModel"
    :%view-name "HTMLView")
  (:documentation "Renders the string `value` as HTML."))

(register-widget html)


(defclass html-math (label)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "HTMLMathModel"
    :%view-name "HTMLMathView")
  (:documentation "Renders the string `value` as HTML, and render mathematics."))

(register-widget html-math)
