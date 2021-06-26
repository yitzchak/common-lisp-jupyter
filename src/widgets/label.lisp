(in-package #:jupyter-widgets)


(defwidget base-text (description-widget disabled-slot continuous-update-slot)
  ())


(defwidget label (description-widget placeholder-slot string-value-slot)
  ()
  (:default-initargs
    :%model-name "LabelModel"
    :%view-name "LabelView")
  (:documentation "Label widget.

It also renders math inside the string `value` as Latex (requires $ $ or
$$ $$ and similar latex tags)."))


(defwidget html (label)
  ()
  (:default-initargs
    :%model-name "HTMLModel"
    :%view-name "HTMLView")
  (:documentation "Renders the string `value` as HTML."))


(defwidget html-math (label)
  ()
  (:default-initargs
    :%model-name "HTMLMathModel"
    :%view-name "HTMLMathView")
  (:documentation "Renders the string `value` as HTML, and render mathematics."))


