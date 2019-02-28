(in-package #:jupyter-widgets)


(defclass base-text (description-widget disabled-slot continuous-update-slot)
  ()
  (:metaclass trait-metaclass))


(defclass text (base-text placeholder-slot string-value-slot)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "TextModel"
    :%view-name "TextView"))

(register-widget text)


(defclass text-area (text)
  ((rows
    :initarg :rows
    :initform nil
    :accessor widget-rows
    :documentation "The number of rows to display."
    :trait :int))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "TextareaModel"
    :%view-name "TextareaView"))

(register-widget text-area)


(defclass password (text)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "PasswordModel"
    :%view-name "PasswordView"))

(register-widget password)


(defclass float-text (base-text float-step-slot float-value-slot)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "FloatTextModel"
    :%view-name "FloatTextView"))

(register-widget float-text)


(defclass bounded-float-text (float-text float-min-max-slots)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "BoundedFloatTextModel"
    :%view-name "BoundedFloatTextView"))

(register-widget bounded-float-text)


(defclass int-text (base-text int-step-slot int-value-slot)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "IntTextModel"
    :%view-name "IntTextView"))

(register-widget int-text)


(defclass bounded-int-text (int-text int-min-max-slots)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "BoundedIntTextModel"
    :%view-name "BoundedIntTextView"))

(register-widget bounded-int-text)

(defmethod validate-trait ((w bounded-int-text) (type (eql :int)) name value)
  (cond
    ((equal name 'value)
      (if (slot-boundp w 'min)
        (let ((min (widget-min w)))
          (if min
            (max min value)
            value))
        value))))
