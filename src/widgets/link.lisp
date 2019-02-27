(in-package #:jupyter-widgets)


(defclass link (widget)
  ((source
    :initarg :source
    :initform nil
    :accessor widget-source
    :trait :link)
  (target
    :initarg :target
    :initform nil
    :accessor widget-target
    :trait :link))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "LinkModel"
    :%model-module +controls-module+
    :%model-module-version +controls-module-version+
    :%view-name ""
    :%view-module +controls-module+
    :%view-module-version +controls-module-version+))

(register-widget link)


(defclass directional-link (link)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "DirectionalLinkModel"))

(register-widget directional-link)
