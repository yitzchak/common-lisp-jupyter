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
    :%view-module-version +controls-module-version+)
  (:documentation "Link Widget"))

(register-widget link)


(defclass directional-link (link)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "DirectionalLinkModel")
  (:documentation "A directional link"))

(register-widget directional-link)


(defmethod (setf trait) (new-value instance name)
  (dolist (def (closer-mop:class-slots (class-of instance)))
    (when (string= (symbol-name name) (symbol-name (closer-mop:slot-definition-name def)))
      (setf (closer-mop:slot-value-using-class (class-of instance) instance def)
            new-value))))


(defgeneric link (source source-trait target target-trait)
  (:documentation "Create a link between traits in the client if possible")
  (:method (source source-trait target target-trait)
    (observe
      source source-trait
      (lambda (instance type name old-value new-value src)
        (declare (ignore instance type name old-value src))
        (setf (trait target target-trait) new-value)))
    (observe
      target target-trait
      (lambda (instance type name old-value new-value src)
        (declare (ignore instance type name old-value src))
        (setf (trait source source-trait) new-value)))))

(defmethod link ((source widget) source-trait (target widget) target-trait)
  (make-instance 'link
                 :source (list source source-trait)
                 :target (list target target-trait))) 
