(in-package #:jupyter-widgets)

(defvar *trait-silence* nil)
(defvar *trait-hold* nil)
(defvar *trait-notifications* nil)

(defgeneric on-trait-change (object name type old-value new-value))

(defmethod on-trait-change (object name type old-value new-value))

(defmacro with-trait-silence (&body body)
  `(let ((*trait-silence* t)) ,@body))

(defmacro with-trait-hold (&body body)
  `(let* ((*trait-hold* t)
          (*trait-notifications* nil)
          (result (progn ,@body)))
    (mapcar #'on-trait-change (reverse *trait-notifications*))
    result))

(defgeneric validate-trait (object type name value))

(defmethod validate-trait (object type name value)
  value)

(defgeneric serialize-trait (object type name value))

(defmethod serialize-trait (object type name value)
  value)

(defgeneric deserialize-trait (object type name value))

(defmethod deserialize-trait (object type name value)
  value)

(defclass trait (closer-mop:slot-definition)
  ((trait :initarg :trait
          :initform nil
          :accessor trait-type)))

(defclass direct-trait
  (trait closer-mop:standard-direct-slot-definition)
  ())

(defclass effective-trait
  (trait closer-mop:standard-effective-slot-definition)
  ())

(defclass trait-metaclass (standard-class)
  ())

(defmethod closer-mop:validate-superclass
           ((class trait-metaclass) (super standard-class))
  t)

(defmethod closer-mop:effective-slot-definition-class
           ((class trait-metaclass) &rest initargs)
  (declare (ignore initargs))
  (find-class 'effective-trait))

(defmethod closer-mop:compute-effective-slot-definition
           :around ((class trait-metaclass) name direct-slot-definitions)
  (declare (ignore name))
  (let ((result (call-next-method)))
    (setf (trait-type result)
      (some #'trait-type direct-slot-definitions))
    result))

(defmethod trait-type ((slot closer-mop:direct-slot-definition)))

(defmethod closer-mop:direct-slot-definition-class
           ((class trait-metaclass) &rest initargs)
  (declare (ignore initargs))
  (find-class 'direct-trait))

; (defmethod (setf closer-mop:slot-value-using-class)
;            (value (class trait-metaclass) object (slot effective-trait))
;   (call-next-method
;     (reduce (lambda (value validator)
;               (funcall validator object value))
;       (trait-validators slot)
;       :initial-value value)
;     class object slot))

(defmethod (setf closer-mop:slot-value-using-class)
           :around (value (mc trait-metaclass) object (slot effective-trait))
  (if (and (not *trait-silence*) (trait-type slot))
    (let* ((name (closer-mop:slot-definition-name slot))
           (type (trait-type slot))
           (old-value (if (slot-boundp object name) (slot-value object name) :unbound))
           (new-value (call-next-method)))
      (when (not (equal old-value new-value))
        (if *trait-hold*
          (push (list object name type old-value new-value) *trait-notifications*)
          (on-trait-change object name type old-value new-value)))
      new-value)
    (call-next-method)))
