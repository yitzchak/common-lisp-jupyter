(in-package #:jupyter-widgets)

(defvar *trait-silence* nil)
(defvar *trait-hold* nil)
(defvar *trait-notifications* nil)

(defgeneric on-trait-change (object type name old-value new-value))

(defmethod on-trait-change (object type name old-value new-value))

(defmacro with-trait-silence (&body body)
  `(let ((*trait-silence* t)) ,@body))

(defmacro with-trait-hold (&body body)
  `(let* ((*trait-hold* t)
          (*trait-notifications* nil)
          (result (progn ,@body)))
    (mapcar (lambda (args) (apply #'on-trait-change args)) (reverse *trait-notifications*))
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

(define-condition trait-error (simple-error)
  ())

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

(defgeneric trait-name (value))

(defmethod trait-name ((slot effective-trait))
  (intern (symbol-name (closer-mop:slot-definition-name slot)) "KEYWORD"))

(defmethod trait-name ((value symbol))
  (intern (symbol-name value) "KEYWORD"))

(defmethod (setf closer-mop:slot-value-using-class)
           (value (class trait-metaclass) object (slot effective-trait))
  (call-next-method
    (let ((type (trait-type slot)))
      (if type
        (validate-trait object type (trait-name slot) value)
        value))
    class object slot))

(defmethod (setf closer-mop:slot-value-using-class)
           :around (value (mc trait-metaclass) object (slot effective-trait))
  (let ((type (trait-type slot)))
    (if (and (not *trait-silence*) type)
      (let* ((name (closer-mop:slot-definition-name slot))
             (trait-name (trait-name name))
             (old-value (if (slot-boundp object name) (slot-value object name) :unbound))
             (new-value (call-next-method)))
        (when (not (equal old-value new-value))
          (if *trait-hold*
            (push (list object type trait-name old-value new-value) *trait-notifications*)
            (on-trait-change object type trait-name old-value new-value)))
        new-value)
      (call-next-method))))

(defclass has-traits ()
  ((on-trait-change
     :initarg :on-trait-change
     :initform nil
     :accessor widget-on-trait-change
     :documentation "Instance specific trait notification"))
  (:metaclass trait-metaclass))

(defun symbol-to-camel-case (s)
  (do ((name (symbol-name s))
       (position 0 (1+ position))
       (result "")
       capitalize)
      ((= position (length name)) result)
    (cond
      ((char= (char name position) #\-)
        (setq capitalize t))
      (capitalize
        (setq result (concatenate 'string result (string (char-upcase (char name position)))))
        (setq capitalize nil))
      (t
        (setq result (concatenate 'string result (string (char-downcase (char name position)))))))))

(defun camel-case-to-symbol (name)
  (intern
    (do ((position 0 (1+ position))
         (result ""))
        ((= position (length name)) result)
      (when (and (not (zerop position))
                 (upper-case-p (char name position)))
        (setq result (concatenate 'string result "-")))
        (setq result (concatenate 'string result (string (char-upcase (char name position))))))
    "KEYWORD"))

(defun symbol-to-snake-case (s)
  (substitute #\_ #\%
    (substitute #\_ #\-
      (string-downcase (symbol-name s)))))

(defun snake-case-to-symbol (k)
  (intern
    (string-upcase
      (substitute #\- #\_
                  (if (and (not (zerop (length k)))
                                (char= (char k 0) #\_))
                    (substitute #\% #\_ k :count 1)
                    k)))
    "KEYWORD"))


