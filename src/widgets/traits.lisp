(in-package #:jupyter-widgets)

(defvar *trait-silence* nil)
(defvar *trait-source* t)
(defvar *trait-notifications* (make-instance 'jupyter::queue))

(defgeneric on-trait-change (object type name old-value new-value source))

(defmethod on-trait-change (object type name old-value new-value source))

(defmacro with-trait-silence (instance &body body)
  `(let ((*trait-silence* (cons ,instance *trait-silence*)))
     ,@body))

(defgeneric validate-trait (object type name value))

(defmethod validate-trait (object type name value)
  value)

(defgeneric serialize-trait (object type name value))

(defmethod serialize-trait (object type name value)
  (values value nil nil))

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


(defun notify-trait-change (object type name old-value new-value &optional (source *trait-source*))
  (jupyter::enqueue *trait-notifications*
    (list object type name old-value new-value source)))


(defmethod (setf closer-mop:slot-value-using-class)
           :around (value (mc trait-metaclass) object (slot effective-trait))
  (let ((type (trait-type slot)))
    (if (and type
             (not (position object *trait-silence*)))
      (let* ((name (closer-mop:slot-definition-name slot))
             (trait-name (trait-name name))
             (old-value (if (slot-boundp object name) (slot-value object name) :unbound))
             (new-value (call-next-method)))
        (when (not (equal old-value new-value))
          (jupyter::enqueue *trait-notifications*
            (list object type trait-name old-value new-value *trait-source*)))
        (let ((*trait-source* t))
          (do ()
              ((jupyter::queue-empty-p *trait-notifications*))
            (apply #'on-trait-change (jupyter::dequeue *trait-notifications*))))
        new-value)
      (call-next-method))))

(defclass has-traits ()
  ((on-trait-change
     :initarg :on-trait-change
     :initform nil
     :accessor widget-on-trait-change
     :documentation "Instance specific trait notification"))
  (:metaclass trait-metaclass))

(defmethod on-trait-change :after ((w has-traits) type name old-value new-value source)
  (dolist (pair (widget-on-trait-change w))
          ()
    (when (eql (car pair) name)
      (funcall (cdr pair) w type name old-value new-value source))))

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

(defun binary-value-p (value)
  (and (vectorp value)
       (position (array-element-type value)
                 '((unsigned-byte 8))
                   ;single-float)
                 :test #'equal)))

