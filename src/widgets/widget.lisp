(in-package #:jupyter-widgets)

(defparameter +protocol-version+ "2.0.0")
(defparameter +base-module+ "@jupyter-widgets/base")
(defparameter +base-module-version+ "1.2.0")
(defparameter +controls-module+ "@jupyter-widgets/controls")
(defparameter +controls-module-version+ "1.5.0")
(defparameter +output-module+ "@jupyter-widgets/output")
(defparameter +output-module-version+ "1.0.0")
(defparameter +sidecar-module+ "@jupyter-widgets/jupyterlab-sidecar")
(defparameter +sidecar-module-version+ "1.0.0")

(defparameter +target-name+ "jupyter.widget")

(defvar *state-lock* nil)
(defvar *widgets* (make-hash-table :test 'equal))

(defun extract-major-version (semver)
  (let* ((v (string-left-trim '(#\0) semver)))
    (subseq v 0 (position #\. v :start (if (equal (char v 0) #\.) 1 0)))))

(defun widget-registry-name (model-module model-module-version model-name
                             view-module view-module-version view-name)
  (when (and model-module model-module-version model-name view-module
             view-module-version view-name)
    (format nil "~A+~A+~A+~A+~A+~A"
      model-module
      (extract-major-version model-module-version)
      model-name
      view-module
      (extract-major-version view-module-version)
      view-name)))


(defmacro register-widgets (&rest names)
  (flet ((def-initarg (slot-name initargs)
           ; CMUCL appears to have the default initarg list in a different order.
           (eval (#+cmucl third #-cmucl second (assoc slot-name initargs)))))
    (cons 'progn
    (loop for name in names
          for class = (find-class name)
          for final = (closer-mop:finalize-inheritance class)
          for slots = (remove-if (lambda (definition &aux (initarg (first (closer-mop:slot-definition-initargs definition))))
                                   (or (null initarg)
                                       (char= #\% (char (symbol-name initarg) 0))
                                       (member initarg '(:on-click :sink :id :on-trait-change :kernel :display-data :target-name))))
                                 (closer-mop:class-slots class))
          for keys = (mapcar (lambda (definition &aux (initarg (first (closer-mop:slot-definition-initargs definition))))
                                       (intern (symbol-name initarg) (symbol-package name)))
                             slots)
          for initargs = (closer-mop:class-default-initargs class)
          for widget-name = (widget-registry-name (def-initarg :%model-module initargs)
                                                  (def-initarg :%model-module-version initargs)
                                                  (def-initarg :%model-name initargs)
                                                  (def-initarg :%view-module initargs)
                                                  (def-initarg :%view-module-version initargs)
                                                  (def-initarg :%view-name initargs))
          for make-fun-sym = (alexandria:format-symbol (symbol-package name) "MAKE-~A" name)
          when widget-name do (setf (gethash widget-name *widgets*) name)
          collect `(defun ,make-fun-sym
                          (&rest initargs &key ,@keys &allow-other-keys)
                     (declare (ignore ,@keys))
                     (apply #'make-instance (quote ,name) initargs))
          collect `(export '(,make-fun-sym) ,(symbol-package name))))))


(defclass widget (has-traits jupyter:comm jupyter:result)
  ((%model-name
     :initarg :%model-name
     :reader widget-%module-name
     :documentation "Name of the model."
     :trait :unicode)
   (%model-module
     :initarg :%model-module
     :reader widget-%module-module
     :documentation "The namespace for the model."
     :trait :unicode)
   (%model-module-version
     :initarg :%model-module-version
     :reader widget-%module-module-version
     :documentation "A semver requirement for namespace version containing the model."
     :trait :unicode)
   (%view-name
     :initarg :%view-name
     :reader widget-%view-name
     :documentation "Name of the view."
     :trait :unicode)
   (%view-module
     :initarg :%view-module
     :reader widget-%view-module
     :documentation "The namespace for the view."
     :trait :unicode)
   (%view-module-version
     :initarg :%view-module-version
     :reader widget-%view-module-version
     :documentation "A semver requirement for namespace version containing the view."
     :trait :unicode))
  (:metaclass trait-metaclass)
  (:default-initargs
    :display-data t
    :target-name +target-name+)
  (:documentation "Base class for all Jupyter widgets."))

(defmethod jupyter:render ((w widget))
  `(:object-alist
     ("text/plain" . "A Jupyter Widget")
     ("application/vnd.jupyter.widget-view+json" .
      (:object-alist
        ("version_major" . 2)
        ("version_minor" . 0)
        ("model_id" . ,(jupyter:comm-id w))))))


(defmethod jupyter:mime-bundle-data ((w widget))
  (list :object-plist
        "text/plain" "A Jupyter Widget"
        "application/vnd.jupyter.widget-view+json"
        (list :object-plist
              "version_major" 2
              "version_minor" 0
              "model_id" (jupyter:comm-id w))))


(defun to-json-state (w &optional nm)
  (let (state
        buffer-paths
        buffers)
    (dolist (def (closer-mop:class-slots (class-of w)) (values (cons :object-alist state) buffer-paths buffers))
      (let* ((name (closer-mop:slot-definition-name def))
             (trait-name (trait-name name))
             (type (trait-type def))
             (path-name (symbol-to-snake-case name)))
        (when (and (or (not nm) (eql trait-name nm))
                   (slot-boundp w name)
                   type
                   (not (eql type t)))
          (multiple-value-bind (value sub-buffer-paths sub-buffers)
                               (serialize-trait w type trait-name (slot-value w name))
            (when sub-buffers
              (setf buffer-paths (nconc buffer-paths (mapcar (lambda (buffer-path)
                                                               (cons path-name buffer-path))
                                                             sub-buffer-paths))
                    buffers (nconc buffers sub-buffers)))
            (setf state (acons path-name value state))))))))

(defun inject-buffer (state buffer-path buffer)
  (let ((node (car buffer-path))
        (rest (cdr buffer-path)))
    (if rest
      (inject-buffer (if (stringp node)
                       (gethash node state)
                       (elt state node))
                     rest buffer)
      (if (stringp node)
        (setf (gethash node state) buffer)
        (setf (elt state node) buffer)))))

(defun inject-buffers (state buffer-paths buffers)
  (map nil
       (lambda (buffer-path buffer)
         (inject-buffer state (coerce buffer-path 'list) buffer))
       buffer-paths
       buffers))

(defun send-state (w &optional name)
  (multiple-value-bind (state buffer-paths buffers)
                       (to-json-state w name)
    (jupyter:send-comm-message w
      `(:object-alist ("method" . "update")
                ("state" . ,state)
                ("buffer_paths" . ,(or buffer-paths :empty-array)))
      `(:object-alist ("version" . ,+protocol-version+))
      buffers)))

(defun update-state (w data buffers)
  (let ((*trait-source* nil)
        (state (gethash "state" data (make-hash-table :test #'equal)))
        (buffer-paths (gethash "buffer_paths" data)))
    (inject-buffers state buffer-paths buffers)
    (dolist (def (closer-mop:class-slots (class-of w)))
      (let ((name (closer-mop:slot-definition-name def)))
        (multiple-value-bind (value present-p)
                             (gethash (symbol-to-snake-case name) state)
          (when present-p
            (setf (slot-value w name)
                  (deserialize-trait w (trait-type def) (trait-name name) value))))))))

(defun send-custom (widget content &optional buffers)
  (jupyter:send-comm-message widget
    `(:object-alist ("method" . "custom")
              ("content" . ,content))
    `(:object-alist ("version" . ,+protocol-version+))
    buffers))

(defgeneric on-custom-message (widget content buffers))

(defmethod on-custom-message (widget content buffers))

(defmethod jupyter:on-comm-message ((w widget) data metadata buffers)
  (declare (ignore metadata))
  (alexandria:switch ((gethash "method" data) :test #'equal)
    ("update"
      (update-state w data buffers))
    ("request_state"
      (send-state w))
    ("custom"
      (on-custom-message w (gethash "content" data) buffers))
    (otherwise
      (call-next-method))))

(defmethod on-trait-change :after ((w widget) type name old-value new-value source)
  (when source
    (send-state w name)))

(defmethod initialize-instance :around ((instance widget) &rest rest &key &allow-other-keys)
  (with-trait-silence instance
    (prog1
      (call-next-method)
      (unless (getf rest :create-comm)
        (multiple-value-bind (state buffer-paths buffers)
                             (to-json-state instance)
          (jupyter:send-comm-open instance
            `(:object-alist ("state" . ,state)
                      ("buffer_paths" . ,(or buffer-paths :empty-array)))
            `(:object-alist ("version" . ,+protocol-version+))
            buffers))))))

(defmethod jupyter:create-comm ((target-name (eql :|jupyter.widget|)) id data metadata buffers)
  (let* ((state (gethash "state" data))
         (model-name (gethash "_model_name" state))
         (model-module (gethash "_model_module" state))
         (model-module-version (gethash "_model_module_version" state))
         (view-name (gethash "_view_name" state))
         (view-module (gethash "_view_module" state))
         (view-module-version (gethash "_view_module_version" state))
         (name (widget-registry-name model-module model-module-version
                                     model-name view-module
                                     view-module-version view-name))
         (class (gethash name *widgets*)))
    (when class
      (let ((w (make-instance class :create-comm t)))
        (update-state w data buffers)
        w))))

(defun observe (instance name/s handler)
  (setf (widget-on-trait-change instance)
        (nconc (widget-on-trait-change instance)
               (if (listp name/s)
                 (mapcar (lambda (name) (cons name handler)) name/s)
                 (list (cons name/s handler))))))

(defgeneric %display (widget &rest args &key &allow-other-keys)
  (:documentation "Prepare widget for display")
  (:method (widget &rest args &key &allow-other-keys)
    (declare (ignore args))
    widget))

(defun display (widget &rest args &key &allow-other-keys)
  (jupyter:send-result (apply #'%display widget args))
  (values))
