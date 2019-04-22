(in-package #:jupyter)

(defclass installer ()
  ((class
     :initarg :class
     :accessor installer-class)
   (display-name
     :initarg :display-name
     :accessor installer-display-name)
   (implementation
     :initarg :implementation
     :initform nil
     :accessor installer-implementation)
   (kernel-name
     :initarg :kernel-name
     :accessor installer-kernel-name)
   (language
     :initarg :language
     :accessor installer-language)
   (local
     :initarg :local
     :initform nil
     :accessor installer-local)
   (local-systems
     :initarg :local-systems
     :initform nil
     :accessor installer-local-systems)
   (prefix
     :initarg :prefix
     :initform nil
     :accessor installer-prefix)
   (resources
     :initarg :resources
     :initform nil
     :accessor installer-resources)
   (systems
     :initarg :systems
     :initform nil
     :accessor installer-systems)))

(defclass system-installer (installer)
  ())

(defclass user-installer (installer)
  ())

(defclass user-image-installer (user-installer)
  ())

(defgeneric installer-path-part (instance part))

(defmethod installer-path-part (instance (part (eql :prefix)))
  (with-slots (prefix) instance
    (if prefix
      (merge-pathnames
        (uiop:relativize-pathname-directory (installer-path-part instance :root))
        (truename prefix))
      (installer-path-part instance :root))))

(defmethod installer-path-part ((instance system-installer) (type (eql :root)))
  (if (uiop:os-windows-p)
    (uiop:getenv-absolute-directory "PROGRAMDATA"))
    (make-pathname :directory
      (if (installer-local instance)
        '(:absolute "usr" "local" "share")
        '(:absolute "usr" "share"))))

(defmethod installer-path-part ((instance user-installer) (type (eql :root)))
  (cond
    ((uiop:os-macosx-p)
      (merge-pathnames (make-pathname :directory '(:relative "Library"))
                       (uiop:getenv-pathname "HOME" :ensure-directory t)))
    ; Use %APPDATA% on Windows
    ((uiop:os-windows-p)
      (uiop:get-folder-path :appdata))
    ; Use XDG_DATA_HOME on all other platforms
    (t
      (uiop:xdg-data-home))))

(defmethod installer-path-part (instance (part (eql :kernel)))
  (make-pathname
    :directory
      (list
        :relative
        ; Just in case HFS+ is case-sensitive
        (if (uiop:os-macosx-p) "Jupyter" "jupyter")
        "kernels"
        (installer-kernel-name instance))))

(defmethod installer-path-part (instance (part (eql :program)))
  (make-pathname
    :directory
      (list
        :relative
        (format nil "~A-jupyter" (installer-kernel-name instance)))))

(defmethod installer-path-part (instance (part (eql :spec)))
  (make-pathname :name "kernel" :type "json"))

(defmethod installer-path-part (instance (part (eql :image)))
  (make-pathname :name "image" :type (when (uiop:os-windows-p) "exe")))

(defmethod installer-path-part (instance (part (eql :bundle)))
  (make-pathname :name "bundle" :type "lisp"))

(defmethod installer-path-part (instance (part (eql :local-projects)))
  (make-pathname
    :directory (list :relative "local-projects")))

(defmethod installer-path-part (instance (part string))
  part)

(defmethod installer-path-part (instance (part pathname))
  part)

(defun installer-path (instance &rest parts)
  (reduce
    (lambda (previous part)
      (merge-pathnames
        (installer-path-part instance part)
        previous))
    parts
    :initial-value (make-pathname)))

(defclass system-installer (installer)
  ())

(defgeneric command-line (instance))

(defmethod command-line ((instance user-image-installer))
  (list
    (namestring (installer-path instance :root :program :image))
    "{connection_file}"))

(defgeneric copy-component (component dest))

(defmethod copy-component ((component asdf:file-component) dest)
  (copy-file
    (asdf:component-pathname component)
    (merge-pathnames (asdf:component-relative-pathname component) dest)))

(defmethod copy-component ((component asdf:module) dest)
  (let ((sub-dest (merge-pathnames (make-pathname
                                     :directory (list :relative (asdf:component-name component)))
                                   dest)))
    (ensure-directories-exist sub-dest)
    (dolist (child (asdf:component-children component))
      (copy-component child sub-dest))))

(defmethod copy-component :after ((component asdf:system) dest)
  (let ((source-file (asdf:system-source-file component)))
    (copy-file source-file
      (merge-pathnames
        (file-namestring source-file)
        (merge-pathnames
          (make-pathname
            :directory (list :relative (asdf:component-name component)))
          dest)))))

(defun install-local-systems (instance)
  (let ((dest (installer-path instance :prefix :program :local-projects)))
    (format t "Installing local systems to ~A~%" dest)
    (dolist (system-sym (installer-local-systems instance))
      (when-let ((system (asdf:find-system system-sym)))
        (copy-component system dest)))))

(defun install-bundle (instance)
  (let ((dest (installer-path instance :prefix :program)))
    (format t "Installing Quicklisp bundle to ~A~%" dest)
    (funcall (fdefinition (find-symbol "BUNDLE-SYSTEMS" 'ql))
      (installer-systems instance) :to dest)))

(defun install-directories (instance)
  (format t "Creating directories.~%")
  (when-let ((prefix (installer-prefix instance)))
    (ensure-directories-exist prefix))
  (ensure-directories-exist (installer-path instance :prefix :kernel))
  (ensure-directories-exist (installer-path instance :prefix :program)))

(defun install-spec (instance)
  (with-slots (display-name language) instance
    (let ((spec-path (installer-path instance :prefix :kernel :spec)))
      (format t "Installing kernel spec file ~A~%" spec-path)
      (with-open-file (stream spec-path :direction :output :if-exists :supersede)
        (write-string
          (jsown:to-json
            (jsown:new-js
              ("argv" (command-line instance))
              ("display_name" display-name)
              ("language" language)))
          stream)))))

(defun install-resources (instance)
  (format t "Installing kernel resources to ~A.~%" (installer-path instance :prefix :kernel))
  (dolist (src (installer-resources instance))
    (copy-file src (installer-path instance :prefix :kernel (file-namestring src)))))

(defgeneric install (instance))

(defmethod install :before ((instance installer))
  (install-directories instance)
  (install-spec instance)
  (install-resources instance))

(defmethod install ((instance user-image-installer))
  (let ((prefixed-image-path (installer-path instance :prefix :program :image))
        (class (installer-class instance)))
    (setq uiop:*image-entry-point*
      `(lambda ()
        (run-kernel
          (find-symbol ,(symbol-name class) ,(symbol-package class))
          (first (uiop:command-line-arguments)))))
    (format t "Creating kernel image ~A~%" prefixed-image-path)
    (uiop:dump-image prefixed-image-path :executable t)))

(defmethod install ((instance user-installer))
  (install-local-systems instance))

(defmethod install ((instance system-installer))
  (install-bundle instance)
  (install-local-systems instance))
