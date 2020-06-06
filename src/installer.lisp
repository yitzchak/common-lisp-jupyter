(in-package #:jupyter)

(defclass installer ()
  ((class
     :initarg :class
     :accessor installer-class
     :documentation "Class that implements the kernel. Used by image based installations.")
   (display-name
     :initarg :display-name
     :accessor installer-display-name
     :documentation "Name of the kernel displayed to the user.")
   (implementation
     :initarg :implementation
     :initform nil
     :accessor installer-implementation
     :documentation "Path to specific binary used by the kernel.")
   (kernel-name
     :initarg :kernel-name
     :accessor installer-kernel-name
     :documentation "Name of the kernel.")
   (language
     :initarg :language
     :accessor installer-language
     :documentation "Language that the kernel supports.")
   (local
     :initarg :local
     :initform nil
     :accessor installer-local
     :documentation "Is the installation a local or packaged installation?")
   (local-systems
     :initarg :local-systems
     :initform nil
     :accessor installer-local-systems
     :documentation "List of systems to package into local-projects.")
   (prefix
     :initarg :prefix
     :initform nil
     :accessor installer-prefix
     :documentation "Directory to put installed files into. Used by packaging system, should be nil otherwise.")
   (resources
     :initarg :resources
     :initform nil
     :accessor installer-resources
     :documentation "List of paths of resource files such as icons.")
   (systems
     :initarg :systems
     :initform nil
     :accessor installer-systems
     :documentation "List of systems to bundle for system installs."))
  (:documentation "Base installer class."))

(defclass system-installer (installer)
  ()
  (:documentation "System installer class."))

(defclass user-installer (installer)
  ()
  (:documentation "User installer class."))

(defclass user-image-installer (user-installer)
  ()
  (:documentation "User image installer class."))

(defgeneric installer-path-part (instance part)
  (:documentation "Get a specific part of an installer path. part is a keyword symbol specifying which part."))

(defmethod installer-path-part (instance (part (eql :prefix)))
  "Get the directory prefix if it exists."
  (with-slots (prefix) instance
    (if prefix
      (merge-pathnames
        (uiop:relativize-pathname-directory (installer-path-part instance :root))
        (truename prefix))
      (installer-path-part instance :root))))

(defmethod installer-path-part ((instance system-installer) (type (eql :root)))
  "Get the root directory for a system installation."
  (if (uiop:os-windows-p)
    ; Use %PROGRAMDATA% on Windows
    (uiop:getenv-absolute-directory "PROGRAMDATA"))
    ; Otherwise use either /usr/local/share/ or /usr/share/
    (make-pathname :directory
      (if (installer-local instance)
        '(:absolute "usr" "local" "share")
        '(:absolute "usr" "share"))))

(defmethod installer-path-part ((instance user-installer) (type (eql :root)))
  "Get the root directory for a user installation"
  (cond
    ; use $HOME/Library/ on Mac
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
  "Get the kernel directory."
  (make-pathname
    :directory
      (list
        :relative
        ; Just in case HFS+ is case-sensitive
        (if (uiop:os-macosx-p) "Jupyter" "jupyter")
        "kernels"
        (installer-kernel-name instance))))

(defmethod installer-path-part (instance (part (eql :program)))
  "Get the program directory."
  (make-pathname
    :directory
      (list
        :relative
        (format nil "~A-jupyter" (installer-kernel-name instance)))))

(defmethod installer-path-part (instance (part (eql :spec)))
  "Get the kernel spec file name."
  (make-pathname :name "kernel" :type "json"))

(defmethod installer-path-part (instance (part (eql :image)))
  "Get the image file name."
  (make-pathname :name "image" :type (when (uiop:os-windows-p) "exe")))

(defmethod installer-path-part (instance (part (eql :bundle)))
  "Get the Quicklisp bundle file name."
  (make-pathname :name "bundle" :type "lisp"))

(defmethod installer-path-part (instance (part (eql :local-projects)))
  "Get the local-projects directory."
  (make-pathname
    :directory (list :relative "local-projects")))

(defmethod installer-path-part (instance (part string))
  "If the part is a string then just return it."
  part)

(defmethod installer-path-part (instance (part pathname))
  "If the part is already a pathname then just return it."
  part)

(defun installer-path (instance &rest parts)
  "Resolve each of the path parts then combine all into a single path using merge-pathnames."
  (reduce
    (lambda (previous part)
      (merge-pathnames
        (installer-path-part instance part)
        previous))
    parts
    :initial-value (make-pathname)))

(defgeneric command-line (instance)
  (:documentation "Get the command line for an installer instance."))

(defmethod command-line ((instance user-image-installer))
  "Get the command for a user image installer."
  (list
    (namestring (installer-path instance :root :program :image))
    "{connection_file}"))

(defgeneric copy-component (component dest)
  (:documentation "Copy a specific ASDF component to the destination."))

(defmethod copy-component ((component asdf:file-component) dest)
  "Copy a file component."
  (copy-file
    (asdf:component-pathname component)
    (merge-pathnames (asdf:component-relative-pathname component) dest)))

(defmethod copy-component ((component asdf:module) dest)
  "Copy a module."
  (let ((sub-dest (merge-pathnames (make-pathname
                                     :directory (list :relative (asdf:component-name component)))
                                   dest)))
    (ensure-directories-exist sub-dest)
    (dolist (child (asdf:component-children component))
      (copy-component child sub-dest))))

(defmethod copy-component :after ((component asdf:system) dest)
  "After the contents of a system are copied (via asdf:module) then copy the asd file."
  (let ((source-file (asdf:system-source-file component)))
    (copy-file source-file
      (merge-pathnames
        (file-namestring source-file)
        (merge-pathnames
          (make-pathname
            :directory (list :relative (asdf:component-name component)))
          dest)))))

(defun install-local-systems (instance)
  "Install the local systems into local-projects."
  (let ((dest (installer-path instance :prefix :program :local-projects)))
    (format t "Installing local systems to ~A~%" dest)
    (dolist (system-sym (installer-local-systems instance))
      (when-let ((system (asdf:find-system system-sym)))
        (copy-component system dest)))))

(defun install-bundle (instance)
  "Install the Quicklisp bundle."
  (let ((dest (installer-path instance :prefix :program)))
    (format t "Installing Quicklisp bundle to ~A~%" dest)
    (funcall (fdefinition (find-symbol "BUNDLE-SYSTEMS" 'ql))
      (installer-systems instance) :to dest)))

(defun install-directories (instance)
  "Create all needed directories."
  (format t "Creating directories.~%")
  (when-let ((prefix (installer-prefix instance)))
    (ensure-directories-exist prefix))
  (ensure-directories-exist (installer-path instance :prefix :kernel))
  (ensure-directories-exist (installer-path instance :prefix :program)))

(defun install-spec (instance)
  "Install the kernel spec file."
  (with-slots (display-name language) instance
    (let ((spec-path (installer-path instance :prefix :kernel :spec)))
      (format t "Installing kernel spec file ~A~%" spec-path)
      (with-open-file (stream spec-path :direction :output :if-exists :supersede)
        (write-string
          (jsown:to-json
            (json-new-obj
              ("argv" (command-line instance))
              ("display_name" display-name)
              ("language" language)))
          stream)))))

(defun install-resources (instance)
  "Install all kernel resources."
  (format t "Installing kernel resources to ~A.~%" (installer-path instance :prefix :kernel))
  (dolist (src (installer-resources instance))
    (copy-file src (installer-path instance :prefix :kernel (file-namestring src)))))

(defgeneric install (instance)
  (:documentation "Install a kernel based on an installer instance."))

(defmethod install :before ((instance installer))
  "Do common installation tasks before the specific ones association with this instance."
  (install-directories instance)
  (install-spec instance)
  (install-resources instance))

(defmethod install ((instance user-image-installer))
  "Create an image for the user image based kernels."
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
  "Install user kernel."
  (install-local-systems instance))

(defmethod install ((instance system-installer))
  "Install system kernel."
  (install-bundle instance)
  (install-local-systems instance))
