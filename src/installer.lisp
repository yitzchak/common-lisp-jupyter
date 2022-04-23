(in-package #:jupyter)

(defclass installer ()
  ((class
     :initarg :class
     :accessor installer-class
     :documentation "Class that implements the kernel. Used by image based installations.")
   (debugger
     :initarg :debugger
     :initform nil
     :accessor installer-debugger
     :documentation "Whether or not the kernel supports debugging.")
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
   (jupyter-path
     :initarg :jupyter-path
     :initform nil
     :accessor installer-jupyter-path
     :documentation "The Jupyter directory. If nil then it will be determined automatically.")
   (program-path
     :initarg :program-path
     :initform nil
     :accessor installer-program-path
     :documentation "The program directory. If nil then it will be determined automatically.")
   (systems
     :initarg :systems
     :initform nil
     :accessor installer-systems
     :documentation "List of systems to bundle for system installs."))
  (:documentation "Base installer class."))

(defclass system-installer (installer)
  ()
  (:documentation "System installer class."))

(defclass system-bundle-installer (system-installer)
  ()
  (:documentation "System bundle installer class."))

(defclass user-installer (installer)
  ()
  (:documentation "User installer class."))

(defclass user-image-installer (user-installer)
  ()
  (:documentation "User image installer class."))

(defgeneric installer-path (instance name))

(defmethod installer-path (instance (name (eql :spec)))
  (merge-pathnames (make-pathname :name "kernel" :type "json")
                   (installer-path instance :kernel)))

(defmethod installer-path (instance (name (eql :kernel)))
  (merge-pathnames (make-pathname :directory (list :relative
                                                   "kernels"
                                                   (installer-kernel-name instance)))
                   (installer-path instance :jupyter)))

(defun add-prefix (instance path)
  (if (installer-prefix instance)
      (merge-pathnames (uiop:relativize-pathname-directory path)
                       (truename (installer-prefix instance)))
      path))

(defmethod installer-path ((instance system-installer) (name (eql :root)))
  #+windows
    (uiop:getenv-absolute-directory "PROGRAMDATA")
  #-windows
    (make-pathname :directory (if (installer-local instance)
                                  '(:absolute "usr" "local" "share")
                                  '(:absolute "usr" "share"))))

(defmethod installer-path ((instance user-installer) (name (eql :root)))
  #+darwin
    (merge-pathnames (make-pathname :directory '(:relative "Library"))
                     (uiop:getenv-pathname "HOME" :ensure-directory t))
  #+windows
    (uiop:get-folder-path :appdata)
  #-(or darwin windows)
    (uiop:xdg-data-home))

(defmethod installer-path ((instance system-installer) (name (eql :jupyter)))
  (add-prefix instance
              (or (installer-jupyter-path instance)
                  (merge-pathnames (make-pathname :directory '(:relative "jupyter"))
                                   (installer-path instance :root)))))

(defmethod installer-path ((instance user-installer) (name (eql :jupyter)))
  (add-prefix instance
              (or (installer-jupyter-path instance)
                  (merge-pathnames (make-pathname :directory '(:relative #+darwin "Jupyter"
                                                                         #-darwin "jupyter"))
                                   (installer-path instance :root)))))

(defmethod installer-path (instance (name (eql :program)))
  (add-prefix instance
              (or (installer-program-path instance)
                  (merge-pathnames (make-pathname :directory (list :relative
                                                             (format nil "~A-jupyter"
                                                                     (installer-kernel-name instance))))
                                   (installer-path instance :root)))))

(defmethod installer-path (instance (name (eql :image)))
  (merge-pathnames (make-pathname :name "image" :type #+windows "exe" #-windows :unspecific)
                   (installer-path instance :program)))

(defmethod installer-path (instance (name (eql :bundle)))
  (merge-pathnames (make-pathname :name "bundle" :type "lisp")
                   (installer-path instance :program)))

(defmethod installer-path (instance (name (eql :local-projects)))
  (merge-pathnames (make-pathname :directory (list :relative "local-projects"))
                   (installer-path instance :program)))

(defmethod installer-path (instance (name pathname))
  name)

(defgeneric command-line (instance)
  (:documentation "Get the command line for an installer instance."))

(defmethod command-line ((instance user-image-installer))
  "Get the command for a user image installer."
  (list (namestring (installer-path instance :image))
        "{connection_file}"))

(defgeneric copy-component (component dest)
  (:documentation "Copy a specific ASDF component to the destination."))

(defmethod copy-component ((component asdf:file-component) dest)
  "Copy a file component."
  (alexandria:copy-file
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
    (alexandria:copy-file source-file
      (merge-pathnames
        (file-namestring source-file)
        (merge-pathnames
          (make-pathname
            :directory (list :relative (asdf:component-name component)))
          dest)))))

(defun install-local-systems (instance)
  "Install the local systems into local-projects."
  (when (installer-local-systems instance)
    (let ((dest (installer-path instance :local-projects)))
      (format t "Installing local systems to ~A~%" dest)
      (ensure-directories-exist dest)
      (dolist (system-sym (installer-local-systems instance))
        (alexandria:when-let ((system (asdf:find-system system-sym)))
          (copy-component system dest))))))

(defun install-bundle (instance)
  "Install the Quicklisp bundle."
  (let ((dest (installer-path instance :program)))
    (format t "Installing Quicklisp bundle to ~A~%" dest)
    (ensure-directories-exist dest)
    (funcall (fdefinition (find-symbol "BUNDLE-SYSTEMS" 'ql))
      (installer-systems instance) :to dest)))

(defun install-directories (instance)
  "Create all needed directories."
  (format t "Creating directories.~%")
  (alexandria:when-let ((installer-prefix (installer-prefix instance)))
    (ensure-directories-exist installer-prefix))
  (ensure-directories-exist (installer-path instance :kernel)))

(defun install-spec (instance)
  "Install the kernel spec file."
  (with-slots (display-name language) instance
    (let ((spec-path (installer-path instance :spec)))
      (format t "Installing kernel spec file ~A~%" spec-path)
      (with-open-file (stream spec-path :direction :output :if-exists :supersede)
        (shasht:write-json
          (list :object-plist
            "argv" (command-line instance)
            "display_name" display-name
            "language" language
            "interrupt_mode" "message"
            "metadata" (if (installer-debugger instance)
                         '(:object-plist "debugger" :true)
                         :empty-object))
          stream)))))

(defun install-resources (instance &aux (kernel-path (installer-path instance :kernel)))
  "Install all kernel resources."
  (format t "Installing kernel resources to ~A.~%" kernel-path)
  (dolist (src (installer-resources instance))
    (alexandria:copy-file src (merge-pathnames (file-namestring src)
                                               kernel-path))))

(defgeneric install (instance)
  (:documentation "Install a kernel based on an installer instance."))

(defmethod install :before ((instance installer))
  "Do common installation tasks before the specific ones association with this instance."
  (install-directories instance)
  (install-spec instance)
  (install-resources instance))

(defmethod install ((instance user-image-installer))
  "Create an image for the user image based kernels."
  (let ((prefixed-image-path (installer-path instance :image))
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
  (install-local-systems instance))

(defmethod install ((instance system-bundle-installer))
  "Install system bundle kernel."
  (install-bundle instance)
  (install-local-systems instance))  
