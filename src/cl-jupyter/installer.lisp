(in-package #:jupyter/common-lisp)

(defclass cl-installer (jupyter:installer)
  ((load-system :accessor installer-load-system
                :initarg :load-system
                :type (or null string)))
  (:default-initargs
    :class 'kernel
    :language +language+
    :debugger t
    :resources
    (mapcar #'asdf:component-pathname
            (asdf:component-children
              (or (asdf:find-component :common-lisp-jupyter (list "res" (format nil "~(~A~)" (uiop:implementation-type))))
                  (asdf:find-component :common-lisp-jupyter '("res" "cl")))))
    :systems '(:common-lisp-jupyter)))


(defclass system-installer (jupyter:system-installer cl-installer)
  ()
  (:documentation "common-lisp-jupyter system installer."))


(defclass system-bundle-installer (jupyter:system-bundle-installer cl-installer)
  ()
  (:documentation "common-lisp-jupyter system bundle installer."))


(defclass user-installer (jupyter:user-installer cl-installer)
  ()
  (:documentation "common-lisp-jupyter user installer."))


(defclass user-image-installer (jupyter:user-image-installer cl-installer)
  ()
  (:documentation "common-lisp-jupyter user image installer."))


(defclass user-roswell-installer (jupyter:user-installer cl-installer)
  ()
  (:documentation "common-lisp-jupyter roswell installer."))


(defmethod jupyter:command-line ((instance user-installer))
  "Get the command line for a user installation."
  (let ((implementation (jupyter:installer-implementation instance)))
    (append (list (or implementation
                       (first (uiop:raw-command-line-arguments))
                       (format nil "~(~A~)" (uiop:implementation-type))))
            (when (installer-load-system instance)
              (list +eval-flag+ (installer-load-system instance)))
            (list +eval-flag+ "(jupyter:run-kernel 'jupyter/common-lisp:kernel)")
            (when +user-options+
              (list +user-options+))
            (list "{connection_file}"))))


(defmethod jupyter:command-line ((instance system-installer))
  "Get the command line for a user installation."
  (let ((implementation (jupyter:installer-implementation instance)))
    (append (list (or implementation
                      (first (uiop:raw-command-line-arguments))
                      (format nil "~(~A~)" (uiop:implementation-type))))
            (when (installer-load-system instance)
              (list +eval-flag+ (installer-load-system instance)))
            (list +eval-flag+ "(jupyter:run-kernel 'jupyter/common-lisp:kernel)")
            (when +user-options+
              (list +user-options+))
            (list "{connection_file}"))))


(defmethod jupyter:command-line ((instance system-bundle-installer))
  "Get the command line for a system bundle installation."
  (let ((implementation (jupyter:installer-implementation instance)))
    (append (list (or implementation
                      (first (uiop:raw-command-line-arguments))
                      (format nil "~(~A~)" (uiop:implementation-type)))
                  +load-flag+ (namestring (jupyter:installer-path instance :root :program :bundle))
                  +eval-flag+ (if (find-package :quicklisp)
                                "(ql:quickload :common-lisp-jupyter)"
                                "(asdf:load-system :common-lisp-jupyter)"))
            (list +eval-flag+ "(jupyter:run-kernel 'jupyter/common-lisp:kernel)")
            (when +user-options+
              (list +user-options+))
            (list "{connection_file}"))))


(defmethod jupyter:command-line ((instance user-roswell-installer))
  "Get the command line for a roswell installation."
  (let ((implementation (jupyter:installer-implementation instance)))
    (append
      (if (or implementation (uiop:os-windows-p))
        '("ros")
        '("cl-jupyter"))
      (when implementation
        (list "--lisp" implementation))
      (when (or implementation (uiop:os-windows-p))
        (list (namestring
                (merge-pathnames
                  (make-pathname :directory '(:relative ".roswell" "bin")
                                 :name "cl-jupyter")
                  (if (uiop:os-windows-p) ; Get the home from %USERPROFILE% if on Windows to avoid MSYS home
                    (uiop:getenv-absolute-directory "USERPROFILE")
                    (truename (user-homedir-pathname)))))))
      '("{connection_file}"))))


(defun install (&key bin-path implementation system bundle local prefix root
                     (load-system (if (find-package :quicklisp)
                                      "(ql:quickload :common-lisp-jupyter)"
                                      "(asdf:load-system :common-lisp-jupyter)")))
  "Install Common Lisp kernel based on the current implementation.
- `bin-path` specifies path to LISP binary.
- `implementation` toggles including implementation details in kernel name.
- `system` toggles system versus user installation.
- `bundle` creates a quicklisp bundle for system installations.
- `local` toggles `/usr/local/share versus` `/usr/share` for system installations.
- `prefix` key specifies directory prefix for packaging.
- `root` key specifies the root under which the Jupyter folder is found. Is automatically determined if not provided."
  (jupyter:install
    (make-instance
      (cond
        ((and system bundle)
          'system-bundle-installer)
        (system
          'system-installer)
        (t
          'user-installer))
      :display-name
        (if implementation
          (format nil "~A (~A)" +display-name+ (if (stringp implementation)
                                                   implementation
                                                   (lisp-implementation-type)))
          +display-name+)
      :implementation bin-path
      :local local
      :load-system load-system
      :kernel-name
        (if implementation
          (format nil "~A_~(~A~)" +language+ (if (stringp implementation)
                                                   implementation
                                                   (lisp-implementation-type)))
          +language+)
      :prefix prefix
      :root root)))


(defun install-image (&key implementation prefix root)
  "Install Common Lisp kernel based on image of current implementation.
- `implementation` toggles including implementation details in kernel name.
- `prefix` key specifies directory prefix for packaging.
- `root` key specifies the root under which the Jupyter folder is found. Is automatically determined if not provided."
  (jupyter:install
    (make-instance 'user-image-installer
      :display-name
        (if implementation
          (format nil "~A (~A)" +display-name+ (if (stringp implementation)
                                                   implementation
                                                   (lisp-implementation-type)))
          +display-name+)
      :kernel-name
        (if implementation
          (format nil "~A_~(~A~)" +language+ (if (stringp implementation)
                                                   implementation
                                                   (lisp-implementation-type)))
          +language+)
      :prefix prefix
      :root root)))


(defun install-roswell (&key implementation)
  "Install Common Lisp kernel using Roswell. `implementation` key toggles
including implementation details in kernel name."
  (jupyter:install
    (make-instance 'user-roswell-installer
      :display-name
        (if implementation
          (format nil "~A (~A)" +display-name+ implementation)
          +display-name+)
      :implementation implementation
      :kernel-name
        (if implementation
          (format nil "~A_~A" +language+ (substitute #\_ #\/ implementation))
          +language+))))
