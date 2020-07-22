(in-package #:common-lisp-jupyter)

(defclass cl-installer (jupyter:installer)
  ()
  (:default-initargs
    :class 'kernel
    :language +language+
    :resources
    (mapcar #'asdf:component-pathname
            (asdf:component-children
              (or (asdf:find-component :common-lisp-jupyter (list "res" (format nil "~(~A~)" (uiop:implementation-type))))
                  (asdf:find-component :common-lisp-jupyter '("res" "cl")))))
    :systems '(:common-lisp-jupyter)))

(defclass system-installer (jupyter:system-installer cl-installer)
  ()
  (:documentation "common-lisp-jupyter system installer."))

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
    (list
      (or implementation
          (first (uiop:raw-command-line-arguments))
          (format nil "~(~A~)" (uiop:implementation-type)))
      +eval-flag+ "(ql:quickload :common-lisp-jupyter)"
      +eval-flag+ "(jupyter:run-kernel 'common-lisp-jupyter:kernel #\"{connection_file}\")")))

(defmethod jupyter:command-line ((instance system-installer))
  "Get the command line for a system installation."
  (let ((implementation (jupyter:installer-implementation instance)))
    (list
      (or implementation
          (first (uiop:raw-command-line-arguments))
          (format nil "~(~A~)" (uiop:implementation-type)))
      +load-flag+ (namestring (jupyter:installer-path instance :root :program :bundle))
      +eval-flag+ "(asdf:load-system :common-lisp-jupyter)"
      +eval-flag+ "(jupyter:run-kernel 'common-lisp-jupyter:kernel #\"{connection_file}\")")))

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

(defun install (&key bin-path use-implementation system local prefix)
  "Install Common Lisp kernel based on the current implementation.
- `bin-path` specifies path to LISP binary.
- `use-implementation` toggles including implementation details in kernel name.
- `system` toggles system versus user installation.
- `local` toggles `/usr/local/share versus` `/usr/share` for system installations.
- `prefix` key specifies directory prefix for packaging.
"
  (jupyter:install
    (make-instance
      (if system
        'system-installer
        'user-installer)
      :display-name
        (if use-implementation
          (format nil "~A (~A)" +display-name+ (lisp-implementation-type))
          +display-name+)
      :implementation bin-path
      :local local
      :kernel-name
        (if use-implementation
          (format nil "~A_~(~A~)" +language+ (uiop:implementation-type))
          +language+)
      :prefix prefix)))

(defun install-image (&key use-implementation prefix)
  "Install Common Lisp kernel based on image of current implementation.
- `use-implementation` toggles including implementation details in kernel name.
- `prefix` key specifies directory prefix for packaging."
  (jupyter:install
    (make-instance 'user-image-installer
      :display-name
        (if use-implementation
          (format nil "~A (~A)" +display-name+ (lisp-implementation-type))
          +display-name+)
      :kernel-name
        (if use-implementation
          (format nil "~A_~(~A~)" +language+ (uiop:implementation-type))
          +language+)
      :prefix prefix)))

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
