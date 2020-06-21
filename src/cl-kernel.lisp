(in-package #:common-lisp-jupyter)

(defvar +display-name+ "Common Lisp")
(defvar +language+ "common-lisp")
(defvar +eval-flag+
  #+clisp "-x" #+(or mkcl cmucl) "-eval" #-(or clisp cmucl mkcl) "--eval")
(defvar +load-flag+
  #+clisp "-i" #+(or mkcl cmucl) "-load" #-(or clisp cmucl mkcl) "--load")

(defclass kernel (jupyter:kernel)
  ()
  (:default-initargs
    :name "common-lisp"
    :package (find-package :common-lisp-user)
    :version "0.1"
    :banner "common-lisp-jupyter: a Common Lisp Jupyter kernel
(C) 2019 Tarn Burton (MIT)"
    :language-name "common-lisp"
    :language-version (uiop:lisp-version-string)
    :mime-type "text/x-common-lisp"
    :file-extension ".lisp"
    :pygments-lexer "common-lisp"
    :codemirror-mode "text/x-common-lisp"
    :help-links '(("Common Lisp Documentation" . "https://common-lisp.net/documentation")
                  ("Common Lisp HyperSpec" . "http://www.lispworks.com/documentation/HyperSpec/Front/index.htm"))))

(defmethod jupyter:code-is-complete ((k kernel) code)
  (handler-case
    (iter
      (for sexpr in-stream (make-string-input-stream code)))
    (end-of-file () "incomplete")
    (serious-condition () "invalid")
    (condition () "invalid")
    (:no-error (val)
      (declare (ignore val))
      "complete")))

(defun my-read (&optional input-stream (eof-error-p t) eof-value recursive-p)
  (jupyter:handling-errors
    (read input-stream eof-error-p eof-value recursive-p)))

(defun my-eval (expr)
  (jupyter:debugging-errors
  (setq common-lisp-user::- expr)
  (let ((evaluated-expr (multiple-value-list (eval expr))))
    (setq common-lisp-user::*** common-lisp-user::**
          common-lisp-user::** common-lisp-user::*
          common-lisp-user::* (car evaluated-expr)
          common-lisp-user::/// common-lisp-user:://
          common-lisp-user::// common-lisp-user::/
          common-lisp-user::/ evaluated-expr
          common-lisp-user::+++ common-lisp-user::++
          common-lisp-user::++ common-lisp-user::+
          common-lisp-user::+ expr)
    (remove nil (mapcar #'jupyter:make-lisp-result evaluated-expr)))))

(defmethod jupyter:evaluate-code ((k kernel) code)
  (iter
    (for sexpr in-stream (make-string-input-stream code) using #'my-read)
    (when (typep sexpr 'jupyter:result)
      (collect sexpr)
      (finish))
    (for result next (my-eval sexpr))
    (if (listp result)
      (appending result)
      (collect result))
    (until (jupyter:quit-eval-error-p result))))

(defun symbol-char-p (c)
  (and (characterp c)
       (or (alphanumericp c)
           (member c '(#\+ #\- #\< #\> #\/ #\* #\& #\= #\. #\? #\_ #\! #\$ #\%
                       #\: #\@ #\[ #\] #\^ #\{ #\} #\~ #\# #\|)))))

(defun symbol-string-at-position (value pos)
  (let ((start-pos (if (symbol-char-p (char value pos)) pos (if (zerop pos) 0 (1- pos)))))
    (if (symbol-char-p (char value start-pos))
      (let ((start (1+ (or (position-if-not #'symbol-char-p value :end start-pos :from-end t) -1)))
            (end (or (position-if-not #'symbol-char-p value :start start-pos) (length value))))
        (values (subseq value start end) start end))
      (values nil nil nil))))

(defclass inspect-result (jupyter:result)
  ((symbol :initarg :symbol
           :reader inspect-result-symbol)
   (status :initarg :status
           :reader inspect-result-status)))

(defmethod jupyter:render ((res inspect-result))
  (jupyter:json-new-obj
    ("text/plain"
      (with-output-to-string (stream)
        (describe (inspect-result-symbol res) stream)))))

(defun normalize-symbol-case (name)
  (case (readtable-case *readtable*)
    (:upcase (string-upcase name))
    (:downcase (string-downcase name))
    (:invert
      (cond
        ((every #'upper-case-p name) (string-downcase name))
        ((every #'lower-case-p name) (string-upcase name))
        (t name)))
    (otherwise name)))

(defun mangle-symbol-case (name)
  (case (readtable-case *readtable*)
    (:upcase (string-downcase name))
    (:downcase (string-upcase name))
    (:invert
      (cond
        ((every #'upper-case-p name) (string-downcase name))
        ((every #'lower-case-p name) (string-upcase name))
        (t name)))
    (otherwise name)))

(defun package-char-p (ch)
  (equal #\: ch))

(defun split-qualified-name (name)
  (let* ((normalized-name (normalize-symbol-case name))
         (pos (position-if #'package-char-p normalized-name))
         (start (if pos
                  (or (position-if-not #'package-char-p normalized-name :start pos)
                      (length name))
                  0)))
    (values
      (subseq normalized-name start)
      (cond
        ((equal pos 0) "KEYWORD")
        (pos (subseq normalized-name 0 pos)))
      (and pos (= 1 (- start pos))))))

(defun find-qualified-symbol (name default-package)
  (multiple-value-bind (name package)
                       (split-qualified-name name)
    (if (or (not name) (zerop (length name)))
      (values nil nil)
      (find-symbol name (or package default-package)))))

(defmethod jupyter:inspect-code ((k kernel) code cursor-pos detail-level)
  (jupyter:handling-errors
    (with-slots (package) k
      (multiple-value-bind (sym status)
                           (find-qualified-symbol
                             (values (symbol-string-at-position code cursor-pos))
                             package)
        (when sym
          (make-instance 'inspect-result
            :symbol sym
            :status status))))))

(defun symbol-name-to-qualified-name (name package-name package)
  (mangle-symbol-case
    (if package-name
      (multiple-value-bind (sym status) (find-symbol name package)
        (declare (ignore sym))
        (format nil "~A~A~A"
          (if (equal package-name "KEYWORD") "" package-name)
          (if (equal status :external) ":" "::")
          name))
      name)))

(defmethod jupyter:complete-code ((k kernel) code cursor-pos)
  (jupyter:handling-errors
    (multiple-value-bind (word start end) (symbol-string-at-position code cursor-pos)
      (when word
        (values
          (multiple-value-bind (name package-name ext) (split-qualified-name word)
            (with-slots (package) k
              (let ((pkg (find-package (or package-name package))))
                (when pkg
                  (if ext
                    (iter
                      (for sym in-package pkg external-only t)
                      (for sym-name next (symbol-name sym))
                      (when (starts-with-subseq name sym-name)
                        (collect
                          (symbol-name-to-qualified-name sym-name package-name pkg))))
                    (iter
                      (for sym in-package pkg)
                      (for sym-name next (symbol-name sym))
                      (when (starts-with-subseq name sym-name)
                        (collect
                          (symbol-name-to-qualified-name sym-name package-name pkg)))))))))
          start
          end)))))

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
