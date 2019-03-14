(in-package #:common-lisp-jupyter)

(defvar +kernel-name+ "Common Lisp")
(defvar +kernel-language+ "common-lisp")
(defvar +kernel-resources+
  (list (asdf:component-pathname (asdf:find-component :common-lisp-jupyter '("res" "logo-64x64.png")))))

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
  (jupyter:handling-errors
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

(defun sexpr-to-text (value)
  (string-trim '(#\Newline)
    (with-output-to-string (s)
      (pprint value s))))

(defmethod jupyter:render ((res inspect-result))
  (jsown:new-js
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


(defun install (&key bin-path (ev-flag #+clisp "-x" #+(or mkcl cmucl) "-eval" #-(or clisp cmucl mkcl) "--eval") preamble)
  "Install Common Lisp kernel based on implementation"
  (jupyter:install-kernel
    :argv (iter
      (for cmd in (append preamble
                    '("(ql:quickload :common-lisp-jupyter)"
                      "(jupyter:run-kernel 'common-lisp-jupyter:kernel \"{connection_file}\")")))
      (if-first-time
        (collect
          (or bin-path (format nil "~(~A~)" (uiop:implementation-type)))))
      (collect ev-flag)
      (collect cmd))
    :name +kernel-name+
    :language +kernel-language+
    :resources +kernel-resources+))

(defun install-image ()
  "Install Common Lisp kernel based on image of current implementation"
  (jupyter:install-kernel
    :class 'kernel
    :name +kernel-name+
    :language +kernel-language+
    :resources +kernel-resources+))

(defun install-roswell ()
  "Install Common Lisp kernel using Roswell"
  (jupyter:install-kernel
    :argv (if (uiop:os-windows-p)
            (list "ros"
              (namestring
                (merge-pathnames
                  (make-pathname :directory '(:relative ".roswell" "bin")
                                 :name "cl-jupyter")
                  (uiop:getenv-absolute-directory "USERPROFILE")))
            "{connection_file}")
          '("cl-jupyter" "{connection_file}"))
    :name +kernel-name+
    :language +kernel-language+
    :resources +kernel-resources+))
