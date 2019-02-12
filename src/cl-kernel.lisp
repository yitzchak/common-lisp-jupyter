(in-package #:cl-jupyter)

(defclass kernel (jupyter:kernel)
  ()
  (:default-initargs :name "common-lisp"
                     :package :common-lisp-user
                     :version "0.7"
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

(defmethod jupyter:is-complete ((k kernel) code)
  (handler-case
    (iter
      (for sexpr in-stream (make-string-input-stream code)))
    (end-of-file () "incomplete")
    #+sbcl (sb-int:simple-reader-error () "incomplete")
    (:no-error (val)
      (declare (ignore val))
      "complete")))

(defmethod jupyter:evaluate ((k kernel) page-output code)
  ; (let ((*package* (find-package :common-lisp-user)))
    (iter
      (for sexpr in-stream (make-string-input-stream code))
      (for result = (jupyter:make-lisp-result
                      (jupyter:handling-errors
                          (eval sexpr))))
      (when result
        (collect result))
      (until (jupyter:quit-eval-error-p result))));)

#+ros.installing
(eval-when (:compile-toplevel)
  (defparameter roswell.install::*build-hook*
    (lambda ()
      (jupyter:install-kernel '("cl-jupyter" "{connection_file}")
                              "Common Lisp"
                              "common-lisp"))))
