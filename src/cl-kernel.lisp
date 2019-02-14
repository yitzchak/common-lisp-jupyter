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

;;; Based on macro taken from: http://www.cliki.net/REPL
(defmacro handling-errors (&body body)
  `(handler-case
    (handler-bind
      ((simple-warning
        (lambda (wrn)
          (apply (function format) *standard-output*
                (simple-condition-format-control   wrn)
                (simple-condition-format-arguments wrn))
          (format *standard-output* "~&")
          (muffle-warning)))
      (warning
        (lambda (wrn)
          (format *standard-output* "~&~A: ~%  ~A~%"
                  (class-name (class-of wrn)) wrn)
          (muffle-warning))))
    	 (progn ,@body))
     (jupyter:quit (err)
       (jupyter:make-eval-error err (format nil "~A" err) :quit t))
     (simple-condition (err)
       (jupyter:make-eval-error err
         (apply #'format nil (simple-condition-format-control err)
                             (simple-condition-format-arguments err))))
     (condition (err)
       (jupyter:make-eval-error err (format nil "~A" err)))))

(defmethod jupyter:is-complete ((k kernel) code)
  (handler-case
    (iter
      (for sexpr in-stream (make-string-input-stream code)))
    (end-of-file () "incomplete")
    (serious-condition () "invalid")
    (condition () "invalid")
    (:no-error (val)
      (declare (ignore val))
      "complete")))

(defmethod jupyter:evaluate ((k kernel) code)
  (iter
    (for sexpr in-stream (make-string-input-stream code))
    (for result = (jupyter:make-lisp-result
                    (handling-errors (eval sexpr))))
    (when result
      (collect result))
    (until (jupyter:quit-eval-error-p result))))

#+ros.installing
(eval-when (:compile-toplevel)
  (defparameter roswell.install::*build-hook*
    (lambda ()
      (jupyter:install-kernel '("cl-jupyter" "{connection_file}")
                              "Common Lisp"
                              "common-lisp"))))
