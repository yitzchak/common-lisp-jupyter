(in-package #:cl-jupyter)

(defclass kernel (jupyter:kernel)
  ()
  (:default-initargs :name "cl-jupyter"
                     :version "0.7"
                     :banner "cl-jupyter: a Common Lisp Jupyter kernel
(C) 2019 Tarn Burton (BSD)"
                     :language-name "common-lisp"
                     :language-version (uiop:lisp-version-string)
                     :mime-type "text/x-common-lisp"
                     :file-extension ".lisp"
                     :pygments-lexer "common-lisp"
                     :codemirror-mode "text/x-common-lisp"
                     :help-links '(("Common Lisp Documentation" . "https://common-lisp.net/documentation")
                                   ("Common Lisp HyperSpec" . "http://www.lispworks.com/documentation/HyperSpec/Front/index.htm"))))

(defmethod jupyter:is-complete ((k kernel) code)
  "complete")

(defmethod jupyter:evaluate ((k kernel) page-output code)
  (iter
    (for sexpr in-stream (make-string-input-stream code))
    (for result = (jupyter:make-lisp-result
                    (jupyter:handling-errors
                      (eval sexpr))))
    (when result
      (collect result))
    (until (jupyter:quit-eval-error-p result))))
