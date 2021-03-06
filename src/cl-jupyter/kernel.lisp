(in-package #:jupyter/common-lisp)

(defvar +display-name+ "Common Lisp")
(defvar +language+ "common-lisp")
(defvar +eval-flag+
  #+clisp "-x" #+(or mkcl cmucl) "-eval" #-(or clisp cmucl mkcl) "--eval")
(defvar +load-flag+
  #+clisp "-i" #+(or mkcl cmucl) "-load" #-(or clisp cmucl mkcl) "--load")
(defvar +user-options+
  #+sbcl nil #-sbcl "--")

(defclass kernel (jupyter:kernel)
  ()
  (:default-initargs
    :name "common-lisp"
    :package (find-package :common-lisp-user)
    :version "0.1"
    :banner "common-lisp-jupyter: a Common Lisp Jupyter kernel
(C) 2019-2020 Tarn Burton (MIT)"
    :language-name "common-lisp"
    :language-version (uiop:lisp-version-string)
    :mime-type "text/x-common-lisp"
    :file-extension ".lisp"
    :pygments-lexer "common-lisp"
    :codemirror-mode "text/x-common-lisp"
    :help-links '(("Common Lisp Documentation" . "https://common-lisp.net/documentation")
                  ("Common Lisp HyperSpec" . "http://www.lispworks.com/documentation/HyperSpec/Front/index.htm")
                  ("Practical Common Lisp" . "http://www.gigamonkeys.com/book/")
                  ("The Common Lisp Cookbook" . "https://lispcookbook.github.io/cl-cookbook/")
                  #+abcl ("ABCL Website" . "https://common-lisp.net/project/armedbear/")
                  #+ccl ("CCL Website" . "https://ccl.clozure.com/")
                  #+clasp ("CLASP Website" . "https://github.com/clasp-developers/clasp")
                  #+clisp ("CLISP Website" . "https://clisp.sourceforge.io/")
                  #+cmucl ("CMUCL Website" . "https://common-lisp.net/project/cmucl/")
                  #+ecl ("ECL Website" . "https://common-lisp.net/project/ecl/")
                  #+sbcl ("SBCL Website" . "http://sbcl.org/"))))


(defmethod jupyter:start :after ((k kernel))
  (bordeaux-threads:make-thread
    (lambda ()
      (jupyter:inform :info k "Loading CLHS map")
      (load-clhs-map))))


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
    evaluated-expr)))


(defmethod jupyter:evaluate-code ((k kernel) code)
  (with-input-from-string (stream code)
    (prog (expr ename evalue traceback)
     repeat
      (multiple-value-setq (expr ename evalue traceback) (my-read stream nil :eof))
      (when (or (eq :eof expr)
                ename)
        (return (values ename evalue traceback)))
      (multiple-value-setq (expr ename evalue traceback) (my-eval expr))
      (when ename
        (return (values ename evalue traceback)))
      (multiple-value-setq (expr ename evalue traceback)
                           (j:handling-errors
                             (dolist (result expr)
                               (jupyter:execute-result result))))
      (when ename
        (return (values ename evalue traceback)))
      (go repeat))))

