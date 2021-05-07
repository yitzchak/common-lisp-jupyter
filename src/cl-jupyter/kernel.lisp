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


(defun my-debugger (condition me-or-my-encapsulation)
  (declare (ignore me-or-my-encapsulation))
  (cond
    ((jupyter:kernel-debugger-started jupyter:*kernel*)
      (format *error-output* "[~S] ~A~%" (type-of condition) condition)
      (finish-output *error-output*)
      (jupyter:debug-stop "exception" (dissect:capture-environment condition)))
    (t
      (dissect:present condition *error-output*)
      (finish-output *error-output*)))
  (abort))


(defmacro debugging-errors (&body body)
  `(let ((#+sbcl sb-ext:*invoke-debugger-hook*
          #+(or clasp ecl) ext:*invoke-debugger-hook*
          #-(or clasp ecl sbcl) *debugger-hook*
            #'my-debugger))
     (with-simple-restart (cl:abort "Exit debugger, returning to top level.")
       ,@body)))


(defmethod jupyter:debug-continue ((k kernel) environment)
  #+sbcl (invoke-restart 'sb-impl::step-continue)
  #-sbcl (continue (dissect:environment-condition environment)))


(defmethod jupyter:debug-in ((k kernel) environment)
  #+sbcl (invoke-restart 'sb-impl::step-into)
  #-sbcl (continue (dissect:environment-condition environment)))


(defmethod jupyter:debug-out ((k kernel) environment)
  #+sbcl (invoke-restart 'sb-impl::step-out)
  #-sbcl (continue (dissect:environment-condition environment)))


(defmethod jupyter:debug-next ((k kernel) environment)
  #+sbcl (invoke-restart 'sb-impl::step-next)
  #-sbcl (error "no stepping"))


(defmethod jupyter:debug-scopes ((k kernel) environment frame)
  (declare (ignore k environment frame)))


(defmethod jupyter:debug-stack-trace ((k kernel) environment)
  (mapcar (lambda (call)
            `(:object-alist
               ("id" . ,(jupyter:add-debug-object k call))
               ("name" . ,(format nil "~S" (dissect:call call)))
               ("source" (:object-alist
                           ("name" . ,(or (dissect:file call) :null))
                           ("path" . ,(or (dissect:file call) :null))))
               ("line" . ,(or (dissect:line call) 0))
               ("column" . 0)))
          (dissect:environment-stack environment)))


(defmethod jupyter:debug-initialize ((k kernel))
  '(:object-plist
     "supportsCompletionsRequest" :true
     "supportsConditionalBreakpoints" :true
     "supportsConfigurationDoneRequest" :true
     "supportsDebuggerProperties" :true
     "supportsDelayedStackTraceLoading" :true
     "supportsEvaluateForHovers" :true
     "supportsExceptionInfoRequest" :true
     "supportsExceptionOptions" :true
     "supportsHitConditionalBreakpoints" :true
     "supportsLogPoints" :true
     "supportsModulesRequest" :true
     "supportsSetExpression" :true
     "supportsSetVariable" :true
     "supportsValueFormattingOptions" :true
     "supportTerminateDebuggee" :true
     "supportsGotoTargetsRequest" :true))


(defparameter *breakpoint-id* 0)


(defmethod jupyter:debug-set-breakpoint (kernel source line column condition hit-condition log-message)
  (declare (ignore condition hit-condition log-message))
  `(:object-alist
     ("id" . ,(incf *breakpoint-id*))
     ("verified" . :true)
     ("source" (:object-alist
                 ("path" . ,source)))
     ("line" . ,line)))


(defun my-read (&optional input-stream (eof-error-p t) eof-value recursive-p)
  (jupyter:handling-errors
    (read input-stream eof-error-p eof-value recursive-p)))


(defun my-eval (expr)
  (debugging-errors
    (setq common-lisp-user::- expr)
    (let ((evaluated-expr (multiple-value-list #+sbcl (eval `(step ,expr))
                                               #-sbcl (eval expr))))
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

