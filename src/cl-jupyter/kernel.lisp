(in-package #:jupyter/common-lisp)

(defvar +display-name+ "Common Lisp")
(defvar +language+ "common-lisp")
(defvar +eval-flag+
  #+clisp "-x" #+(or mkcl cmucl) "-eval" #-(or clisp cmucl mkcl) "--eval")
(defvar +load-flag+
  #+clisp "-i" #+(or mkcl cmucl) "-load" #-(or clisp cmucl mkcl) "--load")
(defvar +user-options+
  #+sbcl nil #-sbcl "--")
(defvar *debug-condition* nil)
(defvar +abort-report+ "Exit debugger and halt cell execution.")

(defclass kernel (jupyter:kernel)
  ((environment
     :reader kernel-environment
     :initform #+sbcl (sb-kernel:make-null-lexenv)
               #-sbcl nil))
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
    :debugger t
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


(defclass debug-environment (jupyter:debug-environment)
  ())


(defmethod initialize-instance :after ((instance debug-environment) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (let ((abort-restart (member +abort-report+ (jupyter:debug-environment-restarts instance)
                               :key (lambda (r) (format nil "~A" r))
                               :test #'equal)))
    (when abort-restart
      (setf (cdr abort-restart) nil))))


(defun invoke-first-restart (instance &rest restarts)
  (dolist (restart-name restarts)
    (let ((restart (find-if (lambda (restart)
                              (equal (restart-name restart) restart-name))
                            (jupyter:debug-environment-restarts instance))))
      (when restart
        (return (invoke-restart-interactively restart))))))


(defclass debug-frame (jupyter:debug-frame)
  ())


(defclass debug-local-scope (jupyter:debug-scope)
  ()
  (:default-initargs
    :name "Locals"
    :presentation-hint "locals"))


(defclass debug-globals-scope (jupyter:debug-scope)
  ()
  (:default-initargs
    :name "Globals"
    :presentation-hint "globals"))


(defclass debug-variable (jupyter:debug-variable)
  ())


(defun make-debug-variable (name &optional (value nil value-present-p) (env nil env-present-p))
  (let ((var (make-instance 'debug-variable
                            :name (write-to-string name))))
    (when value-present-p
      (setf (jupyter:debug-object-value var) (write-to-string value)
            (jupyter:debug-object-type var) (write-to-string (type-of value))
            (jupyter:debug-object-data var) value))
    (unless (or (typep value 'standard-object)
                (typep value 'structure-object))
      (setf (jupyter:debug-object-id var) 0))
    (when env-present-p
      (setf (jupyter::debug-object-environment var) env)
      (jupyter::register-debug-object var))
    var))


(defun contains (sym expr)
  (if (listp expr)
    (some (lambda (x)
            (contains sym x))
          expr)
    (equal sym expr)))


(defun frame-function-name (frame)
  #+ccl   (ccl:function-name (ccl:frame-function (car frame) (cdr frame)))
  #+clasp (clasp-debug:frame-function-name frame)
  #+cmucl (di:debug-function-name (di:frame-debug-function frame))
  #+ecl   (let ((x (car frame)))
            (typecase x
              (symbol
                x)
              (generic-function
                (clos:generic-function-name x))
              (function
                (si:compiled-function-name x))))
  #+sbcl  (let* ((name (sb-debug::frame-call frame))
                 (pos (when (listp name)
                        (position :in name))))
            (if pos
              (subseq name 0 pos)
              name)))


(defun trim-frame-list (frames condition)
  #+sbcl
  (when (typep condition 'sb-impl::step-condition)
    (setf frames (or (cdr (member-if (lambda (frame)
                                       (contains 'sb-di::handle-single-step-around-trap (multiple-value-list (sb-debug::frame-call frame))))
                                     frames))
                     frames)))
  (or (member-if (lambda (frame)
                   (not (member (frame-function-name frame)
                                '(frame-list
                                  compute-debug-frames
                                  debugger-hook
                                  invoke-debugger
                                  break
                                  #+sbcl sb-debug::run-hook
                                  #+sbcl sb-int::%break)
                                :test #'equal)))
                 frames)
      frames))


(defun frame-list ()
  #+ccl   (let (frames)
            (ccl:map-call-frames (lambda (frame context)
                                   (push (cons frame context) frames))
                                 :origin ccl:*top-error-frame*)
            (nreverse frames))
  #+clasp (clasp-debug:with-stack (stack)
            (clasp-debug:list-stack stack))
  #+cmucl (do ((frame (or debug::*current-frame* (di:top-frame))
		                  (di:frame-down frame))
	             (count most-positive-fixnum (1- count))
	             frames)
	            ((or (null frame) (zerop count)) (nreverse frames))
	          (push frame frames))
  #+ecl   (let (frames)
            (dotimes (index (system::ihs-top) frames)
              (push (cons (system::ihs-fun index)
                          (system::ihs-env index))
                    frames)))
  #+sbcl  (let (frames)
            (sb-debug::map-backtrace (lambda (frame)
                                       (push frame frames))
                                     :from :interrupted-frame)
            (nreverse frames)))


(defun frame-name (frame)
  (string-trim '(#\Newline #\Return #\Space #\Tab)
    (let ((*print-case* :downcase))
      (with-output-to-string (*standard-output*)
        (print (frame-function-name frame))))))


(defun frame-source (frame)
  #+ccl   (let* ((function (ccl:frame-function (car frame) (cdr frame)))
                 (source-note (ccl:function-source-note function))
                 (pathname (ccl:source-note-filename source-note)))
            (when pathname
              (multiple-value-call #'values pathname (source-line-column pathname (ccl:source-note-start-pos source-note)))))
  #+clasp (let ((pos (clasp-debug:frame-source-position frame)))
            (when pos
              (let ((pathname (clasp-debug:code-source-line-pathname pos)))
                (values (if (and (uiop:physical-pathname-p pathname)
                                 (not (uiop:absolute-pathname-p pathname)))
                          (merge-pathnames (make-pathname :host "CELL" :version :newest)
                                           pathname)
                          pathname)
                        (clasp-debug:code-source-line-line-number pos)
                        (clasp-debug:code-source-line-column pos)))))
  #+cmucl (let* ((code-location (di:frame-code-location frame))
                 (debug-source (di:code-location-debug-source code-location))
                 (pathname (ignore-errors (di:debug-source-name debug-source))))
            (when (eq :file (di:debug-source-from debug-source))
              (multiple-value-call #'values
                                   pathname
                                   (handler-case
                                       (unless (di:code-location-unknown-p code-location)
                                         (source-line-column
                                           pathname
                                           (di:code-location-top-level-form-offset code-location)
                                           (di:code-location-form-number code-location)))
                                     (error (condition)
                                       (declare (ignore condition)))))))
  #+ecl   (multiple-value-bind (pathname position)
                               (system::bc-file (car frame))
            (when file
              (multiple-value-call #'values pathname (source-line-column pathname position))))
  #+sbcl  (let* ((code-location (sb-di:frame-code-location frame))
                 (pathname (ignore-errors
                             (sb-di:debug-source-namestring (sb-di:code-location-debug-source code-location)))))
            (when pathname
              (multiple-value-call #'values
                                   pathname
                                   (handler-case
                                       (unless (sb-di:code-location-unknown-p code-location)
                                         (source-line-column
                                           pathname
                                           (sb-di:code-location-toplevel-form-offset code-location)
                                           (sb-di:code-location-form-number code-location)))
                                     (error (condition)
                                       (declare (ignore condition))))))))


(defun compute-debug-frames (condition)
  (mapcar (lambda (frame &aux (instance (make-instance 'debug-frame
                                                       :name (frame-name frame)
                                                       :data frame)))
            (multiple-value-bind (pathname line column)
                                 (frame-source frame)
              (when pathname
                (setf (jupyter:debug-object-source instance)
                      (make-instance 'jupyter:debug-source
                                     :name (file-namestring pathname)
                                     :path pathname)))
              (when line
                (setf (jupyter:debug-object-line instance) line))
              (when column
                (setf (jupyter:debug-object-column instance) column)))
            instance)
          (trim-frame-list (frame-list) condition)))


(defmethod jupyter:debug-object-children-resolve ((instance debug-frame))
  (list (make-instance 'debug-local-scope
                       :environment (jupyter:debug-object-environment instance)
                       :parent instance)))


(defmethod jupyter:debug-object-children-resolve ((instance debug-local-scope))
  (stable-sort
    #+ccl   (let ((frame (jupyter:debug-object-data (jupyter:debug-object-parent instance))))
              (mapcar (lambda (var)
                        (make-debug-variable (car var) (cdr var)))
                      (ccl:frame-named-variables (car frame) (cdr frame))))
    #+clasp (mapcar (lambda (var)
                      (make-debug-variable (car var) (cdr var)))
                    (clasp-debug:frame-locals (jupyter:debug-object-data (jupyter:debug-object-parent instance))
                                              :eval #'eval-with-bindings))
    #+cmucl (let* ((frame (jupyter:debug-object-data (jupyter:debug-object-parent instance)))
                   (loc (di:frame-code-location frame))
                   variables)
              (trivial-do:doseq (var (di::debug-function-debug-variables (di:frame-debug-function frame))
                                     (nreverse variables))
                (when (eq :valid (di:debug-variable-validity var loc))
                  (push (make-debug-variable (di:debug-variable-symbol var) (di:debug-variable-value var frame))
                        variables))))
    #+ecl   (mapcan (lambda (record)
                      (when (and (consp record)
                                 (or (symbolp (car record))
                                     (stringp (car record))))
                        (list (make-debug-variable (car record) (cdr record)))))
                    (system::decode-ihs-env (cdr (jupyter:debug-object-data (jupyter:debug-object-parent instance)))))
    #+sbcl  (let* ((frame (jupyter:debug-object-data (jupyter:debug-object-parent instance)))
                   (loc (sb-di:frame-code-location frame))
                   results)
              (trivial-do:doseq (var (sb-di::debug-fun-debug-vars (sb-di:frame-debug-fun frame)) (nreverse results))
                (when (eq :valid (sb-di:debug-var-validity var loc))
                  (push (make-debug-variable (sb-di:debug-var-symbol var) (sb-di:debug-var-value var frame)) results))))
    #-(or ccl clasp cmucl ecl sbcl) nil
    #'string<
    :key (lambda (variable)
           (write-to-string (jupyter:debug-object-name variable)))))


(defmethod jupyter:debug-object-children-resolve ((instance debug-variable))
  (let ((value (jupyter:debug-object-data instance))
        results)
    (typecase value
      ((or standard-object structure-object)
        (dolist (def (closer-mop:class-slots (class-of value)) value)
          (let ((slot-name (closer-mop:slot-definition-name def)))
            (push (if (slot-boundp value slot-name)
                    (make-debug-variable slot-name (slot-value value slot-name))
                    (make-debug-variable slot-name))
                  results)))))
    (stable-sort results #'string<
                 :key (lambda (variable)
                        (write-to-string (jupyter:debug-object-name variable))))))


(defmethod jupyter:debug-evaluate ((kernel kernel) environment code frame)
  (make-debug-variable "EVAL" (eval-in-frame (read-from-string code) frame)))


(defmethod jupyter:debug-inspect-variables ((kernel kernel) environment)
  (setf (fill-pointer (jupyter::debug-environment-objects environment)) 1)
  (let (variables)
    (do-symbols (symbol (jupyter::kernel-package kernel))
      (when (and (boundp symbol)
                 (not (eq :inherited (nth-value 1 (find-symbol (symbol-name symbol) (jupyter::kernel-package kernel))))))
        (push (make-debug-variable symbol (symbol-value symbol) environment) variables)))
    (stable-sort variables #'string<
                 :key (lambda (variable)
                        (write-to-string (jupyter:debug-object-name variable))))))


(defun debugger-hook (condition me-or-my-encapsulation)
  (declare (ignore me-or-my-encapsulation))
  (let ((environment (make-instance 'debug-environment
                                    :condition condition
                                    :restarts (compute-restarts condition)
                                    :frames (compute-debug-frames condition)))
        (*debug-condition* condition))
    (format (if (typep condition 'warning)
              *standard-output*
              *error-output*)
            "[~S] ~A~%" (type-of condition) condition)
    (finish-output (if (typep condition 'warning)
                     *standard-output*
                     *error-output*))
    (jupyter:debug-stop
      (typecase condition
        #+sbcl (sb-impl::step-form-condition "step")
        (otherwise "exception"))
      environment)
    (abort)))


(defun read-evaluated-form ()
  (format *query-io* "~&Type a form to be evaluated:~%")
  (list (read *query-io*)))


(defun eval-in-frame (form frame &aux (data (jupyter:debug-object-data frame)))
  #+ccl   (eval-with-bindings form (ccl:frame-named-variables (car data) (cdr data)))
  #+clasp (eval-with-bindings form (clasp-debug:frame-locals data :eval #'eval-with-bindings))
  #+cmucl (di:eval-in-frame data form)
  #+ecl   (si:eval-with-env form (cdr data))
  #+sbcl  (sb-di:eval-in-frame data form))


(defmacro debugging-errors (&body body)
  `(unwind-protect
       (prog ((*debugger-hook* #'debugger-hook)
              #+sbcl      (sb-ext:*invoke-debugger-hook* #'debugger-hook)
              #+ccl       (ccl:*break-hook* #'debugger-hook)
              #+ecl       (ext:*invoke-debugger-hook* #'debugger-hook)
              #+clasp     (ext:*invoke-debugger-hook* #'debugger-hook)
              #+abcl      (sys::*invoke-debugger-hook* #'debugger-hook)
              #+clisp     (sys::*break-driver* (lambda (continuable &optional condition print)
                                                 (declare (ignore continuable print))
                                                 (debugger-hook condition nil)))
              #+allegro   (excl::*break-hook* (lambda (&rest args)
                                                (debugger-hook (fifth args))))
              #+lispworks (dbg::*debugger-wrapper-list* (lambda (function condition)
                                                          (declare (ignore function))
                                                          (debugger-hook condition nil)))
              #+mezzano   (mezzano.debug:*global-debugger* (lambda (condition)
                                                             (debugger-hook condition nil))))
         (setf (get 'eval :debug-restart) t)
         (restart-bind
             (#+(or clasp ecl)
              (store-value
                (lambda (form)
                  (print `(set ,(cell-error-name *debug-condition*) ,form)))
                :interactive-function #'read-evaluated-form
                :report-function (lambda (stream)
                                   (write-string "Set specified value and use it." stream))
                :test-function (lambda (condition)
                                 (typep condition 'unbound-variable)))
              (eval
                (lambda (form)
                  (jupyter:handling-comm-errors
                    (dolist (result (multiple-value-list (eval-in-frame form jupyter:*debug-frame*)))
                      (jupyter:execute-result result)))
                  (jupyter:debug-enter-loop))
                :interactive-function #'read-evaluated-form
                :report-function (lambda (stream)
                                   (write-string "Evaluate form in selected frame." stream)))
              #+sbcl
              (step
                (lambda ()
                  (sb-impl::enable-stepping)
                  (continue))
                :report-function (lambda (stream)
                                   (write-string "Enable stepping and continue." stream))
                :test-function (lambda (condition)
                                 (and (not (sb-impl::stepping-enabled-p))
                                      (find-restart 'continue condition))))
              (abort (lambda ()
                       (return (values "ABORT" "Cell execution halted." nil)))
                :report-function (lambda (stream)
                                   (write-string +abort-report+ stream))))
           ,@body
           (values)))
     #+sbcl (sb-impl::disable-stepping)))


(defmethod jupyter:debug-abort ((k kernel) environment)
  (invoke-first-restart environment
                        'abort))


(defmethod jupyter:debug-continue ((k kernel) environment &optional restart-number)
  (if restart-number
    (invoke-restart-interactively (elt (jupyter:debug-environment-restarts environment) restart-number))
    (invoke-first-restart environment
                          #+sbcl 'sb-impl::step-continue
                          'continue)))


(defmethod jupyter:debug-in ((k kernel) environment)
  (invoke-first-restart environment
                        #+sbcl 'sb-impl::step-into
                        'continue))


(defmethod jupyter:debug-out ((k kernel) environment)
  (invoke-first-restart environment
                        #+sbcl 'sb-impl::step-out
                        'continue))


(defmethod jupyter:debug-next ((k kernel) environment)
  (invoke-first-restart environment
                        #+sbcl 'sb-impl::step-next
                        'continue))


(defmethod jupyter:debug-initialize ((k kernel))
  '(:object-plist
     "supportsConfigurationDoneRequest" :true
     "supportsDelayedStackTraceLoading" :true
     "supportsValueFormattingOptions" :true
     "supportTerminateDebuggee" :true
     "supportTerminateRequest" :true))


(defmethod jupyter:debug-new-breakpoint ((kernel kernel) source line)
  (make-instance 'jupyter:debug-breakpoint :line line))


(defmethod jupyter:debug-remove-breakpoint ((kernel kernel) source breakpoint)
  (declare (ignore kernel source #-sbcl breakpoint))
  #+sbcl
  (when (jupyter:debug-breakpoint-data breakpoint)
    (sb-di:delete-breakpoint (jupyter:debug-breakpoint-data breakpoint))))


(defmethod jupyter:debug-activate-breakpoints ((kernel kernel) source breakpoints)
  (declare (ignore kernel #-sbcl source #-sbcl breakpoints))
  #+sbcl
  (update-breakpoints source breakpoints))


(defun breakpoint-hook (frame breakpoint &rest args)
  (declare (ignore frame breakpoint args))
  (when (jupyter:user-thread-p)
    (with-simple-restart (continue "Continue execution from breakpoint.")
      (let ((environment (make-instance 'debug-environment
                                        :restarts (compute-restarts nil)
                                        :frames (compute-debug-frames nil))))
        (jupyter:debug-stop "breakpoint" environment)))))


#+sbcl
(defun set-breakpoints (fun source breakpoints &aux (possible-breakpoints (possible-breakpoints fun)))
  (dolist (pb possible-breakpoints)
    (let* ((pb-source (ignore-errors (parse-namestring (sb-di:debug-source-namestring (sb-di:code-location-debug-source pb)))))
           (line (source-line-column
                   pb-source
                   (sb-di:code-location-toplevel-form-offset pb)
                   (sb-di:code-location-form-number pb))))
      (pprint pb-source)
      (pprint line)
      (when (equal source pb-source)
        (dolist (bp breakpoints)
          (when (and (not (jupyter:debug-breakpoint-data bp))
               (equal (jupyter:debug-breakpoint-line bp) line))
            (pprint line)
           (pprint (sb-di:code-location-kind pb))

            (sb-di:activate-breakpoint (setf (jupyter:debug-breakpoint-data bp)
                                             (sb-di::make-breakpoint #'breakpoint-hook pb)))
            (return)))))))


#+sbcl
(defun update-breakpoints (source breakpoints)
  (let ((unset-breakpoints (remove-if #'jupyter:debug-breakpoint-data breakpoints)))
    (when unset-breakpoints
      (sb-int:call-with-each-globaldb-name
        (lambda (name)
          (when (and (symbolp name)
                     (some (lambda (ds)
                             (and (sb-introspect:definition-source-pathname ds)
                                  (equal (sb-introspect:definition-source-pathname ds) source)))
                           (sb-introspect:find-definition-sources-by-name name :function)))
              (set-breakpoints (fdefinition name) source unset-breakpoints)
              (setf unset-breakpoints (delete-if #'jupyter:debug-breakpoint-data unset-breakpoints))
              (unless unset-breakpoints
                (return-from update-breakpoints))))))))



(defun eval-and-print (form aux-form breakpoints)
  (setf common-lisp-user::- form)
  (let* ((results (multiple-value-list
                    #+ccl   (ccl::cheap-eval-in-environment form (kernel-environment jupyter:*kernel*))
                    #+clasp (funcall (clasp-cleavir::bir-compile-cst-in-env
                                       (cst:list (cst:cst-from-expression 'lambda)
                                                 (cst:cst-from-expression nil)
                                                 aux-form)))
                    #+sbcl  (handler-bind ((sb-c::compiler-note #'muffle-warning))
                              (let* ((sb-c::*source-paths* (make-hash-table :test 'eq))
                                     (lambda (sb-impl::make-eval-lambda form))
                                     (sb-c::*source-form-context-alist*
                                       (acons lambda form
                                              sb-c::*source-form-context-alist*)))
                                (sb-c::find-source-paths form aux-form)
                                (let ((fun (sb-c:compile-in-lexenv lambda (kernel-environment jupyter:*kernel*)
                                                                   nil sb-c::*source-info*
                                                                   aux-form nil nil)))
                                  (trivial-do:dohash (source config (jupyter::kernel-breakpoints jupyter:*kernel*))
                                    (declare (ignore source))
                                    (dolist (breakpoint (jupyter::debug-configuration-breakpoints config))
                                      (when (jupyter:debug-breakpoint-data breakpoint)
                                        (sb-di::activate-breakpoint
                                          (sb-di::deactivate-breakpoint
                                            (jupyter:debug-breakpoint-data breakpoint))))))
                                  (funcall fun))))
                    #-(or ccl clasp sbcl)
                            (eval form))))
    (setf common-lisp-user::*** common-lisp-user::**
          common-lisp-user::** common-lisp-user::*
          common-lisp-user::* (car results)
          common-lisp-user::/// common-lisp-user:://
          common-lisp-user::// common-lisp-user::/
          common-lisp-user::/ results
          common-lisp-user::+++ common-lisp-user::++
          common-lisp-user::++ common-lisp-user::+
          common-lisp-user::+ form)
    (dolist (result results)
      (jupyter:execute-result result))
    #+sbcl (update-breakpoints *load-pathname* breakpoints)))


(defun repl (code source-path breakpoints)
  (dolist (breakpoint breakpoints)
    (when (jupyter:debug-breakpoint-data breakpoint)
      #+sbcl (sb-di:delete-breakpoint (jupyter:debug-breakpoint-data breakpoint))
      (setf (jupyter:debug-breakpoint-data breakpoint) nil)))
  (cond
    (source-path ; If source-path is supplied then try to do source tracking.
      #+ccl
      (with-open-file (stream source-path)
        (prog ((*load-truename* (truename source-path))
               (*load-pathname* source-path)
               (ccl::*loading-file-source-file* source-path)
               (ccl::*nx-source-note-map* (make-hash-table :test #'eq :shared nil))
               (ccl::*loading-toplevel-location* nil)
               form)
         next
          (multiple-value-setq (form ccl::*loading-toplevel-location*)
                               (ccl::read-recording-source stream
                                                           :eofval :eof
                                                           :file-name source-path
                                                           :map ccl::*nx-source-note-map*
                                                           :save-source-text t))

          (unless (eq form :eof)
            (eval-and-print form nil breakpoints)
            (go next))))
      #+clasp
      (with-open-file (stream source-path)
        (do* ((eclector.reader:*client* clasp-cleavir::*cst-client*)
              (eclector.readtable:*readtable* cl:*readtable*)
              (*load-truename* (truename source-path))
              (*load-pathname* source-path)
              (cmp::*compile-file-pathname* source-path)
              (cmp::*compile-file-truename* (truename source-path))
              (cmp::*compile-file-source-debug-pathname* source-path)
              (cmp::*compile-file-file-scope* (core:file-scope source-path))
              (cmp::*compile-file-source-debug-lineno* 0)
              (cmp::*compile-file-source-debug-offset* 0)
              (core:*current-source-pos-info* (cmp:compile-file-source-pos-info stream))
              (cst (eclector.concrete-syntax-tree:cst-read stream nil :eof)
                   (eclector.concrete-syntax-tree:cst-read stream nil :eof)))
            ((eq :eof cst) (values))
          (eval-and-print (concrete-syntax-tree:raw cst) cst breakpoints)))
      #+sbcl
      (sb-c::with-compiler-error-resignalling
        (let ((sb-c::*last-message-count* (list* 0 nil nil))
              (sb-c::*compile-verbose* nil)
              (*load-truename* (truename source-path))
              (*load-pathname* source-path))
          (sb-c::do-forms-from-info ((form current-index)
                                     (sb-c::make-file-source-info source-path :default t))
            (eval-and-print form current-index breakpoints))))
      #-(or ccl clasp sbcl)
      (with-open-file (stream source-path)
        (do* ((*load-truename* (truename source-path))
              (*load-pathname* source-path)
              #+ecl (ext:*source-location* (cons source-path (file-position stream))
                                           (cons source-path (file-position stream)))
              (form (read stream nil :eof)
                    (read stream nil :eof)))
             ((eq :eof form) (values))
          (eval-and-print form nil breakpoints))))
    (t ; Fallback REPL
      (with-input-from-string (stream code)
        (do ((form (read stream nil :eof)
                   (read stream nil :eof)))
            ((eq :eof form) (values))
          (eval-and-print form nil breakpoints))))))


(defmethod jupyter:evaluate-code ((k kernel) code &optional source-path breakpoints)
  (if (jupyter:kernel-debugger-started jupyter:*kernel*)
    (debugging-errors (repl code source-path breakpoints))
    (jupyter:handling-errors (repl code source-path breakpoints))))
