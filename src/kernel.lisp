(in-package #:jupyter)

(defvar *stdout* nil)
(defvar *stdin* nil)
(defvar *stderr* nil)
(defvar *enable-debugger* t)
(defvar *enable-internal-debugger* t)
(defvar *payload* nil)
(defvar *markdown-output* nil)
(defvar *html-output* nil)
(defvar *thread-id* nil)
(defvar *debug-environment* nil)
(defvar *debug-frame* nil)
(defconstant +thread-bits+ 8)
(defconstant +max-thread-count+ (ash 1 +thread-bits+))
(defconstant +thread-mask+ (1- +max-thread-count+))

(defvar *page-output* nil
  "Output stream sent to Jupyter pager. Available during calls to evaluate-code.")

(defconstant +zmq-poll-timeout+ 500)

(defclass thread ()
  ((queue
     :reader thread-queue
     :initform (make-instance 'queue))
   (stopped
     :accessor thread-stopped
     :initform nil)))


(defclass kernel (source)
  ((name
     :initarg :name
     :initform ""
     :reader kernel-name
     :documentation "Kernel name. Used as a unique identifier in kernel description.")
   (version
     :initarg :version
     :initform ""
     :reader kernel-version
     :documentation "Kernel version.")
   (banner
     :initarg :banner
     :initform ""
     :reader kernel-banner
     :documentation "Banner text used to describe kernel. Used in kernel_info_reply messages.")
   (language-name
     :initarg :language-name
     :initform ""
     :reader kernel-language-name
     :documentation "Display name of implementation language. Used in kernel_info_reply messages.")
   (language-version
     :initarg :language-version
     :initform ""
     :reader kernel-language-version
     :documentation "Version of implementation language. Used in kernel_info_reply messages.")
   (mime-type
     :initarg :mime-type
     :initform ""
     :reader kernel-mime-type
     :documentation "Default MIME type for source files. Used in kernel_info_reply messages.")
   (file-extension
     :initarg :file-extension
     :initform ""
     :reader kernel-file-extension
     :documentation "Default file extension for source files. Used in kernel_info_reply messages.")
   (pygments-lexer
     :initarg :pygments-lexer
     :initform ""
     :reader kernel-pygments-lexer
     :documentation "Name of Pygments lexer for source files. Used in kernel_info_reply messages.")
   (codemirror-mode
     :initarg :codemirror-mode
     :initform ""
     :reader kernel-codemirror-mode
     :documentation "CodeMirror mode for source files. Used in kernel_info_reply messages.")
   (help-links
     :initarg :help-links
     :initform nil
     :reader kernel-help-links
     :documentation "An association list of help links. The car is the description and the cdr is
       URL. Used in kernel_info_reply messages.")
   (package
     :initarg :package
     :accessor kernel-package
     :documentation "The package in which evaluate-code, code-is-complete and others are called.")
   (readtable
     :initarg :readtable
     :initform (copy-readtable)
     :accessor kernel-readtable
     :documentation "The readtable used bu evaluate-code, code-is-complete and others.")
   (connection-file
     :initarg :connection-file
     :reader kernel-connection-file
     :documentation "Pathname of connection file.")
   (transport
     :accessor kernel-transport
     :type string
     :documentation "Transport protocol from connection file.")
   (ip
     :accessor kernel-ip
     :type string
     :documentation "IP address from connection file.")
   (shell-port
     :accessor kernel-shell-port
     :type fixnum
     :documentation "SHELL port from connection file.")
   (stdin-port
     :accessor kernel-stdin-port
     :type fixnum
     :documentation "STDIN port from connection file.")
   (iopub-port
     :accessor kernel-iopub-port
     :type fixnum
     :documentation "IOPUB port from connection file.")
   (control-port
     :accessor kernel-control-port
     :type fixnum
     :documentation "CONTROL port from connection file.")
   (hb-port
     :accessor kernel-hb-port
     :type fixnum
     :documentation "HB port from connection file.")
   (signature-scheme
     :accessor kernel-signature-scheme
     :type string
     :documentation "Signature scheme from connection file.")
   (key
     :accessor kernel-key
     :documentation "Signing key from connection file.")
   (prompt-prefix
     :initarg :prompt-prefix
     :initform (coerce '(#\Escape #\X) 'string)
     :reader kernel-prompt-prefix
     :documentation "String prefix using in *standard-output* to indicate the start of prompt.")
   (prompt-suffix
     :initarg :prompt-suffix
     :initform (coerce '(#\Escape #\\) 'string)
     :reader kernel-prompt-suffix
     :documentation "String suffix using in *standard-output* to indicate the end of prompt.")
   (ctx
     :initform nil
     :accessor kernel-ctx
     :documentation "pzmq ctx handle.")
   (mac
     :initform nil
     :accessor kernel-mac
     :documentation "Message authentification.")
   (hb
     :initform nil
     :accessor kernel-hb
     :documentation "Heartbeat channel.")
   (shell
     :initform nil
     :accessor kernel-shell
     :documentation "SHELL channel.")
   (stdin
     :initform nil
     :accessor kernel-stdin
     :documentation "STDIN channel.")
   (control
     :initform nil
     :accessor kernel-control
     :documentation "CONTROL channel.")
   (iopub
     :initform nil
     :accessor kernel-iopub
     :documentation "IOPUB channel.")
   (session
     :initform nil
     :accessor kernel-session
     :documentation "Session identifier.")
   (input-queue
     :initarg :input-queue
     :initform (make-instance 'queue)
     :reader kernel-input-queue
     :documentation "Input queue used to feed values into execute_result payloads.")
   (history
     :initform nil
     :accessor kernel-history
     :documentation "Kernel history manager.")
   (execution-count
     :initform 0
     :accessor kernel-execution-count
     :documentation "Kernel execution count.")
   (comms
     :initform (make-hash-table :test #'equal)
     :reader kernel-comms
     :documentation "Currently open comms.")
   (control-thread
     :accessor kernel-control-thread
     :initarg :control-thread
     :initform nil
     :documentation "Control thread")
   (shell-thread
     :accessor kernel-shell-thread
     :initform nil
     :documentation "Shell thread")
   (html-output
     :reader kernel-html-output
     :initform (make-display-stream +html-mime-type+)
     :documentation "HTML display output stream")
   (markdown-output
     :reader kernel-markdown-output
     :initform (make-display-stream +markdown-mime-type+)
     :documentation "Markdown display output stream")
   (error-output
     :accessor kernel-error-output
     :documentation "Error output stream")
   (standard-output
     :accessor kernel-standard-output
     :documentation "Standard output stream")
   (standard-input
     :accessor kernel-standard-input
     :documentation "Standard input stream")
   (tmp-file-prefix
     :accessor kernel-tmp-file-prefix
     :documentation "Prefix for temporary debugger files")
   (tmp-file-suffix
     :accessor kernel-tmp-file-suffix
     :documentation "Suffix for temporary debugger files")
   (hash-seed
     :accessor kernel-hash-seed
     :documentation "Hash seed for temporary debugger files")
   (breakpoints
     :accessor kernel-breakpoints
     :initform (make-hash-table :test #'equal)
     :documentation "Currently set breakpoints.")
   (debugger
     :accessor kernel-debugger
     :initform nil
     :initarg :debugger
     :documentation "Whether the debugger is supported")
   (debugger-started
     :accessor kernel-debugger-started
     :initform nil
     :documentation "Whether the debugger has been started")
   (threads
     :reader kernel-threads
     :initform (make-array +max-thread-count+ :fill-pointer 0 :element-type '(or null thread))))
  (:documentation "Kernel state representation."))


(defun add-thread (kernel-instance)
  "Create a thread queue in the kernel and assign the thread an id number."
  (with-slots (threads)
              kernel-instance
    (let ((thread-id (position nil threads))
          (thread (make-instance 'thread)))
      (if thread-id
        (setf (aref threads thread-id) thread)
        (setf thread-id (vector-push thread threads)))
      thread-id)))


(defun remove-thread (kernel-instance &optional (thread-id *thread-id*))
  "Remove the thread queue and reset the thread id number."
  (setf (aref (kernel-threads kernel-instance) thread-id) nil)
  (values))


(defun user-thread-p ()
  "Return non-NIL if the current thread is not the control thread."
  (and *thread-id*
       (not (zerop *thread-id*))))


(defun compute-source-path (code)
  "Return the source path for a fragment of code."
  (logical-pathname
    (format nil "~A~A~A"
            (kernel-tmp-file-prefix *kernel*)
            (murmur-hash-2 (babel:string-to-octets code) (kernel-hash-seed *kernel*))
            (kernel-tmp-file-suffix *kernel*))))


(defgeneric evaluate-code (kernel code &optional source-path breakpoints)
  (:documentation "Evaluate code along with paged output. Evaluation results should be sent
  with `execute-result`. Errors should be returned as `(values ename evalue traceback)`")
  (:method (kernel code &optional source-path breakpoints)
    (declare (ignore kernel code source-path breakpoints))
    (values))
  (:method :after (kernel code &optional source-path breakpoints)
    (declare (ignore kernel code source-path breakpoints))
    (finish-output *stdout*)
    (finish-output *stderr*)))

(defgeneric evaluate-form (kernel stream source-path breakpoints &optional line column))


(defgeneric code-is-complete (kernel code)
  (:documentation "Check code for completeness. Kernel implementations should
  result one of the permitted values of complete, incomplete, unknown or
  invalid.")
  (:method (kernel code)
    (declare (ignore kernel code))
    "unknown"))


(defgeneric inspect-code (kernel code cursor-pos detail-level)
  (:documentation "Inspect code at cursor-pos with detail-level. Successful
  inspection should return a single result that implements mime-bundle-data and
  optionally mime-bundle-metadata. Errors should be returned as
  `(values nil ename evalue traceback)`.")
  (:method (kernel code cursor-pos detail-level)
    (declare (ignore kernel code cursor-pos detail-level))
    (text "No results found.")))


(defgeneric complete-code (kernel match-set code cursor-pos)
  (:documentation "Complete code at cursor-pos. Successful matches should be added to match-set
  via match-set-add. Errors should be returned as `(values ename evalue traceback)`.")
  (:method (kernel match-set code cursor-pos)
    (declare (ignore kernel match-set code cursor-pos))
    (values)))


(defgeneric debug-initialize (kernel)
  (:documentation "Perform any kernel specific initialization of the debugger and return capabilities."))


(defgeneric debug-abort (kernel environment)
  (:documentation "Abort the current stopped thread."))


(defgeneric debug-continue (kernel environment &optional restart-number)
  (:documentation "Continue execution of the stopped thread."))


(defgeneric debug-in (kernel environment)
  (:documentation "Step into a function on the stopped thread."))


(defgeneric debug-out (kernel environment)
  (:documentation "Step out on the stopped thread."))


(defgeneric debug-next (kernel environment)
  (:documentation "Step to the next form on the stopped thread."))


(defgeneric debug-new-breakpoint (kernel source line)
  (:documentation "Create a new breakpoint or return NIL if not possible"))


(defgeneric debug-remove-breakpoint (kernel source breakpoint)
  (:documentation "Remove a specific breakpoint"))


(defgeneric debug-activate-breakpoints (kernel source breakpoints)
  (:documentation "Activate a breakpoint."))


(defgeneric debug-object-children-resolve (instance)
  (:documentation "Return a list of debug-objects for the children of the instance.")
  (:method (instance)
    (declare (ignore instance))
    nil))


(defgeneric debug-dump-cell (kernel code source-path)
  (:documentation "Save the code to the provided source-path.")
  (:method (kernel code source-path)
    (declare (ignore kernel))
    (let ((path (translate-logical-pathname source-path)))
      (ensure-directories-exist path)
      (with-open-file (stream path :direction :output :if-exists :supersede)
        (write-string code stream)))))


(defgeneric debug-evaluate-code (kernel environment code frame context)
  (:documentation "Evaluate code in the context of a frame"))


(defgeneric debug-evaluate-form (kernel environment stream frame context)
  (:documentation "Evaluate a single form in the context of a frame"))


(defgeneric debug-inspect-variables (kernel environment)
  (:documentation "Return a list of debug-objects represents the variables in the global scope."))

(defgeneric debug-modules (kernel)
  (:documentation "Return a list of debug-modules representing the modules available.")
  (:method (kernel)
    (declare (ignore kernel))
    nil))

(defclass debug-source ()
  ((name
     :reader debug-source-name
     :initarg :name
     :type string
     :documentation "The name associated with the source.")
   (path
     :reader debug-source-path
     :initarg :path
     :type (or pathname string)
     :documentation "The path of the source."))
  (:documentation "A source reference in the debugger."))


(defmethod shasht:print-json-key-value :around ((object debug-source) key value output-stream)
  (let ((shasht:*symbol-name-function* #'symbol-to-camel-case))
    (call-next-method)))


(defclass debug-configuration ()
  ((source
     :reader debug-configuration-source
     :initarg :source
     :type (or pathname string)
     :documentation "The source reference")
   (breakpoints
     :accessor debug-configuration-breakpoints
     :initarg :breakpoints
     :initform nil
     :type list
     :documentation "A list of the current breakpoints"))
  (:documentation "A debug configuration for a source."))


(defmethod shasht:print-json-key-value :around ((object debug-configuration) key value output-stream)
  (let ((shasht:*symbol-name-function* #'symbol-to-camel-case))
    (call-next-method)))


(defclass debug-breakpoint ()
  ((line
     :reader debug-breakpoint-line
     :initarg :line
     :type integer
     :documentation "The line number associated with the breakpoint.")
   (data
     :accessor debug-breakpoint-data
     :initarg :data
     :initform nil
     :documentation "Implementation specific data for the breakpoint"))
  (:documentation "A line oriented breakpoint."))


(defmethod shasht:print-json-key-value :around ((object debug-breakpoint) key value output-stream)
  (when (equal 'line key)
    (let ((shasht:*symbol-name-function* #'symbol-to-camel-case))
      (call-next-method))))


(defclass debug-environment ()
  ((frames
     :accessor debug-environment-frames
     :initarg :frames
     :documentation "The frames associated with the environment")
   (objects
     :reader debug-environment-objects
     :initform (make-array 32 :adjustable t :fill-pointer 0)
     :documentation "The debug-objects created in the environment. The id number is the same as the index in the array.")
   (condition
     :accessor debug-environment-condition
     :initarg :condition
     :documentation "The condition which caused the debugger to be entered.")
   (restarts
     :accessor debug-environment-restarts
     :initarg :restarts
     :initform nil
     :documentation "Applicable restarts for the environment."))
  (:documentation "A debug environment for a stopped thread."))


(defclass debug-object ()
  ((id
     :accessor debug-object-id
     :initarg :id
     :type integer
     :documentation "The id of the object.")
   (environment
     :accessor debug-object-environment
     :initarg :environment
     :documentation "An environment reference for object. If the object does not have children then
     it doesn't need an environment reference.")
   (parent
     :accessor debug-object-parent
     :initarg :parent
     :documentation "The parent object")
   (children
     :accessor debug-object-children
     :documentation "Children of the object, i.e. scopes, slots, etc.")
   (name
     :accessor debug-object-name
     :initarg :name
     :type string
     :documentation "The name of the object.")
   (data
     :accessor debug-object-data
     :initarg :data
     :documentation "Implementation specific data associated with the object."))
  (:documentation "A debug object. Superclass of frames, scopes, variables, etc."))

(defclass debug-module (debug-object)
  ((path
     :reader debug-object-path
     :initarg :path
     :type (or pathname string)
     :documentation "The path of the source."))
  (:documentation "A module in the debugger."))

(defmethod shasht:print-json-key-value :around ((object debug-module) key value output-stream)
  (when (member key '(id name path) :test #'equal)
    (let ((shasht:*symbol-name-function* #'symbol-to-camel-case))
      (call-next-method))))

(defmethod debug-object-children :before ((instance debug-object))
  (unless (slot-boundp instance 'children)
    (setf (debug-object-children instance)
          (debug-object-children-resolve instance))
    (when (slot-boundp instance 'environment)
      (dolist (child (debug-object-children instance))
        (setf (debug-object-environment child) (debug-object-environment instance))
        (register-debug-object child)))))


(defun register-debug-object (instance)
  (when (and (not (slot-boundp instance 'id))
             (slot-boundp instance 'environment))
    (let ((id (vector-push-extend instance (debug-environment-objects (debug-object-environment instance)))))
      (setf (debug-object-id instance) (logior (ash id +thread-bits+) *thread-id*)))))


(defmethod initialize-instance :after ((instance debug-object) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (register-debug-object instance))


(defclass debug-frame (debug-object)
  ((source
     :accessor debug-object-source
     :initarg :source
     :documentation "The source reference of the frame.")
   (line
     :accessor debug-object-line
     :initarg :line
     :initform 0
     :type integer
     :documentation "A line number of the frame.")
   (column
     :accessor debug-object-column
     :initarg :column
     :initform 0
     :type integer
     :documentation "The column number of the frame."))
  (:documentation "A debugger frame."))


(defmethod shasht:print-json-key-value :around ((object debug-frame) key value output-stream)
  (when (member key '(id name source line column) :test #'equal)
    (let ((shasht:*symbol-name-function* #'symbol-to-camel-case))
      (call-next-method))))


(defclass debug-scope (debug-object)
  ((presentation-hint
     :reader debug-presentation-hint
     :initarg :presentation-hint
     :type string
     :documentation "Any presentation hints associated with the scope."))
  (:documentation "A scope inside a frame."))


(defmethod shasht:print-json-key-value :around ((object debug-scope) key value output-stream)
  (cond
    ((equal key 'id)
      (call-next-method object "variablesReference" value output-stream))
    ((member key '(name presentation-hint) :test #'equal)
      (let ((shasht:*symbol-name-function* #'symbol-to-camel-case))
        (call-next-method)))))


(defclass debug-variable (debug-object)
  ((value
     :accessor debug-object-value
     :initarg :value
     :initform "UNBOUND"
     :type string
     :documentation "A printed representation of the value.")
   (type
     :accessor debug-object-type
     :initarg :type
     :type string
     :documentation "The type associated with the variable"))
  (:documentation "A debugger variable"))


(defmethod shasht:print-json-key-value :around ((object debug-variable) key value output-stream)
  (cond
    ((equal key 'id)
      (call-next-method object "variablesReference" value output-stream))
    ((member key '(name value type) :test #'equal)
      (let ((shasht:*symbol-name-function* #'symbol-to-camel-case))
        (call-next-method)))))


(defmethod initialize-instance :after ((instance debug-environment) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (when (slot-boundp instance 'frames)
    (dolist (frame (debug-environment-frames instance))
      (setf (debug-object-environment frame) instance)
      (register-debug-object frame))))


(defun debug-environment-object (environment id)
  "Get a debug-object associated with a specific id."
  (aref (debug-environment-objects environment) (ash id (- +thread-bits+))))


(defmacro handling-errors (&body body)
  "Macro for catching any conditions during code evaluation."
  (let ((traceback (gensym)))
    `(let (,traceback)
       (handler-case
          (handler-bind
              ((warning
                 (lambda (wrn)
                   (format t "~S: ~A~%" (type-of wrn) wrn)
                   (muffle-warning)))
               (serious-condition
                 (lambda (err)
                   (let ((env (dissect:capture-environment err)))
                     (setf ,traceback
                           (mapcar (lambda (frame)
                                     (dissect:present frame nil))
                                   (dissect:environment-stack env)))
                     #+(or)(dissect:present err *error-output*)))))
            (progn ,@body (values)))
         (simple-error (err)
           (make-eval-error err
                            (apply #'format nil (simple-condition-format-control err)
                                   (simple-condition-format-arguments err))
                            ,traceback))
         (simple-type-error (err)
           (make-eval-error err
                            (apply #'format nil (simple-condition-format-control err)
                                   (simple-condition-format-arguments err))
                            ,traceback))
         (serious-condition (err)
           (make-eval-error err (format nil "~A" err)
                            ,traceback))))))


(defmacro handling-comm-errors (&body body)
  "Macro for catching any conditions during comm messages."
  (let ((ename-var (gensym))
        (evalue-var (gensym))
        (traceback-var (gensym)))
    `(multiple-value-bind (,ename-var ,evalue-var ,traceback-var)
                          (handling-errors ,@body)
       (declare (ignore ,evalue-var))
       (when ,ename-var
         (format *error-output* "~{~A~%~}" ,traceback-var)
         (finish-output *error-output*))
       (values))))


(defmacro handling-control-errors (&body body)
  "Macro for catching any conditions during CONTROL messages."
  (let ((ename-var (gensym))
        (evalue-var (gensym))
        (traceback-var (gensym)))
    `(multiple-value-bind (,ename-var ,evalue-var ,traceback-var)
                          (handling-errors ,@body)
       (declare (ignore ,evalue-var))
       (when ,ename-var
         (inform :error *kernel* "~{~A~%~}" ,traceback-var))
       (values))))


(defun control-debugger-hook (condition me-or-my-encapsulation)
  (declare (ignore me-or-my-encapsulation))
  (cond ((typep condition 'warning)
         (inform :warning "[~S] ~A~%" (type-of condition) condition)
         (muffle-warning))
        (t
         (inform :error "[~S] ~A~%" (type-of condition) condition)
         (abort))))

(defun shell-debugger-hook (condition me-or-my-encapsulation)
  (declare (ignore me-or-my-encapsulation))
  (cond ((typep condition 'warning)
         (format *standard-output* "[~S] ~A~%" (type-of condition) condition)
         (finish-output *standard-output*)
         (muffle-warning))
        (t
         (let ((env (dissect:capture-environment condition)))
           (format *error-output* "[~S] ~A~%" (type-of condition) condition)
           (finish-output *error-output*)
           (throw 'debug-error
             (make-eval-error condition (format nil "~A" condition)
                              (mapcar (lambda (frame)
                                        (dissect:present frame nil))
                                      (dissect:environment-stack env))))))))

(defun debugger-type ()
  (cond ((or (not *enable-debugger*)
             #+clasp (core:debugger-disabled-p)
             #+sbcl (eq sb-ext:*invoke-debugger-hook* 'sb-debug::debugger-disabled-hook))
         :none)
        #+abcl
        (sys::*invoke-debugger-hook* :external)
        #+allegro
        (excl::*break-hook* :external)
        #+ccl
        (ccl:*break-hook* :external)
        #+clisp
        (sys::*break-driver* :external)
        #+clasp
        (ext:*invoke-debugger-hook* :external)
        #+ecl
        (ext:*invoke-debugger-hook* :external)
        #+lispworks
        (dbg::*debugger-wrapper-list* :external)
        #+mezzano
        (mezzano.debug:*global-debugger* :external)
        #+sbcl
        (sb-ext:*invoke-debugger-hook* :external)
        (*enable-internal-debugger* :internal)
        (t :none)))

(defmacro with-debugger ((&key control internal) &body body)
  (let ((debugger-hook (if control
                           'control-debugger-hook
                           'shell-debugger-hook)))
    `(flet ((body-func ()
              (catch 'debug-error
                (with-simple-restart
                    (abort "Exit debugger, returning to top level.")
                  ,@body))))
       (case (debugger-type)
         (:external
          (body-func))
         ,@(when internal
             #+clasp
             `((:internal
                (catch sys::*quit-tag*
                  (body-func))))
             #-clasp
             `((:internal
                (body-func))))
         (otherwise
          (let ((*debugger-hook* ',debugger-hook)
                #+sbcl      (sb-ext:*invoke-debugger-hook* ',debugger-hook)
                #+ccl       (ccl:*break-hook* ',debugger-hook)
                #+ecl       (ext:*invoke-debugger-hook* ',debugger-hook)
                #+clasp     (ext:*invoke-debugger-hook* ',debugger-hook)
                #+abcl      (sys::*invoke-debugger-hook* ',debugger-hook)
                #+clisp     (sys::*break-driver* (lambda (continuable &optional condition print)
                                                   (declare (ignore continuable print))
                                                   (,debugger-hook condition nil)))
                #+allegro   (excl::*break-hook* (lambda (&rest args)
                                                  (,debugger-hook (fifth args))))
                #+lispworks (dbg::*debugger-wrapper-list* (lambda (function condition)
                                                            (declare (ignore function))
                                                            (,debugger-hook condition nil)))
                #+mezzano   (mezzano.debug:*global-debugger* (lambda (condition)
                                                               (,debugger-hook condition nil))))
            (body-func)))))))

(defun debug-enter-loop ()
  "Re-enter the debug loop after a restart which implements a debugger command."
  (throw 'enter-loop t))


(defun debug-stop (reason environment)
  "Enter a stopped state on the current thread. This function will dispatch messages received from
  the CONTROL thread. Resumption of the thread is done through continue restarts so this function
  will not return."
  (finish-output *markdown-output*)
  (finish-output *html-output*)
  (finish-output)
  (finish-output *error-output*)
  (with-debugger ()
    (with-slots (stopped queue)
                (aref (kernel-threads *kernel*) *thread-id*)
      (setf stopped t)
      (send-debug-event "stopped"
                        (list :object-plist
                              "reason" reason
                              "restarts" (or (when (slot-boundp environment 'restarts)
                                               (mapcar (lambda (restart)
                                                         (list :object-plist
                                                               "name" (restart-name restart)
                                                               "text" (write-to-string restart :escape nil :readably nil)))
                                                       (debug-environment-restarts environment)))
                                             :empty-array)
                              "threadId" *thread-id*))
      (prog ((*suspended-message* *message*)
             (*debug-environment* environment)
             *message*)
       wait
        (setf *message* (dequeue queue))
        (send-status-busy)
        (catch 'enter-loop
          (hash-case (gethash "command" (message-content *message*))
            ("continue"
              (handle-debug-request/continue environment))
            ("disconnect"
              (debug-abort *kernel* environment))
            ("evaluate"
              (handle-debug-request/evaluate environment))
            ("stepIn"
              (handle-debug-request/step-in environment))
            ("stepOut"
              (handle-debug-request/step-out environment))
            ("next"
              (handle-debug-request/next environment))
            ("scopes"
              (handle-debug-request/scopes environment))
            ("stackTrace"
              (handle-debug-request/stack-trace environment))
            ("variables"
              (handle-debug-request/variables environment)))
          (send-status-idle))
        (go wait)))))


;; Start all channels.
(defmethod start ((k kernel))
  (with-slots (connection-file control-port ctx hb hb-port history iopub
               iopub-port ip key language-name mac name prompt-prefix prompt-suffix
               session shell shell-port signature-scheme sink stdin stdin-port control
               transport error-output standard-output standard-input tmp-file-prefix
               tmp-file-suffix hash-seed file-extension)
              k
    (setf sink (make-instance 'sink
                              :path (uiop:xdg-runtime-dir
                              (make-pathname :directory '(:relative "common-lisp-jupyter")
                                             :name (pathname-name connection-file)
                                             :type "log"))))
    (start sink)
    (inform :info k "Starting ~A kernel" name)
    (inform :info k "Parsing connection file ~A" connection-file)
    (let* ((config-js (shasht:read-json (alexandria:read-file-into-string connection-file)))
           (encoded-key (gethash "key" config-js)))
      (setq transport (gethash "transport" config-js)
            ip (gethash "ip" config-js)
            shell-port (gethash "shell_port" config-js)
            stdin-port (gethash "stdin_port" config-js)
            iopub-port (gethash "iopub_port" config-js)
            control-port (gethash "control_port" config-js)
            hb-port (gethash "hb_port" config-js)
            key (if (string= encoded-key "")
                  nil
                  (babel:string-to-octets encoded-key :encoding :ASCII))
            signature-scheme (gethash "signature_scheme" config-js)))
    (setf *uuid-random-state* (make-random-state t)
          session (make-uuid)
          hash-seed (random #xffffffff *uuid-random-state*)
          (logical-pathname-translations "CELL") (list (list "*.*.*"
                                                             (namestring (merge-pathnames (uiop:default-temporary-directory)
                                                                                          (make-pathname :directory `(:relative ,session)
                                                                                                         :name :wild
                                                                                                         :type :wild)))))
          tmp-file-prefix "CELL:"
          tmp-file-suffix (concatenate 'string (string-upcase file-extension) #+ccl ".newest" #-ccl ".NEWEST")
          ctx (pzmq:ctx-new)
          mac (make-instance 'mac
                             :sink sink
                             :key key
                             :signature-scheme signature-scheme)
          hb (make-instance 'hb-channel
                            :sink sink
                            :session session
                            :mac mac
                            :socket (pzmq:socket ctx :rep)
                            :transport transport
                            :ip ip
                            :port hb-port)
          iopub (make-instance 'iopub-channel
                               :sink sink
                               :session session
                               :mac mac
                               :socket (pzmq:socket ctx :pub)
                               :transport transport
                               :ip ip
                               :port iopub-port
                               :prompt-prefix prompt-prefix
                               :prompt-suffix prompt-suffix)
          shell (make-instance 'shell-channel
                               :sink sink
                               :session session
                               :mac mac
                               :socket (pzmq:socket ctx :router)
                               :transport transport
                               :ip ip
                               :port shell-port)
          stdin (make-instance 'stdin-channel
                               :sink sink
                               :session session
                               :mac mac
                               :socket (pzmq:socket ctx :router)
                               :transport transport
                               :ip ip
                               :port stdin-port)
          control (make-instance 'control-channel
                                 :sink sink
                                 :session session
                                 :mac mac
                                 :socket (pzmq:socket ctx :router)
                                 :transport transport
                                 :ip ip
                                 :port control-port)
          history (make-instance 'history
                                 :sink sink
                                 :path (uiop:xdg-data-home
                                         (make-pathname :directory '(:relative "common-lisp-jupyter")
                                                        :name language-name
                                                        :type "history")))
          error-output (make-iopub-stream iopub '*error-output*)
          standard-output (make-iopub-stream iopub '*standard-output*)
          standard-input (make-stdin-stream stdin iopub))
    (start mac)
    (start hb)
    (start iopub)
    (start shell)
    (start stdin)
    (start control)
    (start history)
    (send-status iopub "starting")
    (send-status iopub "idle")
    (setf (kernel-shell-thread k)
          (bordeaux-threads:make-thread (lambda ()
                                          (run-shell k))
                                        :name "Jupyter Shell"))))


;; Stop all channels and destroy the control.
(defmethod stop ((k kernel))
  (with-slots (sink ctx hb iopub shell stdin control history mac name) k
    (inform :info k "Stopping ~A kernel" name)
    (when (bordeaux-threads:thread-alive-p (kernel-shell-thread k))
      (bordeaux-threads:destroy-thread (kernel-shell-thread k)))
    (stop hb)
    (stop iopub)
    (stop shell)
    (stop stdin)
    (stop control)
    (stop mac)
    (stop history)
    (stop sink)
    (pzmq:ctx-destroy ctx)
    (uiop:quit)))


(defun run-shell (kernel)
  (pzmq:with-poll-items items (((channel-socket (kernel-shell kernel)) :pollin))
    (catch 'kernel-shutdown
      (prog* ((shell (kernel-shell kernel))
              (iopub (kernel-iopub kernel))
              (*thread-id* (add-thread kernel))
              (*stdin* (kernel-standard-input kernel))
              (*stdout* (kernel-standard-output kernel))
              (*stderr* (kernel-error-output kernel))
              (*standard-input* (make-synonym-stream '*stdin*))
              (*standard-output* (make-synonym-stream '*stdout*))
              (*error-output* (make-synonym-stream '*stderr*))
              (*debug-io* (make-two-way-stream *standard-input* *standard-output*))
              (*query-io* *debug-io*)
              (*terminal-io* *debug-io*)
              (*trace-output* *standard-output*)
              (*html-output* (kernel-html-output kernel))
              (*markdown-output* (kernel-markdown-output kernel))
              (*kernel* kernel)
              *message* msg-type)
       poll
        (catch 'kernel-interrupt
          (when (zerop (pzmq:poll items +zmq-poll-timeout+))
            #+cmucl (bordeaux-threads:thread-yield)
            (go poll))
          (setf *message* (message-recv shell)
                msg-type (gethash "msg_type" (message-header *message*)))
          (send-status iopub "busy")
          (unwind-protect
              (hash-case msg-type
                ("comm_close"
                  (handle-comm-close))
                ("comm_info_request"
                  (handle-comm-info-request))
                ("comm_msg"
                  (handle-comm-message))
                ("comm_open"
                  (handle-comm-open))
                ("complete_request"
                  (handle-complete-request))
                ("execute_request"
                  (handle-execute-request))
                ("history_request"
                  (handle-history-request))
                ("inspect_request"
                  (handle-inspect-request))
                ("is_complete_request"
                  (handle-is-complete-request))
                ("kernel_info_request"
                  (handle-kernel-info-request shell))
                (otherwise
                  (inform :warn kernel "Ignoring ~A shell message since there is no appropriate handler." msg-type)))
            ;; send any remaining stdout
            (finish-output *standard-output*)
            ;; send any remaining stderr
            (finish-output *error-output*)
            ;; send any remaining HTML
            (finish-output *html-output*)
            ;; send any remaining markdown
            (finish-output *markdown-output*)
            (send-status iopub "idle")))
        (go poll))))
  (inform :info kernel "Shell thread exiting normally."))


(defun run-control (kernel)
  #+sbcl (sb-thread:release-foreground)
  (pzmq:with-poll-items items (((channel-socket (kernel-control kernel)) :pollin))
    (prog ((*thread-id* 0)
           (environment (make-instance 'debug-environment))
           (control (kernel-control kernel))
           (iopub (kernel-iopub kernel))
           (thread-message-types (alexandria:plist-hash-table
                                   '("debug_request/continue" "threadId"
                                     "debug_request/evaluate" "frameId"
                                     "debug_request/next" "threadId"
                                     "debug_request/stackTrace" "threadId"
                                     "debug_request/stepIn" "threadId"
                                     "debug_request/stepOut" "threadId"
                                     "debug_request/scopes" "frameId"
                                     "debug_request/variables" "variablesReference")
                                   :test #'equal))
           (*kernel* kernel)
           *message* msg-type id-field)
     poll
      (when (zerop (pzmq:poll items +zmq-poll-timeout+))
        #+cmucl (bordeaux-threads:thread-yield)
        (go poll))
      (with-debugger (:control t)
        (setf *message* (message-recv control)
              msg-type (format nil "~A~@[/~A~]"
                               (gethash "msg_type" (message-header *message*))
                               (gethash "command" (message-content *message*)))
              id-field (gethash msg-type thread-message-types))
        (when id-field
          (let ((thread-id (logand +thread-mask+
                                   (gethash id-field
                                            (gethash "arguments"
                                                     (message-content *message*))))))
            (unless (zerop thread-id)
              (enqueue (thread-queue (aref (kernel-threads kernel) thread-id))
                                           *message*)
              (go poll))))
        (send-status iopub "busy")
        (unwind-protect
            (hash-case msg-type
              ("kernel_info_request"
                (handle-kernel-info-request control))
              ("interrupt_request"
                (handle-interrupt-request))
              ("shutdown_request"
                (handle-shutdown-request)
                (return))
              ("debug_request/attach"
                (handle-debug-request/attach))
              ("debug_request/configurationDone"
                (handle-debug-request/configuration-done))
              ("debug_request/debugInfo"
                (handle-debug-request/debug-info))
              ("debug_request/disconnect"
                (handle-debug-request/disconnect))
              ("debug_request/dumpCell"
                (handle-debug-request/dump-cell))
              ("debug_request/initialize"
                (handle-debug-request/initialize))
              ("debug_request/inspectVariables"
                (handle-debug-request/inspect-variables environment))
              ("debug_request/modules"
                (handle-debug-request/modules))
              ("debug_request/setBreakpoints"
                (handle-debug-request/set-breakpoints))
              ("debug_request/source"
                (handle-debug-request/source))
              ("debug_request/variables"
                (handle-debug-request/variables environment))
              (otherwise
                (inform :warn kernel "Ignoring ~A control message since there is no appropriate handler." msg-type)))
          (send-status iopub "idle")))
      (go poll))))


(defun run-kernel (kernel-class &optional (connection-file (first (uiop:command-line-arguments))))
  "Run a kernel based on a kernel class and a connection file."
  (unless (stringp connection-file)
    (error "Wrong connection file argument (expecting a string)"))
  (let ((kernel (make-instance kernel-class
                               :connection-file connection-file
                               :control-thread (bordeaux-threads:current-thread))))
    #+sbcl (setf (sb-thread:thread-name (bordeaux-threads:current-thread))
                 "Jupyter Control")
    (add-thread kernel)
    (start kernel)
    (unwind-protect
        (run-control kernel)
      (stop kernel))))


(defun make-eval-error (err msg traceback)
  (format *error-output* "~A: ~A~%~%" (symbol-name (class-name (class-of err))) msg)
  (finish-output *error-output*)
  (values (symbol-name (class-name (class-of err))) msg traceback))


(defun choose ()
  (write-string "Choice: " *query-io*)
  (finish-output *query-io*)
  (read))

; Trim restarts to only include ones from our debugger. If our debugger's exit restart cannot be
; found then don't present any restarts.
(defmethod initialize-instance :after ((instance dissect:environment) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (do ((restarts (dissect:environment-restarts instance) (cdr restarts)))
      ((null restarts) (setf (slot-value instance 'dissect:restarts) nil))
    (when (equal 'exit (dissect:name (car restarts)))
      (rplacd restarts nil)
      (return)))
  instance)


#|

### Message type: kernel_info_request ###

|#

(defun handle-kernel-info-request (channel)
  (inform :info *kernel* "Handling kernel_info_request message")
  (with-slots (name version language-name language-version mime-type session
               file-extension pygments-lexer codemirror-mode help-links banner)
              *kernel*
    (message-send channel
      (make-message session "kernel_info_reply"
                    `(:object-alist
                       ("status" . "ok")
                       ("protocol_version" . ,+KERNEL-PROTOCOL-VERSION+)
                       ("implementation" . ,name)
                       ("implementation_version" . ,version)
                       ("banner" . ,banner)
                       ("debugger" . :true)
                       ("help_links" . ,(or (mapcar
                                              (lambda (p)
                                                (list :object-plist
                                                      "text" (car p)
                                                      "url" (cdr p)))
                                              help-links)
                                            :empty-array))
                       ("language_info" . (:object-alist
                                            ("name" . ,language-name)
                                            ("version" . ,language-version)
                                            ("mimetype" . ,mime-type)
                                            ("file_extension" . ,file-extension)
                                            ("pygments_lexer" . ,pygments-lexer)
                                            ("codemirror_mode" . ,codemirror-mode))))))))

#|

### Message type: debug_request / attach ###

|#

(defun handle-debug-request/attach ()
  (inform :info *kernel* "Handling debug_request/attach message")
  (send-debug-reply))


#|

### Message type: debug_request / configurationDone ###

|#

(defun handle-debug-request/configuration-done ()
  (inform :info *kernel* "Handling debug_request/configurationDone message")
  (send-debug-reply))


#|

### Message type: debug_request / continue ###

|#

(defun handle-debug-request/continue (environment)
  (inform :info *kernel* "Handling debug_request/continue message")
  (send-debug-reply)
  (let ((restart-number (gethash "restart" (gethash "arguments" (message-content *message*))))
        (*debug-frame* (find (gethash "frameId" (gethash "arguments" (message-content *message*)))
                             (debug-environment-frames environment)
                             :key #'debug-object-id
                             :test #'equal)))
    (unless (and restart-number
                 (get (restart-name (elt (debug-environment-restarts environment) restart-number))
                      :debug-restart))
      (send-debug-event "continued")
      (setf (thread-stopped (aref (kernel-threads *kernel*) *thread-id*)) nil))
    (send-status-idle)
    (let ((*message* *suspended-message*))
      (debug-continue *kernel* environment restart-number))))

#|

### Message type: debug_request / debugInfo ###

|#

(defun handle-debug-request/debug-info ()
  (inform :info *kernel* "Handling debug_request/debugInfo message")
  (send-debug-reply
    (list :object-plist
          "isStarted" (if (kernel-debugger-started *kernel*) :true :false)
          "hashMethod" "Murmur2"
          "hashSeed" (kernel-hash-seed *kernel*)
          "tmpFilePrefix" (kernel-tmp-file-prefix *kernel*)
          "tmpFileSuffix" (kernel-tmp-file-suffix *kernel*)
          "stoppedThreads" (do ((threads (kernel-threads *kernel*))
                                (thread-id 0 (1+ thread-id))
                                stopped-threads)
                               ((= thread-id (length threads)) (or stopped-threads :empty-array))
                             (when (thread-stopped (aref threads thread-id))
                               (push thread-id stopped-threads)))
          "breakpoints" (or (let (bp)
                              (trivial-do:dohash (source config (kernel-breakpoints *kernel*) bp)
                                (declare (ignore source))
                                (push config bp)))
                            :empty-array))))

#|

### Message type: debug_request / disconnect ###

|#

(defun handle-debug-request/disconnect ()
  (inform :info *kernel* "Handling debug_request/disconnect message")
  (setf (kernel-debugger-started *kernel*) nil)
  (let ((thread (aref (kernel-threads *kernel*) 1)))
    (when thread
      (enqueue (thread-queue thread) *message*)))
  (send-debug-reply))


#|

### Message type: debug_request / dumpCell ###

|#

(defun handle-debug-request/dump-cell ()
  (inform :info *kernel* "Handling debug_request/dumpCell message")
  (let* ((code (gethash "code" (gethash "arguments" (message-content *message*))))
         (source-path (compute-source-path code)))
    (debug-dump-cell *kernel* code source-path)
    (send-debug-reply
      `(:object-alist
         ("sourcePath" . ,source-path)))))


#|

### Message type: debug_request / initialize ###

|#

(defun handle-debug-request/initialize ()
  (inform :info *kernel* "Handling debug_request/initialize message")
  (send-debug-reply
    (debug-initialize *kernel*))
  (setf (kernel-debugger-started *kernel*) t)
  (send-debug-event "initialized"))


#|

### Message type: debug_request / evaluate ###

|#

(defun handle-debug-request/evaluate (environment &aux)
  (inform :info *kernel* "Handling debug_request/evaluate message")
  (let* ((arguments (gethash "arguments" (message-content *message*)))
         (*message* *suspended-message*)
         (var (debug-evaluate-code *kernel* environment
                                   (gethash "expression" arguments)
                                   (debug-environment-object environment
                                                             (gethash "frameId" arguments))
                                   (gethash "context" arguments))))
    (send-debug-reply
      (list :object-plist
            "result" (debug-object-value var)
            "type" (debug-object-type var)
            "variablesReference" (debug-object-id var)))
    (send-status-idle)))

#|

### Message type: debug_request / inspectVariables ###

|#

(defun handle-debug-request/inspect-variables (environment)
  (inform :info *kernel* "Handling debug_request/inspectVariables message")
  (send-debug-reply
    (list :object-plist
          "variables" (or (debug-inspect-variables *kernel* environment)
                          :empty-array)))
  (send-status-idle))

#|

### Message type: debug_request / modules ###

|#

(defun handle-debug-request/modules ()
  (inform :info *kernel* "Handling debug_request/modules message")
  (let ((modules (debug-modules *kernel*)))
    (send-debug-reply
      `(:object-plist
        "modules" ,(or modules :empty-array)
        "totalModules" ,(length modules))))
  (send-status-idle))

#|

### Message type: debug_request / next ###

|#

(defun handle-debug-request/next (environment)
  (inform :info *kernel* "Handling debug_request/next message")
  (send-debug-reply)
  (send-debug-event "continued")
  (send-status-idle)
  (setf (thread-stopped (aref (kernel-threads *kernel*) *thread-id*)) nil)
  (let ((*message* *suspended-message*))
    (debug-next *kernel* environment)))


#|

### Message type: debug_request / scopes ###

|#

(defun handle-debug-request/scopes (environment)
  (inform :info *kernel* "Handling debug_request/scopes message")
  (send-debug-reply
    (list :object-plist
          "scopes" (or (debug-object-children
                         (debug-environment-object environment
                                                   (gethash "frameId"
                                                            (gethash "arguments"
                                                                     (message-content *message*)))))
                       :empty-array))))


#|

### Message type: debug_request / setBreakpoints ###

|#

(defun handle-debug-request/set-breakpoints ()
  (inform :info *kernel* "Handling debug_request/setBreakpoints message")
  (with-slots (breakpoints)
              *kernel*
    (let* ((arguments (gethash "arguments" (message-content *message*)))
           (source (parse-namestring (gethash "path" (gethash "source" arguments))))
           (new-breakpoints (coerce (gethash "breakpoints" arguments) 'list))
           (config (gethash source breakpoints)))
      (cond
        (new-breakpoints
          (unless config
            (setf (gethash source breakpoints)
                  (setf config (make-instance 'debug-configuration :source source))))
          (dolist (breakpoint (debug-configuration-breakpoints config))
            (when (notany (lambda (new-breakpoint)
                            (equal (debug-breakpoint-line breakpoint)
                                   (gethash "line" new-breakpoint)))
                          new-breakpoints)
              (debug-remove-breakpoint *kernel* source breakpoint)))
          (setf new-breakpoints
                (mapcar (lambda (new-breakpoint &aux (line (gethash "line" new-breakpoint)))
                          (or (find-if (lambda (breakpoint)
                                         (equal (debug-breakpoint-line breakpoint)
                                                line))
                                       (debug-configuration-breakpoints config))
                              (debug-new-breakpoint *kernel* source line)))
                        new-breakpoints))
          (setf (debug-configuration-breakpoints config)
                (remove nil new-breakpoints))
          (if (debug-configuration-breakpoints config)
            (debug-activate-breakpoints *kernel* source (debug-configuration-breakpoints config))
            (remhash source breakpoints))
          (send-debug-reply (list :object-plist
                                  "breakpoints"
                                  (mapcar (lambda (bp)
                                            (if bp
                                              (list :object-plist
                                                    "verified" :true
                                                    "source" (list :object-plist "path" source)
                                                    "line" (debug-breakpoint-line bp))
                                               '(:object-plist "verified" :false)))
                                          new-breakpoints))))
        (config
          (dolist (breakpoint (debug-configuration-breakpoints config))
            (debug-remove-breakpoint *kernel* source breakpoint))
          (remhash source breakpoints)
          (send-debug-reply '(:object-plist "breakpoints" :empty-array)))))))

#|

### Message type: debug_request / source ###

|#

(defun handle-debug-request/source ()
  (inform :info *kernel* "Handling debug_request/source message")
  (handler-case
      (send-debug-reply
        `(:object-alist
           ("content" . ,(alexandria:read-file-into-string (truename (gethash "path" (gethash "source" (gethash "arguments" (message-content *message*)))))))
           ("mimeType" . ,(kernel-mime-type *kernel*))))
    (error (err)
      (declare (ignore err))
      (send-debug-reply-failure "Unable to load source"))))


#|

### Message type: debug_request / stackTrace ###

|#

(defun handle-debug-request/stack-trace (environment)
  (inform :info *kernel* "Handling debug_request/stackTrace message")
  (send-debug-reply
    (list :object-plist
          "stackFrames" (or (debug-environment-frames environment)
                            :empty-array))))

#|

### Message type: debug_request / stepIn ###

|#

(defun handle-debug-request/step-in (environment)
  (inform :info *kernel* "Handling debug_request/stepIn message")
  (send-debug-reply)
  (send-debug-event "continued")
  (send-status-idle)
  (setf (thread-stopped (aref (kernel-threads *kernel*) *thread-id*)) nil)
  (let ((*message* *suspended-message*))
    (debug-in *kernel* environment)))


#|

### Message type: debug_request / stepOut ###

|#

(defun handle-debug-request/step-out (environment)
  (inform :info *kernel* "Handling debug_request/stepOut message")
  (send-debug-reply)
  (send-debug-event "continued")
  (send-status-idle)
  (setf (thread-stopped (aref (kernel-threads *kernel*) *thread-id*)) nil)
  (let ((*message* *suspended-message*))
    (debug-out *kernel* environment)))


#|

### Message type: debug_request / variables ###

|#

(defun handle-debug-request/variables (environment)
  (inform :info *kernel* "Handling debug_request/variables message")
  (send-debug-reply
    (list :object-plist
          "variables" (or (debug-object-children
                            (debug-environment-object
                              environment
                              (gethash "variablesReference"
                                       (gethash "arguments"
                                                (message-content *message*)))))
                          :empty-array))))

#|

### Message type: execute_request ###

|#

(defun handle-execute-request ()
  (inform :info *kernel* "Handling execute_request message")
  (with-slots (execution-count history iopub package readtable input-queue breakpoints)
              *kernel*
    (let* ((code (gethash "code" (message-content *message*)))
           (silent (gethash "silent" (message-content *message*)))
           (allow-stdin (gethash "allow_stdin" (message-content *message*)))
           (ename "interrupt")
           (evalue "Execution interrupted")
           traceback
           (*payload* (make-array 16 :adjustable t :fill-pointer 0))
           (*page-output* (make-string-output-stream))
           (*enable-debugger* (and (gethash "stop_on_error" (message-content *message*))
                                   *enable-debugger*))
           (*stdin* (if allow-stdin
                        *stdin*
                        (make-instance 'closed-input-stream)))
           (*stderr* (if silent
                         (make-broadcast-stream)
                         *stderr*))
           (*stdout* (if silent
                         (make-broadcast-stream)
                         *stdout*))
           (*enable-internal-debugger* (and (not silent)
                                            allow-stdin
                                            *enable-internal-debugger*)))
      (incf execution-count)
      (add-cell history execution-count code)
      (unwind-protect
          (let* ((*package* package)
                 (*readtable* readtable))
            (multiple-value-setq (ename evalue traceback)
                                 (if (kernel-debugger *kernel*)
                                   (let* ((source-path (compute-source-path code))
                                          (config (gethash source-path breakpoints)))
                                     (debug-dump-cell *kernel* code source-path)
                                     (evaluate-code *kernel* code source-path
                                                    (when config
                                                      (debug-configuration-breakpoints config))))
                                   (evaluate-code *kernel* code)))
            (setf package *package*)
            (setf readtable *readtable*))
        ;; broadcast the code to connected frontends
        (send-execute-code iopub execution-count code)
        (cond
          (ename
            (send-execute-error iopub ename evalue traceback)
            (send-execute-reply-error execution-count ename evalue traceback))
          (t
            (let ((p (get-output-stream-string *page-output*)))
              (unless (queue-empty-p input-queue)
                (set-next-input (dequeue input-queue)))
              (unless (zerop (length p))
                (page (text p)))
              (send-execute-reply-ok execution-count *payload*))))))))

#|

### Message type: shutdown_request ###

|#

(defun handle-shutdown-request ()
  (inform :info *kernel* "Handling shutdown_request message")
  (send-shutdown-reply (kernel-control *kernel*)
                       (gethash "restart" (message-content *message*)))
  (bordeaux-threads:interrupt-thread (kernel-shell-thread *kernel*)
                                     (lambda ()
                                       (throw 'kernel-shutdown nil))))

#|

### Message type: interrupt_request ###

|#

(defun handle-interrupt-request ()
  (inform :info *kernel* "Handling interrupt_request message")
  (bordeaux-threads:interrupt-thread (kernel-shell-thread *kernel*)
                                     (lambda ()
                                       (throw 'kernel-interrupt nil)))
  (sleep 1)
  (send-interrupt-reply (kernel-control *kernel*)))

#|

### Message type: is_complete_request ###

|#

(defun handle-is-complete-request ()
  (inform :info *kernel* "Handling is_complete_request message")
  (let* ((content (message-content *message*))
         (code (gethash "code" content))
         (status (code-is-complete *kernel* code)))
    (send-is-complete-reply status)))

#|

### Message type: inspect_request ###

|#

(defun handle-inspect-request ()
  (inform :info *kernel* "Handling inspect_request message")
  (with-slots (package readtable) *kernel*
    (let* ((content (message-content *message*))
           (code (gethash "code" content)))
      (multiple-value-bind (result ename evalue traceback)
                           (let ((*package* package)
                                 (*readtable* readtable))
                             (inspect-code *kernel*
                                           code
                                           (min (1- (length code)) (gethash "cursor_pos" content))
                                           (gethash "detail_level" content)))
      (if ename
        (send-inspect-reply-error ename evalue traceback)
        (send-inspect-reply-ok (mime-bundle-data result) (mime-bundle-metadata result)))))))

#|

### Message type: complete_request ###

|#

(defun handle-complete-request ()
  (inform :info *kernel* "Handling complete_request message")
  (with-slots (package readtable) *kernel*
    (let* ((content (message-content *message*))
           (code (gethash "code" content))
           (cursor-pos (gethash "cursor_pos" content))
           (match-set (make-match-set :start cursor-pos :end cursor-pos :code code)))
      (multiple-value-bind (ename evalue traceback)
          (let ((*package* package)
                (*readtable* readtable))
            (complete-code *kernel* match-set code cursor-pos))
        (cond (ename
               (send-complete-reply-error ename evalue traceback))
              (t
               (setf (match-set-matches match-set)
                     (sort (match-set-matches match-set) #'string< :key #'match-text))
               (send-complete-reply-ok (mapcar #'match-text (match-set-matches match-set))
                                       (match-set-start match-set)
                                       (match-set-end match-set)
                                       (list :object-plist
                                             "_jupyter_types_experimental" (or (mapcan (lambda (match)
                                                                                         (when (match-type match)
                                                                                           (list (list :object-plist
                                                                                                       "text" (match-text match)
                                                                                                       "type" (match-type match)))))
                                                                                       (match-set-matches match-set))
                                                                               :empty-array)))))))))

(defun handle-comm-info-request ()
  (inform :info *kernel* "Handling comm_info_request message")
  (let* ((content (message-content *message*))
         (target-name (gethash "target_name" content))
         (comms-alist (alexandria:hash-table-alist (kernel-comms *kernel*))))
    (send-comm-info-reply (if target-name
                            (remove-if-not (lambda (p) (equal target-name (cdr p)))
                              comms-alist)
                            comms-alist))))


(defun handle-comm-open ()
  (inform :info *kernel* "Handling comm_open message")
  (with-debugger ()
    (let* ((content (message-content *message*))
           (metadata (message-metadata *message*))
           (buffers (message-buffers *message*))
           (id (gethash "comm_id" content))
           (target-name (gethash "target_name" content))
           (data (gethash "data" content (make-object)))
           (inst (create-comm (intern target-name 'keyword) id data metadata buffers)))
      (cond
        (inst
          (setf (gethash id (kernel-comms *kernel*)) inst)
          (on-comm-open inst data metadata buffers))
        (t
          (send-comm-close-orphan id))))))


(defun handle-comm-message ()
  (inform :info *kernel* "Handling comm_msg message")
  (with-debugger ()
    (let* ((content (message-content *message*))
           (metadata (message-metadata *message*))
           (buffers (message-buffers *message*))
           (id (gethash "comm_id" content))
           (data (gethash "data" content))
           (inst (gethash id (kernel-comms *kernel*))))
      (if inst
        (on-comm-message inst data metadata buffers)
        (inform :error *kernel* "Received COMM message with unknown comm_id of ~A." id)))))


(defun handle-comm-close ()
  (inform :info *kernel* "Handling comm_close")
  (with-debugger ()
    (with-slots (comms iopub)
                *kernel*
      (let* ((content (message-content *message*))
             (metadata (message-metadata *message*))
             (buffers (message-buffers *message*))
             (id (gethash "comm_id" content))
             (data (gethash "data" content))
             (inst (gethash id comms)))
        (when inst
          (on-comm-close inst data metadata buffers)
          (remhash id comms))))))


(defun handle-history-request ()
  (inform :info *kernel* "Handling history_request message")
  (with-slots (history) *kernel*
    (let* ((content (message-content *message*))
           (output (gethash "output" content))
           (history-type (gethash "hist_access_type" content))
           (results (hash-case history-type
                      ("range" (history-range history
                                              (gethash "session" content)
                                              (gethash "start" content)
                                              (gethash "stop" content)))
                      ("search" (history-search history
                                                (gethash "n" content)
                                                (gethash "pattern" content)
                                                (gethash "unique" content)))
                      ("tail" (history-tail history
                                            (gethash "n" content))))))
      (send-history-reply
        (if output
          (mapcar
               (lambda (item)
                 (list (first item)
                       (second item)
                       (list (third item) :null)))
            results)
          results)))))


(defun set-next-input (text &optional (replace nil))
  (declare (ignore replace))
  (vector-push-extend (list :object-plist
                            "source" "set_next_input"
                            "text" text)
                      *payload*)
  (values))


(defun page (result &optional (start 0))
  (vector-push-extend (list :object-plist
                            "source" "page"
                            "data" (mime-bundle-data result)
                            "start" start)
                      *payload*)
  (values))


(defun quit (&optional keep-kernel)
  (vector-push-extend (list :object-plist
                            "source" "ask_exit"
                            "keepkernel" (if keep-kernel :true :false))
                      *payload*)
  (values))


(defun edit (path &optional (line-number 0))
  (vector-push-extend (list :object-plist
                            "source" "edit_magic"
                            "filename" path
                            "line_number" line-number)
                      *payload*)
  (values))


(defun enqueue-input (kernel code)
  "Add code to input queue."
  (enqueue (kernel-input-queue kernel) code))


(defun clear (&optional (wait nil))
  "Send clear output message to frontend."
  (send-clear-output (kernel-iopub *kernel*) wait))
