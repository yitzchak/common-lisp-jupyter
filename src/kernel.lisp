(in-package #:jupyter)

(defvar *payload* nil)
(defvar *debugger* nil)
(defvar *markdown-output* nil)
(defvar *html-output* nil)

(defvar *page-output* nil
  "Output stream sent to Jupyter pager. Available during calls to evaluate-code.")

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
     :documentation "Standard input stream"))
  (:documentation "Kernel state representation."))

(defgeneric evaluate-code (kernel code)
  (:documentation "Evaluate code along with paged output. Evaluation results should be sent
  with `execute-result`. Errors should be returned as `(values ename evalue traceback)`"))

(defmethod evaluate-code (kernel code))

(defgeneric code-is-complete (kernel code)
  (:documentation "Check code for completeness. Kernel implementations should
  result one of the permitted values of complete, incomplete, unknown or
  invalid."))

(defmethod code-is-complete (kernel code)
  "unknown")

(defgeneric inspect-code (kernel code cursor-pos detail-level)
  (:documentation "Inspect code at cursor-pos with detail-level. Successful
  inspection should return a single result that implements mime-bundle-data and
  optionally mime-bundle-metadata. Errors should be returned as
  `(values nil ename evalue traceback)`."))

(defmethod inspect-code (kernel code cursor-pos detail-level))

(defgeneric complete-code (kernel match-set code cursor-pos)
  (:documentation "Complete code at cursor-pos. Successful matches should be added to match-set
  via match-set-add. Errors should be returned as `(values ename evalue traceback)`."))

(defmethod complete-code (kernel match-set code cursor-pos))

;; Start all channels.
(defmethod start ((k kernel))
  (with-slots (connection-file control-port ctx hb hb-port history iopub
               iopub-port ip key language-name mac name prompt-prefix prompt-suffix
               session shell shell-port signature-scheme sink stdin stdin-port control
               transport error-output standard-output standard-input)
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
                               :port iopub-port)
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
          error-output (make-iopub-stream iopub "stderr" prompt-prefix prompt-suffix)
          standard-output (make-iopub-stream iopub "stdout" prompt-prefix prompt-suffix)
          standard-input (make-stdin-stream stdin))
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
                                        :name "SHELL Thread"))))

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
      (prog ((*kernel* kernel)
             *message* msg-type)
       poll
        (catch 'kernel-interrupt
          (when (zerop (pzmq:poll items 500))
            (go poll))
          (setf *message* (message-recv (kernel-shell kernel))
                msg-type (gethash "msg_type" (message-header *message*)))
          (send-status-update (kernel-iopub kernel) *message* "busy")
          (unwind-protect
              (alexandria:switch (msg-type :test #'equal)
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
                  (handle-kernel-info-request))
                (otherwise
                  (inform :warn kernel "Ignoring ~A message since there is no appropriate handler." msg-type)))
            (send-status-update (kernel-iopub kernel) *message* "idle")))
        (go poll))))
  (inform :info kernel "Shell thread exiting normally."))


(defun run-control (kernel)
  (pzmq:with-poll-items items (((channel-socket (kernel-control kernel)) :pollin))
    (prog ((*kernel* kernel)
           *message* msg-type)
      ;(declare (special *kernel* *message*))
     poll
      (when (zerop (pzmq:poll items 500))
        (go poll))
      (setf *message* (message-recv (kernel-control kernel))
            msg-type (format nil "~A~@[/~A~]"
                            (gethash "msg_type" (message-header *message*))
                            (gethash "command" (message-header *message*))))
      (send-status-update (kernel-iopub kernel) *message* "busy")
      (unwind-protect
          (alexandria:switch (msg-type :test #'equal)
            ("interrupt_request"
              (handle-interrupt-request))
            ("shutdown_request"
              (handle-shutdown-request)
              (return))
            (otherwise
              (inform :warn kernel "Ignoring ~A message since there is no appropriate handler." msg-type)))
        (send-status-update (kernel-iopub kernel) *message* "idle"))
      (go poll)))
  (inform :info kernel "Control thread exiting normally."))


(defun run-kernel (kernel-class &optional (connection-file (first (uiop:command-line-arguments))))
  "Run a kernel based on a kernel class and a connection file."
  (unless (stringp connection-file)
    (error "Wrong connection file argument (expecting a string)"))
  (let ((kernel (make-instance kernel-class
                               :connection-file connection-file
                               :control-thread (bordeaux-threads:current-thread))))
    (start kernel)
    (unwind-protect
        (run-control kernel)
      (stop kernel))))


(defun make-eval-error (err msg traceback)
  (values nil (symbol-name (class-name (class-of err))) msg traceback))


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

(defun my-debugger (condition me-or-my-encapsulation)
  (declare (ignore me-or-my-encapsulation))
  (let* ((restarts (compute-restarts condition))
         (applicable-restarts (subseq restarts 0 (1+ (position 'exit restarts :key #'restart-name :test #'equal)))))
    (dissect:present condition *error-output*)
    (finish-output *error-output*)
    (terpri)
    (terpri)
    (finish-output)
    (do ((choice (choose) (choose)))
        ((and (integerp choice)
              (< -1 choice (length applicable-restarts)))
         (invoke-restart-interactively (nth choice applicable-restarts))))))


(defmacro handling-errors (&body body)
  "Macro for catching any conditions during code evaluation."
  (let ((traceback (gensym)))
    `(let (,traceback)
       (handler-case
          (handler-bind
              ((warning
                 (lambda (wrn)
                   (format t "[~S] ~A~%" (type-of wrn) wrn)
                   (muffle-warning)))
               (serious-condition
                 (lambda (err)
                   (let ((env (dissect:capture-environment err)))
                     (setf ,traceback
                           (mapcar (lambda (frame)
                                     (dissect:present frame nil))
                                   (dissect:environment-stack env)))
                     (dissect:present env *error-output*)))))
            (progn ,@body))
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


(defmacro debugging-errors (&body body)
  `(if *debugger*
     (let ((*debugger-hook* #'my-debugger))
       (with-simple-restart (exit "Exit debugger, returning to top level.")
         ,@body))
     (handling-errors ,@body)))

(defmacro handling-comm-errors (&body body)
  "Macro for catching any conditions during comm messages."
  `(handler-case
       (handler-bind
           ((serious-condition
              (lambda (err)
                (inform :error *kernel* (dissect:present err nil))))
            (warning
              (lambda (wrn)
                (inform :warn *kernel* (dissect:present wrn nil))
                (muffle-warning))))
         (progn ,@body))
     (serious-condition (err)
       (declare (ignore err)))))


#|

### Message type: kernel_info_request ###

|#

(defun handle-kernel-info-request ()
  (inform :info *kernel* "Handling kernel_info_request message")
  (with-slots (name version language-name language-version mime-type session
               file-extension pygments-lexer codemirror-mode help-links banner
               shell)
              *kernel*
    (message-send shell
      (make-message session "kernel_info_reply"
                    `(:object-alist
                       ("protocol_version" . ,+KERNEL-PROTOCOL-VERSION+)
                       ("implementation" . ,name)
                       ("implementation_version" . ,version)
                       ("banner" . ,banner)
                       ("help_links" . ,(or (mapcar
                                              (lambda (p)
                                                (list :object-alist
                                                      (cons "text" (car p))
                                                      (cons "url" (cdr p))))
                                              help-links)
                                            :empty-array))
                       ("language_info" . (:object-alist
                                            ("name" . ,language-name)
                                            ("version" . ,language-version)
                                            ("mimetype" . ,mime-type)
                                            ("file_extension" . ,file-extension)
                                            ("pygments_lexer" . ,pygments-lexer)
                                            ("codemirror_mode" . ,codemirror-mode))))
      :parent *message*)))
  t)

#|

### Message type: execute_request ###

|#

(defun handle-execute-request ()
  (inform :info *kernel* "Handling execute_request message")
  (with-slots (execution-count history iopub package shell stdin input-queue html-output
               markdown-output error-output standard-output standard-input)
              *kernel*
    (let* ((code (gethash "code" (message-content *message*)))
           (ename "interrupt")
           (evalue "Execution interrupted")
           traceback
           (*payload* (make-array 16 :adjustable t :fill-pointer 0))
           (*page-output* (make-string-output-stream))
           (*query-io* standard-input)
           (*standard-input* standard-input)
           (*error-output* error-output)
           (*standard-output* standard-output)
           (*debug-io* standard-output)
           (*trace-output* standard-output)
           (*html-output* html-output)
           (*markdown-output* markdown-output))
      (incf execution-count)
      (add-cell history execution-count code)
      (unwind-protect
          (let* ((*package* package))
            (multiple-value-setq (ename evalue traceback)
                                 (evaluate-code *kernel* code))
            (setf package *package*))
        ;; broadcast the code to connected frontends
        (send-execute-code iopub *message* execution-count code)
        ;; send any remaining stdout
        (finish-output *standard-output*)
        ;; send any remaining stderr
        (finish-output *error-output*)
        ;; send any remaining HTML
        (finish-output *html-output*)
        ;; send any remaining markdown
        (finish-output *markdown-output*)
        ;; send reply (control)
        (cond
          (ename
            (send-execute-error iopub *message* ename evalue traceback)
            (send-execute-reply-error shell *message* execution-count ename evalue traceback))
          (t
            (let ((p (get-output-stream-string *page-output*)))
              (unless (queue-empty-p input-queue)
                (set-next-input (dequeue input-queue)))
              (unless (zerop (length p))
                (page p))
              (send-execute-reply-ok shell *message* execution-count *payload*)))))
      ;; return t if there is no quit errors present
      t)))

#|

### Message type: shutdown_request ###

|#

(defun handle-shutdown-request ()
  (inform :info *kernel* "Handling shutdown_request message")
  (let* ((control (kernel-control *kernel*))
         (content (message-content *message*))
         (restart (gethash "restart" content)))
    (send-shutdown-reply control *message* restart)
    (bordeaux-threads:interrupt-thread (kernel-shell-thread *kernel*) (lambda () (throw 'kernel-shutdown nil)))))

#|

### Message type: interrupt_request ###

|#

(defun handle-interrupt-request ()
  (inform :info *kernel* "Handling interrupt_request message")
  (let ((control (kernel-control *kernel*)))
    (bordeaux-threads:interrupt-thread (kernel-shell-thread *kernel*) (lambda () (throw 'kernel-interrupt nil)))
    (sleep 1)
    (send-interrupt-reply control *message*)))

#|

### Message type: is_complete_request ###

|#

(defun handle-is-complete-request ()
  (inform :info *kernel* "Handling is_complete_request message")
  (let* ((shell (kernel-shell *kernel*))
         (content (message-content *message*))
         (code (gethash "code" content))
         (status (code-is-complete *kernel* code)))
    (send-is-complete-reply shell *message* status)
    t))

#|

### Message type: inspect_request ###

|#

(defun handle-inspect-request ()
  (inform :info *kernel* "Handling inspect_request message")
  (with-slots (shell package) *kernel*
    (let* ((content (message-content *message*))
           (code (gethash "code" content)))
      (multiple-value-bind (result ename evalue traceback)
                           (let ((*package* package))
                             (inspect-code *kernel*
                                           code
                                           (min (1- (length code)) (gethash "cursor_pos" content))
                                           (gethash "detail_level" content)))
      (if ename
        (send-inspect-reply-error shell *message* ename evalue traceback)
        (send-inspect-reply-ok shell *message* (mime-bundle-data result) (mime-bundle-metadata result))))))
  t)

#|

### Message type: complete_request ###

|#

(defun handle-complete-request ()
  (inform :info *kernel* "Handling complete_request message")
  (with-slots (shell package) *kernel*
    (let* ((content (message-content *message*))
           (code (gethash "code" content))
           (cursor-pos (gethash "cursor_pos" content))
           (match-set (make-match-set :start cursor-pos :end cursor-pos :code code)))
      (multiple-value-bind (ename evalue traceback)
                           (let ((*package* package))
                             (complete-code *kernel* match-set code cursor-pos))
        (if ename
          (send-complete-reply-error shell *message* ename evalue traceback)
          (send-complete-reply-ok shell *message*
                                  (sort (mapcar #'match-text (match-set-matches match-set)) #'string<)
                                  (match-set-start match-set)
                                  (match-set-end match-set)
                                  `(:object-alist
                                     ("_jupyter_types_experimental" . ,(or (mapcan (lambda (match)
                                                                                     (when (match-type match)
                                                                                       (list (list :object-alist
                                                                                               (cons "text" (match-text match))
                                                                                               (cons "type" (match-type match))))))
                                                                                   (match-set-matches match-set))
                                                                           :empty-object)))))))))


(defun handle-comm-info-request ()
  (inform :info *kernel* "Handling comm_info_request message")
  (with-slots (shell comms) *kernel*
    (let* ((content (message-content *message*))
           (target-name (gethash "target_name" content))
           (comms-alist (alexandria:hash-table-alist comms)))
      (send-comm-info-reply shell *message*
                            (if target-name
                              (remove-if-not (lambda (p) (equal target-name (cdr p)))
                                comms-alist)
                              comms-alist))))
  t)

(defun handle-comm-open ()
  (inform :info *kernel* "Handling comm_open message")
  (with-slots (iopub comms stdin error-output standard-output standard-input)
              *kernel*
    (let* ((content (message-content *message*))
           (metadata (message-metadata *message*))
           (buffers (message-buffers *message*))
           (*query-io* standard-input)
           (*standard-input* standard-input)
           (*error-output* error-output)
           (*standard-output* standard-output)
           (*debug-io* standard-output)
           (*trace-output* standard-output)
           (id (gethash "comm_id" content))
           (target-name (gethash "target_name" content))
           (data (gethash "data" content (make-hash-table :test #'equal)))
           (inst (create-comm (intern target-name 'keyword) id data metadata buffers)))
      (if inst
        (progn
          (setf (gethash id comms) inst)
          (handling-comm-errors
            (on-comm-open inst data metadata buffers)))
        (send-comm-close-orphan iopub id))))
  t)

(defun handle-comm-message ()
  (inform :info *kernel* "Handling comm_msg message")
  (with-slots (comms stdin iopub error-output standard-output standard-input)
              *kernel*
    (let* ((content (message-content *message*))
           (metadata (message-metadata *message*))
           (buffers (message-buffers *message*))
           (*query-io* standard-input)
           (*standard-input* standard-input)
           (*error-output* error-output)
           (*standard-output* standard-output)
           (*debug-io* standard-output)
           (*trace-output* standard-output)
           (id (gethash "comm_id" content))
           (data (gethash "data" content))
           (inst (gethash id comms)))
      (when inst
        (handling-comm-errors
          (on-comm-message inst data metadata buffers)))))
  t)


(defun handle-comm-close ()
  (inform :info *kernel* "Handling comm_close")
  (with-slots (comms stdin iopub error-output standard-output standard-input)
              *kernel*
    (let* ((content (message-content *message*))
           (metadata (message-metadata *message*))
           (buffers (message-buffers *message*))
           (*query-io* standard-input)
           (*standard-input* standard-input)
           (*error-output* error-output)
           (*standard-output* standard-output)
           (*debug-io* standard-output)
           (*trace-output* standard-output)
           (id (gethash "comm_id" content))
           (data (gethash "data" content))
           (inst (gethash id comms)))
      (when inst
        (handling-comm-errors
          (on-comm-close inst data metadata buffers))
        (remhash id comms)))))


(defun handle-history-request ()
  (inform :info *kernel* "Handling history_request message")
  (with-slots (shell history) *kernel*
    (let* ((content (message-content *message*))
           (output (gethash "output" content))
           (history-type (gethash "hist_access_type" content))
           (results (alexandria:switch (history-type :test #'equal)
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
      (send-history-reply shell *message*
        (if output
          (mapcar
               (lambda (item)
                 (list (first item)
                       (second item)
                       (list (third item) :null)))
            results)
          results))))
          t)


(defun set-next-input (text &optional (replace nil))
  (declare (ignore replace))
  (vector-push-extend `(:object-alist
                         ("source" . "set_next_input")
                         ("text" . ,text))
                      *payload*)
  (values))


(defun page (result &optional (start 0))
  (vector-push-extend `(:object-alist
                         ("source" . "page")
                         ("data" . ,(mime-bundle-data result))
                         ("start" . ,start))
                      *payload*)
  (values))

(defun quit (&optional keep-kernel)
  (vector-push-extend `(:object-alist
                         ("source" . "ask_exit")
                         ("keepkernel" . ,(if keep-kernel :true :false)))
                      *payload*)
  (values))

(defun enqueue-input (kernel code)
  "Add code to input queue."
  (enqueue (kernel-input-queue kernel) code))

(defun clear (&optional (wait nil))
  "Send clear output message to frontend."
  (send-clear-output (kernel-iopub *kernel*) *message* wait))

