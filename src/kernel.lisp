(in-package #:jupyter)

(defvar *kernel* nil)
(defvar *message* nil)
(defvar *payload* nil)

(defvar *page-output* nil
  "Output stream sent to Jupyter pager. Available during calls to evaluate-code.")

(defclass kernel ()
  ((name :initarg :name
         :initform ""
         :reader kernel-name
         :documentation "Kernel name. Used as a unique identifier in kernel
         description.")
   (version :initarg :version
            :initform ""
            :reader kernel-version
            :documentation "Kernel version.")
   (banner :initarg :banner
           :initform ""
           :reader kernel-banner
           :documentation "Banner text used to describe kernel. Used in
           kernel_info_reply messages.")
   (language-name :initarg :language-name
                  :initform ""
                  :reader kernel-language-name
                  :documentation "Display name of implementation language. Used
                  in kernel_info_reply messages.")
   (language-version :initarg :language-version
                     :initform ""
                     :reader kernel-language-version
                     :documentation "Version of implementation language. Used in
                     kernel_info_reply messages.")
   (mime-type :initarg :mime-type
              :initform ""
              :reader kernel-mime-type
              :documentation "Default MIME type for source files. Used in
              kernel_info_reply messages.")
   (file-extension :initarg :file-extension
                   :initform ""
                   :reader kernel-file-extension
                   :documentation "Default file extension for source files. Used
                   in kernel_info_reply messages.")
   (pygments-lexer :initarg :pygments-lexer
                   :initform ""
                   :reader kernel-pygments-lexer
                   :documentation "Name of Pygments lexer for source files. Used
                   in kernel_info_reply messages.")
   (codemirror-mode :initarg :codemirror-mode
                    :initform ""
                    :reader kernel-codemirror-mode
                    :documentation "CodeMirror mode for source files. Used in
                    kernel_info_reply messages.")
   (help-links :initarg :help-links
               :initform nil
               :reader kernel-help-links
               :documentation "An association list of help links. The car is the
               description and the cdr is URL. Used in kernel_info_reply
               messages.")
   (package :initarg :package
            :initform nil
            :reader kernel-package
            :documentation "The name of the package in which evaluate-code,
            code-is-complete and others are called.")
   (transport :initarg :transport
              :reader kernel-transport
              :type string
              :documentation "Transport protocol from connection file.")
   (ip :initarg :ip
       :reader kernel-ip
       :type string
       :documentation "IP address from connection file.")
   (shell-port :initarg :shell-port
               :reader kernel-shell-port
               :type fixnum
               :documentation "SHELL port from connection file.")
   (stdin-port :initarg :stdin-port
               :reader kernel-stdin-port
               :type fixnum
               :documentation "STDIN port from connection file.")
   (iopub-port :initarg :iopub-port
               :reader kernel-iopub-port
               :type fixnum
               :documentation "IOPUB port from connection file.")
   (control-port :initarg :control-port
                 :reader kernel-control-port
                 :type fixnum
                 :documentation "CONTROL port from connection file.")
   (hb-port :initarg :hb-port
            :reader kernel-hb-port
            :type fixnum
            :documentation "HB port from connection file.")
   (signature-scheme :initarg :signature-scheme
                     :reader kernel-signature-scheme
                     :type string
                     :documentation "Signature scheme from connection file.")
   (key :initarg :key
        :reader kernel-key
        :documentation "Signing key from connection file.")
   (prompt-prefix :initarg :prompt-prefix
                  :initform (coerce '(#\Escape #\X) 'string)
                  :reader kernel-prompt-prefix
                  :documentation "String prefix using in *standard-output* to
                  indicate the start of prompt.")
   (prompt-suffix :initarg :prompt-suffix
                  :initform (coerce '(#\Escape #\\) 'string)
                  :reader kernel-prompt-suffix
                  :documentation "String suffix using in *standard-output* to
                  indicate the end of prompt.")
   (ctx :initform nil
        :accessor kernel-ctx
        :documentation "pzmq ctx handle.")
   (hb :initform nil
       :accessor kernel-hb
       :documentation "Heartbeat channel.")
   (shell :initform nil
          :accessor kernel-shell
          :documentation "SHELL channel.")
   (stdin :initform nil
          :accessor kernel-stdin
          :documentation "STDIN channel.")
   (iopub :initform nil
          :accessor kernel-iopub
          :documentation "IOPUB channel.")
   (session :initform nil
            :accessor kernel-session
            :documentation "Session identifier.")
   (input-queue :initarg :input-queue
                :initform (make-instance 'cl-containers:basic-queue)
                :reader kernel-input-queue
                :documentation "Input queue used to feed values into
                execute_result payloads.")
   (history-in :initform (make-array 64 :fill-pointer 0 :adjustable t)
               :reader kernel-history-in
               :documentation "History of execute_request input values.")
   (history-out :initform (make-array 64 :fill-pointer 0 :adjustable t)
                :reader kernel-history-out
                :documentation "History of execute_result output values.")
   (comms :initform (make-hash-table :test #'equal)
          :reader kernel-comms
          :documentation "Currently open comms."))
  (:documentation "Kernel state representation."))

(defgeneric evaluate-code (kernel code)
  (:documentation "Evaluate code along with paged output. Kernel implementations
  must return a list of evaluated results. Each result should be wrapped with an
  appropriate `result` class instance. Sending the results to the client will be
  handled by the calling method."))

(defmethod evaluate-code (kernel code))

(defgeneric code-is-complete (kernel code)
  (:documentation "Check code for completeness. Kernel implementations should
  result one of the permitted values of complete, incomplete, unknown or
  invalid."))

(defmethod code-is-complete (kernel code)
  "unknown")

(defgeneric inspect-code (kernel code cursor-pos detail-level)
  (:documentation "Inspect code at cursor-pos with detail-level. Successful
  inspection should return a single wrapped result."))

(defmethod inspect-code (kernel code cursor-pos detail-level))

(defgeneric complete-code (kernel code cursor-pos)
  (:documentation "Complete code at cursor-pos. Successful completion should
  return three values, first a list of strings, then the cursor start position
  and finally the cursor end position."))

(defmethod complete-code (kernel code cursor-pos))

;; Start all channels.
(defmethod start ((k kernel))
  (info "[kernel] Starting...~%")
  (with-slots (ctx key transport ip hb-port hb shell-port shell stdin-port stdin
               iopub-port iopub session prompt-prefix prompt-suffix)
              k
    (setq session (make-uuid))
    (setq ctx (pzmq:ctx-new))
    (setq hb (make-instance 'hb-channel
                            :key key
                            :socket (pzmq:socket ctx :rep)
                            :transport transport
                            :ip ip
                            :port hb-port))
    (setq iopub (make-instance 'iopub-channel
                               :key key
                               :socket (pzmq:socket ctx :pub)
                               :transport transport
                               :ip ip
                               :port iopub-port))
    (setq shell (make-instance 'iopub-channel
                               :key key
                               :socket (pzmq:socket ctx :router)
                               :transport transport
                               :ip ip
                               :port shell-port))
    (setq stdin (make-instance 'iopub-channel
                               :key key
                               :socket (pzmq:socket ctx :dealer)
                               :transport transport
                               :ip ip
                               :port stdin-port))
    (start hb)
    (start iopub)
    (start shell)
    (start stdin)
    (send-status iopub session "starting")
    (send-status iopub session "idle")))

;; Stop all channels and destroy the control.
(defmethod stop ((k kernel))
  (info "[kernel] Stopped.~%")
  (with-slots (ctx hb iopub shell stdin) k
    (stop hb)
    (stop iopub)
    (stop shell)
    (stop stdin)
    (pzmq:ctx-destroy ctx)))

(defun run-kernel (kernel-class connection-file-name)
  "Run a kernel based on a kernel class and a connection file."
  (info "[kernel] Connection file = ~A~%" connection-file-name)
  (unless (stringp connection-file-name)
    (error "[kernel] Wrong connection file argument (expecting a string)"))
  (let* ((config-js (jsown:parse (read-string-file connection-file-name)))
         (transport (json-getf config-js "transport"))
         (ip (json-getf config-js "ip"))
         (shell-port (json-getf config-js "shell_port"))
         (stdin-port (json-getf config-js "stdin_port"))
         (iopub-port (json-getf config-js "iopub_port"))
         (control-port (json-getf config-js "control_port"))
         (hb-port (json-getf config-js "hb_port"))
         (key (json-getf config-js "key"))
         (signature-scheme (json-getf config-js "signature_scheme")))
    (when (not (string= signature-scheme "hmac-sha256"))
      (error "[kernel] Signature scheme 'hmac-sha256' required, was provided ~S." signature-scheme))
    (iter
      (with kernel = (make-instance kernel-class
                                    :transport transport
                                    :ip ip
                                    :shell-port shell-port
                                    :stdin-port stdin-port
                                    :iopub-port iopub-port
                                    :control-port control-port
                                    :hb-port hb-port
                                    :signature-scheme signature-scheme
                                    :key (if (string= key "")
                                           nil
                                           (babel:string-to-octets key :encoding :ASCII))))
      (initially
        (start kernel))
      (for msg = (message-recv (kernel-shell kernel)))
      (send-status-update (kernel-iopub kernel) msg "busy")
      (while (handle-message kernel msg))
      (after-each
        (send-status-update (kernel-iopub kernel) msg "idle"))
      (finally-protected
        (stop kernel)))))

#|

### Message type: kernel_info_request ###

|#

(defun handle-message (kernel msg)
  (let ((msg-type (json-getf (message-header msg) "msg_type"))
        (*kernel* kernel)
        (*message* msg))
    (cond ((equal msg-type "kernel_info_request")
           (handle-kernel-info-request kernel msg))
          ((equal msg-type "execute_request")
           (handle-execute-request kernel msg))
          ((equal msg-type "shutdown_request")
           (handle-shutdown-request kernel msg))
          ((equal msg-type "is_complete_request")
           (handle-is-complete-request kernel msg))
          ((equal msg-type "inspect_request")
           (handle-inspect-request kernel msg))
          ((equal msg-type "complete_request")
           (handle-complete-request kernel msg))
          ((equal msg-type "comm_info_request")
           (handle-comm-info-request kernel msg))
          ((equal msg-type "comm_open")
           (handle-comm-open kernel msg))
          ((equal msg-type "comm_msg")
           (handle-comm-message kernel msg))
          ((equal msg-type "comm_close")
           (handle-comm-close kernel msg))
          (t
           (warn "[Shell] message type '~A' not supported, skipping..." msg-type)
           t))))

#|

### Message type: kernel_info_request ###

|#

(defun handle-kernel-info-request (kernel msg)
  (info "[kernel] Handling 'kernel_info_request'~%")
  (with-slots (name version language-name language-version mime-type
               file-extension pygments-lexer codemirror-mode help-links banner
               shell)
              kernel
    (message-send shell
      (make-message msg "kernel_info_reply"
        (jsown:new-js
          ("protocol_version" +KERNEL-PROTOCOL-VERSION+)
          ("implementation" name)
          ("implementation_version" version)
          ("banner" banner)
          ("help_links"
            (mapcar
              (lambda (p)
                (list :obj (cons "text" (car p)) (cons "url" (cdr p))))
              help-links))
          ("language_info"
            (jsown:new-js
              ("name" language-name)
              ("version" language-version)
              ("mimetype" mime-type)
              ("file_extension" file-extension)
              ("pygments_lexer" pygments-lexer)
              ("codemirror_mode" codemirror-mode))))))))

#|

### Message type: execute_request ###

|#

(defun handle-execute-request (kernel msg)
  (info "[kernel] Handling 'execute_request'~%")
  (let ((code (json-getf (message-content msg) "code")))
    (with-slots (shell iopub stdin history-in history-out prompt-prefix
                 prompt-suffix package)
                kernel
      (vector-push code history-in)
      (let* ((execution-count (length history-in))
             (*payload* (make-array 16 :adjustable t :fill-pointer 0))
             (*page-output* (make-string-output-stream))
             (*query-io* (make-stdin-stream stdin msg))
             (*standard-input* *query-io*)
             (*error-output* (make-iopub-stream iopub msg "stderr"
                                                prompt-prefix prompt-suffix))
             (*standard-output* (make-iopub-stream iopub msg "stdout"
                                                   prompt-prefix prompt-suffix))
             (*debug-io* *standard-output*)
             (results (let ((*package* (find-package package)))
                        (evaluate-code kernel code))))
        (dolist (result results)
          (send-result result)
          (vector-push result history-out))
        ;broadcast the code to connected frontends
        (send-execute-code iopub msg execution-count code)
        ;; send any remaining stdout
        (finish-output *standard-output*)
        ;; send any remaining stderr
        (finish-output *error-output*)
        ;; send reply (control)
        (let ((errors (remove-if-not #'eval-error-p results)))
          (if errors
            (let ((ename (format nil "~{~A~^, ~}" (mapcar #'error-result-ename errors)))
                  (evalue (format nil "~{~A~^, ~}" (mapcar #'error-result-evalue errors))))
              (send-execute-reply-error shell msg execution-count ename evalue))
            (let ((input-queue (kernel-input-queue kernel))
                  (p (get-output-stream-string *page-output*)))
              (unless (cl-containers:empty-p input-queue)
                (set-next-input (cl-containers:dequeue input-queue)))
              (unless (zerop (length p))
                (page (make-inline-result p)))
              (send-execute-reply-ok shell msg execution-count (coerce *payload* 'list)))))
        ;; return t if there is no quit errors present
        (notany #'quit-eval-error-p results)))))

#|

### Message type: shutdown_request ###

|#

(defun handle-shutdown-request (kernel msg)
  (info "[kernel] Handling 'shutdown_request'~%")
  (let* ((shell (kernel-shell kernel))
         (content (message-content msg))
         (restart (json-getf content "restart")))
    (send-shutdown-reply shell msg restart)
    nil))

#|

### Message type: is_complete_request ###

|#

(defun handle-is-complete-request (kernel msg)
  (info "[kernel] Handling 'is_complete_request'~%")
  (let* ((shell (kernel-shell kernel))
         (content (message-content msg))
         (code (json-getf content "code"))
         (status (code-is-complete kernel code)))
    (send-is-complete-reply shell msg status)
    t))

#|

### Message type: inspect_request ###

|#

(defun handle-inspect-request (kernel msg)
  (info "[kernel] Handling 'inspect_request'~%")
  (with-slots (shell package) kernel
    (let* ((content (message-content msg))
           (code (json-getf content "code"))
           (result (let ((*package* (find-package package)))
                     (inspect-code kernel
                              code
                              (min (1- (length code)) (json-getf content "cursor_pos"))
                              (json-getf content "detail_level")))))
      (if (eval-error-p result)
        (with-slots (ename evalue) result
          (send-inspect-reply-error shell msg ename evalue))
        (send-inspect-reply-ok shell msg
          (let ((*package* (find-package package)))
            (render result)))))))

#|

### Message type: complete_request ###

|#

(defun handle-complete-request (kernel msg)
  (info "[kernel] Handling 'complete_request'~%")
  (with-slots (shell package) kernel
    (let* ((content (message-content msg))
           (code (json-getf content "code"))
           (cursor-pos (json-getf content "cursor_pos")))
      (multiple-value-bind (result start end)
                           (let ((*package* (find-package package)))
                              (complete-code kernel code
                               (min (1- (length code)) cursor-pos)))
        (cond
          ((eval-error-p result)
            (with-slots (ename evalue) result
              (send-complete-reply-error shell msg ename evalue)))
          (result
            (send-complete-reply-ok shell msg result start end))
          (t
            (send-complete-reply-ok shell msg nil cursor-pos cursor-pos)))))))


(defun handle-comm-info-request (kernel msg)
  (info "[kernel] Handling 'comm_info_request'~%")
  (with-slots (shell comms) kernel
    (let* ((content (message-content msg))
           (target-name (json-getf content "target_name"))
           (comms-alist (alexandria:hash-table-alist comms)))
      (send-comm-info-reply shell msg
                            (if target-name
                              (remove-if-not (lambda (p) (equal target-name (cdr p)))
                                comms-alist)
                              comms-alist))))
  t)

(defun handle-comm-open (kernel msg)
  (info "[kernel] Handling 'comm_open'~%")
  (with-slots (iopub session comms) kernel
    (let* ((content (message-content msg))
           (metadata (message-metadata msg))
           (id (json-getf content "comm_id"))
           (target-name (json-getf content "target_name"))
           (data (json-getf content "data"))
           (inst (create-comm (intern target-name 'keyword) id data metadata)))
      (info "~A ~%" target-name)
      (if inst
        (progn
          (setf (gethash id comms) inst)
          (on-comm-open inst data metadata))
        (send-comm-close-orphan iopub session id))))
  t)

(defun handle-comm-message (kernel msg)
  (info "[kernel] Handling 'comm_msg'~%")
  (with-slots (comms) kernel
    (let* ((content (message-content msg))
           (metadata (message-metadata msg))
           (id (json-getf content "comm_id"))
           (data (json-getf content "data"))
           (inst (gethash id comms)))
      (when inst
        (on-comm-message inst data metadata))))
  t)

(defun handle-comm-close (kernel msg)
  (info "[kernel] Handling 'comm_close'~%")
  (with-slots (comms) kernel
    (let* ((content (message-content msg))
           (metadata (message-metadata msg))
           (id (json-getf content "comm_id"))
           (data (json-getf content "data"))
           (inst (gethash id comms)))
      (when inst
        (on-comm-close inst data metadata)
        (remhash id comms))))
  t)

(defun make-eval-error (err msg &key (quit nil))
  (let ((name (symbol-name (class-name (class-of err)))))
    (write-string msg *error-output*)
    (make-error-result name msg :quit quit)))

(define-condition quit-condition (error)
  ()
  (:documentation "A condition for identifying a request for kernel shutdown.")
  (:report (lambda (c stream) (declare (ignore c stream)))))

(defmacro handling-errors (&body body)
  "Macro for catching any conditions including quit-conditions during code
  evaluation."
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
     (quit-condition (err)
       (make-eval-error err (format nil "~A" err) :quit t))
     (simple-error (err)
       (make-eval-error err
         (apply #'format nil (simple-condition-format-control err)
                             (simple-condition-format-arguments err))))
     (simple-type-error (err)
       (make-eval-error err
         (apply #'format nil (simple-condition-format-control err)
                             (simple-condition-format-arguments err))))
     (serious-condition (err)
       (make-eval-error err (format nil "~A" err)))))

(defun send-result (result)
  "Send a result either as display data or an execute result."
  (with-slots (iopub package history-in) *kernel*
    (let ((execute-count (length history-in)))
      (if (typep result 'error-result)
        (send-execute-error iopub *message* execute-count
                            (error-result-ename result)
                            (error-result-evalue result))
        (let ((data (let ((*package* (find-package package)))
                      (render result))))
          (when data
            (if (result-display-data result)
              (send-display-data iopub *message* data)
              (send-execute-result iopub *message* execute-count data))))))))

(defun set-next-input (text &optional (replace nil))
  (declare (ignore replace))
  (vector-push-extend (jsown:new-js
                        ("source" "set_next_input")
                        ("text" text))
                      *payload*))

(defun page (result &optional (start 0))
  (vector-push-extend (jsown:new-js
                        ("source" "page")
                        ("data" (render result))
                        ("start" start))
                      *payload*))

(defun enqueue-input (kernel code)
  "Add code to input queue."
  (cl-containers:enqueue (kernel-input-queue kernel) code))

(defun clear (&optional (wait nil))
  "Send clear output message to frontend."
  (send-clear-output (kernel-iopub *kernel*) *message* wait))
