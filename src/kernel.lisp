(in-package #:maxima-jupyter)

(defclass kernel ()
  ((config :initarg :config
           :reader kernel-config)
   (ctx :initarg :ctx
        :reader kernel-ctx)
   (hb :initarg :hb
       :reader kernel-hb)
   (shell :initarg :shell
          :reader kernel-shell)
   (stdin :initarg :stdin
          :reader kernel-stdin)
   (iopub :initarg :iopub
          :reader kernel-iopub)
   (session :initarg :session
            :reader kernel-session)
   (evaluator :initarg :evaluator
              :reader kernel-evaluator)
   (input-queue :initarg :input-queue
                :initform (make-instance 'cl-containers:basic-queue)
                :reader kernel-input-queue))
  (:documentation "Kernel state representation."))

(defun make-kernel (config)
  (let ((ctx (pzmq:ctx-new))
        (session-id (format nil "~W" (uuid:make-v4-uuid))))
    (make-instance 'kernel
                   :config config
                   :ctx ctx
                   :hb (make-hb-channel config ctx)
                   :shell (make-shell-channel config ctx)
                   :stdin (make-stdin-channel config ctx)
                   :iopub (make-iopub-channel config ctx)
                   :session session-id
                   :evaluator (make-evaluator))))

(defun get-argv ()
  ;; Borrowed from apply-argv, command-line-arguments.  Temporary solution (?)
  #+sbcl (cdr sb-ext:*posix-argv*)
  #+clozure (cdr ccl:*command-line-argument-list*)
  #+gcl si:*command-args*
  #+ecl (loop for i from 0 below (si:argc) collect (si:argv i))
  #+cmu extensions:*command-line-strings*
  #+allegro (sys:command-line-arguments)
  #+lispworks sys:*line-arguments-list*
  #+clisp ext:*args*
  #-(or sbcl clozure gcl ecl cmu allegro lispworks clisp)
  (error "get-argv not supported for your implementation"))

(defun banner (stream)
  (format stream (concatenate 'string
                              "~A: an enhanced interactive Maxima REPL~%"
                              "(Version ~A - Jupyter protocol v.~A)~%"
                              "--> (C) 2014-2015 Frederic Peschanski (cf. LICENSE)~%")
          +KERNEL-IMPLEMENTATION-NAME+
          +KERNEL-IMPLEMENTATION-VERSION+
          +KERNEL-PROTOCOL-VERSION+))

(defclass kernel-config ()
  ((transport :initarg :transport :reader config-transport :type string)
   (ip :initarg :ip :reader config-ip :type string)
   (shell-port :initarg :shell-port :reader config-shell-port :type fixnum)
   (stdin-port :initarg :stdin-port :reader config-stdin-port :type fixnum)
   (iopub-port :initarg :iopub-port :reader config-iopub-port :type fixnum)
   (control-port :initarg :control-port :reader config-control-port :type fixnum)
   (hb-port :initarg :hb-port :reader config-hb-port :type fixnum)
   (signature-scheme :initarg :signature-scheme :reader config-signature-scheme :type string)
   (key :initarg :key :reader config-key)))

(defun make-kernel-config (connection-file-name)
  (let ((config-js (jsown:parse (read-string-file connection-file-name))))
    (make-instance 'kernel-config
                   :transport (jsown:val config-js "transport")
                   :ip (jsown:val config-js "ip")
                   :shell-port (jsown:val config-js "shell_port")
                   :stdin-port (jsown:val config-js "stdin_port")
                   :iopub-port (jsown:val config-js "iopub_port")
                   :control-port (jsown:val config-js "control_port")
                   :hb-port (jsown:val config-js "hb_port")
                   :signature-scheme (jsown:val config-js "signature_scheme")
                   :key (let ((str-key (jsown:val config-js "key")))
                          (if (string= str-key "")
                              nil
                              (babel:string-to-octets str-key :encoding :ASCII))))))

;; Start all channels.
(defmethod start ((k kernel))
  (info "[kernel] Starting...~%")
  (start (kernel-hb k))
  (start (kernel-iopub k))
  (start (kernel-shell k))
  (start (kernel-stdin k))
  (let ((iopub (kernel-iopub k))
        (session (kernel-session k)))
    (send-status iopub session "starting")
    (send-status iopub session "idle")))

;; Stop all channels and destroy the control.
(defmethod stop ((k kernel))
  (info "[kernel] Stopped.~%")
  (stop (kernel-hb k))
  (stop (kernel-iopub k))
  (stop (kernel-shell k))
  (stop (kernel-stdin k))
  (pzmq:ctx-destroy (kernel-ctx k)))

(defun kernel-start (connection-file-name)
  (info (banner nil))
  (info "[kernel] Connection file = ~A~%" connection-file-name)
  (unless (stringp connection-file-name)
    (error "[kernel] Wrong connection file argument (expecting a string)"))
  (let ((config (make-kernel-config connection-file-name)))
    (when (not (string= (config-signature-scheme config) "hmac-sha256"))
      ;; XXX: only hmac-sha256 supported
      (error "[kernel] Signature scheme 'hmac-sha256' required, was provided ~S." (config-signature-scheme config)))
      ;;(inspect config)
    (iter
      (with kernel = (make-kernel config))
      (with iopub = (kernel-iopub kernel))
      (with shell = (kernel-shell kernel))
      (initially
        (start kernel))
      (for msg = (message-recv shell))
      (send-status-update iopub msg "busy")
      (while (handle-message kernel msg))
      (after-each
        (send-status-update iopub msg "idle"))
      (finally-protected
        (stop kernel)))))

;; This is the entry point for a saved lisp image created by
;; trivial-dump-core:save-executable or equivalent.
(defun kernel-start-exec ()
  ;; IS THERE OTHER STUFF HANDLED BY MAXIMA INIT-CL.LISP THAT WE NEED TO DUPLICATE HERE ??
  (setq *read-default-float-format* 'double-float)
  (kernel-start (car (last (get-argv)))))

#|

### Message type: kernel_info_request ###

|#

(defun handle-message (kernel msg)
  (let ((msg-type (jsown:val (message-header msg) "msg_type")))
    (cond ((equal msg-type "kernel_info_request")
           (handle-kernel-info-request kernel msg))
          ((equal msg-type "execute_request")
           (handle-execute-request kernel msg))
          ((equal msg-type "shutdown_request")
           (handle-shutdown-request kernel msg))
          ((equal msg-type "is_complete_request")
           (handle-is-complete-request kernel msg))
          (t
           (warn "[Shell] message type '~A' not supported, skipping..." msg-type)
           t))))

#|

### Message type: kernel_info_request ###

|#

(defun handle-kernel-info-request (kernel msg)
  (info "[kernel] Handling 'kernel_info_request'~%")
  (message-send (kernel-shell kernel)
    (make-message msg "kernel_info_reply"
      (jsown:new-js
        ("protocol_version" (jsown:val (message-header msg) "version"))
        ("implementation" +KERNEL-IMPLEMENTATION-NAME+)
        ("implementation_version" +KERNEL-IMPLEMENTATION-VERSION+)
        ("banner" (banner nil))
        ("help_links"
          (list
            (jsown:new-js
              ("text" "Maxima Reference Manual")
              ("url" "http://maxima.sourceforge.net/docs/manual/maxima.html"))
            (jsown:new-js
              ("text" "Maxima Documentation")
              ("url" "http://maxima.sourceforge.net/documentation.html"))))
        ("language_info"
          (jsown:new-js
            ("name" "maxima")
            ("version" maxima::*autoconf-version*)
            ("mimetype" "text/x-maxima")
            ("pygments_lexer" "maxima")
            ("codemirror_mode" "maxima")))))))

#|

### Message type: execute_request ###

|#

(defun handle-execute-request (kernel msg)
  (info "[kernel] Handling 'execute_request'~%")
  (let* ((shell (kernel-shell kernel))
         (iopub (kernel-iopub kernel))
         (*kernel* kernel)
         (*message* msg)
         (*page-output* (make-string-output-stream))
         (*payload* (make-array 16 :adjustable t :fill-pointer 0))
         (*error-output* (make-iopub-stream iopub msg "stderr"))
         (*standard-output* (make-iopub-stream iopub msg "stdout"))
         (*debug-io* *standard-output*)
         (content (message-content msg))
         (code (jsown:val content "code")))
    (vbinds (execution-count results)
            (evaluate-code (kernel-evaluator kernel) code)
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
      (notany #'quit-eval-error-p results))))

#|

### Message type: shutdown_request ###

|#

(defun handle-shutdown-request (kernel msg)
  (info "[kernel] Handling 'shutdown_request'~%")
  (let* ((shell (kernel-shell kernel))
         (content (message-content msg))
         (restart (jsown:val content "restart")))
    (send-shutdown-reply shell msg restart)
    nil))

#|

### Message type: is_complete_request ###

|#

(defun handle-is-complete-request (kernel msg)
  (info "[kernel] Handling 'is_complete_request'~%")
  (let* ((shell (kernel-shell kernel))
         (content (message-content msg))
         (code (jsown:val content "code"))
         (status (is-complete (kernel-evaluator kernel) code)))
    (send-is-complete-reply shell msg status)
    t))
