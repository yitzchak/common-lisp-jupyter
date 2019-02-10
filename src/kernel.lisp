(in-package #:jupyter)

(defvar *kernel* nil)
(defvar *message* nil)
(defvar *payload* nil)

(defclass kernel ()
  ((name :initarg :name
         :initform ""
         :reader kernel-name)
   (version :initarg :version
            :initform ""
            :reader kernel-version)
   (banner :initarg :banner
           :initform ""
           :reader kernel-banner)
   (language-name :initarg :language-name
                  :initform ""
                  :reader kernel-language-name)
   (language-version :initarg :language-version
                     :initform ""
                     :reader kernel-language-version)
   (mime-type :initarg :mime-type
              :initform ""
              :reader kernel-mime-type)
   (file-extension :initarg :file-extension
                   :initform ""
                   :reader kernel-file-extension)
   (pygments-lexer :initarg :pygments-lexer
                   :initform ""
                   :reader kernel-pygments-lexer)
   (codemirror-mode :initarg :codemirror-mode
                    :initform ""
                    :reader kernel-codemirror-mode)
   (help-links :initarg :help-links
               :initform nil
               :reader kernel-help-links)
   (transport :initarg :transport
              :reader kernel-transport
              :type string)
   (ip :initarg :ip
       :reader kernel-ip
       :type string)
   (shell-port :initarg :shell-port
               :reader kernel-shell-port
               :type fixnum)
   (stdin-port :initarg :stdin-port
               :reader kernel-stdin-port
               :type fixnum)
   (iopub-port :initarg :iopub-port
               :reader kernel-iopub-port
               :type fixnum)
   (control-port :initarg :control-port
                 :reader kernel-control-port
                 :type fixnum)
   (hb-port :initarg :hb-port
            :reader kernel-hb-port
            :type fixnum)
   (signature-scheme :initarg :signature-scheme
                     :reader kernel-signature-scheme
                     :type string)
   (key :initarg :key
        :reader kernel-key)
   (prompt-prefix :initarg :prompt-prefix
                  :initform (coerce '(#\Escape #\X) 'string)
                  :reader kernel-prompt-prefix)
   (prompt-suffix :initarg :prompt-suffix
                  :initform (coerce '(#\Escape #\\) 'string)
                  :reader kernel-prompt-suffix)
   (ctx :initform nil
        :accessor kernel-ctx)
   (hb :initform nil
       :accessor kernel-hb)
   (shell :initform nil
          :accessor kernel-shell)
   (stdin :initform nil
          :accessor kernel-stdin)
   (iopub :initform nil
          :accessor kernel-iopub)
   (session :initform nil
            :accessor kernel-session)
   (input-queue :initarg :input-queue
                :initform (make-instance 'cl-containers:basic-queue)
                :reader kernel-input-queue)
   (history-in :initform (make-array 64 :fill-pointer 0 :adjustable t)
               :reader kernel-history-in)
   (history-out :initform (make-array 64 :fill-pointer 0 :adjustable t)
                :reader kernel-history-out))
  (:documentation "Kernel state representation."))

(defgeneric evaluate (kernel page-output input))

(defgeneric is-complete (kernel code))

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

;; Start all channels.
(defmethod start ((k kernel))
  (info "[kernel] Starting...~%")
  ; (setq maxima::$linenum 0)
  ; (setq maxima::*display-labels-p* t)
  (with-slots (ctx key transport ip hb-port hb shell-port shell stdin-port stdin
               iopub-port iopub session prompt-prefix prompt-suffix)
              k
    (setq session (format nil "~W" (uuid:make-v4-uuid)))
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
                               :port iopub-port
                               :prompt-prefix prompt-prefix
                               :prompt-suffix prompt-suffix))
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

(defun kernel-start (kernel-class connection-file-name)
  (info "[kernel] Connection file = ~A~%" connection-file-name)
  (unless (stringp connection-file-name)
    (error "[kernel] Wrong connection file argument (expecting a string)"))
  (let* ((config-js (jsown:parse (read-string-file connection-file-name)))
         (transport (jsown:val config-js "transport"))
         (ip (jsown:val config-js "ip"))
         (shell-port (jsown:val config-js "shell_port"))
         (stdin-port (jsown:val config-js "stdin_port"))
         (iopub-port (jsown:val config-js "iopub_port"))
         (control-port (jsown:val config-js "control_port"))
         (hb-port (jsown:val config-js "hb_port"))
         (key (jsown:val config-js "key"))
         (signature-scheme (jsown:val config-js "signature_scheme")))
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

; (setq maxima::*prompt-prefix* (coerce '(#\Escape #\X) 'string))
; (setq maxima::*prompt-suffix* (coerce '(#\Escape #\\) 'string))

(defun handle-execute-request (kernel msg)
  (info "[kernel] Handling 'execute_request'~%")
  (let ((code (jsown:val (message-content msg) "code")))
    (with-slots (shell iopub stdin history-in history-out) kernel
      (vector-push code history-in)
      (let* ((execution-count (length history-in))
             (*kernel* kernel)
             (*message* msg)
             ; (maxima::*alt-display1d* #'my-displa)
             ; (maxima::*alt-display2d* #'my-displa)
             (*payload* (make-array 16 :adjustable t :fill-pointer 0))
             (page-output (make-string-output-stream))
             (*query-io* (make-stdin-stream stdin msg))
             (*standard-input* *query-io*)
             ; (maxima::$stdin *query-io*)
             (*error-output* (make-iopub-stream iopub msg "stderr"))
             ; (maxima::$stderr *error-output*)
             (*standard-output* (make-iopub-stream iopub msg "stdout"))
             (*debug-io* *standard-output*)
             ; (maxima::$stdout *standard-output*)
             ; (content (message-content msg))
             ; (code (jsown:val content "code"))
             (results (evaluate kernel page-output code)))
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
                  (p (get-output-stream-string page-output)))
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
         (status (is-complete kernel code)))
    (send-is-complete-reply shell msg status)
    t))
