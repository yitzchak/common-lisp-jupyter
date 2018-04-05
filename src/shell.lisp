(in-package #:cl-jupyter)

#|

# The shell router socket #

|#

(defclass shell-channel ()
  ((kernel :initarg :kernel
           :reader shell-kernel)
   (socket :initarg :socket
           :initform nil
           :accessor shell-socket)))


(defun make-shell-channel (kernel)
  (let* ((socket (pzmq:socket (kernel-ctx kernel) :router))
         (shell (make-instance 'shell-channel
                               :kernel kernel
                               :socket socket))
         (config (slot-value kernel 'config))
         (endpoint (format nil "~A://~A:~A"
                           (config-transport config)
                           (config-ip config)
                           (config-shell-port config))))
         ;; (format t "shell endpoint is: ~A~%" endpoint)
    (pzmq:bind socket endpoint)
    shell))

(defun shell-loop (shell)
  (let ((active t))
    (format t "[Shell] loop started~%")
    (send-status-starting (kernel-iopub (shell-kernel shell)) (kernel-session (shell-kernel shell)) :key (kernel-key shell))
    (while active
      (let* ((msg (message-recv (shell-socket shell) :key (kernel-key shell)))
             (msg-type (jsown:val (message-header msg) "msg_type")))
        (cond ((equal msg-type "kernel_info_request")
               (handle-kernel-info-request shell msg))
              ((equal msg-type "execute_request")
               (setf active (handle-execute-request shell msg)))
              ((equal msg-type "shutdown_request")
               (setf active (handle-shutdown-request shell msg)))
              ((equal msg-type "is_complete_request")
               (handle-is-complete-request shell msg))
              (t (warn "[Shell] message type '~A' not (yet ?) supported, skipping..." msg-type)))))))

#|

### Message type: kernel_info_reply ###

|#

(defun kernel-key (shell)
  (kernel-config-key (kernel-config (shell-kernel shell))))

(defun handle-kernel-info-request (shell msg)
  ;;(format t "[Shell] handling 'kernel-info-request'~%")
  ;; status to busy
  ;;(send-status-update (kernel-iopub (shell-kernel shell)) msg "busy" :key (kernel-key shell))
  ;; for protocol version 5
  (let ((reply (make-message
                msg "kernel_info_reply"
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
    (message-send (shell-socket shell) reply :key (kernel-key shell))
    ;; status back to idle
    ;;(send-status-update (kernel-iopub (shell-kernel shell)) msg "idle" :key (kernel-key shell))
    ))

#|

### Message type: execute_request ###

|#

(let (execute-request-shell execute-request-msg)

  (defun handle-execute-request (shell msg)
    ;;(format t "[Shell] handling 'execute_request'~%")
    (let* ((key (kernel-key shell))
           (iopub (kernel-iopub (shell-kernel shell)))
           (content (message-content msg))
           (code (jsown:val content "code")))
      (send-status-update (kernel-iopub (shell-kernel shell)) msg "busy" :key key)
      (setq execute-request-shell shell)
      (setq execute-request-msg msg)
      ;;(format t "  ===> Code to execute = ~W~%" code)
      (vbinds (execution-count results stdout stderr)
              (evaluate-code (kernel-evaluator (shell-kernel shell)) code)
        ;(format t "Execution count = ~A~%" execution-count)
        ;(format t "results = ~A~%" results)
        ;(format t "STDOUT = ~A~%" stdout)
        ;(format t "STDERR = ~A~%" stderr)
        ;broadcast the code to connected frontends
        (send-execute-code iopub msg execution-count code :key key)
        ;; send the stdout
        (when (and stdout (> (length stdout) 0))
              (send-stream iopub msg "stdout" stdout :key key))
        ;; send the stderr
        (when (and stderr (> (length stderr) 0))
              (send-stream iopub msg "stderr" stderr :key key))
        ;; send the results
        (dolist (result results)
          (cond ((eval-error-p result)
                 (send-execute-error iopub msg execution-count (caddr result) (cadddr result) :key key))
                ((eq (caar result) 'maxima::displayinput)
                 (send-execute-result iopub msg execution-count (caddr result) :key key))))
        ;; status back to idle
        (send-status-update iopub msg "idle" :key key)
        ;; send reply (control)
        (let ((errors (remove-if-not #'eval-error-p results)))
          (if errors
            (let ((ename (format nil "~{~A~^, ~}" (mapcar #'caddr errors)))
                  (evalue (format nil "~{~A~^, ~}" (mapcar #'cadddr errors))))
              (send-execute-reply-error shell msg execution-count ename evalue :key key))
            (send-execute-reply-ok shell msg execution-count :key key)))
        ;; return t if there is no quit errors present
        (notany #'quit-eval-error-p results))))

  ;; Redefine RETRIEVE in src/macsys.lisp to make use of input-request/input-reply.
  ;; MSG, FLAG, and PRINT? are declared special there, so be careful to
  ;; refer to those symbols in the :maxima package.

  (defun maxima::retrieve (maxima::msg maxima::flag &aux (maxima::print? nil))
    (declare (special maxima::msg maxima::flag maxima::print?))
    (or (eq maxima::flag 'maxima::noprint) (setq maxima::print? t))
    (let* ((retrieve-prompt (cond ((not maxima::print?)
                                   (setq maxima::print? t)
                                   (format nil ""))
                                  ((null maxima::msg)
                                   (format nil ""))
                                  ((atom maxima::msg)
                                   (format nil "~A" maxima::msg))
                                  ((eq maxima::flag t)
                                   (format nil "~{~A~}" (cdr maxima::msg)))
                                  (t
                                   (maxima::aformat nil "~M" maxima::msg))))
           (kernel (shell-kernel execute-request-shell))
           (stdin (kernel-stdin kernel))
           (key (kernel-key execute-request-shell)))
      (send-input-request stdin execute-request-msg retrieve-prompt :key key)
      (let* ((msg (message-recv (stdin-socket stdin) :key key))
             (content (message-content msg))
             (value (jsown:val content "value")))
        (maxima::mread-noprompt (make-string-input-stream (add-terminator value)) nil)))))

#|

### Message type: shutdown_request ###

|#

(defun handle-shutdown-request (shell msg)
  (let* ((content (message-content msg))
         (restart (jsown:val content "restart")))
    (send-shutdown-reply shell msg restart :key (kernel-key shell))
    nil))

#|

### Message type: is_complete_request ###

|#

(defun handle-is-complete-request (shell msg)
  (let* ((content (message-content msg))
         (code (jsown:val content "code"))
         (status (if (ends-with-terminator code)
                     "complete"
                     "incomplete")))
    (send-is-complete-reply shell msg status :key (kernel-key shell))))

#|

# Message sending functions

|#

(defun send-shutdown-reply (shell parent-msg restart &key (key nil))
  (let ((msg (make-message parent-msg "shutdown_reply"
                           (jsown:new-js
                             ("restart" (if restart t :f))))))
    (message-send (shell-socket shell) msg :key key)))

(defun send-is-complete-reply (shell parent-msg status &key (key nil))
  (let ((msg (make-message parent-msg "is_complete_reply"
                           (jsown:new-js
                             ("status" status)
                             ("indent" "")))))
    (message-send (shell-socket shell) msg :key key)))

(defun send-execute-reply-ok (shell parent-msg execution-count &key (key nil))
  (let ((msg (make-message parent-msg "execute_reply"
                           (jsown:new-js
                             ("status" "ok")
                             ("execution_count" execution-count)
                             ("payload" '())))))
    (message-send (shell-socket shell) msg :key key)))

(defun send-execute-reply-error (shell parent-msg execution-count ename evalue &key (key nil))
  (let ((msg (make-message parent-msg "execute_reply"
                           (jsown:new-js
                             ("status" "error")
                             ("execution_count" execution-count)
                             ("ename" ename)
                             ("evalue" evalue)
                             ("traceback" nil)))))
    (message-send (shell-socket shell) msg :key key)))

#|

## Message content ##

|#

(defclass message-content ()
  ()
  (:documentation "The base class of message contents."))
