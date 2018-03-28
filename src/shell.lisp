(in-package #:cl-jupyter)

#|

# The shell router socket #

|#

(defclass shell-channel ()
  ((kernel :initarg :kernel :reader shell-kernel)
   (socket :initarg :socket :initform nil :accessor shell-socket)))


(defun make-shell-channel (kernel)
  (let ((socket (pzmq:socket (kernel-ctx kernel) :router)))
    (let ((shell (make-instance 'shell-channel
                                :kernel kernel
                                :socket socket)))
      (let ((config (slot-value kernel 'config)))
        (let ((endpoint (format nil "~A://~A:~A"
                                  (config-transport config)
                                  (config-ip config)
                                  (config-shell-port config))))
          ;; (format t "shell endpoint is: ~A~%" endpoint)
          (pzmq:bind socket endpoint)
          shell)))))

(defun shell-loop (shell)
  (let ((active t))
    (format t "[Shell] loop started~%")
    (send-status-starting (kernel-iopub (shell-kernel shell)) (kernel-session (shell-kernel shell)) :key (kernel-key shell))
    (while active
      (vbinds (identities sig msg buffers)  (message-recv (shell-socket shell))
	      ;;(format t "Shell Received:~%")
	      ;;(format t "  | identities: ~A~%" identities)
	      ;;(format t "  | signature: ~W~%" sig)
	      ;;(format t "  | message: ~A~%" (encode-json-to-string (message-header msg)))
	      ;;(format t "  | buffers: ~W~%" buffers)

	      ;; TODO: check the signature (after that, sig can be forgotten)
	      (let ((msg-type (header-msg-type (message-header msg))))
		(cond ((equal msg-type "kernel_info_request")
		       (handle-kernel-info-request shell identities msg buffers))
		      ((equal msg-type "execute_request")
		       (setf active (handle-execute-request shell identities msg buffers)))
		      ((equal msg-type "shutdown_request")
		       (setf active (handle-shutdown-request shell identities msg buffers)))
		      ((equal msg-type "is_complete_request")
		       (handle-is-complete-request shell identities msg buffers))
		      (t (warn "[Shell] message type '~A' not (yet ?) supported, skipping..." msg-type))))))))

#|

### Message type: kernel_info_reply ###

|#

;; for protocol version 5
(defclass content-kernel-info-reply (message-content)
  ((protocol-version :initarg :protocol-version :type string)
   (implementation :initarg :implementation :type string)
   (implementation-version :initarg :implementation-version :type string)
   (language-info-name :initarg :language-info-name :type string)
   (language-info-version :initarg :language-info-version :type string)
   (language-info-mimetype :initarg :language-info-mimetype :type string)
   (language-info-pygments-lexer :initarg :language-info-pygments-lexer :type string)
   (language-info-codemirror-mode :initarg :language-info-codemirror-mode :type string)
   (language-info-nbconvert-exporter :initarg :language-info-nbconvert-exporter :type string)
   (banner :initarg :banner :type string)
   ;; help links: (text . url) a-list
   (help-links :initarg :help-links)))

;; for protocol version 4.1
;;(defclass content-kernel-info-reply (message-content)
;; ((protocol-version :initarg :protocol-version)
;;   (language-version :initarg :language-version)
;;   (language :initarg :language :type string)))

(defun help-links-to-json (help-links)
  (concatenate 'string "["
	       (concat-all 'string ""
			   (join "," (mapcar (lambda (link)
					       (format nil "{ \"text\": ~W, \"url\": ~W }" (car link) (cdr link)))
					     help-links)))
	       "]"))

;; for protocol version 5
(defmethod encode-json (stream (object content-kernel-info-reply) &key (indent nil) (first-line nil))
  (with-slots (protocol-version
               implementation implementation-version
               language-info-name language-info-version
               language-info-mimetype language-info-pygments-lexer language-info-codemirror-mode
               language-info-nbconvert-exporter
               banner help-links) object
    (encode-json stream `(("protocol_version" . ,protocol-version)
                          ("implementation" . ,implementation)
                          ("implementation_version" . ,implementation-version)
                          ("language_info" . (("name" . ,language-info-name)
                                              ("version" . ,language-info-version)
                                              ("mimetype" . ,language-info-mimetype)
                                              ("pygments_lexer" . ,language-info-pygments-lexer)
                                              ("codemirror_mode" . ,language-info-codemirror-mode)))
                                              ;("nbconvert_exporter" . ,language-info-nbconvert-exporter)))
                          ("banner" . "")) ; ,banner)
                          ;("help_links" . ,help-links))
                 :indent indent :first-line first-line)))

;; for protocol version 4.1
;; (defmethod encode-json (stream (object content-kernel-info-reply) &key (indent nil) (first-line nil))
;;   (with-slots (protocol-version ipython-version language-version language) object
;;     (encode-json stream `(("protocol_version" . ,protocol-version)
;;                           ("language_version" . ,language-version)
;;                           ("language" . ,language))
;;                  :indent indent :first-line first-line)))

(defun kernel-key (shell)
  (kernel-config-key (kernel-config (shell-kernel shell))))

(defun handle-kernel-info-request (shell identities msg buffers)
  ;;(format t "[Shell] handling 'kernel-info-request'~%")
  ;; status to busy
  ;;(send-status-update (kernel-iopub (shell-kernel shell)) msg "busy" :key (kernel-key shell))
  ;; for protocol version 5
  (let ((reply (make-message
                msg "kernel_info_reply" nil
                (make-instance
                 'content-kernel-info-reply
                 :protocol-version (header-version (message-header msg))
                 :implementation +KERNEL-IMPLEMENTATION-NAME+
                 :implementation-version +KERNEL-IMPLEMENTATION-VERSION+
                 :language-info-name "maxima"
                 :language-info-version maxima::*autoconf-version*
                 :language-info-mimetype "text/x-maxima"
                 :language-info-pygments-lexer "maxima"
                 :language-info-codemirror-mode "maxima"
                 :language-info-nbconvert-exporter ""
                 :banner (banner)
                 :help-links (vector)))))
		 ;;'(("Common Lisp Hyperspec" . "http://www.lispworks.com/documentation/HyperSpec/Front/index.htm"))))))
  ;; for protocol version 4.1
  ;; (let ((reply (make-message-from-parent msg "kernel_info_reply" nil
  ;;   				 (make-instance
  ;;   				  'content-kernel-info-reply
  ;;   				  :protocol-version #(4 1)
  ;;   				  :language-version #(1 2 7)  ;; XXX: impl. dependent but really cares ?
    ;;   				  :language "common-lisp"))))
    (message-send (shell-socket shell) reply :identities identities :key (kernel-key shell))
    ;; status back to idle
    ;;(send-status-update (kernel-iopub (shell-kernel shell)) msg "idle" :key (kernel-key shell))
    ))

#|

### Message type: execute_request ###

|#

(let (execute-request-shell execute-request-msg)

  (defun handle-execute-request (shell identities msg buffers)
    ;;(format t "[Shell] handling 'execute_request'~%")
    (let* ((key (kernel-key shell))
           (iopub (kernel-iopub (shell-kernel shell)))
           (content (parse-json-from-string (message-content msg)))
           (code (afetch "code" content :test #'equal)))
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
        (when (and (consp results) (typep (car results) 'cl-jupyter-user::cl-jupyter-quit-obj))
              ;; ----- ** request for shutdown ** -----
              (send-execute-reply shell identities msg "abort" execution-count :key key)
              (return-from handle-execute-request nil))
        ;; ----- ** normal request ** -----
        ;; send the stdout
        (when (and stdout (> (length stdout) 0))
              (send-stream iopub msg "stdout" stdout :key key))
        ;; send the stderr
        (when (and stderr (> (length stderr) 0))
              (send-stream iopub msg "stderr" stderr :key key))
        ;; send the results
        (dolist (result results)
          (when (eq (caar result) 'maxima::displayinput)
                (send-execute-result iopub msg execution-count (caddr result) :key key)))
        ;; status back to idle
        (send-status-update iopub msg "idle" :key key)
        ;; send reply (control)
        (send-execute-reply shell identities msg "ok" execution-count :key key)
  	    t)))

  ;; Redefine RETRIEVE in src/macsys.lisp to make use of input-request/input-reply.
  ;; MSG, FLAG, and PRINT? are declared special there, so be careful to
  ;; refer to those symbols in the :maxima package.

  (defun maxima::retrieve (maxima::msg maxima::flag &aux (maxima::print? nil))
    (declare (special maxima::msg maxima::flag maxima::print?))
    (or (eq maxima::flag 'maxima::noprint) (setq maxima::print? t))
    (let
      ((retrieve-prompt
         (cond
           ((not maxima::print?)
            (setq maxima::print? t)
            (format nil ""))
           ((null maxima::msg)
            (format nil ""))
           ((atom maxima::msg)
            (format nil "~A" maxima::msg))
           ((eq maxima::flag t)
            (format nil "~{~A~}" (cdr maxima::msg)))
           (t
            (maxima::aformat nil "~M" maxima::msg)))))
      (let ((kernel (shell-kernel execute-request-shell)))
        (let ((stdin (kernel-stdin kernel)))
          (send-input-request stdin execute-request-msg retrieve-prompt :key (kernel-key execute-request-shell))
          (multiple-value-bind (identities signature message buffers) (message-recv (stdin-socket stdin))
            (let*
              ((content (parse-json-from-string (message-content message)))
               (value (afetch "value" content :test #'equal)))
               (maxima::mread-noprompt (make-string-input-stream (add-terminator value)) nil))))))))

#|

### Message type: shutdown_request ###

|#

(defun handle-shutdown-request (shell identities msg buffers)
  (let* ((content (parse-json-from-string (message-content msg)))
         (restart (afetch "restart" content :test #'equal)))
    (send-shutdown-reply shell identities msg restart :key (kernel-key shell))
    nil))

#|

### Message type: is_complete_request ###

|#

(defun handle-is-complete-request (shell identities msg buffers)
  (let* ((content (parse-json-from-string (message-content msg)))
         (code (afetch "code" content :test #'equal))
         (status (if (ends-with-terminator code)
                     "complete"
                     "incomplete")))
    (send-is-complete-reply shell identities msg status :key (kernel-key shell))))

#|

# Message sending functions

|#

(defun send-shutdown-reply (shell identities parent-msg restart &key (key nil))
  (let ((msg (make-message parent-msg "shutdown_reply" nil
                           `(("restart" . ,restart)))))
    (message-send (shell-socket shell) msg :identities identities :key key)))

(defun send-is-complete-reply (shell identities parent-msg status &key (key nil))
  (let ((msg (make-message parent-msg "is_complete_reply" nil
                           `(("status" . ,status)
                             ("indent" . "")))))
    (message-send (shell-socket shell) msg :identities identities :key key)))

(defun send-execute-reply (shell identities parent-msg status execution-count &key (key nil))
  (let ((msg (make-message parent-msg "execute_reply" nil
                           `(("status" . ,status)
                             ("execution_count" . ,execution-count)
                             ("payload" . ,(vector))))))
    (message-send (shell-socket shell) msg :identities identities :key key)))

#|

## Message content ##

|#

(defclass message-content ()
  ()
  (:documentation "The base class of message contents."))
