(in-package #:fishbowl)

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
    (while active
      (vbinds (ids sig msg raw)  (message-recv (shell-socket shell))
        ;;(format t "Shell Received:~%")
	;;(format t "  | identities: ~A~%" ids)
	;;(format t "  | signature: ~W~%" sig)
	;;(format t "  | message: ~A~%" (to-json(message-header msg)))
	;;(format t "  | raw: ~W~%" raw)
	(let ((msg-type (header-msg-type (message-header msg))))
	  (cond ((equal msg-type "kernel_info_request")
		 (handle-kernel-info-request shell ids msg sig raw))
		((equal msg-type "execute_request")
		 (handle-execute-request shell ids msg sig raw))
		(t (warn "[Shell] message type '~A' not (yet ?) supported, skipping..." msg-type))))))))


#|

### Message type: kernel_info_reply ###

|#
  
(defclass content-kernel-info-reply (message-content)
  ((protocol-version :initarg :protocol-version :type string)
   (implementation :initarg :implementation :type string)
   (implementation-version :initarg :implementation-version :type string)
   (language :initarg :language :type string)
   (language-version :initarg :language-version :type string)
   (language-info-mimetype :initarg :language-info-mimetype :type string)
   (language-info-pygments-lexer :initarg :language-info-pygments-lexer :type string)
   (language-info-codemirror-mode :initarg :language-info-codemirror-mode :type string)
   (banner :initarg :banner :type string)
   ;; help links: (text . url) a-list
   (help-links :initarg :help-links)))

(defun help-links-to-json (help-links)
  (concatenate 'string "["
	       (concat-all 'string ""
			   (join "," (mapcar (lambda (link) 
					       (format nil "{ \"text\": ~W, \"url\": ~W }" (car link) (cdr link))) 
					     help-links)))
	       "]"))

(defmethod encode-json (stream (object content-kernel-info-reply) &key (indent nil) (first-line nil))
  (with-slots (protocol-version implementation implementation-version 
				language language-version language-info-mimetype
				language-info-pygments-lexer language-info-codemirror-mode
				banner help-links) object
    (encode-json stream `(("protocol_version" . ,protocol-version)
                          ("implementation" . ,implementation)
                          ("implementation_version" . ,implementation-version)
                          ("language" . ,language)
                          ("language_version" . ,language-version)
                          ("language_info" . (("mimetype" . language-info-mimetype)
                                              ("pygments_lexer" . language-info-pygments-lexer)
                                              ("codemirror_mode" . language-info-codemirror-mode)
                                              ("banner" . banner)
                                              ("help_links" . (help-links-to-json help-links)))))
                 :indent indent :first-line first-line)))

(defvar *status-starting-sent* nil)

(defun handle-kernel-info-request (shell ids msg sig raw)
  ;; (format t "[Shell] handling 'kernel-info-request'~%")
  (when (not *status-starting-sent*)
    (setf *status-starting-sent* t)
    (send-status-update (kernel-iopub (shell-kernel shell)) msg sig "starting"))
  (let ((reply (make-message-from-parent msg "kernel_info_reply" nil 
					 (make-instance
					  'content-kernel-info-reply
					  :protocol-version (header-version (message-header msg))
					  :implementation +KERNEL-IMPLEMENTATION-NAME+
					  :implementation-version +KERNEL-IMPLEMENTATION-VERSION+
					  :language "common-lisp"
					  :language-version "X3J13"
					  :language-info-mimetype "text/x-common-lisp"
					  :language-info-pygments-lexer "common-lisp"
					  :language-info-codemirror-mode "text/x-common-lisp"
					  :banner (banner)
					  :help-links '(("Common Lisp Hyperspec" . "http://www.lispworks.com/documentation/HyperSpec/Front/index.htm"))))))
    (message-send (shell-socket shell) reply :identities ids)))						      

#|

### Message type: execute_request ###

|#
  

(defun handle-execute-request (shell ids msg sig raw)
  ;;(format t "[Shell] handling 'execute_request'~%")
  (send-status-update (kernel-iopub (shell-kernel shell)) msg sig "busy")
  (let ((content (parse-json-from-string (message-content msg))))
    ;;(format t "  ==> Message content = ~W~%" content)
    (let ((code (afetch "code" content :test #'equal)))
      (format t "  ===> Code to execute = ~W~%" code)
      (vbinds (execution-count results stdout stderr)
          (evaluate-code (kernel-evaluator (shell-kernel shell)) code)
        ;(format t "Execution count = ~A~%" execution-count)
        ;(format t "results = ~A~%" results)
        ;(format t "STDOUT = ~A~%" stdout)
        ;(format t "STDERR = ~A~%" stderr)
        ;; broadcast the code to connected frontends
        (send-execute-code (kernel-iopub (shell-kernel shell)) msg sig execution-count code)
        ;; send the stdout
        (when stdout
          (send-stream (kernel-iopub (shell-kernel shell)) msg sig "stdout" stdout))
        ;; send the stderr
        (when stderr
          (send-stream (kernel-iopub (shell-kernel shell)) msg sig "stderr" stderr))
	;; send the first result
	(send-execute-result (kernel-iopub (shell-kernel shell)) 
			     msg sig execution-count (car results))
	;; status back to idle
	(send-status-update (kernel-iopub (shell-kernel shell)) msg sig "idle")
	;; send reply (control)
	(let ((reply (make-message-from-parent msg "execute_reply" nil
					       `(("status" . "ok")
						 ("execution_count" . ,execution-count)
						 ("payload" . ,(vector))))))
	  (message-send (shell-socket shell) reply :identities ids))))))
