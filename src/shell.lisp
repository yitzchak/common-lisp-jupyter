(in-package #:uncommonshell)

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

(defmethod to-json ((object content-kernel-info-reply) &key (indent nil) (first-indent nil) (newlines nil))
  (with-slots (protocol-version implementation implementation-version 
				language language-version language-info-mimetype
				language-info-pygments-lexer language-info-codemirror-mode
				banner help-links) object
    (concatenate 'string
                 (to-json-line first-indent newlines "{")
                 (to-json-line indent newlines "\"protocol_version\": ~W," protocol-version)
                 (to-json-line indent newlines "\"implementation\": ~W," implementation)
                 (to-json-line indent newlines"\"implementation_version\": ~W," implementation-version)
                 (to-json-line indent newlines"\"language\": ~W," language)
                 (to-json-line indent newlines"\"language_version\": ~W," language-version)
                 (to-json-line indent newlines"\"language_info\": {")
                 (to-json-line indent newlines "  \"mimetype\": ~W," language-info-mimetype)
                 (to-json-line indent newlines "  \"pygments_lexer\": ~W," language-info-pygments-lexer)
                 (to-json-line indent newlines "  \"codemirror_mode\": ~W" language-info-codemirror-mode)
                 (to-json-line indent newlines "},")
                 (to-json-line indent newlines"\"banner\": ~W," (json:encode-json-to-string banner))
                 (to-json-line indent newlines"\"help_links\": ~A" (help-links-to-json help-links))
                 (to-json-line first-indent nil "}"))))

(defvar *status-starting-sent* nil)

(defun handle-kernel-info-request (shell ids msg sig raw)
  ;; (format t "[Shell] handling 'kernel-info-request'~%")
  (when (not *status-starting-sent*)
    (setf *status-starting-sent* t)
    (send-status-update (kernel-iopub (shell-kernel shell)) (message-header msg) ids sig :starting))
  (let ((hdr (message-header msg)))
    (let ((reply (make-instance 
		  'message
		  :header (make-instance 
			   'header
			   :msg-id (format nil "~W" (uuid:make-v4-uuid))
			   :username (header-username hdr)
			   :session (header-session hdr)
			   :msg-type "kernel_info_reply"
			   :version (header-version hdr))
		  :parent-header hdr
		  :metadata ""
		  :content (make-instance
			    'content-kernel-info-reply
			    :protocol-version (header-version hdr)
			    :implementation +KERNEL-IMPLEMENTATION-NAME+
			    :implementation-version +KERNEL-IMPLEMENTATION-VERSION+
			    :language "common-lisp"
			    :language-version "X3J13"
			    :language-info-mimetype "text/x-common-lisp"
			    :language-info-pygments-lexer "common-lisp"
			    :language-info-codemirror-mode "text/x-common-lisp"
			    :banner (banner)
			    :help-links '(("Common Lisp Hyperspec" . "http://www.lispworks.com/documentation/HyperSpec/Front/index.htm"))))))
      (message-send (shell-socket shell) reply :identities ids))))
						      

#|

### Message type: execute_request ###

|#
  

(defun handle-execute-request (shell ids msg sig raw)
  (format t "[Shell] handling 'execute_request'~%")
  (let ((hdr (message-header msg)))
    (send-status-update (kernel-iopub (shell-kernel shell)) hdr ids sig :busy)
    (let ((content (json:decode-json-from-string (message-content msg))))
      (format t "  ==> Message content = ~W~%" content)
      (let ((code (afetch :code content)))
	(vbinds (execution-count results)
	    (evaluate-code (kernel-evaluator (shell-kernel shell)) code)
	  (send-status-update (kernel-iopub (shell-kernel shell)) hdr ids sig :idle)
	  (let ((reply (make-instance 
			'message
			:header (make-instance 
				 'header
				 :msg-id (format nil "~W" (uuid:make-v4-uuid))
				 :username (header-username hdr)
				 :session (header-session hdr)
				 :msg-type "execute_reply"
				 :version (header-version hdr))
			:parent-header hdr
			:metadata ""
			:content (json:encode-json-to-string
				  `((:status . "ok")
				    (:execution--count . ,execution-count)
				    (:payload . ,(vector)))))))
	    (message-send (shell-socket shell) reply :identities ids :raw-content t)))))))

