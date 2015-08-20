(in-package #:cl-jupyter)

(defclass kernel ()
  ((config :initarg :config :reader kerner-config)
   (ctx :initarg :ctx :reader kernel-ctx)
   (shell :initarg :shell :initform nil :reader kernel-shell)
   (iopub :initarg :iopub :initform nil :reader kernel-iopub)
   (session :initarg :session :reader kernel-session)
   (evaluator :initarg :evaluator :initform nil :reader kernel-evaluator))
  (:documentation "Kernel state representation."))

(defun make-kernel (config)
  (let ((ctx (pzmq:ctx-new))
	(session-id (format nil "~W" (uuid:make-v4-uuid))))
    (make-instance 'kernel
                   :config config
                   :ctx ctx
		   :session session-id)))

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

(defun join (e l)
  (cond ((endp l) (list))
        ((endp (cdr l)) l)
        (t (cons (car l) (cons e (join e (cdr l)))))))

(example (join 1 '(a b c d e)) 
         => '(a 1 b 1 c 1 d 1 e))

(defun concat-all (kind term ls)
  (if (endp ls)
      term
      (concatenate kind (car ls) (concat-all kind term (cdr ls)))))

(example (concat-all 'string "" '("a" "b" "c" "d" "e"))
         => "abcde")
  
(defun banner ()
  (concat-all
   'string ""
   (join (format nil "~%")
         '("                                 __________       "
           "                                /         /.      "
           "     .-----------------.       /_________/ |      "
           "    /                 / |      |         | |      "
           "   /+================+\\ |      | |====|  | |      "
           "   ||cl-jupyter      || |      |         | |      "
           "   ||                || |      | |====|  | |      "
           "   ||* (fact 5)      || |      |         | |      "
           "   ||120             || |      |   ___   | |      "
           "   ||                || |      |  |166|  | |      "
           "   ||                ||/@@@    |   ---   | |      "
           "   \\+================+/    @   |_________|./.     "
           "                         @           ..  ....'    "
           "     ..................@      __.'. '  ''         "
           "    /oooooooooooooooo//      ///                  "
           "   /................//      /_/                   "
           "   ------------------                             "
           ""))))

;; (format t (banner))



(defclass kernel-config ()
  ((transport :initarg :transport :reader config-transport :type string)
   (ip :initarg :ip :reader config-ip :type string)
   (shell-port :initarg :shell-port :reader config-shell-port :type fixnum)
   (iopub-port :initarg :iopub-port :reader config-iopub-port :type fixnum)
   (control-port :initarg :control-port :reader config-control-port :type fixnum)
   (stdin-port :initarg :stdin-port :reader config-stdin-port :type fixnum)
   (hb-port :initarg :hb-port :reader config-hb-port :type fixnum)
   (signature-scheme :initarg :signature-scheme :reader kernel-config-signature-scheme :type string)
   (key :initarg :key :reader kernel-config-key :type string)))

(defun kernel-start ()
  ;; IS THERE OTHER STUFF HANDLED BY MAXIMA INIT-CL.LISP THAT WE NEED TO DUPLICATE HERE ??
  (setq *read-default-float-format* 'double-float)

  (let ((cmd-args (get-argv)))
    ;(princ (banner))
    (write-line "")
    (format t "~A: an enhanced interactive Maxima REPL~%" +KERNEL-IMPLEMENTATION-NAME+)
    (format t "(Version ~A - Jupyter protocol v.~A)~%"
            +KERNEL-IMPLEMENTATION-VERSION+
            +KERNEL-PROTOCOL-VERSION+)
    (format t "--> (C) 2014-2015 Frederic Peschanski (cf. LICENSE)~%")
    (write-line "")
    (let ((connection-file-name  (car (last cmd-args))))
      (format t "connection file = ~A~%" connection-file-name)
      (unless (stringp connection-file-name)
        (error "Wrong connection file argument (expecting a string)"))
      (let ((config-alist (parse-json-from-string (concat-all 'string "" (read-file-lines connection-file-name)))))
        ;; (format t "kernel configuration = ~A~%" config-alist)
        (let ((config
               (make-instance 'kernel-config
                              :transport (afetch "transport" config-alist :test #'equal)
                              :ip (afetch "ip" config-alist :test #'equal)
                              :shell-port (afetch "shell_port" config-alist :test #'equal)
                              :iopub-port (afetch "iopub_port" config-alist :test #'equal)
                              :control-port (afetch "control_port" config-alist :test #'equal)
                              :hb-port (afetch "hb_port" config-alist :test #'equal)
                              :signature-scheme (afetch "signature_scheme" config-alist :test #'equal)
                              :key (afetch "key" config-alist :test #'equal))))
          (when (not (string= (kernel-config-key config) ""))
            ;; TODO: add support for encryption
            (error "Secure connection not yet supported: please use an empty encryption key"))
          ;;(inspect config)
          (let* ((kernel (make-kernel config))
                 (evaluator (make-evaluator kernel))
                 (shell (make-shell-channel kernel))
                 (iopub (make-iopub-channel kernel)))
	    ;; Launch the hearbeat thread
	    (let ((hb-socket (pzmq:socket (kernel-ctx kernel) :rep)))
	      (let ((hb-endpoint (format nil "~A://~A:~A"
					 (config-transport config)
					 (config-ip config)
					 (config-hb-port config))))
		;;(format t "heartbeat endpoint is: ~A~%" endpoint)
		(pzmq:bind hb-socket hb-endpoint)
		(let ((heartbeat-thread-id (start-heartbeat hb-socket)))
		  ;; main loop
		  (unwind-protect
		       (format t "[Kernel] Entering mainloop ...~%")
		       (shell-loop shell)
		    ;; clean up when exiting
		    (bordeaux-threads:destroy-thread heartbeat-thread-id)
		    (pzmq:close hb-socket)
		    (pzmq:close (iopub-socket iopub))
		    (pzmq:close (shell-socket shell))
		    (pzmq:ctx-destroy (kernel-ctx kernel))
		    (format t "Bye bye.~%")))))))))))

(defun start-heartbeat (socket)
  (let ((thread-id (bordeaux-threads:make-thread
		    (lambda ()
		      (format t "[Heartbeat] thread started~%")
		      (pzmq:proxy socket socket (cffi:null-pointer))))))

    ;; XXX: without proxy
    ;; (loop
    ;; 	 (pzmq:with-message msg
    ;; 	   (pzmq:msg-recv msg socket)
    ;; 			;;(format t "Heartbeat Received:~%")
    ;; 	   (pzmq:msg-send msg socket)
    ;; 			;;(format t "  | message: ~A~%" msg)
    ;; 	   ))))))
    thread-id))
