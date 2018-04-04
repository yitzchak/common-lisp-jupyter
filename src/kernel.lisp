(in-package #:cl-jupyter)

(defclass kernel ()
  ((config :initarg :config :reader kernel-config)
   (ctx :initarg :ctx :reader kernel-ctx)
   (shell :initarg :shell :initform nil :reader kernel-shell)
   (stdin :initarg :stdin :initform nil :reader kernel-stdin)
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
   (stdin-port :initarg :stdin-port :reader config-stdin-port :type fixnum)
   (iopub-port :initarg :iopub-port :reader config-iopub-port :type fixnum)
   (control-port :initarg :control-port :reader config-control-port :type fixnum)
   (hb-port :initarg :hb-port :reader config-hb-port :type fixnum)
   (signature-scheme :initarg :signature-scheme :reader config-signature-scheme :type string)
   (key :initarg :key :reader kernel-config-key)))

(defun kernel-start (connection-file-name)
  (write-line "")
  (format t "~A: an enhanced interactive Maxima REPL~%" +KERNEL-IMPLEMENTATION-NAME+)
  (format t "(Version ~A - Jupyter protocol v.~A)~%"
          +KERNEL-IMPLEMENTATION-VERSION+
          +KERNEL-PROTOCOL-VERSION+)
  (format t "--> (C) 2014-2015 Frederic Peschanski (cf. LICENSE)~%")
  (write-line "")
  (format t "connection file = ~A~%" connection-file-name)
  (unless (stringp connection-file-name)
    (error "Wrong connection file argument (expecting a string)"))
  (let* ((config-js (jsown:parse (concat-all 'string "" (read-file-lines connection-file-name))))
         (config (make-instance 'kernel-config
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
    (when (not (string= (config-signature-scheme config) "hmac-sha256"))
      ;; XXX: only hmac-sha256 supported
      (error "Kernel only support signature scheme 'hmac-sha256' (provided ~S)" (config-signature-scheme config)))
      ;;(inspect config)
    (let* ((kernel (make-kernel config))
           (evaluator (make-evaluator kernel))
           (shell (make-shell-channel kernel))
           (stdin (make-stdin-channel kernel))
           (iopub (make-iopub-channel kernel))
           (hb-socket (pzmq:socket (kernel-ctx kernel) :rep)) ;; Launch the hearbeat thread
           (hb-endpoint (format nil "~A://~A:~A"
                                (config-transport config)
                                (config-ip config)
                                (config-hb-port config)))
           (hb-bind (pzmq:bind hb-socket hb-endpoint))
           (heartbeat-thread-id (start-heartbeat hb-socket)))
      ;; main loop
      (unwind-protect
           (progn (format t "[Kernel] Entering mainloop ...~%")
    	      (shell-loop shell))
        ;; clean up when exiting
        (bordeaux-threads:destroy-thread heartbeat-thread-id)
        (pzmq:close hb-socket)
        (pzmq:close (iopub-socket iopub))
        (pzmq:close (shell-socket shell))
        (pzmq:close (stdin-socket stdin))
        (pzmq:ctx-destroy (kernel-ctx kernel))
        (format t "Bye bye.~%")))))

;; This is the entry point for a saved lisp image created by
;; trivial-dump-core:save-executable or equivalent.
(defun kernel-start-exec ()
  ;; IS THERE OTHER STUFF HANDLED BY MAXIMA INIT-CL.LISP THAT WE NEED TO DUPLICATE HERE ??
  (setq *read-default-float-format* 'double-float)
  (kernel-start (car (last (get-argv)))))

;; This is the entry point for starting the kernel from within an existing
;; Maxima session.
(maxima::defmfun maxima::$kernel_start (connection-file-name)
  (kernel-start connection-file-name))

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
