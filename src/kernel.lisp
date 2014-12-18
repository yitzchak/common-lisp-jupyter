(in-package #:uncommonshell)

(defclass kernel ()
  ((config :initarg :config :reader kerner-config)
   (ctx :initarg :ctx :reader kernel-ctx)
   (shell :initarg :shell :initform nil :reader kernel-shell))
  (:documentation "Kernel state representation."))

(defun make-kernel (config)
  (let ((ctx (pzmq:ctx-new)))
    (make-instance 'kernel
                   :config config
                   :ctx ctx)))

(defun get-argv ()
  ;; Borrowed from apply-argv, command-line-arguments.  Temporary solution (?)
  ;; This is not PvE's code.
  #+sbcl sb-ext:*posix-argv*
  #+clozure (ccl::command-line-arguments)
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
           "   ||uncommon shell  || |      |         | |      "
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

(banner)


(defun read-file-lines (filename)
  (with-open-file (input filename)
    (loop
       for line = (read-line input nil 'eof)
       until (eq line 'eof)
       collect line)))

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
  (let ((cmd-args (get-argv)))
    (princ (banner))
    (write-line "")
    (format t "UncommonShell: a Common Lisp Ipython-compatible kernel~%")
    (format t "--> (C) 2014 Frederic Peschanski (cf.  LICENSE)~%")
    (write-line "")
    (unless (stringp (cadr cmd-args))
      (error "Missing connection file argument"))
    (let ((config-alist (json:decode-json-from-string (concat-all 'string "" (read-file-lines (cadr cmd-args))))))
      (let ((config
             (make-instance 'kernel-config
                            :transport (afetch :transport config-alist)
                            :ip (afetch :ip config-alist)
                            :shell-port (afetch :shell--port config-alist)
                            :iopub-port (afetch :iopub--port config-alist)
                            :control-port (afetch :control--port config-alist)
                            :hb-port (afetch :hb--port config-alist)
                            :signature-scheme (afetch :signature--scheme config-alist)
                            :key (afetch :key config-alist))))
        (inspect config)
        (let* ((kernel (make-kernel config))
               (shell (make-shell-channel kernel)))
          (format t "Entering mainloop ...")
          (shell-loop shell))))))





     
                                 
                      
