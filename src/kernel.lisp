(in-package #:uncommonshell)

(defclass kernel ()
  ((ctx :initarg :ctx :reader kernel-ctx)
   (shell :initarg :shell :reader kernel-shell)))


(defun make-kernel (shell-endpoint)
    (let ((ctx (pzmq:ctx-new)))
      (make-instance 'kernel
                     :ctx ctx
                     :shell (make-shell-channel ctx shell-endpoint))))


(defun get-argv ()
  ;; Borrowed from apply-argv, command-line-arguments.  Temporary solution.
  ;; This is not PvE's code.
  #+sbcl (cdr sb-ext:*posix-argv*)
  #+clozure (cdr (ccl::command-line-arguments))
  #+gcl (cdr si:*command-args*)
  #+ecl (loop for i from 1 below (si:argc) collect (si:argv i))
  #+cmu (cdr extensions:*command-line-strings*)
  #+allegro (cdr (sys:command-line-arguments))
  #+lispworks (cdr sys:*line-arguments-list*)
  #+clisp ext:*args*
  #-(or sbcl clozure gcl ecl cmu allegro lispworks clisp)
  (error "get-argv not supported for your implementation"))





     
                                 
                      
