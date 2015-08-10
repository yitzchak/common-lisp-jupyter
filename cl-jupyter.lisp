
(let ((cmd-args
       ;; Borrowed from apply-argv, command-line-arguments.  Temporary solution (?)
       ;; This is not PvE's code.
       #+sbcl (cdr sb-ext:*posix-argv*) ; remove the program argument
       #+clozure CCL:*UNPROCESSED-COMMAND-LINE-ARGUMENTS*  ;(ccl::command-line-arguments)
       #+gcl si:*command-args*
       #+ecl (loop for i from 0 below (si:argc) collect (si:argv i))
       #+cmu extensions:*command-line-strings*
       #+allegro (sys:command-line-arguments)
       #+lispworks sys:*line-arguments-list*
       #+clisp ext:*args*
       #-(or sbcl clozure gcl ecl cmu allegro lispworks clisp)
       (error "get-argv not supported for your implementation")))
  (when (not (>= (length cmd-args) 3))
    (error "Wrong number of arguments (given ~A, expecting at least 3)" (length cmd-args)))
  (let ((def-dir (truename (car (last cmd-args 3)))))
        ;;(run-dir (truename (cadr cmd-args))))
    ;; add the source directory to the ASDF registry
    ;; (format t "Definition dir = ~A~%" def-dir)
    (push def-dir asdf:*central-registry*)))

;; for debugging
;;(push (truename "./src") asdf:*central-registry*)

;; activate debugging
(declaim (optimize (speed 0) (space 0) (debug 3) (safety 3)))

;; in production (?)
;;(declaim (optimize (speed 3) (space 0) (debug 0) (safety 2)))

(ql:quickload "fishbowl")

(in-package #:fishbowl-user)

;; start main loop
(fishbowl:kernel-start)

