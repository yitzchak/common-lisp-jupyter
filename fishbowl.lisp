
(let ((cmd-args
       ;; Borrowed from apply-argv, command-line-arguments.  Temporary solution (?)
       ;; This is not PvE's code.
       #+sbcl sb-ext:*posix-argv*
       #+clozure CCL:*UNPROCESSED-COMMAND-LINE-ARGUMENTS*  ;(ccl::command-line-arguments)
       #+gcl si:*command-args*
       #+ecl (loop for i from 0 below (si:argc) collect (si:argv i))
       #+cmu extensions:*command-line-strings*
       #+allegro (sys:command-line-arguments)
       #+lispworks sys:*line-arguments-list*
       #+clisp ext:*args*
       #-(or sbcl clozure gcl ecl cmu allegro lispworks clisp)
       (error "get-argv not supported for your implementation")))
  (when (not (= (length cmd-args)
              #+sbcl 4
              #+clozure 3 
              #-(or sbcl clozure)
              (error "at this point only sbcl and clozure cl are supported")))
    (error "Wrong number of arguments (given ~A, expecting ~A)" (length cmd-args)
           #+sbcl 4
           #+clozure 3))
  (let ((def-dir (truename #+sbcl (cadr cmd-args)
                           #+clozure (car cmd-args)
                           #-(or sbcl clozure)
                           (error "at this point only sbcl and clozure cl are supported"))))
        ;;(run-dir (truename #+sbcl (caddr cmd-args)
        ;;                   #+clozure (cadr cmd-args)
        ;;                   #-(or sbcl clozure)
        ;;                   (error "at this point only sbcl and clozure cl are supported"))))

    ;; add the source directory to the ASDF registry
    (push def-dir asdf:*central-registry*)))    

;; activate debugging
(declaim (optimize (speed 0) (space 0) (debug 3) (safety 3)))

;; in production (?)
;;(declaim (optimize (speed 3) (space 0) (debug 0) (safety 2)))

(ql:quickload "fishbowl")

(in-package #:fishbowl-user)

;; start main loop
(fishbowl:kernel-start)

