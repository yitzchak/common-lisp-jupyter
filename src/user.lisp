
(in-package #:fishbowl-user)

;;; taken from: http://stackoverflow.com/questions/4425400/is-there-a-command-to-halt-the-interpreter-in-common-lisp
(defun quit ()
  ;; TODO : quit property from the Ipython point of view
  #+sbcl (sb-ext:quit)
  #+clisp (ext:exit)
  #+ccl (ccl:quit)
  #+allegro (excl:exit))

