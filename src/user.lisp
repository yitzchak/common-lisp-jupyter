
(in-package #:fishbowl-user)

;;; taken from: http://stackoverflow.com/questions/4425400/is-there-a-command-to-halt-the-interpreter-in-common-lisp
(defun quit ()
  ;; TODO : quit property from the Ipython point of view
  #+sbcl (sb-ext:quit)
  #+clisp (ext:exit)
  #+ccl (ccl:quit)
  #+allegro (excl:exit))



#|

  ## Basic PNG support ## 

|#

(defclass png-bytes ()
  ((bytes :initarg :bytes :reader png-bytes)))

(defmethod render-jpg ((img png-bytes))
  (cl-base64:usb8-array-to-base64-string bytes))

(defun png-from-file (filename)
  (let ((bytes (read-binary-file filename)))
    (display-png (make-instance 'png-bytes :bytes bytes))))

