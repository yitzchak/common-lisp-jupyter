(require :asdf)
#+sbcl (require :sb-rotate-byte)

(let ((src-dir
        (make-pathname :device (pathname-device *load-truename*) ;; if ever this is running on Windows ...
                       :directory (pathname-directory *load-truename*)
                       :name "src")))
 ;; Why do I have to append the slash? Without it, ASDF complains that SRC-DIR
 ;; "is not an absolute path". Oh, for crying out loud.
 (push (concatenate 'string (namestring src-dir) "/") asdf:*central-registry*))

(format t "now ASDF:*CENTRAL-REGISTRY*=~S~%" asdf:*central-registry*)

;; activate debugging
(declaim (optimize (speed 0) (space 0) (debug 3) (safety 3)))

(maxima::$load "stringproc")
(ql:quickload "maxima-jupyter")
