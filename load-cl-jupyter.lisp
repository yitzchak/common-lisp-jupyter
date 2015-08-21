(require :asdf)

(let ((src-dir
        (make-pathname :device (pathname-device *load-pathname*) ;; :device if ever this is running on Windows ...
                       :directory (concatenate 'string (namestring (pathname-directory *load-pathname*)) "/src"))))
 (push src-dir asdf:*central-registry*)

;; activate debugging
(declaim (optimize (speed 0) (space 0) (debug 3) (safety 3)))

(ql:quickload "cl-jupyter")
