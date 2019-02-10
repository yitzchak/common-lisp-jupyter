(pushnew (make-pathname :device (pathname-device *load-truename*)
                        :directory (pathname-directory *load-truename*))
         ql:*local-project-directories*)

(ql:register-local-projects)

(ql:quickload :cl-jupyter :silent t)
