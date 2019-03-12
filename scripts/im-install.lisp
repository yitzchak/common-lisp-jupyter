(load "quicklisp.lisp")

(quicklisp-quickstart:install)
(ql-util:without-prompting
  (ql:add-to-init-file))

(pushnew (uiop:getcwd) ql:*local-project-directories*)
(ql:register-local-projects)

(ql:quickload :common-lisp-jupyter)
(cl-jupyter:install-image)
