(load "quicklisp.lisp")

(quicklisp-quickstart:install)
(ql-util:without-prompting
  (ql:add-to-init-file))

(defvar root (uiop:getcwd))

(pushnew root ql:*local-project-directories*)
(ql:register-local-projects)

(defvar preamble (list (format nil "(pushnew ~S ql:*local-project-directories*)" (namestring root))
                   "(ql:register-local-projects)"))

(ql:quickload :common-lisp-jupyter)
(cl-jupyter:install :preamble preamble)
