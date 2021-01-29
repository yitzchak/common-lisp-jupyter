(load "quicklisp.lisp")

(ql-util:without-prompting
  (ql:add-to-init-file))

(ql:quickload :common-lisp-jupyter)

(cl-jupyter:install :use-implementation t)
