(load "quicklisp.lisp")

(handler-bind
    ((simple-error (lambda (err)
                     (declare (ignore err))
                     (invoke-restart 'quicklisp-quickstart::load-setup))))
  (quicklisp-quickstart:install))

(ql-util:without-prompting
  (ql:add-to-init-file))

(ql:quickload :common-lisp-jupyter)

(clj:install :use-implementation t :bin-path (first (uiop:command-line-arguments)))
