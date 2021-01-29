(load "quicklisp.lisp")

(unless (probe-file (merge-pathnames (make-pathname :directory '(:relative "quicklisp"))
                                     (uiop:getenv-pathname "HOME" :ensure-directory t)))
  (quicklisp-quickstart:install))

(ql-util:without-prompting
  (ql:add-to-init-file))

(ql:quickload :common-lisp-jupyter)

(clj:install :use-implementation t :bin-path (first (uiop:command-line-arguments)))
