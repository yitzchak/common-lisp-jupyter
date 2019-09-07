(load "quicklisp.lisp")

(quicklisp-quickstart:install)
(ql-util:without-prompting
  (ql:add-to-init-file))

(ql:quickload :ziz)

(ziz:with-distribution (dist :releases '("."))
  (ql-dist:install-dist (ziz:distribution-info-url dist) :prompt nil)
  (ql:quickload :common-lisp-jupyter))

(cl-jupyter:install)
