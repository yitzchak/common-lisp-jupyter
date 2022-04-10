(load "quicklisp.lisp")

(handler-bind
    ((simple-error (lambda (err)
                     (declare (ignore err))
                     (invoke-restart 'quicklisp-quickstart::load-setup))))
  (quicklisp-quickstart:install))

(let ((src-reg (uiop:xdg-config-home (make-pathname :directory '(:relative "common-lisp") :name "source-registry" :type "conf"))))
  (unless (probe-file src-reg)
    (push (uiop:getcwd) asdf:*central-registry*)
    (ensure-directories-exist src-reg)
    (with-open-file (stream src-reg :direction :output)
      (write `(:source-registry
               (:tree (,(uiop:getcwd)))
               :inherit-configuration)
             :stream stream))))

(ql-util:without-prompting
  (ql:add-to-init-file))

#+cmucl (handler-bind ((error (lambda (e)
                                (declare (ignore e))
                                (dolist (restart-name '(asdf:try-recompiling asdf:retry))
                                  (when (find-restart restart-name)
                                    (invoke-restart restart-name))))))
          (asdf:load-system :ironclad))

(ql:quickload :common-lisp-jupyter)

(clj:install :implementation t :bin-path (first (uiop:command-line-arguments)))
