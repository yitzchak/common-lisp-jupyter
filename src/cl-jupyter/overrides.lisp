(in-package #:common-lisp-jupyter)


#+sbcl
(sb-ext:without-package-locks
  (defun sb-impl::inspect (object)
    (let ((*standard-input* *query-io*)
          (*standard-output* *query-io*))
      (funcall sb-impl::*inspect-fun* object *query-io* *query-io*))))
