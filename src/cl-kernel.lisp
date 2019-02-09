(in-package #:cl-jupyter)

(defclass kernel (jupyter-kernel:kernel)
  ())

(defmethod jupyter-kernel:is-complete ((k kernel) code)
  "complete")

(defmethod jupyter-kernel:evaluate ((k kernel) code)
  (iter
    (for sexpr in-stream (make-string-input-stream code))
    (for result = (jupyter-kernel:make-lisp-result
                    (jupyter-kernel:handling-errors
                      (eval sexpr))))
    (when result
      (collect result))
    (until (jupyter-kernel:quit-eval-error-p result))))
    ; (finally
    ;   (return results))))
