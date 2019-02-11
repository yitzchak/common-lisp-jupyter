(in-package #:jupyter)

#|

# Evaluator #

The evaluator is where the "interesting stuff" takes place :
 user expressions are evaluated here.

The history of evaluations is also saved by the evaluator.

|#

(defun make-eval-error (err msg &key (quit nil))
  (let ((name (symbol-name (class-name (class-of err)))))
    (write-string msg *error-output*)
    (make-error-result name msg :quit quit)))

(define-condition quit (error)
  ()
  (:documentation "A quit condition for identifying a request for kernel shutdown.")
  (:report (lambda (c stream) (declare (ignore c stream)))))

;;; Based on macro taken from: http://www.cliki.net/REPL
(defmacro handling-errors (&body body)
  `(handler-case
    (handler-bind
      ((simple-warning
        (lambda (wrn)
          (apply (function format) *standard-output*
                (simple-condition-format-control   wrn)
                (simple-condition-format-arguments wrn))
          (format *standard-output* "~&")
          (muffle-warning)))
      (warning
        (lambda (wrn)
          (format *standard-output* "~&~A: ~%  ~A~%"
                  (class-name (class-of wrn)) wrn)
          (muffle-warning))))
    	 (progn ,@body))
     (quit (err)
       (make-eval-error err (format nil "~A" err) :quit t))
     (simple-condition (err)
       (make-eval-error err
         (apply #'format nil (simple-condition-format-control err)
                             (simple-condition-format-arguments err))))
     (condition (err)
       (make-eval-error err (format nil "~A" err)))))

(defun eval-error-p (result)
  (typep result 'error-result))

(defun quit-eval-error-p (result)
  (and (typep result 'error-result) (error-result-quit result)))

(defun send-result (result)
  (let ((iopub (kernel-iopub *kernel*))
        (execute-count (+ 1 (length (kernel-history-in *kernel*)))))
    (if (typep result 'error-result)
      (send-execute-error iopub *message* execute-count
                          (error-result-ename result)
                          (error-result-evalue result))
      (let ((data (render result)))
        (when data
          (if (result-display result)
            (send-display-data iopub *message* data)
            (send-execute-result iopub *message* execute-count data)))))))

(defun set-next-input (text &optional (replace nil))
  (declare (ignore replace))
  (vector-push-extend (jsown:new-js
                        ("source" "set_next_input")
                        ("text" text))
                      *payload*))

(defun page (result &optional (start 0))
  (vector-push-extend (jsown:new-js
                        ("source" "page")
                        ("data" (render result))
                        ("start" start))
                      *payload*))

(defun enqueue-input (kernel text)
  (cl-containers:enqueue (kernel-input-queue kernel) text))
