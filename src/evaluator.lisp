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
  (:documentation "A condition for identifying a request for kernel shutdown.")
  (:report (lambda (c stream) (declare (ignore c stream)))))

(defun eval-error-p (result)
  (typep result 'error-result))

(defun quit-eval-error-p (result)
  (and (typep result 'error-result) (error-result-quit result)))

(defun send-result (result)
  (with-slots (iopub package history-in) *kernel*
    (let ((execute-count (+ 1 (length history-in))))
      (if (typep result 'error-result)
        (send-execute-error iopub *message* execute-count
                            (error-result-ename result)
                            (error-result-evalue result))
        (let ((data (let ((*package* (find-package package)))
                      (render result))))
          (when data
            (if (result-display result)
              (send-display-data iopub *message* data)
              (send-execute-result iopub *message* execute-count data))))))))

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
