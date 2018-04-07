(in-package #:cl-jupyter)

#|

# Evaluator #

The evaluator is where the "interesting stuff" takes place :
 user expressions are evaluated here.

The history of evaluations is also saved by the evaluator.

|#

(defclass evaluator ()
  ((history-in :initform (make-array 64 :fill-pointer 0 :adjustable t)
	       :reader evaluator-history-in)
   (history-out :initform (make-array 64 :fill-pointer 0 :adjustable t)
		:reader evaluator-history-out)))

(defun make-evaluator ()
  (make-instance 'evaluator))

;; Error message is sent to *error-output* and returned in eval-error pattern.
(defun make-eval-error (quit err msg)
  (let ((name (symbol-name (class-name (class-of err)))))
    (write-string msg *error-output*)
    `((eval-error) ,quit ,name ,msg)))

(define-condition maxima-syntax-error (error)
  ((message :initarg :message
            :reader maxima-syntax-error-message))
  (:documentation "Maxima syntax error.")
  (:report (lambda (condition stream)
             (write-string (maxima-syntax-error-message condition) stream))))

;;; Based on macro taken from: http://www.cliki.net/REPL
(defmacro handling-errors (&body body)
  `(catch 'maxima::return-from-debugger
    (handler-case (progn ,@body)
       (cl-jupyter-user::quit (err)
         (make-eval-error t err (format nil "~A" err)))
       (simple-condition (err)
         (make-eval-error nil err
           (apply #'format nil (simple-condition-format-control err)
                               (simple-condition-format-arguments err))))
       (condition (err)
         (make-eval-error nil err (format nil "~A" err))))))

(defun my-mread (input)
  (when (and (open-stream-p input) (peek-char nil input nil))
    (let ((maxima::*mread-prompt* "") (maxima::*prompt-on-read-hang*))
      (declare (special maxima::*mread-prompt* maxima::*prompt-on-read-hang*))
      (maxima::mread input nil))))

(defun eval-error-p (result)
  (and result (eq 'eval-error (caar result))))

(defun quit-eval-error-p (result)
  (and result (eq 'eval-error (caar result)) (cadr result)))

(let ((old-mread-synerr #'maxima::mread-synerr))
  (defun maxima::mread-synerr (&rest args)
    (error (make-condition 'maxima-syntax-error :message
      (with-output-to-string (*standard-output*)
        (catch 'maxima::macsyma-quit
          (apply old-mread-synerr args)))))))

(defun read-and-eval (input)
  (handling-errors
    (let ((code-to-eval (my-mread input)))
      (when code-to-eval
        (info "[evaluator] Parsed expression to evaluate: ~W~%" code-to-eval)
        (let* ((*package* (find-package :maxima))
               (result (maxima::with-$error (maxima::meval* code-to-eval))))
          (info "[evaluator] Evaluated result: ~W~%" result)
          (setq maxima::$% (caddr result))
          result)))))

(defun evaluate-code (evaluator code)
  (iter
    (initially
      (info "[evaluator] Unparsed input: ~W~%" code)
      (vector-push code (evaluator-history-in evaluator)))
    (with *standard-output* = (make-string-output-stream))
    (with *error-output* = (make-string-output-stream))
    (with input = (make-string-input-stream (add-terminator code)))
    (for result = (read-and-eval input))
    (while result)
    (collect result into results)
    (until (quit-eval-error-p result))
    (finally
      (vector-push results (evaluator-history-out evaluator))
      (return
        (values (length (evaluator-history-in evaluator))
                results
                (get-output-stream-string *standard-output*)
                (get-output-stream-string *error-output*))))))
