(in-package #:cl-jupyter)

#|

# Evaluator #

The evaluator is where the "interesting stuff" takes place :
 user expressions are evaluated here.

The history of evaluations is also saved by the evaluator.

|#

(defparameter maxima::$debug_evaluator nil)

(defclass evaluator ()
  ((kernel :initarg :kernel :reader evaluator-kernel)
   (history-in :initform (make-array 64 :fill-pointer 0 :adjustable t)
	       :reader evaluator-history-in)
   (history-out :initform (make-array 64 :fill-pointer 0 :adjustable t)
		:reader evaluator-history-out)))

(defun make-evaluator (kernel)
  (let ((evaluator (make-instance 'evaluator
				  :kernel kernel)))
    (setf (slot-value kernel 'evaluator) evaluator)
    evaluator))

;; Error message is sent to *error-output* and returned in eval-error pattern.
(defun make-eval-error (quit err msg)
  (let ((name (symbol-name (class-name (class-of err)))))
    (write-string msg *error-output*)
    `((eval-error) ,quit ,name ,msg)))

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

(defun evaluate-code (evaluator code)
  (when maxima::$debug_evaluator
    (format t "[Evaluator] unparsed input: ~W~%" code)
    (terpri))
  (vector-push code (evaluator-history-in evaluator))
  (let* ((execution-count (length (evaluator-history-in evaluator)))
         (stdout (make-string-output-stream))
         (stderr (make-string-output-stream))
         (input (make-string-input-stream (add-terminator code)))
         (results (do ((results '())
                       (code-to-eval (my-mread input) (my-mread input)))
                      ((or (not code-to-eval) (quit-eval-error-p (last results)))
                       (reverse results))
                    (when maxima::$debug_evaluator
                      (format t "[Evaluator] parsed expression to evaluate: ~W~%" code-to-eval)
                      (terpri))
                    (let* ((*standard-output* stdout)
                           (*error-output* stderr)
                           (result (handling-errors
                                     (let ((*package* (find-package :maxima)))
                                       (maxima::with-$error
                                         (maxima::meval* code-to-eval))))))
                      (when maxima::$debug_evaluator
                        (format t "[Evaluator] evaluated result: ~W~%" result)
                        (terpri))
                      (unless (eval-error-p result)
                        (setq maxima::$% (caddr result)))
                      (setq results (cons result results))))))
    (vector-push results (evaluator-history-out evaluator))
    (values execution-count results
            (get-output-stream-string stdout) (get-output-stream-string stderr))))
