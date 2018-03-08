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

;;; macro taken from: http://www.cliki.net/REPL
(defmacro handling-errors (&body body)
  `(catch 'maxima::return-from-debugger
    (handler-case (progn ,@body)
       (simple-condition (err)
         (format *error-output* "~&~A: ~%" (class-name (class-of err)))
         (apply (function format) *error-output*
                (simple-condition-format-control   err)
                (simple-condition-format-arguments err))
         (format *error-output* "~&"))
       (condition (err)
         (format *error-output* "~&~A: ~%  ~S~%"
                 (class-name (class-of err)) err)))))

(defun my-mread (input)
  (when (and (open-stream-p input) (peek-char nil input nil))
    (let ((maxima::*mread-prompt* "") (maxima::*prompt-on-read-hang*))
      (declare (special maxima::*mread-prompt* maxima::*prompt-on-read-hang*))
      (maxima::mread input nil))))

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
                    ((not code-to-eval) (reverse results))
                    (when maxima::$debug_evaluator
                      (format t "[Evaluator] parsed expression to evaluate: ~W~%" code-to-eval)
                      (terpri))
                    (let ((result (handling-errors
                                   (let ((*standard-output* stdout)
                                         (*error-output* stderr)
                                         (*package* (find-package :maxima)))
                              				   (maxima::meval* code-to-eval)))))
                      (when maxima::$debug_evaluator
                        (format t "[Evaluator] evaluated result: ~W~%" result)
                        (terpri))
                      (setq maxima::$% (caddr result))
                  	  (setq results (cons result results))))))
    (vector-push results (evaluator-history-out evaluator))
    (values execution-count results
            (get-output-stream-string stdout) (get-output-stream-string stderr))))
