(in-package #:uncommonshell)

#|

# Evaluator #

The evaluator is where the "interesting stuff" takes place :
 user expressions are evaluated here. 

The history of evaluations is also saved by the evaluator.

|#


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

(defun evaluate-code (evaluator code)
  (vector-push code (evaluator-history-in evaluator))
  (let ((execution-count (length (evaluator-history-in evaluator))))
    (let ((code-to-eval (read-from-string code)))
      (format t "[Evaluator] evaluating: ~A~%" code-to-eval)
      (let ((results (multiple-value-list (eval code-to-eval))))
	(format t "[Evaluator] : results = ~W~%" results)
	(vector-push results (evaluator-history-out evaluator))
	(values execution-count results)))))

