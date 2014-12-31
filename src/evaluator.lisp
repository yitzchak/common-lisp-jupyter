(in-package #:uncommonshell)

#|

# Evaluator #

The evaluator is where the "interesting stuff" takes place :
 user expressions are evaluated here. 

The history of evaluations is also saved by the evaluator.

|#


(defclass evaluator ()
  ((kernel :initarg :kernel :reader evaluator-kernel)
   (execution-count :initform 0 :reader evaluator-execution-count :type integer)
   (history :initform (make-array 64 :fill-pointer 0 :adjustable t)
	    :reader evaluator-history)))

(defun make-evaluator (kernel)
  (let ((evaluator (make-instance 'evaluator
				  :kernel kernel)))
    (setf (slot-value kernel 'evaluator) evaluator)
    evaluator))

(defun evaluate-code (evaluator code)
  (let ((code-to-eval (read-from-string code)))
    (format t "[Evaluator] evaluating: ~A~%" code-to-eval)
    (let ((results (multiple-value-list (eval code-to-eval))))
      (format t "[Evaluator] : results = ~W~%" results))))

