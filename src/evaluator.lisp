(in-package #:cl-jupyter)

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

(defun evaluate-code (evaluator code)
  (format t "[Evaluator] Code to evaluate: ~W; (TYPE-OF CODE)=>~S~%" code (type-of code))
  (vector-push code (evaluator-history-in evaluator))
  (let ((execution-count (length (evaluator-history-in evaluator))))
    (let ((code-to-eval
            (handling-errors
              (let ((*package* (find-package :maxima)))
                (maxima::$parse_string (format nil "~A" code))))))
      (if (and (consp code-to-eval)
	       (eql (car code-to-eval) 'quicklisp-client:quickload)
	       (stringp (cadr code-to-eval)))
	  ;; quicklisp hook
	  (let ((results (multiple-value-list (ql:quickload (cadr code-to-eval)))))
	    (values execution-count results "" ""))
	  ;; else "normal" evaluation
	  (progn 
        (format t "[Evaluator] Code to evaluate: ~W~%" code-to-eval)
	    (let* ((stdout-str (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))
		   (stderr-str (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
	      (let ((results (with-output-to-string (stdout stdout-str)
			       (with-output-to-string (stderr stderr-str)
			         (let ((*standard-output* stdout)
				       (*error-output* stderr))
				   (handling-errors
					  ;(if (and (consp code-to-eval)
					  ;	(eql (car code-to-eval) 'quicklisp-client:quickload)
					  ;	(stringp (cadr code-to-eval)))
				    ;; quicklisp hook
					  ;  (multiple-value-list (ql:quickload (cadr code-to-eval)))
				    ;; normal evaluation
				    (multiple-value-list (let ((*package* (find-package :maxima))) (maxima::meval code-to-eval)))))))));)
	      ;;(format t "[Evaluator] : results = ~W~%" results)
	      (vector-push results (evaluator-history-out evaluator))
	      (values execution-count results stdout-str stderr-str))))))))

