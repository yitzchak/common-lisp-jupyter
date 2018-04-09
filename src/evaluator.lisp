(in-package #:maxima-jupyter)

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

(defun make-eval-error (err msg &key (quit nil))
  (let ((name (symbol-name (class-name (class-of err)))))
    (write-string msg *error-output*)
    (make-error-result name msg :quit quit)))

(define-condition quit (error)
  ()
  (:documentation "A quit condition for identifying a request for kernel shutdown.")
  (:report (lambda (c stream))))

(maxima::defmfun maxima::$quit ()
  (error (make-condition 'quit)))

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
       (quit (err)
         (make-eval-error err (format nil "~A" err) :quit t))
       (simple-condition (err)
         (make-eval-error err
           (apply #'format nil (simple-condition-format-control err)
                               (simple-condition-format-arguments err))))
       (condition (err)
         (make-eval-error err (format nil "~A" err))))))

(defun my-mread (input)
  (when (and (open-stream-p input) (peek-char nil input nil))
    (let ((maxima::*mread-prompt* "")
          (maxima::*prompt-on-read-hang*))
      (declare (special maxima::*mread-prompt*
                        maxima::*prompt-on-read-hang*))
      (maxima::dbm-read input nil))))

(defun eval-error-p (result)
  (typep result 'error-result))

(defun quit-eval-error-p (result)
  (and (typep result 'error-result) (error-result-quit result)))

(let ((old-mread-synerr #'maxima::mread-synerr))
  (defun maxima::mread-synerr (&rest args)
    (error (make-condition 'maxima-syntax-error :message
      (with-output-to-string (*standard-output*)
        (catch 'maxima::macsyma-quit
          (apply old-mread-synerr args)))))))

(defun my-eval (code)
  (let ((*package* (find-package :maxima)))
    (cond ((and (consp code) (equal ':lisp (car code)))
           (cons (list ':lisp) (multiple-value-list (eval (cons 'progn code)))))
          ((and (consp code) (keywordp (car code)))
           (maxima::break-call (car code) (cdr code) 'maxima::break-command))
          (t
           (maxima::meval* code)))))

(defun read-and-eval (input)
  (handling-errors
    (let ((code-to-eval (my-mread input)))
      (when code-to-eval
        (info "[evaluator] Parsed expression to evaluate: ~W~%" code-to-eval)
        (let ((result (my-eval code-to-eval)))
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
    (for wrapped-result = (make-maxima-result result))
    (if wrapped-result
      (collect wrapped-result into results))
    (until (quit-eval-error-p wrapped-result))
    (finally
      (vector-push results (evaluator-history-out evaluator))
      (return
        (values (length (evaluator-history-in evaluator))
                results
                (get-output-stream-string *standard-output*)
                (get-output-stream-string *error-output*))))))
