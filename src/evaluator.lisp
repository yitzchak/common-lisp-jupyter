(in-package #:maxima-jupyter)

(defvar *kernel* nil)
(defvar *message* nil)

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
                :reader evaluator-history-out)
   (in-maxima :initform t
              :accessor evaluator-in-maxima)))

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

(defun my-lread (input)
  (when (and (open-stream-p input) (peek-char nil input nil))
    (read input)))

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

(defun keyword-lisp-p (code)
  (and (consp code)
       (or (equal ':lisp (car code)) (equal ':lisp-quiet (car code)))))

(defun keyword-command-p (code)
  (and (consp code) (keywordp (car code))))

(defun my-eval (code)
  (let ((*package* (find-package :maxima)))
    (cond ((keyword-lisp-p code)
           (cons (list (car code))
                 (multiple-value-list (eval (cons 'progn code)))))
          ((keyword-command-p code)
           (cons (list (car code))
                 (maxima::break-call (car code) (cdr code)
                                     'maxima::break-command)))
          (t
           (maxima::meval* code)))))

(defun read-and-eval (input in-maxima)
  (catch 'state-change
    (handling-errors
      (let ((code-to-eval (if in-maxima
                            (my-mread input)
                            (my-lread input))))
        (if code-to-eval
          (progn
            (info "[evaluator] Parsed expression to evaluate: ~W~%" code-to-eval)
            (let ((result (if in-maxima
                            (my-eval code-to-eval)
                            (eval code-to-eval))))
              (info "[evaluator] Evaluated result: ~W~%" result)
              (when (and in-maxima (not (keyword-result-p result)))
                (setq maxima::$% (caddr result)))
              result))
          'no-more-code)))))

(defun evaluate-code (evaluator code)
  (iter
    (initially
      (info "[evaluator] Unparsed input: ~W~%" code)
      (vector-push code (evaluator-history-in evaluator)))
    (with input = (make-string-input-stream code))
    (for in-maxima = (evaluator-in-maxima evaluator))
    (for result = (read-and-eval input in-maxima))
    (until (eq result 'no-more-code))
    (for wrapped-result = (if in-maxima
                            (make-maxima-result result)
                            (make-lisp-result result)))
    (when wrapped-result
      (send-result wrapped-result)
      (collect wrapped-result into results))
    (until (quit-eval-error-p wrapped-result))
    (finally
      (vector-push results (evaluator-history-out evaluator))
      (return
        (values (length (evaluator-history-in evaluator))
                results)))))

(defun my-dbm-prompt (at)
  (format nil "~@[(~a:~a) ~]"
              (unless (stringp at) "dbm")
              (length maxima::*quit-tags*)))

(defun maxima::set-env (bkpt)
  (format *debug-io*
          (intl:gettext "(~a line ~a~@[, in function ~a~])")
          (maxima::short-name (maxima::bkpt-file bkpt))
	  (maxima::bkpt-file-line bkpt)
	  (maxima::bkpt-function bkpt))
  (format *debug-io* "~&~a:~a::~%" (maxima::bkpt-file bkpt)
	  (maxima::bkpt-file-line bkpt)))

(defun maxima::break-frame (&optional (n 0) (print-frame-number t))
  (maxima::restore-bindings)
  (multiple-value-bind (fname vals params backtr lineinfo bdlist)
      (maxima::print-one-frame n print-frame-number)
    backtr params vals fname
    (maxima::remove-bindings bdlist)
    (when lineinfo
      (fresh-line *debug-io*)
      (format *debug-io* "~a:~a::~%" (cadr lineinfo) (+ 0 (car lineinfo))))
    (values)))

(defun maxima::break-dbm-loop (at)
  (let* ((maxima::*quit-tags* (cons (cons maxima::*break-level* maxima::*quit-tag*) maxima::*quit-tags*))
         (maxima::*break-level* (if (not at) maxima::*break-level* (cons t maxima::*break-level*)))
         (maxima::*quit-tag* (cons nil nil))
         (maxima::*break-env* maxima::*break-env*)
         (maxima::*mread-prompt* "")
         (maxima::*diff-bindlist* nil)
         (maxima::*diff-mspeclist* nil)
	       val)
    (declare (special maxima::*mread-prompt*))
    (and (consp at) (maxima::set-env at))
    (cond ((null at)
           (maxima::break-frame 0 nil)))
    (catch 'maxima::step-continue
      (catch maxima::*quit-tag*
        (unwind-protect
          (do ((stdin (kernel-stdin *kernel*))
               (prompt (my-dbm-prompt at) (my-dbm-prompt at)))
              (())
            (finish-output *debug-io*)
	          (setq val (catch 'maxima::macsyma-quit
                        (let* ((inp (get-input stdin *message* prompt))
                               (res (with-input-from-string (f inp)
                                      (maxima::dbm-read f nil))))
                          (declare (special maxima::*mread-prompt*))
                          (cond ((keyword-lisp-p res)
                                 (send-result
                                   (make-maxima-result (cons (list (car res))
                                     (multiple-value-list (eval (cons 'progn res)))))))
                                ((keyword-command-p res)
                                 (let ((value (maxima::break-call (car res) (cdr res) 'maxima::break-command)))
                                   (cond ((eq value :resume) (return)))))
                                ((eq res maxima::*top-eof*)
                                 (funcall (get :top 'maxima::break-command)))
                                (t
                                 (let ((v (maxima::meval* res)))
                        				   (setq maxima::$% (third v))
                                   (send-result (make-maxima-result v)))))
			                    nil)))
	          (and (eql val 'maxima::top)
		        (maxima::throw-macsyma-top)))
	        (maxima::restore-bindings))))))

;; Redefine RETRIEVE in src/macsys.lisp to make use of input-request/input-reply.
;; MSG, FLAG, and PRINT? are declared special there, so be careful to
;; refer to those symbols in the :maxima package.

(defun maxima::retrieve (maxima::msg maxima::flag &aux (maxima::print? nil))
  (declare (special maxima::msg maxima::flag maxima::print?))
  (or (eq maxima::flag 'maxima::noprint) (setq maxima::print? t))
  (let* ((retrieve-prompt (cond ((not maxima::print?)
                                 (setq maxima::print? t)
                                 (format nil ""))
                                ((null maxima::msg)
                                 (format nil ""))
                                ((atom maxima::msg)
                                 (format nil "~A" maxima::msg))
                                ((eq maxima::flag t)
                                 (format nil "~{~A~}" (cdr maxima::msg)))
                                (t
                                 (maxima::aformat nil "~M" maxima::msg))))
         (stdin (kernel-stdin *kernel*)))
    (let ((value (get-input stdin *message* retrieve-prompt)))
      (maxima::mread-noprompt (make-string-input-stream (add-terminator value)) nil))))

(defun send-result (result)
  (let ((iopub (kernel-iopub *kernel*))
        (execute-count (+ 1 (length (evaluator-history-in (kernel-evaluator *kernel*))))))
    (if (typep result 'error-result)
      (send-execute-error iopub *message* execute-count
                          (error-result-ename result)
                          (error-result-evalue result))
      (let ((data (display result)))
        (when data
          (send-execute-result iopub *message* execute-count data))))))

(defun state-change-p (expr)
  (and (listp expr)
       (or (eq (car expr) 'maxima::$to_lisp)
           (eq (car expr) 'maxima::to-maxima)
           (some #'state-change-p expr))))

(defun is-complete (evaluator code)
  (handler-case
    (iter
      (with *standard-output* = (make-string-output-stream))
      (with *error-output* = (make-string-output-stream))
      (with input = (make-string-input-stream code))
      (with in-maxima = (evaluator-in-maxima evaluator))
      (for char = (peek-char nil input nil))
      (while char)
      (for parsed = (if in-maxima (maxima::dbm-read input nil) (my-lread input)))
      (when (state-change-p parsed)
        (leave +status-unknown+))
      (finally (return +status-complete+)))
    (end-of-file ()
      +status-incomplete+)
    #+sbcl (sb-int:simple-reader-error ()
      +status-incomplete+)
    (simple-condition (err)
      (if (equal (simple-condition-format-control err)
                 "parser: end of file while scanning expression.")
        +status-incomplete+
        +status-invalid+))
    (condition ()
      +status-invalid+)
    (simple-error ()
      +status-invalid+)))

(defun maxima::$to_lisp ()
  (setf (evaluator-in-maxima (kernel-evaluator *kernel*)) nil)
  (format t "~&Type (to-maxima) to restart, ($quit) to quit Maxima.~%")
  (throw 'state-change 'no-output))

(defun maxima::to-maxima ()
  (setf (evaluator-in-maxima (kernel-evaluator *kernel*)) t)
  (format t "Returning to Maxima~%")
  (throw 'state-change 'no-output))
