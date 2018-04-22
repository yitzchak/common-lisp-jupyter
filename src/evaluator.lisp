(in-package #:maxima-jupyter)

(defvar *kernel* nil)
(defvar *message* nil)
(defvar *payload* nil)
(defvar *page-output* nil)

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

(define-condition maxima-syntax-error (error)
  ((message :initarg :message
            :reader maxima-syntax-error-message))
  (:documentation "Maxima syntax error.")
  (:report (lambda (condition stream)
             (write-string (maxima-syntax-error-message condition) stream))))

;;; Based on macro taken from: http://www.cliki.net/REPL
(defmacro handling-errors (&body body)
  `(catch 'maxima::return-from-debugger
    (catch 'maxima::macsyma-quit
    (handler-case (progn ,@body)
       (quit (err)
         (make-eval-error err (format nil "~A" err) :quit t))
       (simple-condition (err)
         (make-eval-error err
           (apply #'format nil (simple-condition-format-control err)
                               (simple-condition-format-arguments err))))
       (condition (err)
         (make-eval-error err (format nil "~A" err)))))))

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

(defun keyword-lisp-p (code)
  (and (consp code)
       (or (equal ':lisp (car code)) (equal ':lisp-quiet (car code)))))

(defun keyword-command-p (code)
  (and (consp code) (keywordp (car code))))

(defparameter old-draw_gnuplot nil)

(defun get-draw-file-name (extension)
  (namestring
    (merge-pathnames
      (concatenate 'string (funcall 'maxima::get-option 'maxima::$file_name)
                           extension)
      (uiop:getcwd))))

(defun my-draw_gnuplot (&rest args)
  (let* ((result (apply old-draw_gnuplot args))
         (terminal (funcall 'maxima::get-option 'maxima::$terminal)))
    (case terminal
      ((maxima::$pdf maxima::$multipage_pdf maxima::$pdfcairo maxima::$multipage_pdfcairo)
        (send-result (make-file-result (get-draw-file-name ".pdf")
                                       :mime-type *pdf-mime-type*
                                       :display t)))
      ((maxima::$gif maxima::$animated_gif)
        (send-result (make-file-result (get-draw-file-name ".gif")
                                       :mime-type *gif-mime-type*
                                       :display t)))
      ((maxima::$png maxima::$pngcairo)
        (send-result (make-file-result (get-draw-file-name ".png")
                                       :mime-type *png-mime-type*
                                       :display t)))
      (maxima::$jpg
        (send-result (make-file-result (get-draw-file-name ".jpeg")
                                       :mime-type *jpeg-mime-type*
                                       :display t)))
      (maxima::$svg
        (send-result (make-file-result (get-draw-file-name ".svg")
                                       :mime-type *svg-mime-type*
                                       :display t))))
    result))

(defun my-eval (code)
  (when (and (fboundp 'maxima::draw_gnuplot) (not old-draw_gnuplot))
    (setq old-draw_gnuplot (symbol-function 'maxima::draw_gnuplot))
    (setf (symbol-function 'maxima::draw_gnuplot) #'my-draw_gnuplot))
  (let ((*package* (find-package :maxima)))
    (cond ((keyword-lisp-p code)
           (cons (list (car code))
                 (multiple-value-list (eval (cons 'progn code)))))
          ((keyword-command-p code)
           (cons (list (car code))
                 (maxima::break-call (car code) (cdr code)
                                     'maxima::break-command)))
          (t
           (setq maxima::$__ (third code))
           (let ((result (maxima::meval* code)))
           (setq maxima::$_ maxima::$__)
           result)))))

(defun read-and-eval (input in-maxima)
  (catch 'state-change
    (handling-errors
      (let ((code-to-eval (if in-maxima
                            (my-mread input)
                            (my-lread input))))
        (if code-to-eval
          (progn
            (info "[evaluator] Parsed expression to evaluate: ~W~%" code-to-eval)
            (when in-maxima
              (incf maxima::$linenum)
              (let ((label (maxima::makelabel maxima::$inchar)))
                (unless maxima::$nolabels
                  (setf (symbol-value label) (third code-to-eval)))))
            (let ((result (if in-maxima
                            (my-eval code-to-eval)
                            (eval code-to-eval))))
              (info "[evaluator] Evaluated result: ~W~%" result)
              (when (and in-maxima (not (keyword-result-p result)))
                (setq maxima::$% (caddr result)))
              result))
          'no-more-code)))))

(defun make-maxima-label (result)
  (cond
    ((displayinput-result-p result)
      (let ((label (maxima::makelabel maxima::$outchar)))
        (unless maxima::$nolabels
          (setf (symbol-value label) (third result)))
        (make-maxima-result `((maxima::mlabel) ,label ,(third result)))))
    ((lisp-result-p result)
      (make-lisp-result (second result)))))

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
                            (make-maxima-label result)
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

(defun send-result (result)
  (let ((iopub (kernel-iopub *kernel*))
        (execute-count (+ 1 (length (evaluator-history-in (kernel-evaluator *kernel*))))))
    (if (typep result 'error-result)
      (send-execute-error iopub *message* execute-count
                          (error-result-ename result)
                          (error-result-evalue result))
      (let ((data (render result)))
        (when data
          (if (result-display result)
            (send-display-data iopub *message* data)
            (send-execute-result iopub *message* execute-count data)))))))

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

(defun to-lisp ()
  (setf (evaluator-in-maxima (kernel-evaluator *kernel*)) nil)
  (throw 'state-change 'no-output))

(defun to-maxima ()
  (setf (evaluator-in-maxima (kernel-evaluator *kernel*)) t)
  (throw 'state-change 'no-output))

(defun set-next-input (text &optional (replace nil))
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

(defun enqueue-input (text)
  (cl-containers:enqueue (kernel-input-queue *kernel*) text))

(defun my-displa (form)
  (if (mtext-result-p form)
    (let ((maxima::*alt-display1d* nil)
          (maxima::*alt-display2d* nil))
      (maxima::displa form))
    (send-result
      (make-maxima-result form :display t))))
