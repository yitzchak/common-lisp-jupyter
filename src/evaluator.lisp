(in-package #:jupyter-kernel)

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
  `(handler-case
    (handler-bind
      ((simple-warning
        (lambda (wrn)
          (apply (function format) *standard-output*
                (simple-condition-format-control   wrn)
                (simple-condition-format-arguments wrn))
          (format *standard-output* "~&")
          (muffle-warning)))
      (warning
        (lambda (wrn)
          (format *standard-output* "~&~A: ~%  ~A~%"
                  (class-name (class-of wrn)) wrn)
          (muffle-warning))))
    	 (progn ,@body))
     (quit (err)
       (make-eval-error err (format nil "~A" err) :quit t))
     (simple-condition (err)
       (make-eval-error err
         (apply #'format nil (simple-condition-format-control err)
                             (simple-condition-format-arguments err))))
     (condition (err)
       (make-eval-error err (format nil "~A" err)))))

(defun eval-error-p (result)
  (typep result 'error-result))

(defun quit-eval-error-p (result)
  (and (typep result 'error-result) (error-result-quit result)))

(defun keyword-lisp-p (code)
  (and (consp code)
       (or (equal ':lisp (car code)) (equal ':lisp-quiet (car code)))))

(defun keyword-command-p (code)
  (and (consp code) (keywordp (car code))))

; (defgeneric read-and-eval (kernel input))
;
; (defgeneric wrap-result (kernel result))
;
; (defmethod read-and-eval ((k kernel) input)
;   (handling-errors
;     (let (code (read input nil 'no-more-code))
;       (info "read-and-eval ~A~%" code)
;       (if (equalp code 'no-more-code)
;         code
;         (eval code)))))

; (defun make-maxima-label (result)
;   (cond
;     ((displayinput-result-p result)
;       (let ((label (maxima::makelabel maxima::$outchar)))
;         (unless maxima::$nolabels
;           (setf (symbol-value label) (third result)))
;         (make-maxima-result `((maxima::mlabel) ,label ,(third result)))))
;     ((lisp-result-p result)
;       (make-lisp-result (second result)))))

; (defmethod wrap-result ((k kernel) result)
;   (make-lisp-result result))

; (defun do-evaluate (k code)
;   (iter
;     (initially
;       (info "[kernel] Unparsed input: ~W~%" code)
;       (vector-push code (kernel-history-in k)))
;     ; (with input = (make-string-input-stream code))
;     (for sexpr in-stream (make-string-input-stream code))
;     (for result = (make-lisp-result (handling-errors (eval sexpr))))
;     ; (until (eq result 'no-more-code))
;     ; (for wrapped-result = (wrap-result k result))
;     (when result
;       (send-result result)
;       (collect result into results))
;     (until (quit-eval-error-p result))
;     (finally
;       (vector-push results (kernel-history-out k))
;       (return
;         (values (length (kernel-history-in k))
;                 results)))))

(defun send-result (result)
  (let ((iopub (kernel-iopub *kernel*))
        (execute-count (+ 1 (length (kernel-history-in *kernel*)))))
    (if (typep result 'error-result)
      (send-execute-error iopub *message* execute-count
                          (error-result-ename result)
                          (error-result-evalue result))
      (let ((data (render result)))
        (when data
          (if (result-display result)
            (send-display-data iopub *message* data)
            (send-execute-result iopub *message* execute-count data)))))))

; (defmethod is-complete ((k kernel) code)
;   +status-complete+)
  ; (handler-case
  ;   (iter
  ;     (with *standard-output* = (make-string-output-stream))
  ;     (with *error-output* = (make-string-output-stream))
  ;     (with input = (make-string-input-stream code))
  ;     (with in-maxima = (kernel-in-maxima k))
  ;     (initially
  ;       (apply-overrides))
  ;     (for char = (peek-char nil input nil))
  ;     (while char)
  ;     (for parsed = (if in-maxima (maxima::dbm-read input nil) (my-lread input)))
  ;     (when (state-change-p parsed)
  ;       (leave +status-unknown+))
  ;     (finally (return +status-complete+)))
  ;   (end-of-file ()
  ;     +status-incomplete+)
  ;   #+sbcl (sb-int:simple-reader-error ()
  ;     +status-incomplete+)
  ;   (simple-condition (err)
  ;     (if (equal (simple-condition-format-control err)
  ;                "parser: end of file while scanning expression.")
  ;       +status-incomplete+
  ;       +status-invalid+))
  ;   (condition ()
  ;     +status-invalid+)
  ;   (simple-error ()
  ;     +status-invalid+)))

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

; (defun my-displa (form)
;   (if (mtext-result-p form)
;     (let ((maxima::*alt-display1d* nil)
;           (maxima::*alt-display2d* nil))
;       (maxima::displa form))
;     (make-maxima-result form :display t :handle t)))
