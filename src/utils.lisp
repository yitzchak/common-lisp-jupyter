
(in-package #:common-lisp-utilities)

#|

# CommonTypes: Utilities #

|#

;; To activate the inline examples
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *example-enabled* t)

  (defparameter *example-equal-predicate* #'equal)

  (defparameter *example-with-echo* nil)
  
  )


(defmacro example (expr arrow expected &key (warn-only nil))
  "Show an evaluation example, useful for documentation and lightweight testing.

   (example `EXPR` => `EXPECTED`) evaluates `EXPR` and compare, wrt. `EQUIV`
 (EQUAL by default) to `EXPECTED` and raise an error if inequal.

  Set `WARN-ONLY` to T for warning instead of error.
"
  (if (not *example-enabled*)
      (progn
        (when *example-with-echo*
          (format t "------------------~%")
          (format t "Example:~%~A~%=?= ~A~%" (format nil "~A" expr) expected)
          (format t "  ===> SKIP~%"))
        (values));; synonymous of nil if disabled
      ;; when enabled
      (let ((result-var (gensym "result-"))
            (expected-var (gensym "expected-"))
            (err-fun-var (gensym "err-fun-"))
            (expr-str (format nil "~A" expr)))
        `(progn
           (when *example-with-echo*
             (format t "------------------~%")
             (format t "Example:~%~A~%=?= ~A~%" ,expr-str ,expected))
           (let ((,err-fun-var (if ,warn-only #'warn #'error))
                 (,result-var ,expr)
                 (,expected-var ,expected))
             (if (not (equal (symbol-name (quote ,arrow)) "=>"))
                 (error "Missing arrow '=>' in example expression"))
             (if (funcall *example-equal-predicate* ,result-var ,expected-var)
                 (progn (if *example-with-echo*
                            (format t "  ===> PASS~%"))
                        t)
                 (funcall ,err-fun-var "Failed example:~%  Expression: ~S~%  ==> expected: ~A~%  ==> evaluated: ~A~%"
                          ,expr-str ,expected-var ,result-var)))))))


(defmacro example-progn (&body body)
  "The toplevel forms of BODY are evaluated only if examples are enabled"
  (if *example-enabled*
      `(progn ,@body)
      (values)))

(defmacro logg (level fmt &rest args)
  "Log the passed ARGS using the format string FMT and its
 arguments ARGS."
  (if (or (not *log-enabled*)
          (< level *log-level*))
      (values);; disabled
      ;; when enabled
      `(progn (format ,*log-out-stream* "[LOG]:")
              (format ,*log-out-stream* ,fmt ,@args)
              (format ,*log-out-stream* "~%"))))
  
(defmacro vbinds (binders expr &body body)
  "An abbreviation for MULTIPLE-VALUE-BIND."
  (labels ((replace-underscores (bs &optional (result nil) (fresh-vars nil) (replaced nil))
             (if (null bs)
                 (let ((nresult (nreverse result))
                       (nfresh (nreverse fresh-vars)))
                   (values replaced nresult nfresh))
                 (if (equal (symbol-name (car bs)) "_")
                     (let ((fresh-var (gensym "underscore-")))
                       (replace-underscores (cdr bs) (cons fresh-var result) (cons fresh-var fresh-vars) t))
                     (replace-underscores (cdr bs) (cons (car bs) result) fresh-vars replaced)))))
    (multiple-value-bind (has-underscore nbinders fresh-vars) (replace-underscores binders)
      (if has-underscore
          `(multiple-value-bind ,nbinders ,expr
             (declare (ignore ,@fresh-vars))
             ,@body)
          `(multiple-value-bind ,binders ,expr ,@body)))))

(example (vbinds (a _ b) (values 1 2 3)
           (cons a b))
         => '(1 . 3)) ;; without a warning

(example (vbinds (a _ b _) (values 1 2 3 4)
           (cons a b))
         => '(1 . 3)) ;; without a warning


(defun afetch (comp alist)
  (let ((binding (assoc comp alist)))
    (if binding
        (cdr binding)
        (error "No such key: ~A" comp))))
