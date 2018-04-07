(in-package #:fredokun-utilities)

#|

# CommonTypes: Utilities #

|#

;; To activate the inline examples
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *example-enabled* t) ;; nil in production / t for self-testing

  (defparameter *example-equal-predicate* #'equal)

  (defparameter *example-with-echo* nil))

(defvar maxima::$kernel_info nil)


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

(defun info (&rest args)
  (when maxima::$kernel_info
    (apply #'format *debug-io* args)))

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

(defun read-file-lines (filename)
  (with-open-file (input filename)
    (loop
       for line = (read-line input nil 'eof)
       until (eq line 'eof)
       collect line)))

(defun read-binary-file (filename)
  (with-open-file (stream filename :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence bytes stream)
      bytes)))

(defun read-string-file (filename)
  (with-open-file (stream filename)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun file-to-base64-string (path)
  (cl-base64:usb8-array-to-base64-string (read-binary-file path)))

;; nicked from: https://rosettacode.org/wiki/String_matching#Common_Lisp
(defun ends-with-p (str1 str2)
  (let ((p (mismatch str2 str1 :from-end T)))
    (or (not p) (= 0 p))))

(defun ends-with-terminator (code)
  (let ((trimmed (string-right-trim
                  '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout)
                  code)))
    (or (ends-with-p trimmed "$") (ends-with-p trimmed ";"))))

(defun add-terminator (code)
  (if (ends-with-terminator code)
    code
    (concatenate 'string code ";")))
