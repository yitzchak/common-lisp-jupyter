(in-package #:common-lisp-jupyter)

(defclass kernel (jupyter:kernel)
  ()
  (:default-initargs :name "common-lisp"
                     :package :common-lisp-user
                     :version "0.1"
                     :banner "common-lisp-jupyter: a Common Lisp Jupyter kernel
(C) 2019 Tarn Burton (MIT)"
                     :language-name "common-lisp"
                     :language-version (uiop:lisp-version-string)
                     :mime-type "text/x-common-lisp"
                     :file-extension ".lisp"
                     :pygments-lexer "common-lisp"
                     :codemirror-mode "text/x-common-lisp"
                     :help-links '(("Common Lisp Documentation" . "https://common-lisp.net/documentation")
                                   ("Common Lisp HyperSpec" . "http://www.lispworks.com/documentation/HyperSpec/Front/index.htm"))))

(defmethod jupyter:code-is-complete ((k kernel) code)
  (handler-case
    (iter
      (for sexpr in-stream (make-string-input-stream code)))
    (end-of-file () "incomplete")
    (serious-condition () "invalid")
    (condition () "invalid")
    (:no-error (val)
      (declare (ignore val))
      "complete")))

(defmethod jupyter:evaluate-code ((k kernel) code)
  (iter
    (for sexpr in-stream (make-string-input-stream code))
    (for result = (jupyter:make-lisp-result
                    (jupyter:handling-errors
                      (eval sexpr))))
    (when result
      (collect result))
    (until (jupyter:quit-eval-error-p result))))

(defun symbol-char-p (c)
  (and (characterp c)
       (or (alphanumericp c)
           (member c '(#\+ #\- #\< #\> #\/ #\* #\& #\= #\. #\? #\_ #\! #\$ #\%
                       #\: #\@ #\[ #\] #\^ #\{ #\} #\~ #\# #\|)))))

(defun symbol-string-at-position (value pos)
  (when (symbol-char-p (char value pos))
    (let ((start (position-if-not #'symbol-char-p value :end pos :from-end t))
          (end (position-if-not #'symbol-char-p value :start pos)))
      (subseq value (if start (1+ start) 0) (if end end (length value))))))

(defclass inspect-result (jupyter:result)
  ((symbol :initarg :symbol
           :reader inspect-result-symbol)
   (status :initarg :status
           :reader inspect-result-status)
   (definitions :initarg :definitions
                :reader inspect-result-definitions)))

(defun sexpr-to-text (value)
  (string-trim '(#\Newline)
    (with-output-to-string (s)
      (pprint value s))))

(defun format-definition (symbol status definition)
  (let ((kind (getf definition :kind))
        (documentation (getf definition :documentation)))
     (format nil "~@[~A ~]~@[~A ~]~A~@[~%~%~A~]~@[~%~%~A~]~%"
       (case status
         (:inherited "Inherited")
         (:internal "Internal")
         (:external "External"))
       (case kind
         (:symbol "Symbol")
         (:constant "Constant")
         (:variable "Variable")
         (:function "Function")
         (:generic-function "Generic Function")
         (:macro "Macro")
         (:structure "Structure")
         (:class "Class")
         (:type "Type"))
       (sexpr-to-text symbol)
       (case kind
         (:class
           (let ((*package* (symbol-package symbol)))
             (format nil "Superclasses~%~{~S ~}~%~%Initial Arguments~%~{~S ~}~%~%Slots~%~{~%~S~%  ~A~}"
               (getf definition :precedence-list)
               (getf definition :initargs)
               (getf definition :slots))))
         ((:constant :variable)
          (sexpr-to-text (getf definition :value)))
         ((:function :generic-function :macro)
          (let ((*package* (symbol-package symbol)))
            (sexpr-to-text (cons symbol (getf definition :lambda-list))))))
       documentation)))

(defmethod jupyter:render ((res inspect-result))
  (with-slots (symbol status definitions) res
    (jsown:new-js
      ("text/plain"
        (format nil "~{~a~^~%~%~}"
          (mapcar (lambda (def) (format-definition symbol status def))
            definitions))))))

(defun normalize-symbol-case (name)
  (case (readtable-case *readtable*)
    (:upcase (string-upcase name))
    (:downcase (string-downcase name))
    (:invert
      (cond
        ((every #'upper-case-p name) (string-downcase name))
        ((every #'lower-case-p name) (string-upcase name))
        (t name)))
    (otherwise name)))

(defun package-char-p (ch)
  (equal #\: ch))

(defun find-qualified-symbol (name default-package)
  (let* ((normalized-name (normalize-symbol-case name))
         (pos (position-if #'package-char-p normalized-name))
         (package
           (cond
             ((not pos) default-package)
             ((= pos 0) 'keyword)
             (t (values (find-package (subseq normalized-name 0 pos))))))
         (npos (if pos (position-if-not #'package-char-p normalized-name :start pos) 0)))
    (if (and package npos)
      (find-symbol (subseq normalized-name npos) package)
      (values nil nil))))

(defmethod jupyter:inspect-code ((k kernel) code cursor-pos detail-level)
  (jupyter:handling-errors
    (with-slots (package) k
      (multiple-value-bind (sym status)
                           (find-qualified-symbol
                             (symbol-string-at-position code cursor-pos)
                             package)
        (when sym
          (make-instance 'inspect-result
            :symbol sym
            :status status
            :definitions (trivial-documentation:symbol-definitions sym)))))))

#+ros.installing
(eval-when (:compile-toplevel)
  (defparameter roswell.install::*build-hook*
    (lambda ()
      (jupyter:install-kernel (if (uiop:os-windows-p)
                                (list "ros"
                                      (namestring (merge-pathnames
                                        (make-pathname :directory '(:relative ".roswell" "bin")
                                                       :name "cl-jupyter")
                                        (uiop:getenv-absolute-directory "USERPROFILE")))
                                      "{connection_file}")
                                '("cl-jupyter" "{connection_file}"))
                              "Common Lisp"
                              "common-lisp"))))
