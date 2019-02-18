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
  (let ((start-pos (if (symbol-char-p (char value pos)) pos (if (zerop pos) 0 (1- pos)))))
    (if (symbol-char-p (char value start-pos))
      (let ((start (1+ (or (position-if-not #'symbol-char-p value :end start-pos :from-end t) -1)))
            (end (or (position-if-not #'symbol-char-p value :start start-pos) (length value))))
        (values (subseq value start end) start end))
      (values nil nil nil))))

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

(defun mangle-symbol-case (name)
  (case (readtable-case *readtable*)
    (:upcase (string-downcase name))
    (:downcase (string-upcase name))
    (:invert
      (cond
        ((every #'upper-case-p name) (string-downcase name))
        ((every #'lower-case-p name) (string-upcase name))
        (t name)))
    (otherwise name)))

(defun package-char-p (ch)
  (equal #\: ch))

(defun split-qualified-name (name)
  (let* ((normalized-name (normalize-symbol-case name))
         (pos (position-if #'package-char-p normalized-name))
         (start (if pos
                  (or (position-if-not #'package-char-p normalized-name :start pos)
                      (length name))
                  0)))
    (values
      (subseq normalized-name start)
      (cond
        ((equal pos 0) "KEYWORD")
        (pos (subseq normalized-name 0 pos)))
      (and pos (= 1 (- start pos))))))

(defun find-qualified-symbol (name default-package)
  (multiple-value-bind (name package)
                       (split-qualified-name name)
    (if (or (not name) (zerop (length name)))
      (values nil nil)
      (find-symbol name (or package default-package)))))

(defmethod jupyter:inspect-code ((k kernel) code cursor-pos detail-level)
  (jupyter:handling-errors
    (with-slots (package) k
      (multiple-value-bind (sym status)
                           (find-qualified-symbol
                             (values (symbol-string-at-position code cursor-pos))
                             package)
        (when sym
          (make-instance 'inspect-result
            :symbol sym
            :status status
            :definitions (trivial-documentation:symbol-definitions sym)))))))

;; nicked from: https://rosettacode.org/wiki/String_matching#Common_Lisp
(defun starts-with-p (str1 str2)
  (let ((p (search str2 str1)))
    (and p (= 0 p))))

(defun symbol-name-to-qualified-name (name package-name package)
  (mangle-symbol-case
    (if package-name
      (multiple-value-bind (sym status) (find-symbol name package)
        (declare (ignore sym))
        (format nil "~A~A~A"
          (if (equal package-name "KEYWORD") "" package-name)
          (if (equal status :external) ":" "::")
          name))
      name)))

(defmethod jupyter:complete-code ((k kernel) code cursor-pos)
  (jupyter:handling-errors
    (multiple-value-bind (word start end) (symbol-string-at-position code cursor-pos)
      (when word
        (values
          (multiple-value-bind (name package-name ext) (split-qualified-name word)
            (with-slots (package) k
              (let ((pkg (find-package (or package-name package))))
                (when pkg
                  (if ext
                    (iter
                      (for sym in-package pkg external-only t)
                      (for sym-name next (symbol-name sym))
                      (when (starts-with-p sym-name name)
                        (collect
                          (symbol-name-to-qualified-name sym-name package-name pkg))))
                    (iter
                      (for sym in-package pkg)
                      (for sym-name next (symbol-name sym))
                      (when (starts-with-p sym-name name)
                        (collect
                          (symbol-name-to-qualified-name sym-name package-name pkg)))))))))
          start
          end)))))

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
