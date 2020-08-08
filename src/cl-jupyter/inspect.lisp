(in-package #:common-lisp-jupyter)


(defparameter +clhs-map-root+ "http://www.lispworks.com/documentation/HyperSpec/Data/")
(defparameter +clhs-map-name+ "Map_Sym.txt")


(defmethod multilang-documentation:canonicalize-doctype ((name symbol) (type (eql :clhs)))
  (cons name :clhs))


(defun load-clhs-map ()
  (handler-case
      (let ((stream (dex:get (quri:merge-uris +clhs-map-name+ +clhs-map-root+) :want-stream t)))
        (unwind-protect
            (do* ((name (read-line stream nil) (read-line stream nil))
                  (link (read-line stream nil) (read-line stream nil))
                  (sym (when name
                         (find-symbol name 'common-lisp))
                       (when name
                         (find-symbol name 'common-lisp))))
                 ((or (null name) (null link)))
              (when sym
                (setf (multilang-documentation:documentation* sym :clhs :en)
                      (quri:render-uri (quri:merge-uris link +clhs-map-root+)))))
          (close stream)))
    (dex:http-request-bad-request ())
    (dex:http-request-failed ())))


(defgeneric inspect-fragment (stream frag detail-level)
  (:method (stream frag detail-level)
    (declare (ignore stream frag detail-level))))


(defun inspect-package (stream name detail-level)
  (when-let ((pkg (find-package name)))
    (format stream "# ~:/mdf:text/ [package]~@[~%~%~:/mdf:pre/~]~@[~%~%## Nicknames~%~%~(~{~@/mdf:text/~^, ~}~)~]"
                    (package-name pkg)
                    (documentation pkg t)
                    (package-nicknames pkg))
    (unless (zerop detail-level)
      (let (exports)
        (do-external-symbols (sym pkg)
          (push sym exports))
        (when exports
          (format stream "~%~%## Exports~%~%~{~@/mdf:text/~^, ~}"
                         exports))))))


(defun inspect-method (stream name package statuses detail-level))


(defun inspect-symbol-system (stream sym detail-level)
  #+asdf3.1
  (when-let* ((system-name (asdf/package-inferred-system::package-name-system (package-name (symbol-package sym))))
              (system (asdf:find-system system-name nil)))
    (if (asdf:system-homepage system)
      (format stream "~%~%From system [~A](~A)" system-name (asdf:system-homepage system))
      (format stream "~%~%From system ~A" system-name))
    (when-let ((license (asdf:system-license system)))
      (format stream " (~A)" license))
    (when-let ((author (asdf:system-author system)))
      (format stream " by ~A" author))))


(defun inspect-symbol-variable (stream sym detail-level)
  (when (boundp sym)
    (format stream "~%~%## ~:[Dynamic~;Constant~] Variable~@[~%~%~:/mdf:pre/~]~%~%~@/mdf:code/"
            (constantp sym)
            (documentation sym 'variable)
            (symbol-value sym))))


(defun inspect-symbol-function (stream sym detail-level)
  (when (fboundp sym)
    (format stream "~%~%## ~A~@[~%~%~:/mdf:pre/~%~%~]~%~%### Syntax~%~%~@/mdf:code/"
            (cond
              ((special-operator-p sym)
                "Special")
              ((macro-function sym)
                "Macro")
              ((typep (fdefinition sym) 'standard-generic-function)
                "Generic Function")
              (t
                "Function"))
            (documentation sym 'function)
            (cons sym (lambda-list sym)))))


(defun inspect-symbol-class (stream sym detail-level)
  (when-let ((cls (find-class sym nil)))
    (format stream "~%~%## ~:[Class~;Structure~]~@[~%~%~:/mdf:pre/~]~%~%### Precedence List~%~%~{~@/mdf:text/~^, ~}"
            (subtypep cls (find-class 'structure-object))
            (documentation sym 'type)
            (mapcar #'class-name (closer-mop:class-precedence-list cls)))
    (when (closer-mop:class-slots cls)
      (format stream "~%~%### Slots~%")
      (dolist (slot (closer-mop:class-slots cls))
        (format stream "~%- ~/mdf:text/~@[ \\[~/mdf:text/\\]~]~@[ &mdash; ~:/mdf:text/~]~{~%    - :initarg ~/mdf:text/~}~@[~%    - :allocation ~/mdf:text/~]"
                (closer-mop:slot-definition-name slot)
                (unless (eql t (closer-mop:slot-definition-type slot))
                  (closer-mop:slot-definition-type slot))
                (if (listp (documentation slot t))
                  (first (documentation slot t))
                  (documentation slot t))
                (closer-mop:slot-definition-initargs slot)
                (unless (eql :instance (closer-mop:slot-definition-allocation slot))
                  (closer-mop:slot-definition-allocation slot)))))
    (when-let ((methods (find-methods cls)))
      (format stream "~%~%### Methods~%")
      (dolist (method methods)
        (let ((name (closer-mop:generic-function-name (closer-mop:method-generic-function method)))
              (lambda-list (method-specialized-lambda-list method))
              (mdf:*indent-level* 1))
          (format stream "~%- ~/mdf:text/~@[ &mdash; ~:/mdf:text/~]~%~%~@/mdf:code/~%~%"
                  (if (listp name) (second name) name)
                  (documentation method t)
                  (if (listp name)
                    `(setf (,(second name) ,@(cdr lambda-list)) ,(first lambda-list))
                    `(,name ,@lambda-list))))))))


(defun do-inspect-symbol (stream sym detail-level)
  (format stream "~%~%# ~/mdf:text/ \\[symbol\\]" sym)
  (inspect-symbol-system stream sym detail-level)
  (when-let ((clhs-link (multilang-documentation:documentation sym :clhs)))
    (format stream "~%~%See also: [CLHS](~A)" clhs-link))
  (inspect-symbol-variable stream sym detail-level)
  (inspect-symbol-function stream sym detail-level)
  (inspect-symbol-class stream sym detail-level))


(defun inspect-symbol (stream name package statuses detail-level)
  (when package
    (multiple-value-bind (sym status)
                         (find-symbol name package)
      (when (member status statuses :test #'eql)
        (do-inspect-symbol stream sym detail-level)))))


(defmethod inspect-fragment (stream (frag symbol-name-fragment) detail-level)
  (inspect-fragment stream (fragment-parent frag) detail-level))


(defmethod inspect-fragment (stream (frag package-marker-fragment) detail-level)
  (inspect-fragment stream (fragment-parent frag) detail-level))


(defmethod inspect-fragment (stream (frag symbol-fragment) detail-level)
  (with-slots (status position children)
              frag
    (let ((symbol-name (fragment-value (car (last children)))))
      (cond
        ((eql :local status)
          (inspect-package stream symbol-name detail-level)
          (inspect-method stream symbol-name *package* '(:internal :external :inherited) detail-level)
          (inspect-symbol stream symbol-name *package* '(:internal :external :inherited) detail-level))
        (t
          (let ((pkg (find-package (if (zerop (length (fragment-value (first children))))
                                    "KEYWORD"
                                    (fragment-value (first children))))))
            (inspect-method stream symbol-name pkg (list status) detail-level)
            (inspect-symbol stream symbol-name pkg (list status) detail-level)))))))


(defmethod inspect-fragment (stream (frag package-name-fragment) detail-level)
  (inspect-package stream (fragment-value frag) detail-level))


(defmethod jupyter:inspect-code ((k kernel) code cursor-pos detail-level)
  (jupyter:make-inline-result
    (with-output-to-string (stream)
      (inspect-fragment stream (locate-fragment code (1- cursor-pos)) detail-level))
    :mime-type "text/markdown"))

