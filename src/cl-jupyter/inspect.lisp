(in-package #:common-lisp-jupyter)


(defgeneric inspect-fragment (stream frag detail-level)
  (:method (stream frag detail-level)
    (declare (ignore stream frag detail-level))))


(defun inspect-package (stream name detail-level)
  (when-let ((pkg (find-package name)))
    (format stream "# ~:/mdf:text/ [package]~@[~%~%~:/mdf:text/~]~@[~%~%## Nicknames~%~%~{~@/mdf:text/~^, ~}~]"
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


(defun do-inspect-symbol (stream sym detail-level)
  (format stream "~%~%# ~/mdf:text/ \\[symbol\\]" sym)
  (when (boundp sym)
    (format stream "~%~%## ~:[Dynamic~;Constant~] Variable~@[~%~%~:/mdf:text/~]~%~%~@/mdf:code/"
                   (constantp sym)
                   (documentation sym 'variable)
                   (symbol-value sym)))
  (when (fboundp sym)
    (format stream "~%~%## ~A~@[~%~%~:/mdf:text/~]~%~%~@/mdf:code/"
                   (cond
                     ((macro-function sym)
                       "Macro")
                     ((typep (fdefinition sym) 'standard-generic-function)
                       "Generic Function")
                     (t
                       "Function"))
                   (documentation sym 'function)
                   (cons sym (lambda-list sym))))
  (when-let ((cls (find-class sym nil)))
    (format stream "~%~%## ~:[Class~;Structure~]~@[~%~%~:/mdf:text/~]~%~%### Precedence List~%~%~{~@/mdf:text/~^, ~}"
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
                         (closer-mop:slot-definition-allocation slot)))))))


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

