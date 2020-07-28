(in-package #:common-lisp-jupyter)


(defun lambda-list (sym)
  #+ccl
    (ccl:arglist sym)
  #-ccl
    (let ((func (or (macro-function sym)
                    (fdefinition sym))))
      #+(or clasp ecl)
        (ext:function-lambda-list func)
      #+lispworks
        (lw:function-lambda-list func)
      #+sbcl
        (sb-introspect:function-lambda-list func)
      #-(or clasp ecl lispworks sbcl)
        (second (function-lambda-expression func))))


(defun visible-symbol-p (sym)
  (let ((pkg (symbol-package sym)))
    (or (eq *package* pkg)
        (multiple-value-bind (s status)
                             (find-symbol (symbol-name sym) pkg)
          (declare (ignore s))
          (eql :external status)))))


(defun find-generics (cls)
  (remove-duplicates
    (mapcan (lambda (cls)
              (unless (eql t (class-name cls))
                (remove-if-not (lambda (gf)
                                 (let ((name (closer-mop:generic-function-name gf)))
                                   (or (and (symbolp name)
                                            (visible-symbol-p name))
                                       (and (listp name)
                                            (eql 'setf (car name))
                                            (every #'visible-symbol-p (cdr name))))))
                  (closer-mop:specializer-direct-generic-functions cls))))
            (closer-mop:class-precedence-list cls))))


(defun find-methods (cls)
  (remove-duplicates
    (mapcan (lambda (cls)
              (unless (eql t (class-name cls))
                (remove-if-not (lambda (method)
                                 (let ((name (closer-mop:generic-function-name (closer-mop:method-generic-function method))))
                                   (or (and (symbolp name)
                                            (visible-symbol-p name))
                                       (and (listp name)
                                            (eql 'setf (car name))
                                            (every #'visible-symbol-p (cdr name))))))
                  (closer-mop:specializer-direct-methods cls))))
            (closer-mop:class-precedence-list cls))))
