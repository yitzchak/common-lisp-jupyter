(in-package #:common-lisp-jupyter)


#+sbcl (require :sb-introspect)


(defun lambda-list (sym)
  #+ccl
    (ccl:arglist sym)
  #+clisp
    (system::arglist sym)
  #+(or clasp ecl)
    (ext:function-lambda-list sym)
  #+sbcl
    (sb-introspect:function-lambda-list sym)
  #+lispworks
    (lispworks:function-lambda-list sym)
  #-(or ccl clisp clasp ecl lispworks sbcl)
    (second (function-lambda-expression (or (macro-function sym)
                                            (fdefinition sym)))))


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


(defun method-specialized-lambda-list (method)
  (do* ((lambda-list (closer-mop:method-lambda-list method) (cdr lambda-list))
        (specializers (closer-mop:method-specializers method) (cdr specializers))
        specialized-lambda-list)
      ((null lambda-list) (nreverse specialized-lambda-list))
    (cond
      ((and specializers
            (typep (car specializers) 'closer-mop:eql-specializer))
        (push (list (car lambda-list)
                    (list 'eql (closer-mop:eql-specializer-object (car specializers))))
              specialized-lambda-list))
      ((and specializers
            (eql t (class-name (car specializers))))
        (push (car lambda-list)
              specialized-lambda-list))
      (specializers
        (push (list (car lambda-list) (class-name (car specializers)))
              specialized-lambda-list))
      (t
        (push (car lambda-list)
              specialized-lambda-list)))))
