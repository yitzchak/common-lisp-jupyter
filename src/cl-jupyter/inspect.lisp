(in-package #:common-lisp-jupyter)


(defgeneric inspect-fragment (frag type)
  (:method (frag type)
    (declare (ignore frag type))))


(defun cl-user::code (stream value colon at &rest args)
  (declare (ignore at args))
  (cond
    (colon
      (write-line "```common-lisp" stream)
      (write value :escape t :case :downcase :pretty t :stream stream)
      (terpri stream)
      (write-line "```" stream))
    (t
      (let* ((rep (write-to-string value :escape t :case :downcase))
             (delim (if (position #\` rep) "``" "`")))
        (write-string delim stream)
        (write-string rep stream)
        (write-string delim stream)))))


(defun inspect-package (name))


(defun inspect-method (name package statuses))


(defun inspect-symbol (name package statuses)
  (when package
    (multiple-value-bind (sym status)
                         (find-symbol name package)
      (when (member status statuses :test #'eql)
        (list (format nil "# Symbol: ~/code/~%~@[~%~A~%~]~@[~%- Value = ~/code/~%~]"
                      sym
                      (documentation sym 'variable)
                      (if (boundp sym) (symbol-value sym) "UNBOUND")))))))


(defmethod inspect-fragment (frag (type (eql nil)))
  (when (and frag
             (fragment-types frag))
    (mapcan (lambda (sub-type)
              (inspect-fragment frag sub-type))
            (fragment-types frag))))


(defmethod inspect-fragment (frag (type (eql :symbol-name)))
  (let ((parent (fragment-parent frag))
        results)
    (dolist (symbol-type (fragment-types parent) results)
      (let ((pkg (cond
                    ((eql :local-symbol symbol-type)
                      *package*)
                    ((zerop (length (fragment-result (first (fragment-children parent)))))
                      (find-package "KEYWORD"))
                    (t
                      (find-package (fragment-result (first (fragment-children parent))))))))
        (case symbol-type
          (:local-symbol
            (setf results
                (nconc results
                       (inspect-package (fragment-result frag))
                       (inspect-method (fragment-result frag)
                                        pkg
                                        '(:internal :external :inherited))
                       (inspect-symbol (fragment-result frag)
                                        pkg
                                        '(:internal :external :inherited)))))
          (:external-symbol
            (setf results
                (nconc results
                       (inspect-method (fragment-result frag)
                                        pkg
                                        '(:external))
                       (inspect-symbol (fragment-result frag)
                                        pkg
                                        '(:external)))))
          (:internal-symbol
            (setf results
                (nconc results
                       (inspect-method (fragment-result frag)
                                        pkg
                                        '(:internal))
                       (inspect-symbol (fragment-result frag)
                                        pkg
                                        '(:internal))))))))))




(defmethod jupyter:inspect-code ((k kernel) code cursor-pos detail-level)
  (declare (ignore detail-level))
  ;(jupyter:inform :error k "~A ~A" code cursor-pos)
  (when-let ((result (inspect-fragment (locate-fragment code cursor-pos) nil)))
    (jupyter:make-inline-result (format nil "~{~&~A~}" result) :mime-type "text/markdown")))

