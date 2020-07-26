(in-package #:common-lisp-jupyter)


(defgeneric inspect-fragment (frag)
  (:method (frag)
    (declare (ignore frag))))


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


(defun inspect-package (name)
  (when-let ((pkg (find-package name)))
    (list (with-output-to-string (stream)
            (format stream "## Package ~A~%~%" (package-name pkg))
            (format stream "### Nicknames~%~%~{- ~A~%~}" (package-nicknames pkg))))))


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


(defmethod inspect-fragment ((frag symbol-name-fragment))
  (inspect-fragment (fragment-parent frag)))


(defmethod inspect-fragment ((frag package-marker-fragment))
  (inspect-fragment (fragment-parent frag)))


(defmethod inspect-fragment ((frag symbol-fragment))
  (with-slots (status position children)
              frag
    (let ((symbol-name (fragment-value (car (last children)))))
      (if (eql :local status)
        (nconc (inspect-package symbol-name)
               (inspect-method symbol-name *package* '(:internal :external :inherited))
               (inspect-symbol symbol-name *package* '(:internal :external :inherited)))
        (let ((pkg (find-package (if (zerop (length (fragment-value (first children))))
                                  "KEYWORD"
                                  (fragment-value (first children))))))
          (nconc (inspect-method symbol-name pkg (list status))
                 (inspect-symbol symbol-name pkg (list status))))))))


(defmethod inspect-fragment ((frag package-name-fragment))
  (inspect-package (fragment-value frag)))


(defmethod jupyter:inspect-code ((k kernel) code cursor-pos detail-level)
  (declare (ignore detail-level))
  ;(jupyter:inform :error k "~A ~A" code cursor-pos)
  (when-let ((result (inspect-fragment (locate-fragment code (1- cursor-pos)))))
    (jupyter:make-inline-result (format nil "~{~&~A~}" result) :mime-type "text/markdown")))

