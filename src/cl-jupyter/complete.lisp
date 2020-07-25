(in-package #:common-lisp-jupyter)


(defun remove-if-not-match (partial-name matches)
  (remove-if-not (lambda (match)
                   (starts-with-subseq partial-name match))
                 matches))


(defgeneric complete-fragment (frag type)
  (:method (frag type)
    ;(jupyter:inform :error nil "complete-fragment nil ~A" type)
    nil))

    ;(declare (ignore frag type))))


(defmethod complete-fragment (frag (type (eql nil)))
  ;(jupyter:inform :error nil "complete-fragment nil")
  (when (and frag
             (fragment-types frag))
    (mapcan (lambda (sub-type)
              (complete-fragment frag sub-type))
            (fragment-types frag))))


(defun complete-method (partial-name start end package statuses)
  (jupyter:inform :error nil "complete-method")
  (when package
    (multiple-value-bind (sym status)
                         (find-symbol partial-name package)
      (when (and (member status statuses :test #'eql)
                 (boundp sym))
        (let ((cls (class-of (symbol-value sym))))
          (mapcan (lambda (method)
                    (do* (matches
                          (name (closer-mop:generic-function-name (closer-mop:method-generic-function method)))
                          (specializers (closer-mop:method-specializers method))
                          (lambda-list (closer-mop:method-lambda-list method))
                          (pos (position cls specializers :test #'eql)
                               (position cls specializers :test #'eql :start (1+ pos))))
                         ((null pos) matches)
                        (push (list :match (format nil "~((~S~{ ~A~} ~S~{ ~A~})~)"
                                                name
                                                (subseq lambda-list 0 pos)
                                                sym
                                                (subseq lambda-list (1+ pos)))
                              :start start
                              :end end)
                              matches)))
                  (closer-mop:specializer-direct-methods cls)))))))


(defun complete-symbol (partial-name start end package statuses)
  (jupyter:inform :error nil "complete-symbol")
  (when package
    (let (matches)
      (do-symbols (sym package matches)
        (let ((sym-name (symbol-name sym)))
          (multiple-value-bind (s status)
                               (find-symbol sym-name package)
            (declare (ignore s))
            (when (and (member status statuses :test #'eql)
                       (starts-with-subseq partial-name sym-name))
              (push (list :match (string-downcase sym-name)
                          :start start
                          :end end)
                    matches))))))))


(defun find-matches (partial &rest candidates)
  (mapcan (lambda (candidate)
            (cond
              ((listp candidate)
                (apply #'find-matches partial candidate))
              ((starts-with-subseq partial candidate)
                (list candidate))))
          candidates))


(defun complete-package (partial-name start end &key include-marker)
  (jupyter:inform :error nil "complete-package")
  (mapcar (lambda (name)
            (list :match (format nil "~(~A~)~:[~;:~]" name include-marker)
                  :start start
                  :end end))
          (find-matches partial-name
                       (mapcar #'package-name (list-all-packages))
                       (mapcar #'package-nicknames (list-all-packages))
                       #+(or abcl clasp ecl) (mapcar #'car (ext:package-local-nicknames *package*))
                       #+allegro (mapcar #'car (excl:package-local-nicknames *package*))
                       #+ccl (mapcar #'car (ccl:package-local-nicknames *package*))
                       #+lispworks (mapcar #'car (hcl:package-local-nicknames *package*))
                       #+sbcl (mapcar #'car (sb-ext:package-local-nicknames *package*)))))


(defmethod complete-fragment (frag (type (eql :symbol-name)))
  ;(jupyter:inform :error nil "complete-fragment symbol-name")
  (let ((parent (fragment-parent frag))
        matches)
    (jupyter:inform :error nil "a ~A" matches)
    (dolist (symbol-type (fragment-types parent) matches)
      (let ((pkg (cond
                    ((eql :local-symbol symbol-type)
                      *package*)
                    ((zerop (length (fragment-result (first (fragment-children parent)))))
                      (find-package "KEYWORD"))
                    (t
                      (find-package (fragment-result (first (fragment-children parent))))))))
        (jupyter:inform :error nil "b ~A ~A ~A" matches pkg symbol-type)
        (case symbol-type
          (:local-symbol
            (setf matches
                (nconc matches
                       (complete-package (fragment-result frag) (fragment-start frag) (fragment-end frag) :include-marker t)
                       (complete-method (fragment-result frag) (fragment-start frag) (fragment-end frag)
                                        pkg
                                        '(:internal :external :inherited))
                       (complete-symbol (fragment-result frag) (fragment-start frag) (fragment-end frag)
                                        pkg
                                        '(:internal :external :inherited)))))
          (:external-symbol
            (setf matches
                (nconc matches
                       (complete-method (fragment-result frag) (fragment-start frag) (fragment-end frag)
                                        pkg
                                        '(:external))
                       (complete-symbol (fragment-result frag) (fragment-start frag) (fragment-end frag)
                                        pkg
                                        '(:external)))))
          (:internal-symbol
            (setf matches
                (nconc matches
                       (complete-method (fragment-result frag) (fragment-start frag) (fragment-end frag)
                                        pkg
                                        '(:internal))
                       (complete-symbol (fragment-result frag) (fragment-start frag) (fragment-end frag)
                                        pkg
                                        '(:internal))))))))
           (jupyter:inform :error nil "c ~A" matches)
           matches))



(defmethod complete-fragment (frag (type (eql :package-name)))
  ;(jupyter:inform :error nil "complete-fragment package-name")
  (complete-package (fragment-result frag) (fragment-start frag) (fragment-end frag)))


(defmethod complete-fragment (frag (type (eql :package-marker)))
  ;(jupyter:inform :error nil "complete-fragment package-marker")
  (complete-fragment (car (last (fragment-children (fragment-parent frag)))) nil))


(defmethod jupyter:complete-code ((k kernel) code cursor-pos)
  ;(jupyter:inform :error k "~A ~A" code cursor-pos)
  (let* ((matches (complete-fragment (locate-fragment code cursor-pos) nil))
         (start (apply #'min (or (mapcar (lambda (match)
                                           (getf match :start))
                                          matches)
                                 (list cursor-pos))))
         (end (apply #'max (or (mapcar (lambda (match)
                                         (getf match :end))
                                       matches)
                               (list cursor-pos)))))
    ;(jupyter:inform :error k "~S" matches)
    (values
      (sort
        (remove-duplicates
          (mapcar (lambda (match)
                    (concatenate 'string
                                 (subseq code start (getf match :start))
                                 (getf match :match)
                                 (subseq code (getf match :end) end)))
                  matches)
          :test #'string-equal)
        #'string-lessp)
      start
      end)))


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

