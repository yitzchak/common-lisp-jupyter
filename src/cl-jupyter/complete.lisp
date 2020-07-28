(in-package #:common-lisp-jupyter)


(defun remove-if-not-match (partial-name matches)
  (remove-if-not (lambda (match)
                   (starts-with-subseq partial-name match))
                 matches))


(defgeneric complete-fragment (frag)
  (:method (frag)
    (declare (ignore frag))))


(defun complete-method (partial-name start end package statuses)
  (when package
    (multiple-value-bind (sym status)
                         (find-symbol partial-name package)
      (when (and (member status statuses :test #'eql)
                 (boundp sym))
        (let* ((cls (class-of (symbol-value sym)))
               (supers (remove-if (lambda (cls)
                                    (eql t (class-name cls)))
                                  (closer-mop:class-precedence-list cls))))
          (mapcan (lambda (method)
                    (do* (matches
                          (name (closer-mop:generic-function-name (closer-mop:method-generic-function method)))
                          (specializers (closer-mop:method-specializers method))
                          (lambda-list (closer-mop:method-lambda-list method))
                          (pos (position-if (lambda (spec)
                                              (member spec supers))
                                            specializers)
                               (position-if (lambda (spec)
                                              (member spec supers))
                                            specializers
                                            :start (1+ pos))))
                         ((null pos) matches)
                        (push (list :match (if (listp name)
                                             (format nil "~((setf (~S~{ ~A~} ~S~{ ~A~}) ~A)~)"
                                                     (second name)
                                                     (subseq lambda-list 1 pos)
                                                     sym
                                                     (subseq lambda-list (1+ pos))
                                                     (first lambda-list))
                                             (format nil "~((~S~{ ~A~} ~S~{ ~A~})~)"
                                                     name
                                                     (subseq lambda-list 0 pos)
                                                     sym
                                                     (subseq lambda-list (1+ pos))))
                                    :start start
                                    :end end)
                              matches)))
                  (find-methods cls)))))))


(defun complete-symbol (partial-name start end package func statuses)
  (when package
    (let (matches)
      (do-symbols (sym package matches)
        (let ((sym-name (symbol-name sym)))
          (multiple-value-bind (s status)
                               (find-symbol sym-name package)
            (when (and (member status statuses :test #'eql)
                       (starts-with-subseq partial-name sym-name)
                       (or (and func (fboundp sym))
                           (and (not func) (boundp sym))))
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


(defmethod complete-fragment ((frag symbol-name-fragment))
  (complete-fragment (fragment-parent frag)))


(defmethod complete-fragment ((frag package-marker-fragment))
  (complete-fragment (fragment-parent frag)))


(defmethod complete-fragment ((frag symbol-fragment))
  (with-slots (status position children)
              frag
    (let* ((func (or (equal 0 position)
                     (and (equal 1 position)
                          (equal 'function (fragment-value (first (fragment-children (fragment-parent frag))))))))
           (symbol-name-frag (car (last children)))
           (symbol-name (fragment-value symbol-name-frag))
           (start (fragment-start symbol-name-frag))
           (end (fragment-end symbol-name-frag)))
      (if (eql :local status)
        (nconc (complete-package symbol-name start end :include-marker t)
               (unless func
                 (complete-method symbol-name start end *package*
                                  '(:internal :external :inherited)))
               (complete-symbol symbol-name start end *package* func
                                '(:internal :external :inherited)))
        (let ((pkg (find-package (if (zerop (length (fragment-value (first children))))
                                  "KEYWORD"
                                  (fragment-value (first children))))))
          (nconc (unless func
                   (complete-method symbol-name start end pkg (list status)))
                 (complete-symbol symbol-name start end pkg func (list status))))))))


(defmethod complete-fragment ((frag package-name-fragment))
  (complete-package (fragment-value frag) (fragment-start frag) (fragment-end frag)))


(defmethod jupyter:complete-code ((k kernel) code cursor-pos)
  (let* ((matches (complete-fragment (locate-fragment code (1- cursor-pos))))
         (start (apply #'min (or (mapcar (lambda (match)
                                           (getf match :start))
                                          matches)
                                 (list (1- cursor-pos)))))
         (end (apply #'max (or (mapcar (lambda (match)
                                         (getf match :end))
                                       matches)
                               (list (1- cursor-pos))))))
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

