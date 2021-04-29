(in-package #:common-lisp-jupyter)


(defun remove-if-not-match (partial-name matches)
  (remove-if-not (lambda (match)
                   (alexandria:starts-with-subseq partial-name match))
                 matches))


(defgeneric complete-fragment (match-set frag)
  (:method (match-set frag)
    (declare (ignore match-set frag))))


(defun complete-method (match-set partial-name start end package statuses)
  (when package
    (multiple-value-bind (sym status)
                         (find-symbol partial-name package)
      (when (and (member status statuses :test #'eql)
                 (boundp sym))
        (let* ((cls (class-of (symbol-value sym)))
               (supers (remove-if (lambda (cls)
                                    (eql t (class-name cls)))
                                  (closer-mop:class-precedence-list cls))))
          (dolist (method (find-methods cls))
            (let* ((name (closer-mop:generic-function-name (closer-mop:method-generic-function method)))
                   (specializers (closer-mop:method-specializers method))
                   (lambda-list (alexandria:parse-ordinary-lambda-list (closer-mop:method-lambda-list method)))
                   (pos (position-if (lambda (spec)
                                       (member spec supers))
                                     specializers)))
              (jupyter:match-set-add
                match-set
                (if (listp name)
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
                start end :type "method"))))))))


(defun complete-symbol (match-set partial-name start end package func statuses)
  (when package
    (do-symbols (sym package (values))
      (let ((sym-name (symbol-name sym)))
        (multiple-value-bind (s status)
                             (find-symbol sym-name package)
          (declare (ignore s))
          (when (and (member status statuses :test #'eql)
                     (alexandria:starts-with-subseq partial-name sym-name)
                     (or (and func (fboundp sym))
                         (and (not func)
                              (or (not (fboundp sym))
                                  (boundp sym)))))
            (jupyter:match-set-add
              match-set
              (string-downcase sym-name)
              start end
              :type (cond
                      ((special-operator-p sym)
                        "special")
                      ((and func
                            (macro-function sym))
                        "macro")
                      (func
                        "function")
                      ((keywordp sym)
                        "keyword")
                      ((and (boundp sym)
                            (constantp sym))
                        "constant")
                      ((boundp sym)
                        "variable")
                      ((subtypep (find-class sym nil) (find-class 'structure-object))
                        "structure")
                      ((find-class sym nil)
                        "class")
                      (t
                        "symbol")))))))))


(defun complete-package (match-set partial-name start end &key include-marker)
  (dolist (name (append (mapcar #'package-name (list-all-packages))
                        (mapcan (lambda (pkg) (copy-list (package-nicknames pkg))) (list-all-packages))
                         #+(or abcl clasp ecl) (mapcar #'car (ext:package-local-nicknames *package*))
                         #+allegro (mapcar #'car (excl:package-local-nicknames *package*))
                         #+ccl (mapcar #'car (ccl:package-local-nicknames *package*))
                         #+lispworks (mapcar #'car (hcl:package-local-nicknames *package*))
                         #+sbcl (mapcar #'car (sb-ext:package-local-nicknames *package*))))
    (when (alexandria:starts-with-subseq partial-name name)
      (jupyter:match-set-add match-set (format nil "~(~A~)~:[~;:~]" name include-marker) start end :type "package"))))


(defun complete-pathname (match-set partial-name start end &optional include-sharpsign-p)
  (let* ((path (pathname partial-name))
         (file (file-namestring path)))
    (dolist (d (append (directory (make-pathname :host (pathname-host path)
                                                 :device (pathname-device path)
                                                 :directory (pathname-directory path)
                                                 :name :wild
                                                 :type :wild))
                       (directory (make-pathname :host (pathname-host path)
                                                 :device (pathname-device path)
                                                 :directory (append (or (pathname-directory path)
                                                                        (list :relative))
                                                                    (list :wild))))))
      (let* ((part (if (zerop (length (file-namestring d)))
                     (make-pathname :directory (list :relative (car (last (pathname-directory d)))))
                     (pathname (file-namestring d))))
             (completed (merge-pathnames part (make-pathname :host (pathname-host path)
                                                             :device (pathname-device path)
                                                             :directory  (pathname-directory path)))))
        (when (or (not file)
                  (alexandria:starts-with-subseq file (namestring part)))
          (jupyter:match-set-add match-set
                                 (string-right-trim "\"" (write-to-string (if include-sharpsign-p completed (namestring completed))))
                                 start (if (> (- end start) (1+ (length partial-name)))
                                         (1- end)
                                         end)
                                 :type "pathname"))))))


(defmethod complete-fragment (match-set (frag symbol-name-fragment))
  (complete-fragment match-set (fragment-parent frag)))


(defmethod complete-fragment (match-set (frag package-marker-fragment))
  (complete-fragment match-set (fragment-parent frag)))


(defmethod complete-fragment (match-set (frag symbol-fragment))
  (with-slots (status position children)
              frag
    (let* ((func (or (equal 0 position)
                     (and (equal 1 position)
                          (equal 'function (fragment-value (first (fragment-children (fragment-parent frag))))))))
           (symbol-name-frag (car (last children)))
           (symbol-name (fragment-value symbol-name-frag))
           (start (fragment-start symbol-name-frag))
           (end (fragment-end symbol-name-frag)))
      (cond
        ((eql :local status)
          (complete-package match-set symbol-name start end :include-marker t)
          (unless func
            (complete-method match-set symbol-name start end *package*
                             '(:internal :external :inherited)))
          (complete-symbol match-set symbol-name start end *package* func
                           '(:internal :external :inherited)))
        (t
          (let ((pkg (find-package (if (zerop (length (fragment-value (first children))))
                                     "KEYWORD"
                                     (fragment-value (first children))))))
            (unless func
              (complete-method match-set symbol-name start end pkg (list status)))
            (complete-symbol match-set symbol-name start end pkg func (list status))))))))


(defmethod complete-fragment (match-set (frag package-name-fragment))
  (complete-package match-set (fragment-value frag) (fragment-start frag) (fragment-end frag)))


(defmethod complete-fragment (match-set (frag fragment))
  (when (and frag
             (stringp (fragment-value frag)))
    (complete-pathname match-set (fragment-value frag) (fragment-start frag) (fragment-end frag) nil)))


(defun do-complete-code (match-set code cursor-pos)
  (complete-fragment match-set (locate-fragment code (1- cursor-pos))))


(defun do-indent-code (match-set code cursor-pos)
  (declare (ignore cursor-pos))
  (indentify:initialize-templates)
  (jupyter:match-set-add match-set
                         (with-output-to-string (output-stream)
                           (with-input-from-string (input-stream code)
                             (indentify:indentify input-stream output-stream)))
                         0 (length code)))


(defmethod jupyter:complete-code ((k kernel) match-set code cursor-pos)
  (if (position (char code (1- cursor-pos)) "()")
    (do-indent-code match-set code cursor-pos)
    (do-complete-code match-set code cursor-pos))
  (values))


(defmethod jupyter:code-is-complete ((k kernel) code)
  (handler-case
      (do ((stream (make-string-input-stream code)))
          ((eq :eof (read stream nil :eof))))
    (end-of-file ()
      "incomplete")
    (serious-condition ()
      "invalid")
    (condition ()
      "invalid")
    (:no-error (val)
      (declare (ignore val))
      "complete")))

