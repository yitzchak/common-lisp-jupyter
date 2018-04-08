(in-package #:maxima-jupyter)

(defun plot-p (value)
  (and (listp value)
       (eq (caar value) 'maxima::mlist)
       (eq (list-length value) 3)
       (stringp (second value))
       (stringp (third value))
       (or (ends-with-p (second value) ".gnuplot")
           (ends-with-p (second value) ".gnuplot_pipes"))))

(defun sexpr-to-text (value)
  (format nil "~S" value))

(defun mexpr-to-text (value)
  (with-output-to-string (f)
    (maxima::mgrind value f)))

(defun mexpr-to-latex (value)
  (let ((env (maxima::get-tex-environment value)))
    (apply #'concatenate 'string
           (mapcar #'string
                   (maxima::tex value
                                (list (car env)) (list (cdr env))
                                'maxima::mparen 'maxima::mparen)))))

(defgeneric display (results)
  (:documentation "Display results."))

(defmethod display (res))

(defclass result ()
  ())

(defclass sexpr-result (result)
  ((value :initarg :value
          :reader sexpr-result-value)))

(defmethod display ((res sexpr-result))
  (jsown:new-js
    ("text/plain" (sexpr-to-text (sexpr-result-value res)))))

(defclass mexpr-result (result)
  ((value :initarg :value
          :reader mexpr-result-value)))

(defmethod display ((res mexpr-result))
  (let ((value (mexpr-result-value res)))
    (jsown:new-js
      ("text/plain" (mexpr-to-text value))
      ("text/latex" (mexpr-to-latex value)))))

(defclass inline-result (result)
  ((value :initarg :value
          :reader inline-result-value)
   (mime-type :initarg :mime-type
              :reader inline-result-mime-type)))

(defun make-inline-result (path &key (mime-type "text/plain"))
 (make-instance 'inline-result :path path
                               :mime-type mime-type))

(defmethod display ((res inline-result))
  (let ((value (inline-result-value res))
        (mime-type (inline-result-mime-type res)))
    (if (equal mime-type "text/plain")
      (jsown:new-js
        (mime-type value))
      (jsown:new-js
        ("text/plain" "inline-value")
        (mime-type (if (stringp value)
                       value
                       (cl-base64:usb8-array-to-base64-string value)))))))

(defclass file-result (result)
  ((path :initarg :path
         :reader file-result-path)
   (mime-type :initarg :mime-type
              :initform nil
              :reader file-result-mime-type)))

(defun make-file-result (path &key (mime-type nil))
  (make-instance 'file-result :path path
                              :mime-type mime-type))

(defmethod display ((res file-result))
  (let* ((path (file-result-path res))
         (mime-type (or (file-result-mime-type res) (trivial-mimes:mime path))))
    (if (equal mime-type "text/plain")
      (jsown:new-js
        (mime-type (read-string-file path)))
      (jsown:new-js
        ("text/plain" path)
        (mime-type
          (if (or (equal mime-type "image/svg+xml") (starts-with-p mime-type "text/"))
            (read-string-file path)
            (file-to-base64-string path)))))))

(defclass error-result (result)
  ((ename :initarg :ename
          :reader error-result-ename)
   (evalue :initarg :evalue
           :reader error-result-evalue)
   (quit :initarg :quit
         :initform nil
         :reader error-result-quit)
   (traceback :initarg :traceback
              :initform nil
              :reader error-result-traceback)))

(defun make-error-result (ename evalue msg &key (quit nil) (traceback nil))
  (make-instance 'error-result :ename ename :evalue evalue
                               :quit quit :traceback traceback))

(defun make-maxima-result (value)
  (if (typep value 'result)
    value
    (when (eq (caar value) 'maxima::displayinput)
      (let ((actual-value (third value)))
        (cond ((typep actual-value 'result)
               actual-value)
              ((plot-p actual-value)
               (make-instance 'file-result :path (third actual-value)))
              (t
               (make-instance 'mexpr-result :value actual-value)))))))

(defun make-lisp-result (value)
  (if (typep value 'result)
    value
    (make-instance 'sexpr-result :value value)))
