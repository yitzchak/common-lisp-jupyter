(in-package #:jupyter/common-lisp)


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


(defclass tracking-stream (trivial-gray-streams:fundamental-character-input-stream)
  ((stream
     :reader tracking-stream-stream
     :initarg :stream)
   (line
     :accessor tracking-stream-line
     :initform 1)
   (column
     :accessor tracking-stream-column
     :initform 0)
   (previous-column
     :accessor tracking-stream-previous-column
     :initform 1)))


(defmethod trivial-gray-streams:stream-file-position ((stream tracking-stream))
  (file-position (tracking-stream-stream stream)))


(defmethod trivial-gray-streams:stream-listen ((stream tracking-stream))
  (listen (tracking-stream-stream stream)))


(defmethod trivial-gray-streams:stream-read-char ((stream tracking-stream))
  (let ((ch (read-char (tracking-stream-stream stream) nil :eof)))
    (cond
      ((eq :eof ch))
      ((char= ch #\Newline)
        (incf (tracking-stream-line stream))
        (setf (tracking-stream-previous-column stream) (tracking-stream-column stream)
              (tracking-stream-column stream) 1))
      ((char= ch #\Tab)
        (incf (tracking-stream-column stream) 8))
      ((graphic-char-p ch)
        (incf (tracking-stream-column stream))))
    ch))


(defmethod trivial-gray-streams:stream-peek-char ((stream tracking-stream))
  (peek-char nil (tracking-stream-stream stream) nil :eof))


(defmethod trivial-gray-streams:stream-unread-char ((stream tracking-stream) char)
  (cond
    ((char= char #\Newline)
      (decf (tracking-stream-line stream))
      (setf (tracking-stream-column stream) (tracking-stream-previous-column stream)))
    ((char= char #\Tab)
      (decf (tracking-stream-column stream) 8))
    ((graphic-char-p char)
      (decf (tracking-stream-column stream))))
  (unread-char char (tracking-stream-stream stream)))


(defmacro with-tracking-stream ((stream &rest rest) &body body)
  `(let ((,stream (make-instance 'tracking-stream :stream (open ,@rest))))
     (unwind-protect
         (progn
           ,@body)
       (close (tracking-stream-stream ,stream)))))


(defparameter *source-maps* (make-hash-table :test #'equal))


(defun reset-source-map (pathname &aux (truename (namestring (truename pathname))))
  (setf (gethash truename *source-maps*)
        (make-array 32 :fill-pointer 0 :adjustable t)))


(defun get-source-map (pathname &aux (truename (namestring (truename pathname))))
  #+(or ccl cmucl ecl sbcl)
  (multiple-value-bind (source-map present)
                       (gethash truename *source-maps*)
    (if present
      source-map
      #+(or cmucl sbcl) (handler-bind
                            ((error #'eclector.base:recover))
                          (with-tracking-stream (stream truename)
                            (do* ((client (make-instance 'tracking-client))
                                  (source-map (make-array 32 :adjustable t :fill-pointer 0))
                                  (form-map (eclector.parse-result:read client stream nil :eof)
                                            (eclector.parse-result:read client stream nil :eof)))
                                ((eq :eof form-map) (setf (gethash truename *source-maps*) source-map))
                              (vector-push-extend form-map source-map))))
      #+(or ccl ecl) (with-tracking-stream (stream truename)
                       (prog (char
                              (left 0)
                              (right 0)
                              (source-map (make-array 32 :adjustable t :fill-pointer 0)))
                        next
                         (setf char (peek-char nil stream nil))
                         (when (or (null char)
                                   (char= #\Tab char)
                                   (char= #\Newline char))
                           (vector-push-extend (list left
                                                     right
                                                     (tracking-stream-line stream)
                                                     (1+ (- (tracking-stream-column stream) right)))
                                               source-map)
                           (setf left right))
                         (unless char
                           (return (setf (gethash truename *source-maps*) source-map)))
                         (read-char stream nil)
                         (setf right (file-position stream))
                         (go next))))))


(defun source-line-column (pathname position1 #+(or cmucl sbcl) position2)
  (handler-case
      #+(or cmucl sbcl) (values-list (elt (elt (get-source-map pathname) position1) position2))
      #+(or ccl ecl) (let ((record (find-if (lambda (record)
                                              (< (1- (first record)) position1 (second record)))
                                            (get-source-map pathname))))
                       (when record
                         (values (third record)
                                 (+ (fourth record) position1))))
    (error (condition)
      (declare (ignore condition)))))


(defun file-position-to-line-column (pathname position)
  (when (and pathname position)
    (with-tracking-stream (stream pathname)
      (prog ((index 0) char)
       next
        (setf char (peek-char nil stream nil))
        (unless char
          (return (values 0 0)))
        (when (and (>= index position)
                   (not (member char '(#\Newline #\Return #\Space #\Tab) :test #'equal)))
          (return (values (tracking-stream-line stream)
                          (tracking-stream-column stream))))
        (read-char stream nil)
        (incf index)
        (go next)))))


#-clasp
(defmethod eclector.base:source-position (client (stream tracking-stream))
  (list (tracking-stream-line stream)
        (tracking-stream-column stream)))

#-clasp
(defclass tracking-client (eclector.parse-result:parse-result-client)
  ())

#-clasp
(defmethod eclector.parse-result:make-expression-result ((client tracking-client) (result (eql nil)) children source)
  nil)

#-clasp
(defmethod eclector.parse-result:make-expression-result ((client tracking-client) result children source)
  nil)

#-clasp
(defmethod eclector.parse-result:make-expression-result ((client tracking-client) (result list) children source)
  (cons (car source)
        (remove-if #'null (apply #'append children))))

#-clasp
(defun form-offset-to-line-column (pathname form-offset form-number)
  (handler-bind
      ((error #'eclector.base:recover))
    (when (and pathname form-offset form-number)
      (with-tracking-stream (stream pathname)
        (let ((client (make-instance 'tracking-client)))
          (dotimes (count form-offset)
            (eclector.parse-result:read client stream nil))
          (let ((source-map (nreverse (eclector.parse-result:read client stream nil))))
          (when (< form-number (length source-map))
            (values-list (elt source-map form-number)))))))))


#+sbcl
(defvar *only-block-start-locations* nil)


#+sbcl
(defun first-code-location (debug-block)
  (let ((found nil)
        (first-code-location nil))
    (sb-di:do-debug-block-locations (code-location debug-block)
                                    (unless found
                                      (setf first-code-location code-location)
                                      (setf found t)))
    first-code-location))


#+sbcl
(defun possible-breakpoints (function)
  (let ((possible-breakpoints nil))
    (sb-di:do-debug-fun-blocks (debug-block (sb-di::fun-debug-fun function))
      (unless (sb-di:debug-block-elsewhere-p debug-block)
        (if *only-block-start-locations*
          (push (first-code-location debug-block) possible-breakpoints)
          (sb-di:do-debug-block-locations (code-location debug-block)
            (when (not (member (sb-di:code-location-kind code-location)
                               '(:call-site :internal-error)))
              (push code-location possible-breakpoints))))))
    (sort possible-breakpoints
          (lambda (x y)
            (or (< (sb-di:code-location-toplevel-form-offset x)
                   (sb-di:code-location-toplevel-form-offset y))
                (< (sb-di:code-location-form-number x)
                   (sb-di:code-location-form-number y)))))))


(defun eval-with-bindings (form bindings)
  (eval `(let ,(loop for (var . val) in bindings collect `(,var ',val))
           (declare (ignorable ,@(mapcar #'car bindings)))
           ,form)))



