(in-package #:jupyter)


(defvar *kernel* nil)


(defclass sink ()
  ((path
     :initarg :path
     :accessor logger-path)
   (stream
     :initform nil
     :accessor logger-stream)
   (lock
     :initform (bordeaux-threads:make-lock (make-uuid))
     :reader logger-lock)))

(defclass source ()
  ((sink
     :initarg :sink
     :initform nil
     :accessor source-sink)))

(defmethod start ((l sink))
  (with-slots (lock path stream) l
    (bordeaux-threads:with-lock-held (lock)
      (uiop:ensure-all-directories-exist (list path))
      (setf stream (open path :direction :output
                              :if-exists :rename
                              :if-does-not-exist :create
                              #+ccl :sharing #+ccl :external)))))

(defmethod stop ((l sink))
  (with-slots (lock stream) l
    (bordeaux-threads:with-lock-held (lock)
      (close stream)
      (setf stream nil))))

(defun inform (level src format-control &rest format-arguments)
  (alexandria:when-let ((s (or src *kernel*)))
    (with-slots (lock stream) (source-sink s)
      (bordeaux-threads:with-lock-held (lock)
        (ignore-errors
          (when (and (eql :error level)
                     *kernel*)
            (send-execute-error (kernel-iopub *kernel*)
                                (class-name (class-of src)) (apply #'format nil format-control format-arguments))))
        (multiple-value-bind (second minute hour day month year)
                             (get-decoded-time)
          (format stream "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d [~A] <~A> ~?~%"
                         year month day hour minute second
                         level (class-name (class-of src))
                         format-control format-arguments)
          (finish-output stream))))))
