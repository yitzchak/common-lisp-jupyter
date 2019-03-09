(in-package #:jupyter)

(defvar +history-size+ 1024)

(defclass history ()
  ((path :initarg :path
         :accessor history-path)
   (session :initform 0
            :accessor history-session)
   (cells :initform (make-array +history-size+ :fill-pointer 0 :adjustable t)
          :accessor history-cells)))

(defun read-history (history)
  (with-slots (path session cells) history
    (adjust-array cells +history-size+ :fill-pointer 0)
    (with-open-file (stream path :direction :input :if-does-not-exist nil)
      (when stream
        (iter
          (for cell in-stream stream)
          (vector-push cell cells))))))

(defun write-history (history)
  (with-slots (path session cells) history
    (with-open-file (stream path :direction :output :if-exists :supersede)
      (iter
        (for cell in-vector cells)
        (pprint cell stream)))))

(defmethod start ((h history))
  (read-history h))

(defmethod stop ((h history))
  (write-history h))

(defun add-cell (history number input output)
  (with-slots (cells session) history
    (vector-push (list session number input output) cells)))
