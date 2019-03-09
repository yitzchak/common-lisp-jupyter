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
    (uiop:ensure-all-directories-exist (list path))
    (with-open-file (stream path :direction :output :if-exists :supersede)
      (iter
        (for cell in-vector cells)
        (pprint cell stream)))))

(defmethod start ((h history))
  (read-history h))

(defmethod stop ((h history))
  (write-history h))

(defun add-cell (history number input)
  (with-slots (cells session) history
    (vector-push (list session number input) cells)))

(defun add-output (history number output)
  (with-slots (cells session) history
    (let ((cell (find-if (lambda (cell) (and (equal session (first cell)) (equal number (second cell))))
                  cells :from-end t)))
      (when cell
        (nconc cell (list output))))))

(defun history-range (history session start stop)
  (declare (ignore history session start stop)))

(defun history-search (history n pattern unique)
  (declare (ignore history n pattern unique)))

(defun history-tail (history n)
  (with-slots (cells) history
    (subseq cells (max 0 (- (length cells) n)))))
