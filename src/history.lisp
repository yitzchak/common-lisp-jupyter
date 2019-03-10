(in-package #:jupyter)

(defvar +history-size+ 1000)

(defclass history ()
  ((path :initarg :path
         :accessor history-path)
   (date :initform nil
         :accessor history-date)
   (session :initform 1
            :accessor history-session)
   (cells :initform nil
          :accessor history-cells)))

(defmacro lock-file (path &body body)
  `(iter
    (with lock-path = (merge-pathnames (make-pathname :type "lock") ,path))
    (for i from 1 to 20)
    (unless (probe-file lock-path)
      (finish))
    (sleep 0.1)
    (finally
      (open lock-path :direction :probe :if-does-not-exist :create)
      ,@body)
    (finally-protected
      (when (probe-file lock-path)
        (delete-file lock-path)))))

(defun read-history (history)
  (with-slots (path date session cells) history
    (when (and (probe-file path) (or (not date) (or (< date (file-write-date path)))))
      (lock-file path
        (let ((session-cells (remove-if-not (lambda (cell) (equal (car cell) session)) cells)))
          (setf cells
            (with-open-file (stream path :direction :input :if-does-not-exist nil)
              (when stream
                (iter
                  (for cell in-stream stream)
                  (collect cell)))))
          (iter
            (with new-session = (1+ (reduce (lambda (x y) (max x (car y))) cells :initial-value 0)))
            (for cell in session-cells)
            (setf cells (nconc cells (list (cons new-session (cdr cell)))))
            (finally
              (setf session new-session)
              (setf date (file-write-date path)))))))))

(defun write-history (history)
  (read-history history)
  (with-slots (path date session cells) history
    (uiop:ensure-all-directories-exist (list path))
    (lock-file path
      (with-open-file (stream path :direction :output :if-exists :supersede)
        (iter
          (for cell in (subseq cells (max 0 (- (length cells) +history-size+))))
          (pprint cell stream)))
      (setf date (file-write-date path)))))

(defmethod start ((h history))
  (info "[history] Starting...~%"))

(defmethod stop ((h history))
  (info "[history] Stopped.~%")
  (write-history h))

(defun add-cell (history number input)
  (with-slots (cells session) history
    (setf cells (nconc cells (list (list session number input))))))

(defun history-range (history sess start stop)
  (with-slots (cells session) history
    (when (< sess 0)
      (setq sess (+ session sess)))
    (remove-if-not (lambda (cell)
                     (and (equal sess (first cell))
                          (<= start (second cell))
                          (< (second cell) stop)))
      cells)))

(defun string-match-p (value pattern)
  (iter
    (for v in-string value with-index v-i)
    (generate p in-string pattern with-index p-i)
    (unless p (next p))
    (case p
      (#\*
        (when (string-match-p (subseq value v-i) (subseq pattern (1+ p-i)))
          (leave t)))
      (#\?
        (next p))
      (otherwise
        (if (eql v p)
          (next p)
          (leave nil))))
    (finally
      (return
        (and (or (= v-i (1- (length value))))
             (or (< p-i 0)
                 (= p-i (length pattern))
                 (equal "*" (subseq pattern p-i))))))))

(defun history-search (history n pattern unique)
  (with-slots (cells) history
    (iter
      (for cell in cells)
      (when (and (string-match-p (third cell) pattern)
                 (or (not unique)
                     (not (position-if (lambda (rc) (equal (third rc) (third cell))) results))))
        (collect cell into results))
      (finally
        (return (if n
          (subseq results (max 0 (- (length results) n)))
          results))))))

(defun history-tail (history n)
  (read-history history)
  (with-slots (cells) history
    (subseq cells (max 0 (- (length cells) n)))))
