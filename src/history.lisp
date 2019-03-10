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

(defun read-history (history)
  (with-slots (path date session cells) history
    (when (and (probe-file path) (or (not date) (or (< date (file-write-date path)))))
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
            (setf date (file-write-date path))))))))

(defun write-history (history)
  (read-history history)
  (with-slots (path date session cells) history
    (uiop:ensure-all-directories-exist (list path))
    (with-open-file (stream path :direction :output :if-exists :supersede)
      (iter
        (for cell in cells)
        (pprint cell stream)))
    (setf date (file-write-date path))))

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
  (let ((empty-value (zerop (length value)))
        (empty-pattern (zerop (length pattern)))
        (single-star-pattern (equal "*" pattern)))
    (or (and empty-value
             (or empty-pattern single-star-pattern))
        (unless (or empty-value empty-pattern)
          (let ((v (char value 0))
                (p (char pattern 0)))
            (or (and (equal #\* p)
                     (or (string-match-p value (subseq pattern 1))
                         (string-match-p (subseq value 1) pattern)))
                (and (or (equal #\? p)
                         (equal v p))
                     (string-match-p (subseq value 1) (subseq pattern 1)))))))))

(defun history-search (history n pattern unique)
  (with-slots (cells) history
    (info "~S" cells)
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
