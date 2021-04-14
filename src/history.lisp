(in-package #:jupyter)

(defvar +history-size+ 1000)

(defclass history (source)
  ((path
     :initarg :path
     :accessor history-path)
   (date
     :initform nil
     :accessor history-date)
   (session
     :initform 1
     :accessor history-session)
   (cells
     :initform nil
     :accessor history-cells)))


(defmacro lock-file (path &body body)
  (let ((lock-path (gensym))
        (lock-stream (gensym))
        (i (gensym)))
    `(let ((,lock-path (merge-pathnames (make-pathname :type "lock") ,path)))
       (unwind-protect
           (dotimes (,i 20)
             (declare (ignore ,i))
             (unless (probe-file ,lock-path)
               (with-open-file (,lock-stream ,lock-path :direction :probe :if-does-not-exist :create)
                 (declare (ignore ,lock-stream))
                 ,@body)
               (return))
             (sleep 0.1))
          (when (probe-file ,lock-path)
            (delete-file ,lock-path))))))


(defun read-history (history)
  (with-slots (path date session cells) history
    (when (and (probe-file path) (or (not date) (or (< date (file-write-date path)))))
      (lock-file path
        (let* ((session-cells (remove-if-not (lambda (cell) (equal (car cell) session)) cells))
               (old-cells (ignore-errors ; If the history file has become mangled then just give up.
                            (with-open-file (stream path :direction :input :if-does-not-exist nil)
                              (when stream
                                (do ((cell (read stream nil :eof) (read stream nil :eof))
                                     cells)
                                    ((eq :eof cell) (nreverse cells))
                                  (push cell cells))))))
               (new-session (1+ (reduce (lambda (x y) (max x (car y))) old-cells :initial-value 0))))
            (setf cells (nconc old-cells (mapcar (lambda (cell)
                                                   (cons new-session (cdr cell)))
                                                 session-cells))
                  session new-session
                  date (file-write-date path)))))))


(defun write-history (history)
  (read-history history)
  (with-slots (path date session cells) history
    (uiop:ensure-all-directories-exist (list path))
    (lock-file path
      (with-open-file (stream path :direction :output :if-exists :supersede)
        (dotimes (pos (min (length cells) +history-size+))
          (pprint (elt cells pos) stream)))
      (setf date (file-write-date path)))))


(defmethod start ((h history))
  (inform :info h "Starting history"))

(defmethod stop ((h history))
  (inform :info h "Stopping history")
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

(defun string-match-p (value pattern &optional (value-start 0) (pattern-start 0))
  (do ((value-pos value-start (1+ value-pos))
       (pattern-pos pattern-start))
      ((or (= value-pos (length value))
           (= pattern-pos (length pattern)))
       (and (= value-pos (length value))
            (or (= pattern-pos (length pattern))
                (and (= (1+ pattern-pos) (length pattern))
                     (char= #\* (char pattern pattern-pos))))))
    (case (char pattern pattern-pos)
      (#\*
        (when (string-match-p value pattern value-pos (1+ pattern-pos))
          (return t)))
      (#\?
        (incf pattern-pos))
      (otherwise
        (unless (char= (char value value-pos) (char pattern pattern-pos))
          (return nil))
        (incf pattern-pos)))))


(defun history-search (history n pattern unique)
  (let ((results (remove-if (lambda (cell)
                              (not (string-match-p (third cell) pattern)))
                            (history-cells history))))
    (when unique
      (setf results (delete-duplicates results :key #'third :test #'equal)))
    (if n
      (subseq results (max 0 (- (length results) n)))
      results)))


(defun history-tail (history n)
  (read-history history)
  (with-slots (cells) history
    (subseq cells (max 0 (- (length cells) n)))))
