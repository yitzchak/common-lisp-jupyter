(in-package #:jupyter)


(defstruct match
  (quality 0)
  text
  (type nil))


(defstruct match-set
  code
  (matches nil)
  start
  end)


(defstruct offset-match-set
  parent
  offset)


(defun match-set-expand (ms match start end)
  (when (or (< start (match-set-start ms))
            (> end (match-set-end ms)))
    (let* ((new-start (min start (match-set-start ms)))
           (new-end (max end (match-set-end ms)))
           (prefix (subseq (match-set-code ms) new-start (match-set-start ms)))
           (suffix (subseq (match-set-code ms) (match-set-end ms) new-end)))
      (setf (match-text match)
            (concatenate 'string
                         (subseq (match-set-code ms) new-start start)
                         (match-text match)
                         (subseq (match-set-code ms) end new-end)))
      (setf (match-set-start ms) new-start)
      (setf (match-set-end ms) new-end)
      (dolist (m (match-set-matches ms))
        (setf (match-text m)
              (concatenate 'string prefix (match-text m) suffix))))))


(defun match-quality-lessp (m1 m2)
  (or (< (match-quality m1) (match-quality m2))
      (and (= (match-quality m1) (match-quality m2))
           (string-greaterp (match-text m1) (match-text m2)))))


(defgeneric match-set-add (instance text start end &key type quality))


(defmethod match-set-add ((ms match-set) text start end &key type quality)
  (let ((m (make-match :text text :quality (or quality 0) :type type)))
    (cond
      ((null (match-set-matches ms))
        (setf (match-set-matches ms) (list m))
        (setf (match-set-start ms) start)
        (setf (match-set-end ms) end))
      ((match-quality-lessp (car (match-set-matches ms)) m)
        (match-set-expand ms m start end)
        (push m (match-set-matches ms)))
      (t
        (match-set-expand ms m start end)
        (do ((prev (match-set-matches ms) (cdr prev))
             (current (cdr (match-set-matches ms)) (cdr current)))
            ((or (null current)
                 (match-quality-lessp (car current) m))
              (setf (cdr prev) (cons m current))))))))


(defmethod match-set-add ((ms offset-match-set) text start end &key type quality)
  (match-set-add (offset-match-set-parent ms) text
                 (+ start (offset-match-set-offset ms)) (+ end (offset-match-set-offset ms))
                 :type type :quality quality))

