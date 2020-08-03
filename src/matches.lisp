(in-package #:jupyter)


(defstruct match
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


(defgeneric match-set-add (instance text start end &key type))


(defmethod match-set-add ((ms match-set) text start end &key type)
  (let ((m (make-match :text text :type type)))
    (cond
      ((null (match-set-matches ms))
        (setf (match-set-start ms) start)
        (setf (match-set-end ms) end))
      ((or (< start (match-set-start ms))
           (> end (match-set-end ms)))
        (let* ((new-start (min start (match-set-start ms)))
               (new-end (max end (match-set-end ms)))
               (prefix (subseq (match-set-code ms) new-start (match-set-start ms)))
               (suffix (subseq (match-set-code ms) (match-set-end ms) new-end)))
          (setf (match-text m)
                (concatenate 'string
                             (subseq (match-set-code ms) new-start start)
                             (match-text m)
                             (subseq (match-set-code ms) end new-end)))
          (setf (match-set-start ms) new-start)
          (setf (match-set-end ms) new-end)
          (dolist (m (match-set-matches ms))
            (setf (match-text m)
                  (concatenate 'string prefix (match-text m) suffix))))))
    (push m (match-set-matches ms))))


(defmethod match-set-add ((ms offset-match-set) text start end &key type)
  (match-set-add (offset-match-set-parent ms) text
                 (+ start (offset-match-set-offset ms)) (+ end (offset-match-set-offset ms))
                 :type type))

