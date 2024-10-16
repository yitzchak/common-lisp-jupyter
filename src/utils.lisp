(in-package #:jupyter)

#|

# CommonTypes: Utilities #

|#

; (defvar maxima::$kernel_info nil)
;; asdasd

(defparameter +uuid-size+ 16)
(defparameter *uuid-random-state* (make-random-state t))

(defun octets-to-hex-string (bytes)
  (format nil "~(~{~2,'0X~}~)" (coerce bytes 'list)))

(defun make-uuid (&optional as-bytes)
  (let ((bytes (make-array +uuid-size+ :element-type '(unsigned-byte 8))))
    (dotimes (index +uuid-size+)
      (setf (aref bytes index)
        (if (= 6 index)
          (logior #x40 (random 16 *uuid-random-state*))
          (random 256 *uuid-random-state*))))
    (if as-bytes
      bytes
      (octets-to-hex-string bytes))))

(defun symbol-to-camel-case (s)
  (do ((name (symbol-name s))
       (position 0 (1+ position))
       (result "")
       capitalize)
      ((= position (length name)) result)
    (cond
      ((char= (char name position) #\-)
        (setq capitalize t))
      (capitalize
        (setq result (concatenate 'string result (string (char-upcase (char name position)))))
        (setq capitalize nil))
      (t
        (setq result (concatenate 'string result (string (char-downcase (char name position)))))))))

(defun camel-case-to-symbol (name)
  (intern
    (do ((position 0 (1+ position))
         (result ""))
        ((= position (length name)) result)
      (when (and (not (zerop position))
                 (upper-case-p (char name position)))
        (setq result (concatenate 'string result "-")))
        (setq result (concatenate 'string result (string (char-upcase (char name position))))))
    "KEYWORD"))

(defun symbol-to-snake-case (s)
  (substitute #\_ #\%
    (substitute #\_ #\-
      (string-downcase (symbol-name s)))))

(defun snake-case-to-symbol (k)
  (intern
    (string-upcase
      (substitute #\- #\_
                  (if (and (not (zerop (length k)))
                                (char= (char k 0) #\_))
                    (substitute #\% #\_ k :count 1)
                    k)))
    "KEYWORD"))

(defun json-to-plist (value &key symbol-case)
  (mapcan (lambda (pair)
            (list (case symbol-case
                    (:snake
                      (snake-case-to-symbol (car pair)))
                    (:camel
                      (camel-case-to-symbol (car pair)))
                    (otherwise
                      (intern (car pair) "KEYWORD")))
                  (cdr pair)))
          (cdr value)))

(defun json-to-nested-plist (value &key symbol-case)
  (if (and (listp value)
           (eql :obj (car value)))
    (mapcan (lambda (pair)
              (list (case symbol-case
                      (:snake
                        (snake-case-to-symbol (car pair)))
                      (:camel
                        (camel-case-to-symbol (car pair)))
                      (otherwise
                        (intern (car pair) "KEYWORD")))
                    (json-to-nested-plist (cdr pair) :symbol-case symbol-case)))
            (cdr value))
    value))

(defun murmur-hash-2 (data seed)
  (declare (type (unsigned-byte 32) seed))
  (prog ((m #x5bd1e995)
            (r -24)
      (h (logxor seed (length data)))
         (pos 0)
         (k 0)
         (remaining (length data)))
    (declare (type (unsigned-byte 32) m h k))
   next-word
    (case remaining
      (0)
      (1
        (setq h (ldb (byte 32 0)
                     (* m (logxor h (elt data pos))))))
      (2
           (setq h (ldb (byte 32 0)
                     (* m (logxor h
                                  (logior (elt data pos)
                                          (ash (elt data (1+ pos)) 8)))))))    (3
           (setq h (ldb (byte 32 0)
                     (* m (logxor h
                                  (logior (elt data pos)
                                          (ash (elt data (1+ pos)) 8)
                                          (ash (elt data (+ 2 pos)) 16)))))))
    (otherwise
           (setq k (ldb (byte 32 0)
                     (* m (logior (elt data pos)
                                  (ash (elt data (1+ pos)) 8)
                                  (ash (elt data (+ 2 pos)) 16)
                                  (ash (elt data (+ 3 pos)) 24)))))
        (setq h (ldb (byte 32 0) (logxor (* m h)
                                         (* m (logxor k (ash k r))))))
        (incf pos 4)
        (decf remaining 4)
        (go next-word)))
    (setq h (ldb (byte 32 0)
                 (* m (logxor h (ash h -13)))))
    (return (ldb (byte 32 0)
                 (logxor h (ash h -15))))))


(defmacro hash-case (form &rest clauses)
  (let ((ev-form (gensym))
        (hash-form (gensym)))
    (if (or (< (length clauses) 5) #+abcl t #-abcl nil)
      `(let ((,ev-form ,form))
         (cond
           ,@(mapcar (lambda (clause)
                       (cond
                         ((equal 'otherwise (first clause))
                           `(t ,@(cdr clause)))
                         (t
                           `((equal ,ev-form ,(first clause))
                              ,@(cdr clause)))))
                      clauses)))
      `(let* ((,ev-form ,form)
              (,hash-form (sxhash ,ev-form)))
         (cond
           ,@(mapcar (lambda (clause)
                       (cond
                         ((equal 'otherwise (first clause))
                           `(t ,@(cdr clause)))
                         (t
                           `((and (= ,hash-form ,(sxhash (first clause)))
                                  (equal ,ev-form ,(first clause)))
                              ,@(cdr clause)))))
                      clauses))))))

#+ccl
(defmethod ccl::input-stream-shared-resource ((s synonym-stream))
  (ccl::input-stream-shared-resource (symbol-value (synonym-stream-symbol s))))

#+ccl
(defmethod (setf ccl::input-stream-shared-resource) (new (s synonym-stream))
  (setf (ccl::input-stream-shared-resource (symbol-value (synonym-stream-symbol s))) new))

(define-condition closed-stream (stream-error)
  ())

(defclass closed-input-stream (ngray:fundamental-character-input-stream)
  ())

(defmethod ngray:stream-clear-input ((stream closed-input-stream))
  (error 'closed-stream :stream stream))

(defmethod ngray:stream-read-char ((stream closed-input-stream))
  (error 'closed-stream :stream stream))

(defmethod ngray:open-stream-p ((stream closed-input-stream))
  nil)

(defmethod close ((stream closed-input-stream) &key abort)
  (declare (ignore abort))
  nil)
