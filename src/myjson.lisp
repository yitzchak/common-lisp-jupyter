
(in-package #:myjson)

#|

# (Yet another) JSon encoding/decoding framework #

There are several libraries available for json encoding and decoding,
 but none that satisfies all our requirements.

|#

#|

## Mapping types ##

 - JSon objects map to Lisp a-lists with string keys

 - Json arrays map to Lisp vectors

 - JSon strings map to Lisp strings

 - JSon true maps to :true

 - JSon false maps to :false

 - JSon null maps to :null

|#

#|

## Parsing JSon ##

|#

(define-condition json-parse-error (error)
  ((message :initarg :message
	    :reader match-error-message))
  (:report (lambda (condition stream)
	     (format stream "[JSon parse error] ~A"
		     (match-error-message condition)))))

(defun char-whitespace-p (char)
  (or (char= char #\Space)
      (char= char #\Tab)
      (char= char #\Return)
      (char= char #\Linefeed)))

(defun read-next-char (input)
  (loop 
     (let ((char (read-char input nil :eof)))
       (cond ((eql char :eof) (error 'json-parse-error :message "Unexpected end of file"))
	     ((not (char-whitespace-p char)) (return char))))))


(example (with-input-from-string (s "   {   ]")
	   (list (read-next-char s)
		 (read-next-char s)))
	 => '(#\{ #\]))

(defun parse-json (input)
  (let ((char (read-next-char input)))
    (cond ((char= char #\{) (parse-json-object input))
	  ((char= char #\[) (parse-json-array input))
	  ((char= char #\") (parse-json-string input))
	  ((or (char= char #\-)
	       (and (char>= char #\0)
		    (char<= char #\9))) (parse-json-number char input))
	  (else (error 'json-parse-error :message "Unexpected character: ~A" char)))))

(defun parse-json-object (input)
  ())
