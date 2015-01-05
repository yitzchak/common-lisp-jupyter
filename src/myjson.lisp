
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

(defun peek-next-char (input)
  (loop 
     (let ((char (peek-char nil input nil :eof)))
       (cond ((eql char :eof) (error 'json-parse-error :message "Unexpected end of file"))
	     ((char-whitespace-p char) (read-char input))
	     (t (return char))))))

(example (with-input-from-string (s "   {   ]")
	   (list (peek-next-char s)
		 (peek-next-char s)))
	 => '(#\{ #\{))


(defun parse-json (input)
  (let ((char (read-next-char input)))
    (cond ((char= char #\{) (parse-json-object input))
	  ((char= char #\[) (parse-json-array input))
	  ((char= char #\") (parse-json-string input))
	  ((or (char= char #\-)
	       (and (char>= char #\0)
		    (char<= char #\9))) (parse-json-number char input))
	  ((char= char #\t) (parse-json-literal-true input))
	  ((char= char #\f) (parse-json-literal-false input))
	  ((char= char #\n) (parse-json-literal-null input))
	  (t (error 'json-parse-error :message (format nil "Unexpected character: ~A" char))))))

(defun parse-json-literal (input first literal)
  (loop 
     for expect across literal
     do (let ((char (read-char input nil :eof)))
	  (cond ((eql char :eof)  (error 'json-parse-error :message (format nil "Unexpected end of file while parsing literal: ~A~A" first literal)))
		((not (char= char expect)) (error 'json-parse-error :message (format nil "While parsing literal, expecting '~A' instead of: ~A (literal ~A~A)" expect char first literal))))))
  t)

(defun parse-json-literal-true (input)
  (when (parse-json-literal input #\t "rue")
    :true))

(defun parse-json-literal-false (input)
  (when (parse-json-literal input #\f "alse")
    :false))

(defun parse-json-literal-null (input)
  (when (parse-json-literal input #\n "ull")
    :null))

(defun parse-json-string (input)
  (let ((str (make-array 32 :fill-pointer 0 :adjustable t :element-type 'character)))
    (loop
       (let ((char (read-char input nil :eof)))
	 (cond ((eql char :eof) (error 'json-parse-error :message "Unexpected end of file"))
	       ((char= char #\\)
		(vector-push-extend char str)
		(vector-push-extend (read-next-char input) str))
	       ((char= char #\") (return str))
	       (t (vector-push-extend char str)))))))

(example (with-input-from-string (s "this is a \\\"string\" and the rest")
	   (parse-json-string s))
	 => "this is a \\\"string")

(defun parse-json-object (input)
  (let ((obj (list)))
    (loop
       (let ((ckey (read-next-char input)))
	 (cond ((char= ckey #\}) (return (nreverse obj))) ;; special case : empty object 
	       ((not (char= ckey #\"))
		(error 'json-parse-error :message (format nil "Expecting \" for object key, found: ~A" ckey))))
	 (let ((key (parse-json-string input)))
	   (let ((sep (read-next-char input)))
	     (when (not (char= sep #\:))
	       (error 'json-parse-error :message "Missing key/value separator ':' in object"))
	     (let ((val (parse-json input)))
	       (setf obj (cons (cons key val) obj))
	       (let ((term (read-next-char input)))
		 (cond ((char= term #\,) t) ; continue
		       ((char= term #\}) (return (nreverse obj))) ; in-place is ok
		       (t (error 'json-parse-error :message (format nil "Unexpected character in object: ~A" term))))))))))))

(example (with-input-from-string (s "\"hello\": \"world\", \"val\": \"ue\" } et le reste")
	   (parse-json-object s))
	 => '(("hello" . "world") ("val" . "ue")))

(example (with-input-from-string (s "} et le reste")
	   (parse-json-object s))
	 => '())

(example (with-input-from-string (s "\"hello\": \"world\", } et le reste")
	   (parse-json-object s))
	 => '(("hello" . "world")))  ; slightly more permissinve with commas

(defun parse-json-array (input)
  (let ((array (make-array 32 :fill-pointer 0 :adjustable t)))
    (loop
       (let ((char (peek-next-char input)))
	 (if (char= char #\]) 
	     (progn (read-char input) ; consume the character
		    (return array))
	     ;; any other character
	     (let ((val (parse-json input)))
	       (vector-push-extend val array)
	       (let ((term (read-next-char input)))
		 (cond ((char= term #\]) (return array))
		       ((char= term #\,) t)  ; continue
		       (t (error 'json-parse-error :message (format nil "Unexpected array separator/terminator: ~A" term)))))))))))

(example (aref (with-input-from-string (s "\"first\", \"second\", \"third\" ] et le reste")
		 (parse-json-array s)) 2)
	 => "third")

(example (length (with-input-from-string (s "] et le reste")
		   (parse-json-array s)))
	 => 0)


(defun parse-json-digit (input &key (min #\0) (max #\9))
  (let ((char (read-next-char input)))
    (if (or (char< char min)
	    (char> char max))
	(error 'json-parse-error :message (format nil "Expecting digit between in range [~A..~A], found: ~A" min max char))
	char)))

(example (with-input-from-string (s "43")
	   (parse-json-digit s))
	 => #\4)

(defun parse-json-digits (input &key (min #\0) (max #\9))
  (let ((digits (make-array 8 :fill-pointer 0 :adjustable t :element-type 'character)))
    (loop
       (let ((char (peek-char nil input nil :eof)))
	 (cond ((eql char :eof) (return digits))
	       ((and (char>= char min)
		     (char<= char max))
		(read-char input)
		(vector-push-extend char digits))
	       (t (return digits)))))))

(example (with-input-from-string (s "42")
	   (parse-json-digits s))
	 => "42")

(example (with-input-from-string (s "43 b")
	   (parse-json-digits s))
	 => "43")

(defun parse-json-number (init input)
  (let ((number (format nil "~A" init)))
    ;; (format t "Initial = ~A ~%" number)
    (let ((fractpart (parse-json-number-fractional-part init input)))
      ;; (format t "Fractional = ~A ~%" fractpart)
      (setf number (concatenate 'string number fractpart)))
    (let ((sep (peek-char nil input nil :eof)))
      (when (eql sep #\.)
	(read-char input)
	(let ((decpart (parse-json-number-decimal-part input)))
	  ;; (format t "Decimal = ~A ~%" decpart)
	  (setf number (concatenate 'string number decpart))
	  (setf sep (peek-char nil input nil :eof))))
      (when (or (eql sep #\e) (eql sep #\E))
	(read-char input)
	(let ((exppart (parse-json-number-exponent-part (format nil "~A" sep) input)))
	  ;; (format t "Exponent = ~A ~%" exppart)
	  (setf number (concatenate 'string number exppart)))))
      ;; return the resulting number
      number))
	      
(defun parse-json-number-fractional-part (init input)
  (if (char= init #\0)
      ""
      (concatenate 'string 
		   (format nil "~A" (parse-json-digit input :min #\1))
		   (parse-json-digits input))))

(example (with-input-from-string (s "132402")
	   (parse-json-number-fractional-part #\- s))
	 => "132402")

(example (with-input-from-string (s "toto")
	   (parse-json-number-fractional-part #\0 s))
	 => "")
      
(defun parse-json-number-decimal-part (input)
  (concatenate 'string "." (parse-json-digits input)))

(defun parse-json-number-exponent-part (exp input)
  (let ((exponent exp))
    (let ((char (peek-char nil input nil :eof)))
      (cond ((eql char #\+) (read-char input) (setf exponent (concatenate 'string exponent "+")))
	    ((eql char #\-) (read-char input) (setf exponent (concatenate 'string exponent "-")))
	    ((and (characterp char)
		  (char>= char #\0) 
		  (char<= char #\9)) (read-char input) (setf exponent (concatenate 'string exponent (format nil "~A" char))))
	    (t (error 'json-parse-error :message (format nil "Missing exponent digit(s) or sign, found: ~A" char)))))
    (concatenate 'string exponent (parse-json-digits input))))

(example (with-input-from-string (s "+009")
	   (parse-json-number-exponent-part "e" s))
	 => "e+009")

(example (with-input-from-string (s "-909")
	   (parse-json-number-exponent-part "E" s))
	 => "E-909")

(example (with-input-from-string (s "909")
	   (parse-json-number-exponent-part "E" s))
	 => "E909")


(example (with-input-from-string (s "34.212e-42")
	   (parse-json-number #\- s))
	 => "-34.212e-42")

(example (with-input-from-string (s "34.212e-42")
	   (parse-json-number #\1 s))
	 => "134.212e-42")

(example (with-input-from-string (s ".212E+42")
	   (parse-json-number #\0 s))
	 => "0.212E+42")


(example (afetch "isAlive" 
		 (with-input-from-string (s "{
  \"firstName\": \"John\",
  \"lastName\": \"Smith\",
  \"isAlive\": true,
  \"age\": 25,
  \"height_cm\": 167.6,
  \"address\": {
    \"streetAddress\": \"21 2nd Street\",
    \"city\": \"New York\",
    \"state\": \"NY\",
    \"postalCode\": \"10021-3100\"
  },
  \"phoneNumbers\": [
    {
      \"type\": \"home\",
      \"number\": \"212 555-1234\"
    },
    {
      \"type\": \"office\",
      \"number\": \"646 555-4567\"
    }
  ],
  \"children\": [ \"alfi\", \"alfo\", \"alfa\" ],
  \"spouse\": null
}")
	   (parse-json s)) :test #'equal)
	 => :true)


