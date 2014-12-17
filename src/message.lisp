
(in-package #:uncommonshell)

#|

# Representation and manipulation of kernel messages #

|#

#|

## Message header ## 

|#

(defclass header ()
  ((msg-id :initarg :msg-id :reader header-msg-id :type string)
   (username :initarg :username :reader header-username :type string)
   (session :initarg :session :reader header-session :type string)
   (msg-type :initarg :msg-type :reader header-msg-type :type string)
   (version :initarg :version :initform "5.0" :reader header-version :type string))
  (:documentation "Header representation for IPython messages"))

#|

### JSon encoding ###

The encoding to *JSon* is performed manually (because `cl-json` is a little bit
 too inflexible in the encoding part, especially the CamelCase conversion which is not
 what is not the Python convention.

|#

(defgeneric to-json (object &key indent first-indent newlines)
  (:documentation "Conversion of OBJECT to JSON"))

(defun gen-indent (nb-chars)
  (if nb-chars
      (make-string nb-chars :initial-element #\Space)
      ""))

(example (gen-indent 10)
         => "          ")

(defmacro to-json-line (indent newlines &rest fmt)
  `(concatenate 'string
                (if ,indent
                    (gen-indent ,indent)
                    "")
                (format nil ,@fmt) (if ,newlines (format nil "~%") "")))

(example 
 (let ((toto '(me toto)))
   (to-json-line 4 t "blabla ~A" toto))
 => "    blabla (ME TOTO)
")

(defmethod to-json ((object t) &key (indent nil) (first-indent nil) (newlines nil))
  (declare (ignore indent))
  (to-json-line first-indent newlines "{}"))

(example (to-json "help" :first-indent 4 :newlines nil)
         => "    {}")

(defmethod to-json ((object header) &key (indent nil) (first-indent nil) (newlines nil))
  (with-slots (msg-id username session msg-type version) object
    (concatenate 'string
                 (to-json-line first-indent newlines "{")
                 (to-json-line indent newlines "\"msg_id\": ~W," msg-id)
                 (to-json-line indent newlines "\"username\": ~W," username)
                 (to-json-line indent newlines"\"session\": ~W," session)
                 (to-json-line indent newlines"\"msg_type\": ~W," msg-type)
                 (to-json-line indent newlines "\"version\": ~W" version)
                 (to-json-line first-indent nil "}"))))

(example-progn
  (defparameter *header1* (make-instance 'header
                                         :msg-id "XXX-YYY-ZZZ-TTT"
                                         :username "fredokun"
                                         :session "AAA-BBB-CCC-DDD"
                                         :msg-type "execute_request")))

(example
 (to-json *header1* :first-indent 0 :indent 2 :newlines t)
 => "{
  \"msg_id\": \"XXX-YYY-ZZZ-TTT\",
  \"username\": \"fredokun\",
  \"session\": \"AAA-BBB-CCC-DDD\",
  \"msg_type\": \"execute_request\",
  \"version\": \"5.0\"
}")


(example
 (to-json *header1* :first-indent 2 :indent 4 :newlines t)
 =>"  {
    \"msg_id\": \"XXX-YYY-ZZZ-TTT\",
    \"username\": \"fredokun\",
    \"session\": \"AAA-BBB-CCC-DDD\",
    \"msg_type\": \"execute_request\",
    \"version\": \"5.0\"
  }")

(example
 (to-json *header1*)
 => "{\"msg_id\": \"XXX-YYY-ZZZ-TTT\",\"username\": \"fredokun\",\"session\": \"AAA-BBB-CCC-DDD\",\"msg_type\": \"execute_request\",\"version\": \"5.0\"}")

#|

### JSon decoding ###

For *JSon* decoding *cl-json* works perfectly, only we do not use the direct CLOS decoding to avoid
 going deeper in the API.

|#

(example (json:decode-json-from-string (to-json *header1*))
         => '((:MSG--ID . "XXX-YYY-ZZZ-TTT") (:USERNAME . "fredokun")
              (:SESSION . "AAA-BBB-CCC-DDD") (:MSG--TYPE . "execute_request")
              (:VERSION . "5.0")))

(example
 (afetch :msg--id (json:decode-json-from-string (to-json *header1*)))
 => "XXX-YYY-ZZZ-TTT")

(example
 (afetch :username (json:decode-json-from-string (to-json *header1*)))
 => "fredokun")

(example
 (afetch :session (json:decode-json-from-string (to-json *header1*)))
 => "AAA-BBB-CCC-DDD")

(example
 (afetch :msg--type (json:decode-json-from-string (to-json *header1*)))
 => "execute_request")

(example
 (afetch :version (json:decode-json-from-string (to-json *header1*)))
 => "5.0")

#|

### Wire-deserialization ###

The deserialization of a message header from a JSon string is then trivial.

|#

(defun wire-deserialize-header (hdr)
  (let ((json-list (json:decode-json-from-string hdr)))
    (if json-list
        (make-instance 'header
                       :msg-id (afetch :msg--id json-list)
                       :username (afetch :username json-list)
                       :session (afetch :session json-list)
                       :msg-type (afetch :msg--type json-list))
        nil)))

(example-progn
  (defparameter *header2* (wire-deserialize-header (to-json *header1*))))


(example (header-username *header2*)
         => "fredokun")

#|

## IPython messages ##

|#

(defclass message ()
  ((header :initarg :header :accessor message-header)
   (parent-header :initarg :parent-header :initform nil :accessor message-parent-header)
   (metadata :initarg :metadata :initform nil :accessor message-metadata)
   (content :initarg :content :initform nil :accessor message-content))
  (:documentation "Representation of IPython messages"))

(example-progn
  (defparameter *msg1* (make-instance 'message :header *header1*)))


#|

## Wire-serialization ##

The wire-serialization of IPython kernel messages uses multi-parts ZMQ messages.

|#

(defconstant +WIRE-IDS-MSG-DELIMITER+ "<IDS|MSG>")

(defmethod wire-serialize ((msg message) &key (identities nil))
  (with-slots (header parent-header metadata content) msg
      (append identities
              (list +WIRE-IDS-MSG-DELIMITER+
                    "" ; TODO   HMAC signature
                    (to-json header)
                    (to-json parent-header)
                    (to-json metadata)
                    (to-json content)))))

(example-progn
  (defparameter *wire1* (wire-serialize *msg1* :identities '("XXX-YYY-ZZZ-TTT" "AAA-BBB-CCC-DDD"))))


#|

## Wire-deserialization ##

The wire-deserialization part follows.

|#


(example (position +WIRE-IDS-MSG-DELIMITER+ *wire1*)
         => 2)

(example (nth (position +WIRE-IDS-MSG-DELIMITER+ *wire1*) *wire1*)
         => +WIRE-IDS-MSG-DELIMITER+)

(example
 (subseq *wire1* 0 (position +WIRE-IDS-MSG-DELIMITER+ *wire1*))
 => '("XXX-YYY-ZZZ-TTT" "AAA-BBB-CCC-DDD"))

(example
 (subseq *wire1* (+ 6 (position +WIRE-IDS-MSG-DELIMITER+ *wire1*)))
 => nil)
         
(example
 (let ((delim-index (position +WIRE-IDS-MSG-DELIMITER+ *wire1*)))
   (subseq *wire1* (+ 2 delim-index) (+ 6 delim-index)))
 => '("{\"msg_id\": \"XXX-YYY-ZZZ-TTT\",\"username\": \"fredokun\",\"session\": \"AAA-BBB-CCC-DDD\",\"msg_type\": \"execute_request\",\"version\": \"5.0\"}"
      "{}" "{}" "{}"))


(defun wire-deserialize (parts)
  (let ((delim-index (position +WIRE-IDS-MSG-DELIMITER+ parts)))
    (when (not delim-index)
      (error "no <IDS|MSG> delimiter found in message parts"))
    (let ((identities (subseq parts 0 delim-index))
          (signature (nth (1+ delim-index) parts)))
      (let ((msg (destructuring-bind (header parent-header metadata content)
                     (subseq parts (+ 2 delim-index) (+ 6 delim-index))
                   (make-instance 'message
                                  :header (wire-deserialize-header header)
                                  :parent-header (wire-deserialize-header parent-header)
                                  :metadata metadata
                                  :content content))))
        (values identities
                signature
                msg
                (subseq parts (+ 6 delim-index)))))))


(example-progn
  (defparameter *dewire-1* (multiple-value-bind (ids sig msg raw)
                               (wire-deserialize *wire1*)
                             (list ids sig msg raw))))

(example
 (header-username (message-header (third *dewire-1*)))
 => "fredokun")

#|

### Sending and receiving messages ###

|#

(defun message-send (socket msg &key (identities nil))
  (let ((wire-parts (wire-serialize msg :identities identities)))
    (dolist (identity identities)
      (pzmq:send socket identity :sndmore t))
    (pzmq:send socket +WIRE-IDS-MSG-DELIMITER+ :sndmore t)
    (dolist (part wire-parts)
      (pzmq:send socket part :sndmore t))
    (pzmq:send socket nil)))

(defun zmq-recv-list (socket &optional (parts nil))
  (multiple-value-bind (part more)
      (pzmq:recv-string socket)
    (if more
        (zmq-recv-list socket (cons part parts))
        (reverse (cons part parts)))))

(defun message-recv (socket)
  (let ((parts (zmq-recv-list socket)))
    (wire-deserialize parts)))

     
