
(in-package #:cl-jupyter)

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
   (version :initarg :version :initform +KERNEL-PROTOCOL-VERSION+ :reader header-version :type string))
  (:documentation "Header representation for IPython messages"))

#|

### JSon encoding ###

|#

(defmethod encode-json (stream (object header) &key (indent nil) (first-line nil))
  (with-slots (msg-id username session msg-type version) object
    (encode-json stream `(("msg_id" . ,msg-id)
                          ("username" . ,username)
                          ("session" . ,session)
                          ("msg_type" . ,msg-type)
                          ("version" . ,version))
                 :indent indent :first-line first-line)))
    
(example-progn
  (defparameter *header1* (make-instance 'header
                                         :msg-id "XXX-YYY-ZZZ-TTT"
                                         :username "fredokun"
                                         :session "AAA-BBB-CCC-DDD"
                                         :msg-type "execute_request")))

(example
 (encode-json-to-string *header1* :indent 0)
 => "{
  \"msg_id\": \"XXX-YYY-ZZZ-TTT\",
  \"username\": \"fredokun\",
  \"session\": \"AAA-BBB-CCC-DDD\",
  \"msg_type\": \"execute_request\",
  \"version\": \"4.1\"
}")


(example
 (encode-json-to-string *header1*)
 => "{\"msg_id\": \"XXX-YYY-ZZZ-TTT\",\"username\": \"fredokun\",\"session\": \"AAA-BBB-CCC-DDD\",\"msg_type\": \"execute_request\",\"version\": \"4.1\"}")

#|

### JSon decoding ###

|#

(example (parse-json-from-string (encode-json-to-string *header1*))
         => '(("msg_id" . "XXX-YYY-ZZZ-TTT") ("username" . "fredokun")
	      ("session" . "AAA-BBB-CCC-DDD") ("msg_type" . "execute_request")
	      ("version" . "4.1")))

(example
 (afetch "msg_id" (parse-json-from-string (encode-json-to-string *header1*)) :test #'equal)
 => "XXX-YYY-ZZZ-TTT")

(example
 (afetch "username" (parse-json-from-string (encode-json-to-string *header1*)) :test #'equal)
 => "fredokun")

#|

### Wire-deserialization ###

The deserialization of a message header from a JSon string is then trivial.

|#

(defun wire-deserialize-header (hdr)
  (let ((json-list (parse-json-from-string hdr)))
    (if json-list
        (make-instance 'header
                       :msg-id (afetch "msg_id" json-list :test #'equal)
                       :username (afetch "username"json-list :test #'equal)
                       :session (afetch "session" json-list :test #'equal)
                       :msg-type (afetch "msg_type" json-list :test #'equal))
        nil)))

(example-progn
  (defparameter *header2* (wire-deserialize-header (encode-json-to-string *header1*))))


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

(defun make-message-from-parent (parent_msg msg_type metadata content) 
  (let ((hdr (message-header parent_msg)))
    (make-instance 
     'message
     :header (make-instance 
	      'header
	      :msg-id (format nil "~W" (uuid:make-v4-uuid))
	      :username (header-username hdr)
	      :session (header-session hdr)
	      :msg-type msg_type
	      :version (header-version hdr))
     :parent-header hdr
     :metadata metadata
     :content content)))


(example-progn
  (defparameter *msg1* (make-instance 'message :header *header1*)))


#|

## Wire-serialization ##

The wire-serialization of IPython kernel messages uses multi-parts ZMQ messages.

|#

;; strange issue with defconstant...
(defvar +WIRE-IDS-MSG-DELIMITER+ "<IDS|MSG>")

(defmethod wire-serialize ((msg message) &key (identities nil))
  (with-slots (header parent-header metadata content) msg
      (append identities
              (list +WIRE-IDS-MSG-DELIMITER+
                    "" ; TODO   HMAC signature
                    (encode-json-to-string header)
		    (if parent-header
			(encode-json-to-string parent-header)
			"{}")
		    (if metadata
			(encode-json-to-string metadata)
			"{}")
		    (if content
			(encode-json-to-string content)
			"{}")))))

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
 => '("{\"msg_id\": \"XXX-YYY-ZZZ-TTT\",\"username\": \"fredokun\",\"session\": \"AAA-BBB-CCC-DDD\",\"msg_type\": \"execute_request\",\"version\": \"4.1\"}"
      "{}" "{}" "{}"))


(defun wire-deserialize (parts)
  (let ((delim-index (position +WIRE-IDS-MSG-DELIMITER+ parts :test  #'equal)))
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
    ;;(format t "~%[Send] wire parts: ~W~%" wire-parts)
    (dolist (part wire-parts)
      (pzmq:send socket part :sndmore t))
    (pzmq:send socket nil)))

(defun zmq-recv-list (socket &optional (parts nil) (part-num 1))
  (multiple-value-bind (part more) (pzmq:recv-string socket)
    ;;(format t "[Shell]: received message part #~A: ~W (more? ~A)~%" part-num part more)
    (if more
        (zmq-recv-list socket (cons part parts) (+ part-num 1))
        (reverse (cons part parts)))))

(defun message-recv (socket)
  (let ((parts (zmq-recv-list socket)))
    ;;(format t "[Recv]: parts: ~A~%" (mapcar (lambda (part) (format nil "~W" part)) parts))
    (wire-deserialize parts)))

#|
     
## Message content ##

|#

(defclass message-content ()
  ()
  (:documentation "The base class of message contents."))

