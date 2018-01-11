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
  \"version\": \"5.0\"
}")


(example
 (encode-json-to-string *header1*)
 => "{\"msg_id\": \"XXX-YYY-ZZZ-TTT\",\"username\": \"fredokun\",\"session\": \"AAA-BBB-CCC-DDD\",\"msg_type\": \"execute_request\",\"version\": \"5.0\"}")

#|

### JSon decoding ###

|#

(example (parse-json-from-string (encode-json-to-string *header1*))
         => '(("msg_id" . "XXX-YYY-ZZZ-TTT") ("username" . "fredokun")
	      ("session" . "AAA-BBB-CCC-DDD") ("msg_type" . "execute_request")
	      ("version" . "5.0")))

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

(defun make-message (parent_msg msg_type metadata content)
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

(defun make-orphan-message (session-id msg-type metadata content)
  (make-instance
   'message
   :header (make-instance
	    'header
	    :msg-id (format nil "~W" (uuid:make-v4-uuid))
	    :username "kernel"
	    :session session-id
	    :msg-type msg-type
	    :version +KERNEL-PROTOCOL-VERSION+)
   :parent-header '()
   :metadata metadata
   :content content))

(example-progn
 (defparameter *msg1* (make-instance 'message :header *header1*)))


#|

## Wire-serialization ##

The wire-serialization of IPython kernel messages uses multi-parts ZMQ messages.

|#

(defun octets-to-hex-string (bytes)
  (apply #'concatenate (cons 'string (map 'list (lambda (x) (format nil "~(~2,'0X~)" x)) bytes))))

(defun message-signing (key parts)
  (let ((hmac (ironclad:make-hmac key :SHA256)))
    ;; updates
    (loop for part in parts
       do (let ((part-bin (babel:string-to-octets part)))
            (ironclad:update-hmac hmac part-bin)))
    ;; digest
    (octets-to-hex-string (ironclad:hmac-digest hmac))))

(example
 (message-signing (babel:string-to-octets "toto") '("titi" "tata" "tutu" "tonton"))
 => "d32d091b5aabeb59b4291a8c5d70e0c20302a8bf9f642956b6affe5a16d9e134")

;; XXX: should be a defconstant but  strings are not EQL-able...
(defvar +WIRE-IDS-MSG-DELIMITER+ "<IDS|MSG>")

(defmethod wire-serialize ((msg message) &key (identities nil) (key nil))
  (with-slots (header parent-header metadata content) msg
    (let ((header-json (encode-json-to-string header))
          (parent-header-json (if parent-header
                                  (encode-json-to-string parent-header)
				"{}"))
          (metadata-json (if metadata
                             (encode-json-to-string metadata)
                             "{}"))
          (content-json (if content
                            (encode-json-to-string content)
                            "{}")))
      (let ((sig (if key
                     (message-signing key (list header-json parent-header-json metadata-json content-json))
                     "")))
        (append identities
                (list +WIRE-IDS-MSG-DELIMITER+
                      sig
                      header-json
                      parent-header-json
                      metadata-json
                      content-json))))))

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

;; Locking, courtesy of dmeister, thanks !
(defparameter *message-send-lock* (bordeaux-threads:make-lock "message-send-lock"))

(defun message-send (socket msg &key (identities nil) (key nil))
  (unwind-protect
       (progn
	 (bordeaux-threads:acquire-lock *message-send-lock*)
	 (let ((wire-parts (wire-serialize msg :identities identities :key key)))
	   ;;DEBUG>>
	   ;;(format t "~%[Send] wire parts: ~W~%" wire-parts)
	   (dolist (part wire-parts)
	     (pzmq:send socket part :sndmore t))
	   (pzmq:send socket nil)))
    (bordeaux-threads:release-lock *message-send-lock*)))

(defun recv-string (socket &key dontwait (encoding cffi:*default-foreign-encoding*))
  "Receive a message part from a socket as a string."
  (pzmq:with-message msg
    (pzmq:msg-recv msg socket :dontwait dontwait)
    (values
     (handler-case
         (cffi:foreign-string-to-lisp (pzmq:msg-data msg) :count (pzmq:msg-size msg) :encoding encoding)
       (BABEL-ENCODINGS:INVALID-UTF8-STARTER-BYTE
           ()
         ;; if it's not utf-8 we try latin-1 (Ugly !)
         (format t "[Recv]: issue with UTF-8 decoding~%")
         (cffi:foreign-string-to-lisp (pzmq:msg-data msg) :count (pzmq:msg-size msg) :encoding :latin-1)))
     (pzmq:getsockopt socket :rcvmore))))

(defun zmq-recv-list (socket &optional (parts nil) (part-num 1))
  (multiple-value-bind (part more)
      (handler-case (pzmq:recv-string socket)
		    (BABEL-ENCODINGS:INVALID-UTF8-STARTER-BYTE
		     ()
		     ;; if it's not utf-8 we try latin-1 (Ugly !)
		     (format t "[Recv]: issue with UTF-8 decoding~%")
		     (pzmq:recv-string socket :encoding :latin-1)))
    ;;(format t "[Shell]: received message part #~A: ~W (more? ~A)~%" part-num part more)
    (if more
        (zmq-recv-list socket (cons part parts) (+ part-num 1))
        (reverse (cons part parts)))))

(defparameter *message-recv-lock* (bordeaux-threads:make-lock "message-recv-lock"))

(defun message-recv (socket)
  (unwind-protect
       (progn
	 (bordeaux-threads:acquire-lock *message-recv-lock*)
	 (let ((parts (zmq-recv-list socket)))
	   ;;DEBUG>>
	   (format t "[Recv]: parts: ~A~%" (mapcar (lambda (part) (format nil "~W" part)) parts))
	   (wire-deserialize parts)))
    (bordeaux-threads:release-lock *message-recv-lock*)))
