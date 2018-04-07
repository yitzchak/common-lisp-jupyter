(in-package #:cl-jupyter)

#|

# Representation and manipulation of kernel messages #

|#

#|

## Message header ##

|#

(example-progn
 (defparameter *header1* (jsown:new-js
                           ("msg_id" "XXX-YYY-ZZZ-TTT")
                           ("username" "fredokun")
                           ("session" "AAA-BBB-CCC-DDD")
                           ("msg_type" "execute_request")
                           ("version" "5.0"))))

(example
 (jsown:to-json *header1*)
 => "{\"msg_id\":\"XXX-YYY-ZZZ-TTT\",\"username\":\"fredokun\",\"session\":\"AAA-BBB-CCC-DDD\",\"msg_type\":\"execute_request\",\"version\":\"5.0\"}")

#|

### JSon decoding ###

|#

(example (jsown:parse (jsown:to-json *header1*))
         => '(:obj
              ("msg_id" . "XXX-YYY-ZZZ-TTT")
              ("username" . "fredokun")
              ("session" . "AAA-BBB-CCC-DDD")
              ("msg_type" . "execute_request")
              ("version" . "5.0")))

(example
 (jsown:val (jsown:parse (jsown:to-json *header1*)) "msg_id")
 => "XXX-YYY-ZZZ-TTT")

(example
 (jsown:val (jsown:parse (jsown:to-json *header1*)) "username")
 => "fredokun")

#|

## IPython messages ##

|#

(defclass message ()
  ((header :initarg :header
           :initform (jsown:new-js)
           :accessor message-header)
   (parent-header :initarg :parent-header
                  :initform (jsown:new-js)
                  :accessor message-parent-header)
   (identities :initarg :identities
               :initform nil
               :accessor message-identities)
   (metadata :initarg :metadata
             :initform (jsown:new-js)
             :accessor message-metadata)
   (content :initarg :content
            :initform (jsown:new-js)
            :accessor message-content)
   (buffers :initarg :buffers
            :initform nil
            :accessor message-buffers))
  (:documentation "Representation of IPython messages"))

(defun make-message (parent-msg msg-type content)
  (let ((hdr (message-header parent-msg))
        (identities (message-identities parent-msg)))
    (make-instance 'message
                   :header (jsown:new-js
                             ("msg_id" (format nil "~W" (uuid:make-v4-uuid)))
                             ("username" (jsown:val hdr "username"))
                             ("session" (jsown:val hdr "session"))
                             ("msg_type" msg-type)
                             ("version" +KERNEL-PROTOCOL-VERSION+))
                   :parent-header hdr
                   :identities identities
                   :content content)))

(defun make-orphan-message (session-id msg-type identities content)
  (make-instance 'message
                 :header (jsown:new-js
                           ("msg_id" (format nil "~W" (uuid:make-v4-uuid)))
                           ("username" "kernel")
                           ("session" session-id)
                           ("msg_type" msg-type)
                           ("version" +KERNEL-PROTOCOL-VERSION+))
                 :identities identities
                 :content content))

(example-progn
  (defparameter *msg1* (make-instance 'message
                                      :header *header1*
                                      :identities '("XXX-YYY-ZZZ-TTT" "AAA-BBB-CCC-DDD"))))


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

(defmethod wire-serialize ((msg message) &key (key nil))
  (with-slots (header parent-header identities metadata content buffers) msg
    (let* ((header-json (jsown:to-json header))
           (parent-header-json (jsown:to-json parent-header))
           (metadata-json (jsown:to-json metadata))
           (content-json (jsown:to-json content))
           (sig (if key
                    (message-signing key (list header-json parent-header-json metadata-json content-json))
                    "")))
      (append identities
        (list +WIRE-IDS-MSG-DELIMITER+
          sig
          header-json
          parent-header-json
          metadata-json
          content-json)
        buffers))))

(example-progn
 (defparameter *wire1* (wire-serialize *msg1*)))


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
 => '("{\"msg_id\":\"XXX-YYY-ZZZ-TTT\",\"username\":\"fredokun\",\"session\":\"AAA-BBB-CCC-DDD\",\"msg_type\":\"execute_request\",\"version\":\"5.0\"}"
      "{}" "{}" "{}"))


(defun wire-deserialize (parts &key (key nil))
  (let ((delim-index (position +WIRE-IDS-MSG-DELIMITER+ parts :test  #'equal)))
    (when (not delim-index)
      (error "[Wire] No <IDS|MSG> delimiter found in message parts"))
    (let* ((identities (subseq parts 0 delim-index))
           (sig (nth (1+ delim-index) parts))
           (buffers (subseq parts (+ 6 delim-index)))
           (parts (subseq parts (+ 2 delim-index) (+ 6 delim-index)))
           (expected-sig (if key (message-signing key parts) "")))
      (unless (equal sig expected-sig)
        (error "[Wire] Signature mismatch in message"))
      (destructuring-bind (header parent-header metadata content) parts
        (make-instance 'message
                       :header (jsown:parse header)
                       :parent-header (jsown:parse parent-header)
                       :identities identities
                       :metadata (jsown:parse metadata)
                       :content (jsown:parse content)
                       :buffers buffers)))))


(example-progn
 (defparameter *dewire-1* (wire-deserialize *wire1*)))

(example
 (jsown:val (message-header *dewire-1*) "username")
 => "fredokun")

#|

### Sending and receiving messages ###

|#

;; Locking, courtesy of dmeister, thanks !
(defparameter *message-send-lock* (bordeaux-threads:make-lock "message-send-lock"))

(defun message-send (channel msg)
  (unwind-protect
    (let* ((socket (channel-socket channel))
           (key (channel-key channel))
           (wire-parts (wire-serialize msg :key key)))
      (bordeaux-threads:acquire-lock *message-send-lock*)
      ;;DEBUG>>
      ;;(info "~%[Send] wire parts: ~W~%" wire-parts)
      (dolist (part wire-parts)
        (if (stringp part)
          (pzmq:send socket part :sndmore t)
          (let ((len (length part)))
            (cffi:with-foreign-pointer (m len)
              (dotimes (i len)
                (setf (cffi:mem-aref m :unsigned-char i) (elt part i)))
              (pzmq:send socket m :len len :sndmore t)))))
      (pzmq:send socket nil))
    (bordeaux-threads:release-lock *message-send-lock*)))

(defun recv-parts (socket)
  (pzmq:with-message msg
    (iter
      (pzmq:msg-recv msg socket)
      (for data = (pzmq:msg-data msg))
      (for len = (pzmq:msg-size msg))
      (collect
        (handler-case
          (cffi:foreign-string-to-lisp data :count len :encoding :utf-8)
          (babel-encodings:character-decoding-error ()
            (let ((res (make-array len :element-type 'unsigned-byte)))
              (dotimes (i len res)
                (setf (aref res i) (cffi:mem-aref data :unsigned-char i)))))))
      (while (pzmq:getsockopt socket :rcvmore)))))

(defparameter *message-recv-lock* (bordeaux-threads:make-lock "message-recv-lock"))

(defun message-recv (channel)
  (unwind-protect
    (let ((socket (channel-socket channel))
          (key (channel-key channel)))
      (bordeaux-threads:acquire-lock *message-recv-lock*)
      (let ((parts (recv-parts socket)))
        ;;DEBUG>>
        ;;(info "[Recv]: parts: ~A~%" (mapcar (lambda (part) (format nil "~W" part)) parts))
        (wire-deserialize parts :key key)))
    (bordeaux-threads:release-lock *message-recv-lock*)))
