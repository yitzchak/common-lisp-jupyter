
(in-package #:uncommonshell)

(defclass header ()
  ((msg-id :initarg :msg-id :reader header-msg-id :type string)
   (username :initarg :username :reader header-usename :type string)
   (session :initarg :session :reader header-session :type string)
   (msg-type :initarg :msg-type :reader header-msg-type :type string)
   (version :initarg :version :initform "5.0" :reader header-version :type string))
  (:documentation "Header representation for IPython messages"))

(defgeneric to-json (object &key indent first-indent newlines)
  (:documentation "Conversion of OBJECT to JSON"))

(defun gen-indent (nb-chars)
  (if nb-chars
      (make-string nb-chars :initial-element #\Space)
      ""))

(gen-indent 10)

(defmacro to-json-line (indent newlines &rest fmt)
  `(concatenate 'string
                (if ,indent
                    (gen-indent ,indent)
                    "")
                (format nil ,@fmt) (if ,newlines (format nil "~%") "")))

(let ((toto '(me toto)))
  (to-json-line 4 t "blabla ~A" toto))

(defmethod to-json ((object t) &key (indent nil) (first-indent nil) (newlines nil))
  (declare (ignore indent))
  (to-json-line first-indent newlines "{}"))

(to-json "help" :first-indent 4 :newlines nil)

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

(defparameter *header1* (make-instance 'header
                                 :msg-id (format nil "~A" (uuid:make-v4-uuid))
                                 :username "fredokun"
                                 :session (format nil "~A" (uuid:make-v4-uuid))
                                 :msg-type "execute_request"))

(princ (to-json *header1* :first-indent 0 :indent 2 :newlines t))
(princ (to-json *header1* :first-indent 2 :indent 4 :newlines t))

(princ (to-json *header1*))

(json:decode-json-from-string (to-json *header1*))

(defun fetch-json-component (comp alist)
  (let ((binding (assoc comp alist)))
    (if binding
        (cdr binding)
        (error "Deserialize error: no such component: ~A" comp))))

(fetch-json-component :msg--id (json:decode-json-from-string (to-json *header1*)))
(fetch-json-component :username (json:decode-json-from-string (to-json *header1*)))
(fetch-json-component :session (json:decode-json-from-string (to-json *header1*)))
(fetch-json-component :msg--type (json:decode-json-from-string (to-json *header1*)))

(defun wire-deserialize-header (hdr)
  (let ((json-list (json:decode-json-from-string hdr)))
    (if json-list
        (make-instance 'header
                       :msg-id (fetch-json-component :msg--id json-list)
                       :username (fetch-json-component :username json-list)
                       :session (fetch-json-component :session json-list)
                       :msg-type (fetch-json-component :msg--type json-list))
        nil)))

(defparameter *header2* (wire-deserialize-header (to-json *header1*)))

(inspect *header2*)

(defclass message ()
  ((header :initarg :header :accessor message-header :type header)
   (parent-header :initarg :parent-header :initform nil :accessor message-parent-header :type header)
   (metadata :initarg :metadata :initform nil :accessor message-metadata)
   (content :initarg :content :initform nil :accessor message-content))
  (:documentation "Representation of IPython messages"))


(defparameter *msg1* (make-instance 'message :header *header1*))

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

(defparameter *wire1* (wire-serialize *msg1* :identities `(,(format nil "~A" (uuid:make-v4-uuid)) ,(format nil "~A" (uuid:make-v4-uuid)))))

*wire1*

(position +WIRE-IDS-MSG-DELIMITER+ *wire1*)

(nth (position +WIRE-IDS-MSG-DELIMITER+ *wire1*) *wire1*)

(subseq *wire1* 0 (position +WIRE-IDS-MSG-DELIMITER+ *wire1*))

(subseq *wire1* (+ 6 (position +WIRE-IDS-MSG-DELIMITER+ *wire1*)))

(let ((delim-index (position +WIRE-IDS-MSG-DELIMITER+ *wire1*)))
  (subseq *wire1* (+ 2 delim-index) (+ 6 delim-index)))

(let ((delim-index (position +WIRE-IDS-MSG-DELIMITER+ *wire1*)))
  (destructuring-bind (header parent-header metadata content)
      (subseq *wire1* (+ 2 delim-index) (+ 6 delim-index))
    (format t "header = ~A~%" header)
    (format t "parent-header = ~A~%" parent-header)
    (format t "metadata = ~A~%" metadata)
    (format t "content = ~A~%" content)))
  
    
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

                       
(setq *dewire-1* (multiple-value-bind (ids sig msg raw)
                     (wire-deserialize *wire1*)
                   (list ids sig msg raw)))

*dewire-1*

(inspect (third *dewire-1*))
          
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


     
