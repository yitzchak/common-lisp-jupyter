
(ql:quickload "cl-json")
(ql:quickload "babel")
(ql:quickload "uuid")
(ql:quickload "bordeaux-threads")
(ql:quickload "ironclad")

(defclass header ()
  ((msg-id :initarg :msg-id :reader header-msg-id :type string)
   (username :initarg :username :reader header-usename :type string)
   (session :initarg :session :reader header-session :type string)
   (msg-type :initarg :msg-type :reader header-msg-type :type string)
   (version :initarg :version :initform "5.0" :reader header-version :type string))
  (:documentation "Header representation for IPython messages"))

(defparameter *to-json-indent-increase* 2)

(defgeneric to-json (object &key indent first-indent)
  (:documentation "Conversion of OBJECT to JSON"))

(defun gen-indent (nb-chars)
  (make-string nb-chars :initial-element #\Space))

(gen-indent 10)

(defmethod to-json ((object t) &key (indent 0) (first-indent nil))
  (concatenate 'string
               (gen-indent (or first-indent indent)) (format nil "null")))


(defmethod to-json ((object header) &key (indent 0) (first-indent nil))
  (with-slots (msg-id username session msg-type version) object
    (concatenate 'string     
                 (gen-indent (or first-indent indent)) (format nil "{~%")
                 (gen-indent indent) (format nil "  \"msg_id\": ~W;~%" msg-id)
                 (gen-indent indent) (format nil "  \"username\": ~W;~%" username)
                 (gen-indent indent) (format nil "  \"session\": ~W;~%" session)
                 (gen-indent indent) (format nil "  \"msg_type\": ~W;~%" msg-type)
                 (gen-indent indent) (format nil "  \"version\": ~W;~%" version)
                 (gen-indent indent) (format nil "}"))))

(defvar *header1* (make-instance 'header
                                 :msg-id (format nil "~A" (uuid:make-v4-uuid))
                                 :username "fredokun"
                                 :session (format nil "~A" (uuid:make-v4-uuid))
                                 :msg-type "execute_request"))

(princ (to-json *header1* :indent 4))


(babel:string-to-octets
 (to-json *header1*) :encoding :utf-8)

(defclass message ()
  ((header :initarg :header :accessor message-header :type header)
   (parent-header :initarg :parent-header :initform nil :accessor message-parent-header :type header)
   (metadata :initarg :metadata :initform nil :accessor message-metadata)
   (content :initarg :content :initform nil :accessor message-content))
  (:documentation "Representation of IPython messages"))


(defmethod to-json ((object message) &key (indent 0) (first-indent nil))
  (with-slots (header parent-header metadata content) object
    (concatenate
     'string
     (gen-indent (or first-indent indent)) (format nil "{~%")
     (gen-indent indent) (format nil "  \"header\": ~A~%"
                                 (to-json header :indent (+ indent *to-json-indent-increase*) :first-indent 0))
     (gen-indent indent) (format nil "  \"parent_header\": ~A~%"
                                 (to-json parent-header :indent (+ indent *to-json-indent-increase*)))
     (gen-indent indent)(format nil "  \"metadata\": ~A~%"
                                 (to-json metadata :indent (+ indent *to-json-indent-increase*)))
     (gen-indent indent)(format nil "  \"content\": ~A~%"
                                (to-json content :indent (+ indent *to-json-indent-increase*)))
     (gen-indent indent) (format nil "}"))))


(defvar *msg1* (make-instance 'message :header *header1*))

(princ (to-json *msg1*))
