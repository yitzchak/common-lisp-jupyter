(in-package #:jupyter)

#|

# Representation and manipulation of kernel messages #

|#

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
               :initform (list (uuid:uuid-to-byte-array (uuid:make-v4-uuid)))
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

(defun make-message (parent-msg msg-type content &optional metadata buffers)
  (let ((hdr (message-header parent-msg))
        (identities (message-identities parent-msg)))
    (make-instance 'message
                   :header (jsown:new-js
                             ("msg_id" (make-uuid))
                             ("username" (json-getf hdr "username"))
                             ("session" (json-getf hdr "session"))
                             ("msg_type" msg-type)
                             ("version" +KERNEL-PROTOCOL-VERSION+))
                   :parent-header hdr
                   :identities identities
                   :content content
                   :metadata (or metadata (jsown:new-js))
                   :buffers buffers)))

(defun make-orphan-message (session-id msg-type content &optional metadata buffers)
  (make-instance 'message
                 :header (jsown:new-js
                           ("msg_id" (make-uuid))
                           ("username" "kernel")
                           ("session" session-id)
                           ("msg_type" msg-type)
                           ("version" +KERNEL-PROTOCOL-VERSION+))
                 :content content
                 :metadata (or metadata (jsown:new-js))
                 :buffers buffers))

;; XXX: should be a defconstant but  strings are not EQL-able...
(defvar +IDS-MSG-DELIMITER+ (babel:string-to-octets "<IDS|MSG>"))
(defvar +BODY-LENGTH+ 5)


#|

### Sending and receiving messages ###

|#

(defun send-string-part (ch part)
  (pzmq:send (channel-socket ch) part :sndmore t))

(defun send-binary-part (ch part)
  (let ((len (length part)))
    (cffi:with-foreign-array (m part (list :array :uint8 len))
      (pzmq:send (channel-socket ch) m :len len :sndmore t))))

(defun send-parts (ch identities body buffers)
  (with-slots (send-lock socket) ch
    (bordeaux-threads:with-lock-held (send-lock)
      (iter
        (for part in identities)
        (send-binary-part ch part))
      (send-binary-part ch +IDS-MSG-DELIMITER+)
      (iter
        (for part in body)
        (send-string-part ch part))
      (iter
        (for part in buffers)
        (send-binary-part ch part))
      (pzmq:send socket nil))))

(defun message-send (ch msg)
  (with-slots (mac) ch
    (with-slots (identities header parent-header metadata content buffers) msg
      (let ((tail (mapcar #'jsown:to-json (list header parent-header metadata content))))
        (send-parts ch identities
                    (cons (compute-signature mac tail) tail)
                    buffers)))))

(defun more-parts (ch msg)
  (declare (ignore ch))
  (not (zerop (pzmq::%msg-more msg))))

(defun read-binary-part (ch msg)
  (pzmq:msg-recv msg (channel-socket ch))
  (cffi:foreign-array-to-lisp (pzmq:msg-data msg)
                              (list :array :uint8 (pzmq:msg-size msg))))

(defun read-string-part (ch msg)
  (pzmq:msg-recv msg (channel-socket ch))
  (handler-case
    (cffi:foreign-string-to-lisp (pzmq:msg-data msg)
                                 :count (pzmq:msg-size msg)
                                 :encoding :utf-8)
    (babel-encodings:character-decoding-error ()
      (inform :warn ch "Unable to decode message part.")
      "")))

(defun recv-parts (ch)
  (with-slots (recv-lock) ch
    (bordeaux-threads:with-lock-held (recv-lock)
      (pzmq:with-message msg
        (values
          ; Read the identities first
          (iter
            (for part next (read-binary-part ch msg))
            (until (equalp part +IDS-MSG-DELIMITER+))
            (collect part)
            (unless (more-parts ch msg)
              (inform :warn ch "No identities/message delimiter found in message parts")
              (finish)))
          ; Read the message body
          (iter
            (for i from 1 to +BODY-LENGTH+)
            (unless (more-parts ch msg)
              (inform :warn ch "Incomplete message body.")
              (finish))
            (collect (read-string-part ch msg)))
          ; The remaining parts should be binary buffers
          (iter
            (while (more-parts ch msg))
            (collect (read-binary-part ch msg))))))))

(defun message-recv (ch)
  (multiple-value-bind (identities body buffers) (recv-parts ch)
    (unless (equal (car body) (compute-signature (channel-mac ch) (cdr body)))
      (inform :warn ch "Signature mismatch on received message."))
    (destructuring-bind (header parent-header metadata content) (mapcar #'jsown:parse (cdr body))
      (make-instance 'message :identities identities
                              :header header
                              :parent-header parent-header
                              :metadata metadata
                              :content content
                              :buffers buffers))))
