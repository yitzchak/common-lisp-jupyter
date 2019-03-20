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

(defun make-orphan-message (session-id msg-type identities content &optional metadata buffers)
  (make-instance 'message
                 :header (jsown:new-js
                           ("msg_id" (make-uuid))
                           ("username" "kernel")
                           ("session" session-id)
                           ("msg_type" msg-type)
                           ("version" +KERNEL-PROTOCOL-VERSION+))
                 :identities identities
                 :content content
                 :metadata (or metadata (jsown:new-js))
                 :buffers buffers))

;; XXX: should be a defconstant but  strings are not EQL-able...
(defvar +IDS-MSG-DELIMITER+ "<IDS|MSG>")
(defvar +BODY-LENGTH+ 5)


#|

### Sending and receiving messages ###

|#

(defun send-parts (ch identities body buffers)
  (with-slots (send-lock socket) ch
    (bordeaux-threads:with-lock-held (send-lock)
      (iter
        (for part in identities)
        (for len next (length part))
        (if (stringp part)
          (pzmq:send socket part :sndmore t)
          (cffi:with-foreign-array (m part (list :array :uint8 len))
            (pzmq:send socket m :len len :sndmore t))))
      (pzmq:send socket +IDS-MSG-DELIMITER+ :sndmore t)
      (iter
        (for part in body)
        (pzmq:send socket part :sndmore t))
      (iter
        (for part in buffers)
        (for len next (length part))
        (cffi:with-foreign-array (m part (list :array :uint8 len))
          (pzmq:send socket m :len len :sndmore t)))
      (pzmq:send socket nil))))

(defun message-send (ch msg)
  (with-slots (mac) ch
    (with-slots (identities header parent-header metadata content buffers) msg
      (let ((tail (mapcar #'jsown:to-json (list header parent-header metadata content))))
        (send-parts ch identities
                    (cons (compute-signature mac tail) tail)
                    buffers)))))

(defun more-parts (socket msg)
  (declare (ignore socket))
  (not (zerop (pzmq::%msg-more msg))))

(defun read-array-part (socket msg)
  (pzmq:msg-recv msg socket)
  (cffi:foreign-array-to-lisp (pzmq:msg-data msg)
                              (list :array :uint8 (pzmq:msg-size msg))))

(defun read-string-part (socket msg)
  (pzmq:msg-recv msg socket)
  (let ((data (pzmq:msg-data msg))
        (len (pzmq:msg-size msg)))
    (handler-case
      (cffi:foreign-string-to-lisp data :count len :encoding :utf-8)
        (babel-encodings:character-decoding-error ()
          (cffi:foreign-array-to-lisp data (list :array :uint8 len))))))

(defun recv-parts (ch)
  (with-slots (recv-lock socket) ch
    (bordeaux-threads:with-lock-held (recv-lock)
      (pzmq:with-message msg
        (values
          ; Read the identities first
          (iter
            (for part next (read-string-part socket msg))
            (until (equal part +IDS-MSG-DELIMITER+))
            (collect part)
            (unless (more-parts socket msg)
              (inform :warn ch "No ~A delimiter found in message parts" +IDS-MSG-DELIMITER+)
              (finish)))
          ; Read the message body
          (iter
            (for i from 1 to +BODY-LENGTH+)
            (unless (more-parts socket msg)
              (inform :warn ch "Incomplete message body.")
              (finish))
            (collect (read-string-part socket msg)))
          ; The remaining parts should be binary buffers
          (iter
            (while (more-parts socket msg))
            (collect (read-array-part socket msg))))))))

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
