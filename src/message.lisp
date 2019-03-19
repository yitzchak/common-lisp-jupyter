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

(defun make-message (parent-msg msg-type content &optional metadata)
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
                   :metadata (or metadata (jsown:new-js)))))

(defun make-orphan-message (session-id msg-type identities content &optional metadata)
  (make-instance 'message
                 :header (jsown:new-js
                           ("msg_id" (make-uuid))
                           ("username" "kernel")
                           ("session" session-id)
                           ("msg_type" msg-type)
                           ("version" +KERNEL-PROTOCOL-VERSION+))
                 :identities identities
                 :content content
                 :metadata (or metadata (jsown:new-js))))

#|

## Wire-serialization ##

The wire-serialization of IPython kernel messages uses multi-parts ZMQ messages.

|#

(defun octets-to-hex-string (bytes)
  (apply #'concatenate (cons 'string (map 'list (lambda (x) (format nil "~(~2,'0X~)" x)) bytes))))

(defun message-signing (mac-args parts)
  (let ((mac (apply #'ironclad:make-mac mac-args)))
    ;; updates
    (iter
      (for part in parts)
      (ironclad:update-mac mac (babel:string-to-octets part)))
    ;; digest
    (octets-to-hex-string (ironclad:produce-mac mac))))

;; XXX: should be a defconstant but  strings are not EQL-able...
(defvar +WIRE-IDS-MSG-DELIMITER+ "<IDS|MSG>")

(defun wire-serialize (mac msg)
  (with-slots (header parent-header identities metadata content buffers) msg
    (let* ((header-json (jsown:to-json header))
           (parent-header-json (jsown:to-json parent-header))
           (metadata-json (jsown:to-json metadata))
           (content-json (jsown:to-json content))
           (sig (compute-signature mac (list header-json parent-header-json metadata-json content-json))))
      (append identities
        (list +WIRE-IDS-MSG-DELIMITER+
          sig
          header-json
          parent-header-json
          metadata-json
          content-json)
        buffers))))

#|

## Wire-deserialization ##

The wire-deserialization part follows.

|#

(defun wire-deserialize (mac parts)
  (let ((delim-index (position +WIRE-IDS-MSG-DELIMITER+ parts :test  #'equal)))
    (when (not delim-index)
      (error "[Wire] No <IDS|MSG> delimiter found in message parts"))
    (let* ((identities (subseq parts 0 delim-index))
           (sig (nth (1+ delim-index) parts))
           (buffers (subseq parts (+ 6 delim-index)))
           (parts (subseq parts (+ 2 delim-index) (+ 6 delim-index)))
           (expected-sig (compute-signature mac parts)))
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


#|

### Sending and receiving messages ###

|#

(defun message-send (channel msg)
  (with-slots (send-lock socket mac) channel
    (let ((wire-parts (wire-serialize mac msg)))
      (bordeaux-threads:with-lock-held (send-lock)
        ; (v:debug :message "Sending parts: ~W" wire-parts)
        (dolist (part wire-parts)
          (if (stringp part)
            (pzmq:send socket part :sndmore t)
            (let ((len (length part)))
              (cffi:with-foreign-pointer (m len)
                (dotimes (i len)
                  (setf (cffi:mem-aref m :unsigned-char i) (elt part i)))
                (pzmq:send socket m :len len :sndmore t)))))
        (pzmq:send socket nil)))))

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

(defun message-recv (channel)
  (with-slots (recv-lock socket mac) channel
    (bordeaux-threads:with-lock-held (recv-lock)
      (let ((parts (recv-parts socket)))
        ; (v:debug :message "Received parts: ~A" (mapcar (lambda (part) (format nil "~W" part)) parts))
        (wire-deserialize mac parts)))))
