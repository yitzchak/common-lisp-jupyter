(in-package #:jupyter)

#|

# Representation and manipulation of kernel messages #

|#

#|

## IPython messages ##

|#

(defclass message ()
  ((header
     :initarg :header
     :initform (json-empty-obj)
     :accessor message-header)
   (parent-header
     :initarg :parent-header
     :initform (json-empty-obj)
     :accessor message-parent-header)
   (identities
     :initarg :identities
     :initform (list (make-uuid t))
     :accessor message-identities)
   (metadata
     :initarg :metadata
     :initform (json-empty-obj)
     :accessor message-metadata)
   (content
     :initarg :content
     :initform (json-empty-obj)
     :accessor message-content)
   (buffers
     :initarg :buffers
     :initform nil
     :accessor message-buffers))
  (:documentation "Representation of IPython messages"))

(defun date-now ()
  (multiple-value-bind (s m h dt mth yr day)
        (get-decoded-time)
        (declare (ignore day))
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ" yr mth dt h m s)))

(defun make-message (session-id msg-type content &key metadata buffers parent)
  (if parent
    (let ((hdr (message-header parent))
          (identities (message-identities parent)))
      (make-instance 'message
                     :header (json-new-obj
                               ("msg_id" (make-uuid))
                               ("username" (json-getf hdr "username"))
                               ("session" session-id)
                               ("msg_type" msg-type)
                               ("date" (date-now))
                               ("version" +KERNEL-PROTOCOL-VERSION+))
                     :parent-header hdr
                     :identities identities
                     :content content
                     :metadata (or metadata (json-empty-obj))
                     :buffers buffers))
    (make-instance 'message
                   :header (json-new-obj
                             ("msg_id" (make-uuid))
                             ("username" "kernel")
                             ("session" session-id)
                             ("msg_type" msg-type)
                             ("date" (date-now))
                             ("version" +KERNEL-PROTOCOL-VERSION+))
                   :content content
                   :metadata (or metadata (json-empty-obj))
                   :buffers buffers)))

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
    (cond
      ((typep part '(array single-float *))
        (cffi:with-foreign-array (m part (list :array :float len))
          (pzmq:send (channel-socket ch) m :len (* 4 len) :sndmore t)))
      (t
        (cffi:with-foreign-array (m part (list :array :uint8 len))
          (pzmq:send (channel-socket ch) m :len len :sndmore t))))))

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
                            (list :array :uint8 (pzmq:msg-size msg))
                            ; explicitly defined element type is needed for CLISP
                            :element-type '(unsigned-byte 8)))

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
    (inform :info ch "~S" body)
    (unless (equal (car body) (compute-signature (channel-mac ch) (cdr body)))
      (inform :warn ch "Signature mismatch on received message."))
    (destructuring-bind (header parent-header metadata content) (mapcar #'jsown:parse (cdr body))
      (make-instance 'message :identities identities
                              :header header
                              :parent-header parent-header
                              :metadata metadata
                              :content content
                              :buffers buffers))))
