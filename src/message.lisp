(in-package #:jupyter)

#|

# Representation and manipulation of kernel messages #

|#

#|

## IPython messages ##

|#

(defvar *message* nil)
(defvar *suspended-message* nil)

(defclass message ()
  ((header
     :initarg :header
     :initform (make-hash-table :test #'equal)
     :accessor message-header)
   (parent-header
     :initarg :parent-header
     :initform (make-hash-table :test #'equal)
     :accessor message-parent-header)
   (identities
     :initarg :identities
     :initform (list (make-uuid t))
     :accessor message-identities)
   (metadata
     :initarg :metadata
     :initform (make-hash-table :test #'equal)
     :accessor message-metadata)
   (content
     :initarg :content
     :initform (make-hash-table :test #'equal)
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

(defun make-message (session-id msg-type content &key metadata buffers (parent *message*) topic)
  (if parent
    (let ((hdr (message-header parent))
          (identities (if topic (list topic) (message-identities parent))))
      (make-instance 'message
                     :header `(:object-alist
                                ("msg_id" . ,(make-uuid))
                                ("username" . ,(gethash "username" hdr))
                                ("session" . ,session-id)
                                ("msg_type" . ,msg-type)
                                ("date" . ,(date-now))
                                ("version" . ,+KERNEL-PROTOCOL-VERSION+))
                     :parent-header hdr
                     :identities identities
                     :content content
                     :metadata (or metadata :empty-object)
                     :buffers buffers))
    (make-instance 'message
                   :header `(:object-alist
                              ("msg_id" . ,(make-uuid))
                              ("username" . "")
                              ("session" . ,session-id)
                              ("msg_type" . ,msg-type)
                              ("date" . ,(date-now))
                              ("version" . ,+KERNEL-PROTOCOL-VERSION+))
                   :content content
                   :metadata (or metadata :empty-object)
                   :buffers buffers)))

;; XXX: should be a defconstant but  strings are not EQL-able...
(defvar +IDS-MSG-DELIMITER+ (babel:string-to-octets "<IDS|MSG>"))
(defvar +BODY-LENGTH+ 5)

#|

### Sending and receiving messages ###

|#

(defun message-send (ch msg)
  (with-slots (mac) ch
    (with-slots (identities header parent-header metadata content buffers) msg
      (let* ((*print-pretty* nil)
             (*read-default-float-format* 'double-float)
             (tail (mapcar (lambda (value)
                             (babel:string-to-octets (shasht:write-json value nil)))
                           (list header parent-header metadata content))))
        (j:inform :info ch "send ~s"
                  (append identities
                          (list +IDS-MSG-DELIMITER+
                                (compute-signature mac tail))
                          tail
                          buffers))
        (nilmq:send (channel-socket ch)
                    (append identities
                            (list +IDS-MSG-DELIMITER+
                                  (compute-signature mac tail))
                            tail
                            buffers))))))

(defun message-recv (ch
                     &aux (m (nilmq:receive (channel-socket ch))))
  (j:inform :info ch "recv ~s" m)
  (loop with *read-default-float-format* = 'double-float
        for (part signature header parent-header metadata content . buffers) on m
        if (equalp part +IDS-MSG-DELIMITER+)
          do  (unless (equal (babel:octets-to-string signature)
                             (compute-signature (channel-mac ch)
                                                (list header parent-header metadata content)))
                (inform :warn ch "Signature mismatch on received message."))
              (return (make-instance 'message
                                     :identities identities
                                     :header (shasht:read-json (babel:octets-to-string header))
                                     :parent-header (shasht:read-json (babel:octets-to-string parent-header))
                                     :metadata (shasht:read-json (babel:octets-to-string metadata))
                                     :content (shasht:read-json (babel:octets-to-string content))
                                     :buffers buffers))
        else
          collect part into identities))
