(in-package #:jupyter)

(defclass channel (source)
  ((mac
     :initarg :mac
     :reader channel-mac
     :documentation "Shared channel authentification handler.")
   (session
     :initarg :session
     :initform nil
     :accessor channel-session
     :documentation "Session identifier.")
   (socket
     :initarg :socket
     :reader channel-socket
     :documentation "Channel socket")
   (transport
     :initarg :transport
     :reader channel-transport
     :type string
     :documentation "Transport type")
   (ip
     :initarg :ip
     :reader channel-ip
     :type string
     :documentation "Channel IP address")
   (port
     :initarg :port
     :reader channel-port
     :type fixnum
     :documentation "Channel port number")
   (thread
     :accessor channel-thread))
  (:documentation "Common channel class."))

(defgeneric start (ch)
  (:documentation "Start the resource."))

(defmethod start ((ch channel))
  (with-slots (socket transport ip port) ch
    (let ((uri (format nil "~A://~A:~A" transport ip port)))
      (inform :info ch "Starting channel on ~A" uri)
      (nilmq:bind socket uri))))

(defgeneric stop (ch)
  (:documentation "Stop the resource."))

(defmethod stop :before ((ch channel))
  (when (slot-boundp ch 'thread)
    (inform :info ch "Stopping thread")
    (bordeaux-threads:destroy-thread (channel-thread ch))))

(defmethod stop ((ch channel))
  (inform :info ch "Stopping channel")
  (nilmq:shutdown (channel-socket ch)))

(defun message-available-p (ch)
  (nilmq:input-available-p (channel-socket ch)))
