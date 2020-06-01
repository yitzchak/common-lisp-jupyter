(in-package #:jupyter)

(defclass channel (source)
  ((mac
     :initarg :mac
     :reader channel-mac
     :documentation "Shared channel authentification handler.")
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
   (recv-lock
     :initform (bordeaux-threads:make-lock)
     :reader channel-recv-lock
     :documentation "Lock used during recv actions")
   (send-lock
     :initform (bordeaux-threads:make-lock)
     :reader channel-send-lock
     :documentation "Lock used during send actions")
   (thread
     :accessor channel-thread))
  (:documentation "Common channel class."))

(defgeneric start (ch)
  (:documentation "Start the resource."))

(defmethod start ((ch channel))
  (with-slots (socket transport ip port) ch
    (let ((uri (format nil "~A://~A:~A" transport ip port)))
      (inform :info ch "Starting channel on ~A" uri)
      (pzmq:bind socket uri))))

(defgeneric stop (ch)
  (:documentation "Stop the resource."))

(defmethod stop :before ((ch channel))
  (when (slot-boundp ch 'thread)
    (inform :info ch "Stopping thread")
    (bordeaux-threads:destroy-thread (channel-thread ch))))

(defmethod stop ((ch channel))
  (inform :info ch "Stopping channel")
  (pzmq:close (channel-socket ch)))


(defclass request-channel (channel)
  ((request-queue
     :initarg :request-queue
     :accessor channel-request-queue))
  (:documentation "ROUTER channel used for requests."))

