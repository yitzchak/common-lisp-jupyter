(in-package #:jupyter)

#|

Jupyter protocol constants

|#

(defparameter +status-complete+ "complete")
(defparameter +status-incomplete+ "incomplete")
(defparameter +status-invalid+ "invalid")
(defparameter +status-unknown+ "unknown")


(defclass channel (source)
  ((key :initarg :key
        :reader channel-key)
   (socket :initarg :socket
           :reader channel-socket)
   (transport :initarg :transport
              :reader channel-transport
              :type string)
   (ip :initarg :ip
       :reader channel-ip
       :type string)
   (port :initarg :port
         :reader channel-port
         :type fixnum)
   (recv-lock :initform (bordeaux-threads:make-lock)
              :reader channel-recv-lock)
   (send-lock :initform (bordeaux-threads:make-lock)
              :reader channel-send-lock))
  (:documentation "Common channel class."))

(defgeneric start (ch)
  (:documentation "Start the resource."))

(defmethod start ((ch channel))
  (inform :info ch "Starting channel")
  (pzmq:bind (channel-socket ch)
             (format nil "~A://~A:~A"
                     (channel-transport ch)
                     (channel-ip ch)
                     (channel-port ch))))

(defgeneric stop (ch)
  (:documentation "Stop the resource."))

(defmethod stop ((ch channel))
  (inform :info ch "Stopping channel")
  (pzmq:close (channel-socket ch)))
