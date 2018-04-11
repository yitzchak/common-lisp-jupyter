(in-package #:maxima-jupyter)

#|

Jupyter protocol constants

|#

(defparameter +status-complete+ "complete")
(defparameter +status-incomplete+ "incomplete")
(defparameter +status-invalid+ "invalid")
(defparameter +status-unknown+ "unknown")


(defclass channel ()
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
         :type fixnum))
  (:documentation "Common channel class."))

(defun make-channel (class config socket port)
  (make-instance class
                 :key (config-key config)
                 :socket socket
                 :transport (config-transport config)
                 :ip (config-ip config)
                 :port port))

(defgeneric stop (ch)
  (:documentation "Start the resource."))

(defun start-channel (ch)
  (info "[~(~A~)] Starting...~%" (class-name (class-of ch)))
  (pzmq:bind (channel-socket ch)
             (format nil "~A://~A:~A"
                         (channel-transport ch)
                         (channel-ip ch)
                         (channel-port ch))))

(defmethod start ((ch channel))
  (start-channel ch))

(defgeneric stop (ch)
  (:documentation "Stop the resource."))

(defmethod stop-channel ((ch channel))
  (info "[~(~A~)] Stopped.~%" (class-name (class-of ch)))
  (pzmq:close (channel-socket ch)))

(defmethod stop ((ch channel))
  (stop-channel ch))
