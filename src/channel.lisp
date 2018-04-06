(in-package #:cl-jupyter)

(defclass channel ()
  ((key :initarg :key
        :reader channel-key)
   (socket :initarg :socket
           :reader channel-socket)
   (transport :initarg :transport
              :reader channel-transport)
   (ip :initarg :ip
       :reader channel-ip)
   (port :initarg :port
         :reader channel-port)))

(defun make-channel (config socket port &key (class 'channel))
  (make-instance class
                 :key (config-key config)
                 :socket socket
                 :transport (config-transport config)
                 :ip (config-ip config)
                 :port port))

(defgeneric stop (ch)
  (:documentation "Start the resource."))

(defun start-channel (ch)
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
  (pzmq:close (channel-socket ch)))

(defmethod stop ((ch channel))
  (stop-channel ch))
