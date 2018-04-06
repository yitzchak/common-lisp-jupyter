(in-package #:cl-jupyter)

(defclass channel ()
  ((key :initarg :key
        :reader channel-key)
   (socket :initarg :socket
           :reader channel-socket)))

(defun make-channel (config socket port &key (class 'channel))
  (let* ((key (config-key config))
         (channel (make-instance class
                                 :key key
                                 :socket socket))
         (endpoint (format nil "~A://~A:~A"
                               (config-transport config)
                               (config-ip config)
                               port)))
    (pzmq:bind socket endpoint)
    channel))

(defgeneric stop (ch)
  (:documentation "Stop the channel."))

(defmethod stop ((ch channel))
  (pzmq:close (channel-socket ch)))
