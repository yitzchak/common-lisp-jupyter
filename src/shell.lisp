(in-package #:uncommonshell)

(defclass shell-channel ()
  ((ctx :initarg :ctx :reader shell-channel-ctx)
   (port :initarg :port :reader shell-channel-port)
   (socket :initform nil :accessor shell-channel-socket)))


(defparameter *ctx1* (pzmq:ctx-new))
                      

(defvar *shell1* (make-instance 'shell-channel :ctx *ctx1*))
