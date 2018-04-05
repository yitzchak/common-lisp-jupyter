(in-package #:cl-jupyter)

#|

# The stdin dealer socket #

See: http://jupyter-client.readthedocs.org/en/latest/messaging.html#messages-on-the-stdin-router-dealer-sockets

|#

(defclass stdin-channel ()
  ((kernel :initarg :kernel
           :reader stdin-kernel)
   (socket :initarg :socket
           :initform nil
           :accessor stdin-socket)))

(defun make-stdin-channel (kernel)
  (let* ((socket (pzmq:socket (kernel-ctx kernel) :dealer))
         (stdin (make-instance 'stdin-channel
                               :kernel kernel
                               :socket socket))
         (config (slot-value kernel 'config))
         (endpoint (format nil "~A://~A:~A"
                               (config-transport config)
                               (config-ip config)
                               (config-stdin-port config))))
    (info "[stdin] endpoint is: ~A~%" endpoint)
    (pzmq:bind socket endpoint)
    (setf (slot-value kernel 'stdin) stdin)
    stdin))

#|

### Message type: input_request ###

|#

(defclass content-input-request (message-content)
  ((prompt :initarg :prompt :type string)
   (password :initarg :password :type boolean)))

(defclass content-input-reply (message-content)
  ((value :initarg :value :type string)))

(defun handle-input-reply (stdin msg)
  (info "[stdin] handling 'input_reply'~%")

  ;; AT THIS POINT NEED TO HAND OFF VALUE TO ASKSIGN OR WHATEVER
  ;; CAUSED INPUT_REQUEST TO BE SENT !!
)

(defun send-input-request (stdin parent-msg prompt &key (key nil))
  (let ((message (make-message parent-msg "input_request"
                               (jsown:new-js
                                 ("prompt" prompt)))))
    (message-send (stdin-socket stdin) message :key key)))
