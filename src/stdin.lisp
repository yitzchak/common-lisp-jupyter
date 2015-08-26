(in-package #:cl-jupyter)

#|

# The stdin dealer socket #

See: http://jupyter-client.readthedocs.org/en/latest/messaging.html#messages-on-the-stdin-router-dealer-sockets

|#

(defclass stdin-channel ()
  ((kernel :initarg :kernel :reader stdin-kernel)
   (socket :initarg :socket :initform nil :accessor stdin-socket)))

(defun make-stdin-channel (kernel)
  (let ((socket (pzmq:socket (kernel-ctx kernel) :dealer)))
    (let ((stdin (make-instance 'stdin-channel
                                :kernel kernel
                                :socket socket)))
      (let ((config (slot-value kernel 'config)))
        (let ((endpoint (format nil "~A://~A:~A"
                                  (config-transport config)
                                  (config-ip config)
                                  (config-stdin-port config))))
          (format t "stdin endpoint is: ~A~%" endpoint)
          (pzmq:bind socket endpoint)
          (setf (slot-value kernel 'stdin) stdin)
          stdin)))))

#|

### Message type: input_request ###

|#

(defclass content-input-request (message-content)
  ((prompt :initarg :prompt :type string)
   (password :initarg :password :type boolean)))

(defmethod encode-json (stream (object content-input-request) &key (indent nil) (first-line nil))
  (with-slots (prompt password) object
    (encode-json stream `(("prompt" . ,prompt)
                          ("password" . ,password))
                 :indent indent :first-line first-line)))

(defclass content-input-reply (message-content)
  ((value :initarg :value :type string)))

(defmethod encode-json (stream (object content-input-reply) &key (indent nil) (first-line nil))
  (with-slots (value) object
    (encode-json stream `(("value" . ,value))
                 :indent indent :first-line first-line)))

(defun handle-input-reply (stdin identities msg buffers)
  (format t "[stdin] handling 'input_reply'~%")

  ;; AT THIS POINT NEED TO HAND OFF VALUE TO ASKSIGN OR WHATEVER
  ;; CAUSED INPUT_REQUEST TO BE SENT !!
)

(defun send-input-request (stdin parent-msg prompt)
  (let ((message (make-message parent-msg "input_request" nil `(("prompt" . ,prompt)))))
    (message-send (stdin-socket stdin) message :identities '("input_request"))))

;; Redefine RETRIEVE in src/macsys.lisp to make use of input-request/input-reply.
;; MSG, FLAG, and PRINT? are declared special there, so be careful to
;; refer to those symbols in the :maxima package.

(defun maxima::retrieve (maxima::msg maxima::flag &aux (maxima::print? nil))
  (declare (special maxima::msg maxima::flag maxima::print?))
  (or (eq maxima::flag 'maxima::noprint) (setq maxima::print? t))
  (let
    ((retrieve-prompt
       (cond
         ((not maxima::print?)
          (setq maxima::print? t)
          (format nil ""))
         ((null maxima::msg)
          (format nil ""))
         ((atom maxima::msg)
          (format nil "~A" maxima::msg))
         ((eq maxima::flag t)
          (format nil "~{~A~}" (cdr maxima::msg)))
         (t
          (maxima::aformat nil "~M" maxima::msg)))))
    (let ((kernel (shell-kernel *execute-request-shell*)))
      (let ((stdin (kernel-stdin kernel)))
        (send-input-request stdin *execute-request-msg* retrieve-prompt)
        (multiple-value-bind (identities signature message buffers) (message-recv (stdin-socket stdin))
	      (format *error-output* "CL-JUPYTER::RETRIEVE: stdin received:~%")
	      (format *error-output* "  | identities: ~A~%" identities)
	      (format *error-output* "  | signature: ~W~%" signature)
	      (format *error-output* "  | message-header: ~A~%" (encode-json-to-string (message-header message)))
	      (format *error-output* "  | message-content ~A~%" (message-content message))
	      (format *error-output* "  | buffers: ~W~%" buffers)
          (let*
            ((content (parse-json-from-string (message-content message)))
             (value (afetch "value" content :test #'equal)))
            (maxima::$parse_string value)))))))
