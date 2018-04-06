(in-package #:cl-jupyter)

#|

# The stdin dealer socket #

See: http://jupyter-client.readthedocs.org/en/latest/messaging.html#messages-on-the-stdin-router-dealer-sockets

|#

(defun make-stdin-channel (config ctx)
  (make-channel config
                (pzmq:socket ctx :dealer)
                (config-stdin-port config)))

#|

# Message sending functions

|#

(defun send-input-request (stdin parent-msg prompt)
  (message-send stdin
                (make-message parent-msg "input_request"
                              (jsown:new-js
                                ("prompt" prompt)))))
