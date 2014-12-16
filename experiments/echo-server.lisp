
(ql:quickload "pzmq")

(defvar *zmq-ctx* (pzmq::ctx-new))

(defvar *responder* (pzmq::socket *zmq-ctx* :rep))

(pzmq::bind *responder* "tcp://*:5554")


(defun main-loop ()
  (loop
     (let ((buffer (pzmq::recv-string *responder* :dontwait nil)))
       (format t "[Echo]: ~A~%" buffer)
       (pzmq::send *responder* buffer))))






