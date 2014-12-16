
(ql:quickload "pzmq")

(defvar *zmq-ctx* (pzmq::ctx-new))

(defvar *requester* (pzmq::socket *zmq-ctx* :req))

(pzmq::connect *requester* "tcp://localhost:5554")


(pzmq::send *requester* "hello")

