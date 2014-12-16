

(defun hwclient (&optional (server-address "tcp://localhost:5555"))
  "Translation of http://zguide.zeromq.org/c:hwclient updated for ZMQ 3.  Includes some parameters in with-* macros to demonstrate syntax."
  (pzmq:with-context (ctx :max-sockets 10)
    (pzmq:with-socket (requester ctx) (:req :affinity 3 :linger 100)
      ;; linger is important in case of (keyboard) interrupt;
      ;; see http://api.zeromq.org/3-3:zmq-ctx-destroy
      (write-line "Connecting to hello world server...")
      (pzmq:connect requester server-address)
      (dotimes (i 10)
        (format t "Sending Hello ~d...~%" i)
        (pzmq:send requester "Hello")
        (write-string "Receiving... ")
        (write-line (pzmq:recv-string requester))))))


