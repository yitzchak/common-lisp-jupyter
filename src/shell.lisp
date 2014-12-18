(in-package #:uncommonshell)

(defclass shell-channel ()
  ((kernel :initarg :kernel :reader shell-kernel)
   (socket :initarg :socket :initform nil :accessor shell-socket)))


(defun make-shell-channel (kernel)
  (let ((socket (pzmq:socket (kernel-ctx kernel) :router)))  
    (let ((shell (make-instance 'shell-channel
                                :kernel kernel
                                :socket socket)))
      (let ((config (slot-value kernel 'config)))
        (let ((endpoint (format nil "~A://~A:~A"
                                  (config-transport config)
                                  (config-ip config)
                                  (config-shell-port config))))
          (format t "shell endpoint is: ~A~%" endpoint)
          (pzmq:bind socket endpoint)
          shell)))))

(defmacro while (condition &body body)
  (let ((eval-cond-var (gensym "eval-cond-"))
        (body-val-var (gensym "body-val-")))
    `(flet ((,eval-cond-var () ,`,condition))
       (do ((,body-val-var nil (progn ,@body)))
           ((not (,eval-cond-var))
            ,body-val-var)))))

(example (let ((count 0))
           (while (< count 10)
             (format t "~A " count)
             (incf count)
             count))
         => 10)
      
(defun shell-loop (shell)
  (let ((active t))
    (while active
      (vbinds (ids sig msg raw)  (message-recv (shell-socket shell))
        (format t "Shell Received: ~A~%" (to-json(message-header msg)))))))


                    
    
      
                              
  
