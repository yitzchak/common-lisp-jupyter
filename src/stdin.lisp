(in-package #:jupyter)

#|

# The stdin dealer socket #

See: http://jupyter-client.readthedocs.org/en/latest/messaging.html#messages-on-the-stdin-router-dealer-sockets

|#

(defclass stdin-channel (channel)
  ()
  (:documentation "STDIN channel class."))

#|

# Message sending functions

|#

(defun send-input-request (stdin prompt)
  (message-send stdin
                (make-message (channel-session stdin) "input_request"
                              (list :object-plist
                                    "prompt" prompt))))

#|

stdin-stream is a bidirectional Gray stream that interprets output as prompts
and sends an input_request when finish output is called. This should work in
most cases of *query-io* usage. Makes overloading y-or-no-p unnecessary.

|#

(defvar *stdin-stream-size* 1024)

(defclass stdin-stream (trivial-gray-streams:fundamental-character-output-stream
                        trivial-gray-streams:fundamental-character-input-stream)
  ((channel
     :initarg :channel
     :reader stdin-stream-channel)
   (output
     :initarg :output
     :initform (make-array *stdin-stream-size*
                           :fill-pointer 0
                           :adjustable t
                           :element-type 'character)
     :reader stdin-stream-output)
   (input
     :initarg :input
     :initform (make-array *stdin-stream-size*
                           :fill-pointer 0
                           :adjustable t
                           :element-type 'character)
     :reader stdin-stream-input)
   (lock
     :initform (bordeaux-threads:make-lock (make-uuid))
     :reader stdin-sream-lock
     :documentation "Lock used during prompt actions")))

#+ecl
(defmethod gray:stream-interactive-p ((stream stdin-stream))
  (declare (ignore stream))
  t)

#+sbcl (defmethod interactive-stream-p ((stream stdin-stream))
  (declare (ignore stream))
  t)

(defun make-stdin-stream (stdin)
  (make-instance 'stdin-stream :channel stdin))

(defmethod trivial-gray-streams:stream-write-char ((stream stdin-stream) char)
  (vector-push-extend char (stdin-stream-output stream)))

(defun prompt-and-read (stream need-input)
  (with-slots (channel output input lock) stream
    (when (bordeaux-threads:acquire-lock lock nil)
      (unwind-protect
          (let ((trimmed-output (copy-seq (string-trim '(#\Bel) output))))
            (when (or need-input
                      (not (zerop (length (string-trim '(#\Newline) trimmed-output)))))
              (setf (fill-pointer (stdin-stream-input stream)) 0)
              (setf (fill-pointer output) 0)
              (finish-output)
              (send-input-request channel trimmed-output)
              (loop until (message-available-p channel)
                    do (sleep 0.1))
              (loop with value = (gethash "value" (message-content (message-recv channel)))
                    for i from (1- (length value)) downto 0
                    initially (vector-push-extend #\Newline input)
                    do (vector-push-extend (char value i) input))))
        (bordeaux-threads:release-lock lock)))))

(defmethod trivial-gray-streams:stream-clear-input ((stream stdin-stream))
  (setf (fill-pointer (stdin-stream-input stream)) 0)
  nil)

(defmethod trivial-gray-streams:stream-listen ((stream stdin-stream))
  (not (zerop (length (stdin-stream-input stream)))))

(defmethod trivial-gray-streams:stream-read-char ((stream stdin-stream))
  (let ((input (stdin-stream-input stream)))
    (when (zerop (length input))
      (prompt-and-read stream t))
    (vector-pop input)))

(defmethod trivial-gray-streams:stream-peek-char ((stream stdin-stream))
  (let ((input (stdin-stream-input stream)))
    (when (zerop (length input))
      (prompt-and-read stream t))
    (elt input (- (length input) 1))))

(defmethod trivial-gray-streams:stream-unread-char ((stream stdin-stream) char)
  (vector-push-extend char (stdin-stream-input stream)))

(defmethod trivial-gray-streams:stream-line-column ((stream stdin-stream))
   nil)
