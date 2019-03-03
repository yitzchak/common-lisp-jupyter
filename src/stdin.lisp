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

(defun send-input-request (stdin parent-msg prompt)
  (message-send stdin
                (make-message parent-msg "input_request"
                              (jsown:new-js
                                ("prompt" prompt)))))

#|

stdin-stream is a bidirectional Gray stream that interprets output as prompts
and sends an input_request when finish output is called. This should work in
most cases of *query-io* usage. Makes overloading y-or-no-p unnecessary.

|#

(defvar *stdin-stream-size* 1024)

(defclass stdin-stream (trivial-gray-streams:fundamental-character-output-stream
                        trivial-gray-streams:fundamental-character-input-stream)
  ((channel :initarg :channel
            :reader stdin-stream-channel)
   (parent-msg :initarg :parent-msg
               :reader stdin-stream-parent-msg)
   (output :initarg :output
           :initform (make-array *stdin-stream-size*
                                 :fill-pointer 0
                                 :adjustable t
                                 :element-type 'character)
           :reader stdin-stream-output)
   (input :initarg :input
          :initform (make-array *stdin-stream-size*
                                :fill-pointer 0
                                :adjustable t
                                :element-type 'character)
          :reader stdin-stream-input)))

(defmethod interactive-stream-p ((stream stdin-stream))
  (declare (ignore stream))
  t)

(defun make-stdin-stream (stdin parent-msg)
  (make-instance 'stdin-stream :channel stdin
                               :parent-msg parent-msg))

(defmethod trivial-gray-streams:stream-write-char ((stream stdin-stream) char)
  (vector-push-extend char (stdin-stream-output stream)))

(defun prompt-and-read (stream)
  (with-slots (channel parent-msg output input) stream
    (let ((trimmed-output (copy-seq (string-trim '(#\Bel) output))))
      (adjust-array output *stdin-stream-size* :fill-pointer 0)
      (unless (zerop (length (string-trim '(#\Newline) trimmed-output)))
        (send-input-request channel parent-msg trimmed-output)
        (let ((value (concatenate 'string
                       (jsown:val (message-content (message-recv channel)) "value")
                       '(#\Newline))))
          (adjust-array input (length value)
                        :fill-pointer (length value)
                        :initial-contents (reverse value)))))))

(defmethod trivial-gray-streams:stream-finish-output ((stream stdin-stream))
  (prompt-and-read stream))

(defmethod trivial-gray-streams:stream-listen ((stream stdin-stream))
  (> (length (stdin-stream-input stream)) 0))

(defmethod trivial-gray-streams:stream-read-char ((stream stdin-stream))
  (let ((input (stdin-stream-input stream)))
    (when (zerop (length input))
      (prompt-and-read stream))
    (if (zerop (length input))
      :eof
      (vector-pop input))))

(defmethod trivial-gray-streams:stream-peek-char ((stream stdin-stream))
  (let ((input (stdin-stream-input stream)))
    (if (zerop (length input))
      :eof
      (elt input (- (length input) 1)))))

(defmethod trivial-gray-streams:stream-unread-char ((stream stdin-stream) char)
  (vector-push-extend char (stdin-stream-input stream)))
