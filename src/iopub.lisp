(in-package #:jupyter)

#|

# The IOPUB publish/subscribe channel #

|#

(defclass iopub-channel (channel)
  ()
  (:documentation "IOPUB channel class."))

#|

# Message sending functions

|#

(defun send-status (iopub status)
  (message-send iopub
                (make-message (channel-session iopub) "status"
                              (list :object-plist
                                    "execution_state" status))))

(defun send-clear-output (iopub wait)
  (message-send iopub
                (make-message (channel-session iopub) "clear_output"
                              (list :object-plist
                                    "wait" (if wait :true :false)))))


(defun send-display-data (iopub data &optional metadata transient update)
  (message-send iopub
                (make-message (channel-session iopub)
                              (if update
                                "update_display_data"
                                "display_data")
                              (list :object-plist
                                    "data" data
                                    "metadata" (or metadata :empty-object)
                                    "transient" (or transient :empty-object)))))


(defun send-execute-code (iopub execution-count code)
  (message-send iopub
                (make-message (channel-session iopub) "execute_input"
                              (list :object-plist
                                    "code" code
                                    "execution_count" execution-count))))


(defun send-execute-result (iopub execution-count data &optional metadata)
  (message-send iopub
                (make-message (channel-session iopub) "execute_result"
                              (list :object-plist
                                    "execution_count" execution-count
                                    "data" data
                                    "metadata" (or metadata :empty-object)))))


(defun send-execute-error (iopub ename evalue &optional traceback)
  (message-send iopub
                (make-message (channel-session iopub) "error"
                              (list :object-plist
                                    "ename" ename
                                    "evalue" evalue
                                    "traceback" (or traceback :empty-array)))))


(defun send-stream (iopub stream-name data)
  (message-send iopub
                (make-message (channel-session iopub) "stream"
                              (list :object-plist
                                    "name" stream-name
                                    "text" data))))


(defvar *iopub-stream-size* 1024)

(defclass iopub-stream (trivial-gray-streams:fundamental-character-output-stream
                        trivial-gray-streams:fundamental-character-input-stream)
  ((channel :initarg :channel
            :reader iopub-stream-channel)
   (name :initarg :name
         :reader iopub-stream-name)
   (value :initarg :value
          :initform (make-array *iopub-stream-size*
                                :fill-pointer 0
                                :adjustable t
                                :element-type 'character)
          :reader iopub-stream-value)
   (prompt-prefix :initarg :prompt-prefix
                  :reader iopub-stream-prompt-prefix)
   (prompt-suffix :initarg :prompt-suffix
                  :reader iopub-stream-prompt-suffix)))

(defun make-iopub-stream (iopub name prompt-prefix prompt-suffix)
  (make-instance 'iopub-stream :channel iopub
                               :name name
                               :prompt-prefix prompt-prefix
                               :prompt-suffix prompt-suffix))

(defmethod trivial-gray-streams:stream-write-char ((stream iopub-stream) char)
  (unless (equal char #\Sub) ; Ignore subsititute characters
    (with-slots (channel name value prompt-prefix prompt-suffix) stream
      (vector-push-extend char value)
      ;; After the character has been added look for a prompt terminator at the
      ;; end.
      (if (alexandria:ends-with-subseq prompt-suffix value)
        (let ((start (search prompt-prefix value)))
          ;; If there is a prompt start also then print the prompt and remove it
          ;; from the buffer.
          (when start
            ;; If there is data before the prompt then send it now.
            (unless (zerop start)
              (send-stream channel name (subseq value 0 start)))
            (write-string (subseq value
                                  (+ start (length prompt-prefix))
                                  (- (length value) (length prompt-suffix)))
                          *query-io*)
            (finish-output *query-io*)
            (adjust-array value (array-total-size value)
                          :fill-pointer 0)))))))

(defmethod trivial-gray-streams:stream-finish-output ((stream iopub-stream))
  (with-slots (channel name value prompt-prefix) stream
    (unless (or (zerop (length value))
                (search prompt-prefix value))
      (send-stream channel name value)
      (adjust-array value (array-total-size value)
                    :fill-pointer 0))))

;; Forward all read calls to *query-io*
(defmethod trivial-gray-streams:stream-listen ((stream iopub-stream))
  (trivial-gray-streams:stream-listen *query-io*))

(defmethod trivial-gray-streams:stream-read-char ((stream iopub-stream))
  (trivial-gray-streams:stream-read-char *query-io*))

(defmethod trivial-gray-streams:stream-peek-char ((stream iopub-stream))
  (trivial-gray-streams:stream-peek-char *query-io*))

(defmethod trivial-gray-streams:stream-unread-char ((stream iopub-stream) char)
  (trivial-gray-streams:stream-unread-char *query-io* char))

(defmethod trivial-gray-streams:stream-line-column ((stream iopub-stream))
   nil)


