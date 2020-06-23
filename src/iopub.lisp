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
                              `(:object
                                 ("execution_state" . ,status)))))

(defun send-clear-output (iopub parent-msg wait)
  (message-send iopub
                (make-message (channel-session iopub) "clear_output"
                              `(:object
                                 ("wait" . ,(if wait :true :false)))
                              :parent parent-msg)))

(defun send-status-update (iopub parent-msg status)
  (message-send iopub
                (make-message (channel-session iopub) "status"
                              `(:object
                                 ("execution_state" . ,status))
                              :parent parent-msg)))

(defun send-display-data (iopub parent-msg data)
  (message-send iopub
                (make-message (channel-session iopub) "display_data"
                              `(:object
                                 ("data" . ,data)
                                 ("metadata" . :empty-object))
                              :parent parent-msg)))

(defun send-execute-code (iopub parent-msg execution-count code)
  (message-send iopub
                (make-message (channel-session iopub) "execute_input"
                              `(:object
                                 ("code" . ,code)
                                 ("execution_count" . ,execution-count))
                              :parent parent-msg)))

(defun send-execute-result (iopub parent-msg execution-count data)
  (message-send iopub
                (make-message (channel-session iopub) "execute_result"
                              `(:object
                                 ("execution_count" . ,execution-count)
                                 ("data" . ,data)
                                 ("metadata" . :empty-object))
                              :parent parent-msg)))

(defun send-execute-error (iopub parent-msg ename evalue)
  (message-send iopub
                (make-message (channel-session iopub) "error"
                              `(:object
                                 ("ename" . ,ename)
                                 ("evalue" . ,evalue)
                                 ("traceback" . :empty-array))
                              :parent parent-msg)))

(defun send-stream (iopub parent-msg stream-name data)
  (message-send iopub
                (make-message (channel-session iopub) "stream"
                              `(:object
                                 ("name" . ,stream-name)
                                 ("text" . ,data))
                              :parent parent-msg)))

(defun send-comm-close-orphan (iopub comm-id &optional data)
  (message-send iopub
                (make-message (channel-session iopub) "comm_close"
                              `(:object
                                 ("comm_id" . comm-id)
                                 ("data" . ,(or data :empty-object))))))

(defvar *iopub-stream-size* 1024)

(defclass iopub-stream (trivial-gray-streams:fundamental-character-output-stream
                        trivial-gray-streams:fundamental-character-input-stream)
  ((channel :initarg :channel
            :reader iopub-stream-channel)
   (parent-msg :initarg :parent-msg
               :reader iopub-stream-parent-msg)
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

(defun make-iopub-stream (iopub parent-msg name prompt-prefix prompt-suffix)
  (make-instance 'iopub-stream :channel iopub
                               :parent-msg parent-msg
                               :name name
                               :prompt-prefix prompt-prefix
                               :prompt-suffix prompt-suffix))

(defmethod trivial-gray-streams:stream-write-char ((stream iopub-stream) char)
  (unless (equal char #\Sub) ; Ignore subsititute characters
    (with-slots (channel parent-msg name value prompt-prefix prompt-suffix) stream
      (vector-push-extend char value)
      ;; After the character has been added look for a prompt terminator at the
      ;; end.
      (if (ends-with-subseq prompt-suffix value)
        (let ((start (search prompt-prefix value)))
          ;; If there is a prompt start also then print the prompt and remove it
          ;; from the buffer.
          (when start
            ;; If there is data before the prompt then send it now.
            (unless (zerop start)
              (send-stream channel parent-msg name (subseq value 0 start)))
            (write-string (subseq value
                                  (+ start (length prompt-prefix))
                                  (- (length value) (length prompt-suffix)))
                          *query-io*)
            (finish-output *query-io*)
            (adjust-array value (array-total-size value)
                          :fill-pointer 0)))))))

(defmethod trivial-gray-streams:stream-finish-output ((stream iopub-stream))
  (with-slots (channel parent-msg name value) stream
    (unless (zerop (length value))
      (send-stream channel parent-msg name value)
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
