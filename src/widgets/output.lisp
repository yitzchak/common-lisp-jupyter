(in-package #:jupyter-widgets)

(defclass output (dom-widget)
  ((msg-id
     :initarg :msg-id
     :initform ""
     :accessor widget-msg-id
     :documentation "Parent message id of messages to capture"
     :trait :unicode)
   (outputs
     :initarg :outputs
     :initform nil
     :accessor widget-outputs
     :documentation "The output messages synced from the frontend."
     :trait :list))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "OutputModel"
    :%model-module +output-module+
    :%model-module-version +output-module-version+
    :%view-name "OutputView"
    :%view-module +output-module+
    :%view-module-version +output-module-version+)
  (:documentation
"Widget used as a context manager to display output.

This widget can capture and display stdout, stderr, and rich output.  To use it,
create an instance of it and display it.

You can then use the widget as a context manager: any output produced while in
the context will be captured and displayed in the widget instead of the standard
output area.

### Example

```common-lisp
(use-package :jupyter-widgets)
(defvar out (make-instance 'output))
(with-output out
  (print \"prints to output area\")
```"))




; We should clean up after ourselves, but the messages are processed outside of this lexigraphic context.
(defmacro with-output (output &body body)
  "Evaluate body with all output sent to the output widget."
  `(with-slots (msg-id) ,output
     (unwind-protect 
       (progn
         (finish-output) 
         (finish-output *error-output*) 
         (setf msg-id (gethash "msg_id" (jupyter::message-header jupyter::*message*)))
         ,@body)
       (finish-output) 
       (finish-output *error-output*) 
       (setf msg-id "")))) 


(defclass sidecar (output)
  ((title
     :accessor widget-title
     :initarg :title
     :initform nil
     :trait :unicode))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "SidecarModel"
    :%model-module +sidecar-module+
    :%model-module-version +sidecar-module-version+
    :%view-name "SidecarView"
    :%view-module +sidecar-module+
    :%view-module-version +sidecar-module-version+)
  (:documentation
"Widget used as a context manager to display output.

This widget can capture and display stdout, stderr, and rich output.  To use it,
create an instance of it and display it.

You can then use the widget as a context manager: any output produced while in
the context will be captured and displayed in the widget instead of the standard
output area.

### Example

```common-lisp
(use-package :jupyter-widgets)
(defvar out (make-instance 'output))
(with-output out
  (print \"prints to output area\")
```"))



(defclass output-widget-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((output
     :initarg :output
     :reader output-widget-stream-output)
   (name
     :initarg :name
     :reader output-widget-stream-name)
   (value
     :initform (make-array 1024
                           :fill-pointer 0
                           :adjustable t
                           :element-type 'character)
     :accessor output-widget-stream-value)
   (column
     :accessor output-widget-stream-column
     :initform 0)))


(defun make-output-widget-stream (output &optional error-output)
  (make-instance 'output-widget-stream :output output :name (if error-output "stderr" "stdout")))


(defmethod trivial-gray-streams:stream-write-char ((stream output-widget-stream) char)
  (with-slots (output name value column) stream
    (cond
      ((graphic-char-p char)
        (incf column))
      ((or (char= #\newline)
           (char= #\return))
        (setf column 0))
      ((char= #\tab)
        (incf column 8)))
    (vector-push-extend char value)))


(defmethod trivial-gray-streams:stream-finish-output ((stream output-widget-stream))
  (with-slots (output name value column) stream
    (unless (zerop (length value))
      (let* ((outputs (coerce (widget-outputs output) 'list))
             (last-output (car (last outputs))))
        (cond
          ((and last-output
                (equal (gethash "output_type" last-output) "stream")
                (equal (gethash "name" last-output) name))
            (setf (gethash "text" last-output) (concatenate 'string (gethash "text" last-output) value))
            (notify-trait-change output :list :outputs nil (widget-outputs output) t))
          (t
            (setf (widget-outputs output)
                  (append outputs
                          (list (j:make-object "output_type" "stream"
                                               "name" name
                                               "text" (copy-seq value)))))))
          (adjust-array value (array-total-size value)
                        :fill-pointer 0)))))


(defmethod trivial-gray-streams:stream-line-column ((stream output-widget-stream))
   (output-widget-stream-column stream))

  
