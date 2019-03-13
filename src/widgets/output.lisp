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

```lisp
(use-package :jupyter-widgets)
(defvar out (make-widget 'output))
(with-output out
  (print \"prints to output area\")
```"))

(register-widget output)


; We should clean up after ourselves, but the messages are processed outside of this lexigraphic context.
(defmacro with-output (o &body body)
  "Evaluate body with all output sent to the output widget."
  `(with-slots (msg-id) ,o
    (setf msg-id (jupyter:json-getf (jupyter::message-header jupyter::*message*) "msg_id"))
    ,@body))
